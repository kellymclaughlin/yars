-module(yars).

%-export([]).
-compile([export_all]).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include_lib("kernel/include/file.hrl").

out(Arg, GC, SC) -> out404(Arg, GC, SC).  

out404(Arg, GC, SC) ->
    %% Process request
	{RequestTime, Result} = timer:tc(?MODULE, yaws_process_request, [pg2_ext:get_next_pid(request_pool), Arg, GC, SC]),
	io:format("Processed request in ~w seconds~n", [RequestTime/1000000]),
	Result.

start(SConf) ->
    RequestPoolSize = list_to_integer(proplists:get_value("request_pool_size", SConf#sconf.opaque, 10)),

	%% Create a process group for the request pool
	pg2_ext:create(request_pool),

    %% Spawn process to add instance managers for ruby instances
    spawn(?MODULE, add_instance_managers, [RequestPoolSize, SConf]),

    %% Spawn process to periodically check for changes 
    %% to relevant data in the server configuration.
    spawn(?MODULE, update_configuration, [RequestPoolSize]),
    ok.

update_configuration(RequestPoolSize) ->
    timer:sleep(timer:seconds(10)),
    {ok, _, [[SConf]]} = yaws_api:getconf(),
    NewRequestPoolSize = list_to_integer(proplists:get_value("request_pool_size", SConf#sconf.opaque, 10)),
    case RequestPoolSize = NewRequestPoolSize of
        false ->
            case RequestPoolSize < NewRequestPoolSize of
                true ->
                    %% Start (NewRequestPoolSize-RequestPoolSize) new instance managers
                    add_instance_managers(NewRequestPoolSize-RequestPoolSize);
                false ->
                    %% Stop (RequestPoolSize-NewRequestPoolSize) instance managers
                    stop_instance_managers(RequestPoolSize-NewRequestPoolSize)
            end,
            update_configuration(NewRequestPoolSize);
        _ ->
            update_configuration(RequestPoolSize)
    end.
    
add_instance_managers(Count) ->
    {ok, _, SConf} = yaws_api:getconf(),
    add_instance_managers(Count, SConf).

add_instance_managers(Count, SConf) ->
    RailsEnv = proplists:get_value("rails_env", SConf#sconf.opaque, "production"),
    WaitThreshold = list_to_integer(proplists:get_value("request_wait_threshold", SConf#sconf.opaque, "2000")),
    MaxRequests = list_to_integer(proplists:get_value("max_requests", SConf#sconf.opaque, 100)),

    %%Get the rack application location
    AppRoot = case lists:reverse(SConf#sconf.docroot) of
        "cilbup/" ++ Rest ->
            lists:reverse(Rest);
        _ ->
            SConf#sconf.docroot
    end,
    
    Cmd = lists:flatten(io_lib:format("bundle exec \"ruby ./src/rack_instance.rb -r ~s -e ~s\"", [AppRoot, RailsEnv])),
	[spawn(?MODULE, init_instance_manager, [Cmd, WaitThreshold, MaxRequests]) || _ <- lists:seq(1, Count)].

init_instance_manager(Cmd, WaitThreshold, MaxRequests) ->
	%% Spawn a process to run the rack instance
	RackPid = spawn_ruby_instance(Cmd),
	%% Join the rack instance manager to the process group
	case pg2_ext:join(request_pool, self()) of
		{error, Error} ->
			io:format("Error occurred trying to join process group: ~p~n", [Error]);
		_ ->
			ok
	end,

	%% Begin instance manager loop
	instance_manager_loop(RackPid, {idle, []}, WaitThreshold, Cmd, {0, MaxRequests}).
	
spawn_ruby_instance(Cmd) ->
    spawn(fun() ->
      process_flag(trap_exit, true),
      Port = open_port({spawn, Cmd}, [{packet, 4}, nouse_stdio, exit_status, binary]),
      port_loop(Port, 10000, "cmd")
    end).

stop_instance_managers(Count) ->
    %% Terminate message to Count instance managers
    TerminateFunc = fun() -> 
        %% Get a pid and send a terminate request to it
        Pid = pg2_ext:get_next_pid(request_pool),
        Pid ! terminate,
        %% Remove this instance manager from the pool
        pg2_ext:leave(request_pool, Pid)
    end,
    [TerminateFunc || _ <- lists:seq(1, Count)]. 

%% Instance manager may be in one of three states: idle, busy, or clogged.
%%   idle - the instance manager is not managing any requests
%%   busy - the instance manager is waiting on a request to complete
%%   clogged - the current request being processed by the instance manager
%%             has taken longer than a pre-configured wait threshold. This
%%             value is set in the yaws configuration file.
%% TODO: Maybe use gen_fsm to model instance_manager behavior
instance_manager_loop(RackPid, State, WaitThreshold, RubyCmd, {MaxRequests, MaxRequests}) ->
    %% The configured maximum request count has been reached
    %% for this rack instance. Terminate the rack instance
    %% and restart a new one.
    RackPid ! {self(), terminate},
	NewRackPid = spawn_ruby_instance(RubyCmd),
    instance_manager_loop(NewRackPid, State, WaitThreshold, RubyCmd, {0, MaxRequests});
instance_manager_loop(RackPid, {idle, []}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests}) ->
	receive
        {Requester, {request, Req}} -> 
			RackPid ! {self(), Requester, {request, Req}},
            {ok, TimerRef} = timer:send_after(WaitThreshold, threshold_elapsed),    
			instance_manager_loop(RackPid, {busy, [{timer, TimerRef}]}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests});
        terminate -> 
			RackPid ! {self(), terminate};
        _ ->
			instance_manager_loop(RackPid, {idle, []}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests})
            
	end;
instance_manager_loop(RackPid, {busy, []}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests}) ->
    instance_manager_loop(RackPid, {idle, []}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests});
instance_manager_loop(RackPid, {busy, [{timer, TimerRef} | QueuedRequests]}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests}) ->
	receive
		{Requester, {request, Req}} -> 
			instance_manager_loop(RackPid, {busy, [{timer, TimerRef}, {Requester, {request, Req}} | QueuedRequests]}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests});
		{RackPid, Requester, Result} -> 
            %% Received response from current request.
            %% Cancel the timer for the request
            timer:cancel(TimerRef),

            %% Notify the requesting process
			Requester ! {self(), Result},

            %% Begin processing the next request if there
            %% are any queued. Reverse the list of queued
            %% requests because new requests are added to
            %% the front of the list.
            case lists:reverse(QueuedRequests) of
                [] ->
			        instance_manager_loop(RackPid, {idle, []}, WaitThreshold, _RubyCmd, {RequestCount+1, _MaxRequests});
                [{NextRequester, {request, NextRequest}} | OtherRequests] ->
                    RackPid ! {self(), NextRequester, {request, NextRequest}},
                    {ok, NextTimerRef} = timer:send_after(WaitThreshold, threshold_elapsed),    
			        instance_manager_loop(RackPid, {busy, [{timer, NextTimerRef} | OtherRequests]}, WaitThreshold, _RubyCmd, {RequestCount+1, _MaxRequests})
            end;
        terminate ->
			instance_manager_loop(RackPid, {busy, [{timer, TimerRef}, terminate | QueuedRequests]}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests});
        threshold_elapsed ->
			instance_manager_loop(RackPid, {clogged, [{timer, TimerRef} | QueuedRequests]}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests})
	end;
instance_manager_loop(RackPid, {busy, QueuedRequests}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests}) ->
	receive
		{Requester, {request, Req}} -> 
			instance_manager_loop(RackPid, {busy, [{Requester, {request, Req}} | QueuedRequests]}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests});
		{RackPid, _Requester, _Result} -> 
            %% Begin processing the next request if there
            %% are any queued. Reverse the list of queued
            %% requests because new requests are added to
            %% the front of the list.
            case lists:reverse(QueuedRequests) of
                [] ->
			        instance_manager_loop(RackPid, {idle, []}, WaitThreshold, _RubyCmd, {RequestCount+1, _MaxRequests});
                [{NextRequester, {request, NextRequest}} | OtherRequests] ->
                    RackPid ! {self(), NextRequester, {request, NextRequest}},
                    {ok, NextTimerRef} = timer:send_after(WaitThreshold, threshold_elapsed),    
			        instance_manager_loop(RackPid, {busy, [{timer, NextTimerRef} | OtherRequests]}, WaitThreshold, _RubyCmd, {RequestCount+1, _MaxRequests});
                [terminate] ->
                    done;
                [terminate | _OtherRequests] ->
                    %% This is the case where a request for this 
                    %% instance manager was received after the terminate
                    %% request. This should happen very infrequently and 
                    %% for simplicity we'll reverse the lis of queued
                    %% requests and process the most recent requests first
                    %% and finally get back to the terminate message.
			        instance_manager_loop(RackPid, {busy, lists:reverse(QueuedRequests)}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests})
            end;
		terminate -> 
			instance_manager_loop(RackPid, {busy, [terminate | QueuedRequests]}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests});
        threshold_elapsed ->
			instance_manager_loop(RackPid, {busy, QueuedRequests}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests})
	end;
instance_manager_loop(RackPid, {clogged, []}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests}) ->
    receive
		{Requester, {request, Req}} -> 
            pg2_ext:get_next_pid(request_pool) ! {Requester, {request, Req}},
			instance_manager_loop(RackPid, {clogged, []}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests});
		{RackPid, Requester, Result} -> 
            %% Received response from current request.
            %% Notify the requesting process
			Requester ! {self(), Result},
            instance_manager_loop(RackPid, {busy, []}, WaitThreshold, _RubyCmd, {RequestCount+1, _MaxRequests});
		terminate -> 
            pg2_ext:get_next_pid(request_pool) ! terminate,
			instance_manager_loop(RackPid, {clogged, []}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests})
    end;
instance_manager_loop(RackPid, {clogged, [_TimerRef | QueuedRequests]}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests}) ->
    %% Send all requests queued in RequestList to be handled
    %% by the backup request pool of ruby instances.
    [pg2_ext:get_next_pid(request_pool) ! QueuedRequestData || QueuedRequestData <- lists:reverse(QueuedRequests)],

    receive
		{Requester, {request, Req}} -> 
            pg2_ext:get_next_pid(request_pool) ! {Requester, {request, Req}},
			instance_manager_loop(RackPid, {clogged, []}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests});
		{RackPid, Requester, Result} -> 
            %% Received response from current request.
            %% Notify the requesting process
			Requester ! {self(), Result},
            instance_manager_loop(RackPid, {busy, []}, WaitThreshold, _RubyCmd, {RequestCount+1, _MaxRequests});
		terminate -> 
            pg2_ext:get_next_pid(request_pool) ! terminate,
			instance_manager_loop(RackPid, {clogged, []}, WaitThreshold, _RubyCmd, {RequestCount, _MaxRequests})
    end.

port_loop(Port, Timeout, Command) ->
  receive
    {Source, Requester, {request, Req}} -> 
      port_command(Port, term_to_binary({request, Req})),
      receive
        {Port, {data, Result}} ->
          DB = binary_to_term(Result),
          case DB of
            {last_result, X} ->
              Source ! {self(), Requester, {result, X}},
              port_close(Port),
              exit(last_result);
            Z -> 
              Source ! {self(), Requester, Z}
          end
      after Timeout ->
        error_logger:error_msg("Port ~p timed out while procesing a request (~p)!~n", [self(),Req]),
        % Timed out so close and then restart the port
        port_close(Port),
        exit(timed_out)
      end,
      port_loop(Port, Timeout, Command);
    terminate ->
      io:format("terminate~n"),
      port_close(Port),
      exit(terminate);
    {Port, {exit_status, Code}} ->
      io:format("exit status ~p~n",[Code]),
      % Hard and Unanticipated Crash
      error_logger:error_msg( "Port closed! ~p~n", [Port] ),
      exit({error, Code});
    {'EXIT',_Pid,shutdown} ->
      io:format("EXIT~n"),
      port_close(Port),
      exit(shutdown);
    Any -> 
      error_logger:warning_msg("Port ~p got unexpected message: ~p~n", [self(), Any]),
      port_loop(Port, Timeout, Command)
  end.

yaws_process_request({no_process, _}, _Arg, _GC, _SC) ->
	%% Process has gone away so try to another one
	yaws_process_request(pg2_ext:get_next_pid(request_pool), _Arg, _GC, _SC);
yaws_process_request({no_such_group, _}, _Arg, _GC, _SC) ->
  [{status, 503}, {html, "There are no rack instances available to process your request."}];
yaws_process_request(RackInstance, Arg, _GC, SC) ->
  io:format("Using ~p to process request~n", [RackInstance]),
  Parameters = [{request, {struct, yaws_process_arg(Arg, SC)}}],
  case execute_request(RackInstance, Parameters) of
    {Status, [], Message} -> [{status, Status}, {html, Message}];
    {Status, Headers, Message} -> [{status, Status}, {allheaders, Headers}, {html, Message}]
  end.

yaws_process_arg(Arg, SC) ->
  Headers = Arg#arg.headers,
  [{method, yaws_prepare(method, Arg)},
   {http_version, yaws_prepare(http_version, Arg)},
   {https, determine_ssl(SC)},
   {remote_addr, yaws_prepare(remote_addr, Arg)},
   {querypath, yaws_prepare(querypath, Arg)},
   {querydata, prep(Arg#arg.querydata)}, 
   {servername, prep(SC#sconf.servername)},
   {headers, {struct, yaws_prepare_headers(Headers)}},
   {cookies, {array, lists:map(fun(X) -> prep(X) end, Headers#headers.cookie)}},
   {pathinfo, prep(SC#sconf.docroot)},
   {postdata, Arg#arg.clidata}].
  
yaws_prepare(method, Arg) ->
  {http_request, Method, {_Type, _Path}, _Version} = Arg#arg.req,
  Method;
yaws_prepare(http_version, Arg) ->
  {http_request, _Method, {_Type, _Path}, Version} = Arg#arg.req,
  {array, tuple_to_list(Version)};
yaws_prepare(querypath, Arg) ->
  {http_request, _Method, {_Type, Path}, _Version} = Arg#arg.req,
  prep(Path);
yaws_prepare(remote_addr, Arg) ->
  Socket = Arg#arg.clisock,
  try
    get_remote_addr(Socket)
  catch
    _:_ -> ok
  end.
  
get_remote_addr(Socket) ->
  Peer = inet:peername(Socket),
  case Peer of
    {ok, {AddressIntegerTuple, _Port}} ->
      AddressIntegerList = tuple_to_list(AddressIntegerTuple),
      AddressStringList = lists:map(fun(X) -> integer_to_list(X) end, AddressIntegerList),
      Address = string:join(AddressStringList, "."),
      prep(Address);
    _Else ->
      "0.0.0.0"
  end.

yaws_prepare_headers(Headers) ->
  NormalHeaders = [{connection, prep(Headers#headers.connection)},
                   {accept, prep(Headers#headers.accept)},
                   {host, prep(Headers#headers.host)},
                   {if_modified_since, prep(Headers#headers.if_modified_since)},
                   {if_match, prep(Headers#headers.if_match)},
                   {if_none_match, prep(Headers#headers.if_none_match)},
                   {if_range, prep(Headers#headers.if_range)},
                   {if_unmodified_since, prep(Headers#headers.if_unmodified_since)},
                   {range, prep(Headers#headers.range)},
                   {referer, prep(Headers#headers.referer)},
                   {user_agent, prep(Headers#headers.user_agent)},
                   {accept_ranges, prep(Headers#headers.accept_ranges)},
                   {keep_alive, prep(Headers#headers.keep_alive)},
                   {location, prep(Headers#headers.location)},
                   {content_length, prep(Headers#headers.content_length)},
                   {content_type, prep(Headers#headers.content_type)},
                   {content_encoding, prep(Headers#headers.content_encoding)},
                   {authorization, prep_authorization(Headers#headers.authorization)},
                   {transfer_encoding, prep(Headers#headers.transfer_encoding)}],
  SpecialHeaders = 
    lists:map(fun({http_header, _Len, Name, _, Value}) -> {prep(Name), prep(Value)} end, 
              Headers#headers.other),
  [{Name, Res} || {Name, Res} <- NormalHeaders, Res /= undefined] ++ SpecialHeaders.

determine_ssl(SC) ->
  case SC#sconf.ssl of
    undefined -> 0;
    _Else -> 1
  end.
  
execute_request(InstanceManager, Parameters) ->
  Request = prepare_parameters(Parameters),
  InstanceManager ! {self(), {request, Request}},
  receive
    {_, {result, Result}} -> result_processor(Result);
    {_, {error, Result}} ->
      error_logger:info_msg("500 Internal Server Error: ~p~n", [Result]),
      {500, [], "Internal Server Error due to failed response."};
    Other ->
      error_logger:info_msg("501 Other Server Error: ~p~n", [Other]),
      {501, [], "Other Server Error due to failed response."}
  end.

result_processor({response, {{status, Status}, {allheaders, HeaderTuple}, {html, ResultBinary}}}) ->
  ProcessedHeaderList = lists:map(fun({header, Name, Value}) -> {header, [binary_to_list(Name) ++ ":", binary_to_list(Value)]} end, tuple_to_list(HeaderTuple)),
  {Status, ProcessedHeaderList, binary_to_list(ResultBinary)}.

prep_authorization({_User, _Pass, Auth}) ->
  list_to_binary(Auth);
prep_authorization(Any) ->
  Any.

prep(undefined) ->
    undefined;
prep(A) when is_list(A) -> 
    list_to_binary(A);
prep(A) -> A.

prepare_parameters(L) when is_list(L) ->
  [{K,prepare_pvalue(V)} || {K,V} <- L].

prepare_pvalue({struct, L}) when is_list(L) -> {struct, [prepare_pvalue(X) || X <- L]};
prepare_pvalue({array, L}) when is_list(L) -> {array, [prepare_pvalue(X) || X <- L]};
prepare_pvalue({Atom, V}) when is_atom(Atom) -> {atom_to_binary(Atom, utf8), prepare_pvalue(V)};
prepare_pvalue(L) when is_list(L) ->  list_to_binary(xmerl_ucs:to_utf8(L));
prepare_pvalue(V) -> V.
