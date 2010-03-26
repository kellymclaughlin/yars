-module(yeref).

%-export([]).
-compile([export_all]).

-include("yaws.hrl").

start(SConf) ->
    RequestPoolSize = list_to_integer(proplists:get_value("request_pool_size", SConf#sconf.opaque, 10)),
    SlowRequestPoolSize = list_to_integer(proplists:get_value("slow_request_pool_size", SConf#sconf.opaque, 10)),

    %%TODO: Start rack application instances 
    ok.

