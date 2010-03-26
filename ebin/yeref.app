{application, message_store, 
 [
  {description, "An adapter for running Rack applications behind Yaws."},
  {vsn, "0.0.1"},
  {modules, []},
  {registered, [yeref]},
  {env, [
    {version, "0.0.1"},
    {working_dir, "/usr/lib/erlang/lib/yeref-0.0.1"}
  ]},
  {applications, [kernel, stdlib, sasl, inets]},
  {mod, {message_store, []}},
  {start_phases, []}
 ]}.

