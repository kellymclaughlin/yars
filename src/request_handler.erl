-module(request_handler).

%-export([]).
-compile([export_all]).


out(Arg, SC, GC) ->
    out404(Arg, SC, GC).
    
out404(Arg, SC, GC) ->
    %%TODO: Process request
    ok.
