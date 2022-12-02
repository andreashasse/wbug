-module(wbug).

%% api
-export([line/2]).

%% manual testing
-export([manual/0, test_new/0]).

%%@doc setups up a breakpoint and a callback
line(Module, Line) ->
    int:i(Module),
    int:break(Module, Line),
    int:auto_attach([break], {wbug_srv, attached, [Module]}).

test_new() ->
    wbug_srv:start(),
    line(wbug_test, 6),
    spawn(fun wbug_test:calls/0),
    wbug_shell_listner:start().


manual() ->
    line(wbug_test, 6),
    spawn(fun() ->
                  wbug_test:calls(),
                  io:format("End of wbug:manual/0~n")
          end).
