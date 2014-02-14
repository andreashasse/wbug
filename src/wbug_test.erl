-module(wbug_test).

-compile(export_all).

calls() ->
    io:format("Start of debug fun ~p~n", [self()]),
    CallArg = 3,
    CallRes = test(CallArg),
    CallRes + 1,
    io:format("End of debug fun ~p~n", [self()]).

test(Arg) ->
    A = Arg + 2,
    hmm,
    A.
