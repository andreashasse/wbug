%%% @author Andreas Hasselberg <andreas.hasselberg@wooga.com>
%%% @copyright (C) 2012, Andreas Hasselberg
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2012 by Andreas Hasselberg <andreas.hasselberg@wooga.com>

-module(wbug_test).

-compile(export_all).



calls() ->
    CallArg = 3,
    CallRes = test(CallArg),
    CallRes + 1.

test(Arg) ->
    A = Arg + 2,
    hmm,
    A.
