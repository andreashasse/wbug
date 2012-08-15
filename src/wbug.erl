%%% @author Andreas Hasselberg <andreas.hasselberg@wooga.com>
%%% @doc
%%%  A debugger built at wooga.
%%% @end
%%% Created : 25 Jul 2012 by Andreas Hasselberg <andreas.hasselberg@wooga.com>

-module(wbug).

%% api
-export([line/2]).

%% manual testing
-export([manual/0]).

%% callback for the interpreter
-export([attached/2]).

%%@doc setups up a breakpoint and a callback
line(Module, Line) ->
    int:i(Module),
    int:break(Module, Line),
    int:auto_attach([break], {?MODULE, attached, [Module]}).

%%@doc callback that starts the debugging
attached(Pid, Module) ->
    Contents = int:contents(Module, Pid),
    {ok, Meta} = int:attached(Pid),
    int:meta(Meta, trace, true),
    dbg_message(Meta, Contents).

%% ---------------------------------------------------------------------------
%% Main loop
%%
%% never ending loop, it stop when a linked processes dies.

dbg_message(Meta, Contents) ->
    receive
        {Meta, {attached, _Module, _Line, _}} -> ok;
        {Meta, {break_at, Module, Line, _}} ->
            wbug_fmt:print_debug(Meta, Module, Line, Contents),
            get_input(Meta, Contents);
        %% some messages that I don't care about
        {Meta, {trace, _}} -> ok;
        {Meta, {trace_output, _}} -> ok;
        {Meta, idle} -> ok;
        {_, {exit_at,_,_,_}} -> ok;
        {Meta, running} -> ok;
        {Meta, Cmd} ->
            io:format("Not handled message ~p~n", [Cmd]),
            ok
    end,
    dbg_message(Meta, Contents).

%% ---------------------------------------------------------------------------
%% Input handling

get_input(Meta, Contents) ->
    case rm_last(io:get_line("cmd>")) of
        "c" -> int:meta(Meta, continue);
        "n" -> int:meta(Meta, next);
        "s" -> int:meta(Meta, stop);
        _   -> io:format("heh? (c -> continue, n -> next)~n"),
               get_input(Meta, Contents)
    end.

rm_last(Str) ->
    lists:sublist(Str, 1, length(Str)-1).

manual() ->
    line(wbug_test, 15),
    spawn(fun() ->
                  wbug_test:calls(),
                  io:format("End of wbug:manual/0~n")
          end).
