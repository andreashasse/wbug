%%% @author Andreas Hasselberg <andreas.hasselberg@wooga.com>
%%% @copyright (C) 2012, Andreas Hasselberg
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2012 by Andreas Hasselberg <andreas.hasselberg@wooga.com>

-module(wbug_fmt).

-export([print_debug/4]).

print_debug(Meta, Module, Line, Contents) ->
    do_code(Module, Line, Contents),
    print_from_meta(Meta).

print_code(Code, Highlight) ->
    io:format("Code~n----~n~s~n",
              [string:join(fix_lines(Code, Highlight), "\n")]).

do_code(_Module, Line, Contents) ->
    %% fixme: check that we are in the same module.
    AllLines = re:split(Contents, "\n\\d+:", [{return, list}]),
    LineBefore = wbug_code:find_before(AllLines, Line, Line-10, 20),
    LineAfter = wbug_code:find_after(AllLines, Line, LineBefore+20, 20),
    ShowLines = lists:sublist(AllLines, LineBefore, LineAfter-LineBefore+1),
    print_code(ShowLines, Line-LineBefore).

print_from_meta(Meta) ->
    io:format("Callstack~n---------~n~s~nBindings~n--------~n~s~n",
              [format_backtraces(int:meta(Meta, backtrace, 3)),
               format_bindings(int:meta(Meta, bindings, nostack))
              ]).

fix_lines([], _) -> "";
fix_lines([Line|Lines], N) ->
    Token = if N =:= 0 -> ">";
               true -> " "
            end,
    [Token ++ lists:nthtail(3, Line)|fix_lines(Lines, N-1)].

format_backtraces(Bts) ->
    string:join(lists:map(fun format_backtrace/1, lists:reverse(Bts)), "\n").

format_backtrace({_Number, {M,F,Args}}) ->
    ArgsStr = string:join([io_lib:format("~p", [Arg]) || Arg <- Args], ", "),
    [atom_to_list(M), ":", atom_to_list(F), "("|ArgsStr] ++ ")".

format_bindings(Bindings) ->
    lists:map(fun format_binding/1, Bindings).

format_binding({Name, Val}) ->
    [atom_to_list(Name), ": ", io_lib:format("~p", [Val]), "\n"].
