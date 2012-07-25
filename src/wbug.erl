%%% @author Andreas Hasselberg <andreas.hasselberg@wooga.com>
%%% @copyright (C) 2012, Andreas Hasselberg
%%% @doc
%%%  A debugger built at wooga.
%%% @end
%%% Created : 25 Jul 2012 by Andreas Hasselberg <andreas.hasselberg@wooga.com>

-module(wbug).

-compile(export_all).

line(Module, Line) ->
    int:i(Module),
    int:break(Module, Line),
    int:auto_attach([break], {?MODULE, attached, [Module, Line]}).

attached(Pid, Module, Line) ->
    {ok, Meta} = int:attached(Pid),
    attached(Pid, Module, Line, Meta).

attached(Pid, Module, Line, Meta) ->
    do_code(Module, Line, Pid),
    print_from_meta(Meta),
    input_loop(Pid, Module, Line, Meta).

do_code(Module, Line, Pid) ->
    Contents = int:contents(Module, Pid),
    AllLines = re:split(Contents, "\n\\d+:", [{return, list}]),
    LineBefore = find_before(AllLines, Line, Line-10, 20),
    LineAfter = find_after(AllLines, Line, LineBefore+20, 20),
    ShowLines = lists:sublist(AllLines, LineBefore, LineAfter-LineBefore+1),
    print_code(ShowLines, Line-LineBefore).

%% ---------------------------------------------------------------------------
%% Input handling

input_loop(Pid, Module, Line, Meta) ->
    case rm_last(io:get_line("cmd>")) of
        "c" -> int:meta(Meta, continue);
        "n" -> int:meta(Meta, next),
               attached(Pid, Module, Line+1, Meta)
    end.

rm_last(Str) ->
    lists:sublist(Str, 1, length(Str)-1).

%% ---------------------------------------------------------------------------
%% Finding lines to print

find_code(_Match, _Inc, _AllLines, _LineNo, Default, 0) -> Default;
find_code(Match, Inc, AllLines, LineNo, Default, Trysleft) ->
    case Match(lists:nth(LineNo, AllLines)) of
        true -> LineNo;
        false -> find_code(Match, Inc, AllLines, LineNo+Inc, Default, Trysleft+Inc)
    end.

find_before(AllLines, LineNo, Default, Tries) ->
    find_code(fun match_before/1, -1, AllLines, LineNo-1, Default, Tries)+1.

find_after(AllLines, LineNo, Default, Tries) ->
    find_code(fun match_after/1, +1, AllLines, LineNo, Default, Tries).

match_before(Line) ->
    nomatch =/= re:run(Line, "^\ *$").

match_after(Line) ->
    (nomatch =/= re:run(Line, "\\.\s*%+") orelse
     nomatch =/= re:run(Line, "\\.\s*\$")).

%% ---------------------------------------------------------------------------
%% Formating

print_code(Code, Highlight) ->
    io:format("Code~n----~n~s~n",
              [string:join(fix_lines(Code, Highlight), "\n")]).

print_from_meta(Meta) ->
    io:format("Backtrace?~n---------~n~s~nBindings~n--------~n~s~n",
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
    lists:map(fun format_backtrace/1, Bts).

format_backtrace({_Number, {M,F,Args}}) ->
    ArgsStr = string:join([io_lib:format("~p", [Arg]) || Arg <- Args], ", "),
    [atom_to_list(M), ":", atom_to_list(F), "("|ArgsStr] ++ ")".

format_bindings(Bindings) ->
    lists:map(fun format_binding/1, Bindings).

format_binding({Name, Val}) ->
    [atom_to_list(Name), ": ", io_lib:format("~p", [Val]), "\n"].

%% ---------------------------------------------------------------------------
%% Testing

%% c(wbg, [{d, 'TEST'}]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

match_before_test() ->
    ?assert(match_before("  ")),
    ?assert(match_before("")),
    ?assertNot(match_before(" end.")).

match_after_test() ->
    ?assert(match_after("asdkfj.  %")),
    ?assert(match_after("end.")),
    ?assert(match_after(".%")),
    ?assertNot(match_after(".end")),
    ?assertNot(match_after("end")).

-endif. %% TEST


test_manual() ->
    line(avl, 112),
    spawn(fun() ->
                  avl:enter(k, v, avl:enter(k3, 3, avl:enter(k, v2, avl:empty()))),
                  io:format("done~n")
          end).
