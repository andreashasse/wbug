%%% @author Andreas Hasselberg <andreas.hasselberg@wooga.com>
%%% @doc
%%%  A debugger built at wooga.
%%% @end
%%% Created : 25 Jul 2012 by Andreas Hasselberg <andreas.hasselberg@wooga.com>

-module(wbug).

-compile(export_all).

line(Module, Line) ->
    int:i(Module),
    int:break(Module, Line),
    int:auto_attach([break], {?MODULE, attached, [Module]}).

attached(Pid, Module) ->
    Contents = int:contents(Module, Pid),
    {ok, Meta} = int:attached(Pid),
    int:meta(Meta, trace, true),
    do_wait(Meta, Contents).

do_out(Meta, Module, Line, Contents) ->
    do_code(Module, Line, Contents),
    print_from_meta(Meta),
    input_loop(Meta, Contents).

do_code(_Module, Line, Contents) ->
    %% fixme: check that we are in the same module.
    AllLines = re:split(Contents, "\n\\d+:", [{return, list}]),
    LineBefore = find_before(AllLines, Line, Line-10, 20),
    LineAfter = find_after(AllLines, Line, LineBefore+20, 20),
    ShowLines = lists:sublist(AllLines, LineBefore, LineAfter-LineBefore+1),
    print_code(ShowLines, Line-LineBefore).

%% ---------------------------------------------------------------------------
%% Input handling

input_loop(Meta, Contents) ->
    case rm_last(io:get_line("cmd>")) of
        "c" -> int:meta(Meta, continue);
        "n" -> int:meta(Meta, next);
        "s" -> int:meta(Meta, stop);
        _   -> io:format("heh?"), input_loop(Meta, Contents)
    end,
    do_wait(Meta, Contents).

do_wait(Meta, Contents) ->
    receive
        {Meta, {attached, _Module, _Line, _}} ->
            do_wait(Meta, Contents);
        {Meta, {break_at, Module, Line, _}} ->
            do_out(Meta, Module, Line, Contents),
            input_loop(Meta, Contents);
        %% some messages that I don't care about
        {Meta, {trace, _}} ->
            do_wait(Meta, Contents);
        {Meta, {trace_output, _}} ->
            do_wait(Meta, Contents);
        {Meta, idle} ->
            do_wait(Meta, Contents);
        {_, {exit_at,_,_,_}} ->
            do_wait(Meta, Contents);
        {Meta, running} ->
            do_wait(Meta, Contents);
        {Meta, Cmd} ->
            io:format("Not handled message ~p~n", [Cmd]),
            do_wait(Meta, Contents)
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
    io:format("Call~n----~n~s~nBindings~n--------~n~s~n",
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


manual() ->
    line(wbug_test, 15),
    spawn(fun() ->
                  wbug_test:calls(),
                  io:format("done~n")
          end).
