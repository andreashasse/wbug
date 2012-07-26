%%% @author Andreas Hasselberg <andreas.hasselberg@wooga.com>
%%% @copyright (C) 2012, Andreas Hasselberg
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2012 by Andreas Hasselberg <andreas.hasselberg@wooga.com>

-module(wbug_code).

-export([find_before/4,find_after/4]).

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
%% Testing

%% c(wbg_code, [{d, 'TEST'}]).
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
