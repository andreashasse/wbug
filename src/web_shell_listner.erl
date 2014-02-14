-module(web_shell_listner).

-export([start/0]).

start() ->
    spawn(fun do_start/0).

do_start() ->
    true = register(?MODULE, self()),
    wbug_srv:add_listner(),
    get_input().

get_input() ->
    receive
        {wbug_break, Payload} ->
            [Meta, Module, Line, Contents] =
                gets(Payload, [meta, module, line, contents]),
            wbug_fmt:print_debug(Meta, Module, Line, Contents)
    after 0 ->
            case rm_last(io:get_line("cmd>")) of
                "c" -> wbug_srv:command(continue);
                "n" -> wbug_srv:command(next);
                "s" -> wbug_srv:command(step);
                %% "q" -> wbug_srv:command(stop);
                _   ->
                    io:format("heh? (c -> continue, n -> next, s -> step)~n"),
                    get_input()
            end
    end.

rm_last(Str) ->
    lists:sublist(Str, 1, length(Str)-1).

gets(Payload, Keys) ->
    lists:map(
      fun(Key) ->
              {Key, Value} = lists:keyfind(Key, 1, Payload),
              Value
      end,
      Keys).
