-module(wbug_shell_listner).

-export([start/0]).

start() ->
    true = register(?MODULE, self()),
    wbug_srv:add_listner(),
    get_input().

get_input() ->
    receive
        {wbug_break, Payload} ->
            [Meta, Module, Line, Contents] =
                gets(Payload, [meta, module, line, contents]),
            wbug_fmt:print_debug(Meta, Module, Line, Contents)
    after 1000 ->
            case rm_last(io:get_line("cmd>")) of
                "c" -> wbug_srv:command(continue);
                "n" -> wbug_srv:command(next);
                "s" -> wbug_srv:command(step);
                "p" -> io:format("[{Meta, Pid}] = ~p~n",
                                 [wbug_srv:processes()]);
                "change " ++ MetaStr ->
                    Meta = list_to_pid(MetaStr),
                    io:format("Changing ~p ~p~n", [Meta, MetaStr]),
                    wbug_srv:change_active(Meta);
                %% "q" -> wbug_srv:command(stop);
                _   ->
                    io:format("heh? (c -> continue, n -> next, s -> step)~n")
            end
    end,
    get_input().

rm_last(Str) ->
    lists:sublist(Str, 1, length(Str)-1).

gets(Payload, Keys) ->
    lists:map(
      fun(Key) ->
              {Key, Value} = lists:keyfind(Key, 1, Payload),
              Value
      end,
      Keys).
