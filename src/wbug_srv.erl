-module(wbug_srv).

-behaviour(gen_server).

%% API
-export([start/0,
         add_listner/0,
         attached/2,
         processes/0,
         change_active/1,
         command/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {active,
                processes = [],
                listners = []}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

add_listner() ->
    gen_server:call(?SERVER, {add_listner, self()}).

%%@doc callback that starts the debugging
attached(Pid, Module) ->
    gen_server:call(?SERVER, {attached, {Pid, Module}}).

processes() ->
    gen_server:call(?SERVER, processes).

change_active(Meta) ->
    gen_server:call(?SERVER, {change_active, Meta}).

command(Command) ->
    gen_server:call(?SERVER, {command, Command}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({add_listner, Pid}, _From, State) ->
    Reply = ok,
    NewState = State#state{listners = [Pid|State#state.listners]},
    {reply, Reply, NewState};
handle_call(processes, _From, State) ->
    Reply = State#state.processes,
    {reply, Reply, State};
handle_call({change_active, Meta}, _From, State) ->
    Reply = ok,
    {reply, Reply, State#state{active = Meta}};
handle_call({command, Command}, _From, #state{active = Meta} = State) ->
    int:meta(Meta, Command),
    Reply = ok,
    {reply, Reply, State};
handle_call({attached, {Pid, _Module}}, _From, State) ->
    {ok, Meta} = int:attached(Pid),
    int:meta(Meta, trace, true),
    Reply = ok,
    NewState = State#state{processes = [{Meta, Pid}|State#state.processes]},
    {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({_NewMeta, {exit_at, _, _, _}}, State) ->
    {noreply, State#state{active = undefined}};
handle_info({Meta, Message}, #state{active = undefined} = State) ->
    {ok, Pid} = find_pid(Meta, State),
    handle_active_meta_message({Meta, Pid}, Message, State),
    {noreply, State#state{active = Meta}};
handle_info({Meta, Message}, #state{active = Meta} = State) ->
    {ok, Pid} = find_pid(Meta, State),
    handle_active_meta_message({Meta, Pid}, Message, State),
    {noreply, State};
handle_info({Meta, Message} = Info, State) ->
    case find_pid(Meta, State) of
        {ok, Pid} ->
            handle_meta_message({Meta, Pid}, Message, State);
        false ->
            error_logger:error_msg("Not handled info ~p ~p~n",
                                   [Info, State#state.processes])
    end,
    {noreply, State};
handle_info(Info, State) ->
    error_logger:error_msg("Not handled info ~p~n", [Info]),
    {noreply, State}.

handle_active_meta_message({Meta, Pid}, {break_at, Mod, Line, _}, State) ->
    Contents = int:contents(Mod, Pid),
    Payload = [{meta, Meta}, {module, Mod}, {line, Line}, {contents, Contents}],
    Msg = {wbug_break, Payload},
    [Listner ! Msg || Listner <- State#state.listners],
    ok;
handle_active_meta_message(_Meta, {trace, _},        _State) -> ok;
handle_active_meta_message(_Meta, {trace_output, _}, _State) -> ok;
handle_active_meta_message(_Meta, idle,              _State) -> ok;
handle_active_meta_message(_Meta, {exit_at,_,_,_},   _State) -> ok;
handle_active_meta_message(_Meta, running,           _State) -> ok;
handle_active_meta_message(I, Msg, State) ->
    handle_meta_message(I, Msg, State).

handle_meta_message(_Meta, {attached, _Mod, _Line, _}, _State) ->
    ok;
handle_meta_message(_Meta, Cmd,               _State) ->
    error_logger:error_msg("Not handled debug message ~p~n", [Cmd]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

find_pid(Meta, State) ->
    case lists:keyfind(Meta, 1, State#state.processes) of
        {Meta, Pid} ->
            {ok, Pid};
        false ->
            false
    end.
