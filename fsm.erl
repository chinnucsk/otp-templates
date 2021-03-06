%%%-------------------------------------------------------------------
%%% @author Juraj Hlista <juro.hlista@gmail.com>
%%% @copyright
%%% @doc
%%% gen_fsm template
%%% @end
%%%-------------------------------------------------------------------

-module(fsm).

-behaviour(gen_fsm).

%% Public API
-export([
         start_link/0,
         async_state1/0,
         sync_state2/0,
         async_all_state3/0,
         sync_all_state3/0,
         stop/0
        ]).

%% gen_fsm callbacks
-export([
         init/1,
         state_initial/2,
         state1/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4
        ]).

-define(SERVER, ?MODULE).

-record(state, {}).


%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Starts FSM in state_initial
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Changes to state1 from state_initial
async_state1() ->
    gen_fsm:send_event(?SERVER, to_state1).

%% @doc Changes to state2 from state1
sync_state2() ->
    gen_fsm:sync_send_event(?SERVER, to_state2).

%% @doc Changes to state3 from any state
async_all_state3() ->
    gen_fsm:send_all_state_event(?SERVER, to_state3).

%% @doc Changes to state3 from any state
sync_all_state3() ->
    gen_fsm:sync_send_all_state_event(?SERVER, to_state3).

%% @doc Stops FSM
stop() ->
    gen_fsm:send_all_state_event(?SERVER, stop).


%% ===================================================================
%% gen_fsm callbacks
%% ===================================================================

%% @private
%% @doc FSM init
init([]) ->
    io:format("state_initial~n"),
    {ok, state_initial, #state{}}.

%% @private
%% @doc Handles async transition from state_initial to state1
state_initial(to_state1, State) ->
    io:format("(async) state_initial -> state1~n"),
    {next_state, state1, State};
state_initial(_Event, State) ->
    {next_state, state_initial, State}.

%% @private
%% @doc Handles sync transition from state1 to state2
state1(to_state2, _From, State) ->
    io:format("(sync) state1 -> state2~n"),
    {reply, ok, state2, State};
state1(_Event, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, state1, State}.

%% @private
%% @doc Handles async transition from any state to state3 or stop event
handle_event(to_state3, StateName, State) ->
    io:format("(async) ~p -> state3~n", [StateName]),
    {next_state, state3, State};
handle_event(stop, StateName, State) ->
    io:format("(async) ~p -> stopped~n", [StateName]),
    {stop, stopped, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @private
%% @doc Handles sync transition from any state to state3
handle_sync_event(to_state3, _From, StateName, State) ->
    io:format("(sync) ~p -> state3~n", [StateName]),
    {reply, ok, state3, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, StateName, State}.

%% @private
%% @doc Handles any other messages than sync or async events or system messages
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%% @private
%% @doc It is called when the server is going to terminate
terminate(_Reason, _StateName, _State) ->
  ok.
  
%% @private
%% @doc Converts process state when code is changed
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================
