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
  {ok, state_initial, #state{}}.

%% @private
%% @doc 
state_initial(to_state1, State) ->
    {next_state, state1, State};
state_initial(_Event, State) ->
    {next_state, state_initial, State}.

%% @private
%% @doc 
state1(to_state2, _From, State) ->
    {next_state, state2, State};
state1(_Event, _From, State) ->
    {next_state, state1, State}.

%% @private
%% @doc 
handle_event(to_state3, _StateName, State) ->
    {next_state, state3, State};
handle_event(stop, _StateName, State) ->
    {stop, stopped, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @private
%% @doc
handle_sync_event(to_state3, _From, _StateName, State) ->
    {next_state, state3, State};
handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.


handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================
