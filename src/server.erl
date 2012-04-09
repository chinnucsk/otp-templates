%%%-------------------------------------------------------------------
%%% @author Juraj Hlista <juro.hlista@gmail.com>
%%% @copyright
%%% @doc
%%% gen_server template
%%% @end
%%%-------------------------------------------------------------------

-module(server).

-behaviour(gen_server).

%% Public API
-export([
         start_link/0,
         sync_msg/1,
         async_msg/1
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {}).

%% ===================================================================
%% Public API
%% ===================================================================

%% -------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%% -------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% -------------------------------------------------------------------
%% @doc
%% Tests sync calls
%% @end
%% -------------------------------------------------------------------
-spec sync_msg(term()) -> ok.

sync_msg(Msg) ->
    gen_server:call(?SERVER, {test, Msg}).

%% -------------------------------------------------------------------
%% @doc
%% Tests async calls
%% @end
%% -------------------------------------------------------------------
-spec async_msg(term()) -> ok.

async_msg(Msg) ->
    gen_server:cast(?SERVER, {test, Msg}).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Server init
%% @end
%% -------------------------------------------------------------------
-spec init([]) -> {ok, #state{}}.

init([]) ->
    {ok, #state{}}.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Handles call messages
%% @end
%% -------------------------------------------------------------------
-spec handle_call(_, _, State) -> {reply, ok, State}.

handle_call({test, Msg}, _From, State) ->
    io:format("Got sync message: \'~p\'~n", [Msg]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Handles cast messages
%% @end
%% -------------------------------------------------------------------
-spec handle_cast(_, State) -> {noreply, State}. 

handle_cast({test, Msg}, State) ->
    io:format("Got async message: \'~p\'~n", [Msg]),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Handles all non call/cast messages
%% @end
%% -------------------------------------------------------------------
-spec handle_info(_, State) -> {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% It is called when the server is going to terminate.
%% @end
%% -------------------------------------------------------------------
-spec terminate(_, _) -> ok.

terminate(_Reason, _State) ->
    ok.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Converts process state when code is changed
%% @end
%% -------------------------------------------------------------------
-spec code_change(_, State, _) -> {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================
