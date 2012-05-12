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

%% @doc Starts the server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Tests sync calls
sync_msg(Msg) ->
    gen_server:call(?SERVER, {test, Msg}).

%% @doc Tests async calls
async_msg(Msg) ->
    gen_server:cast(?SERVER, {test, Msg}).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @private
%% @doc Server init
init([]) ->
    {ok, #state{}}.

%% @private
%% @doc Handles call messages
handle_call({test, Msg}, _From, State) ->
    io:format("Got sync message: \'~p\'~n", [Msg]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
%% @doc Handles cast messages
handle_cast({test, Msg}, State) ->
    io:format("Got async message: \'~p\'~n", [Msg]),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
%% @doc Handles all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc It is called when the server is going to terminate.
terminate(_Reason, _State) ->
    ok.

%% @private
%% @doc Converts process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================
