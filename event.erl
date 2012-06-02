%%%-------------------------------------------------------------------
%%% @author Juraj Hlista <juro.hlista@gmail.com>
%%% @copyright
%%% @doc
%%% gen_event template
%%% @end
%%%-------------------------------------------------------------------

-module(event).

-behaviour(gen_event).

%% gen_event callbacks
-export([
         init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {}).


%% ===================================================================
%% gen_event callbacks
%% ===================================================================

%% @doc Event handlet init
init(_InitArgs) ->
    {ok, #state{}}.

%% @doc
handle_event(test_event, State) ->
    io:format("Got 'test_event'~n"),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%% @doc
handle_call(test_request, State) ->
    io:format("Got 'test_request'~n"),
    Reply = ok,
    {ok, Reply, State};
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @doc
handle_info(_Info, State) ->
    {ok, State}.

%% @doc
terminate(_Reason, _State) ->
    ok.

%% @doc
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================
