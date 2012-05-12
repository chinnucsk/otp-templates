%%%-------------------------------------------------------------------
%%% @author Juraj Hlista <juro.hlista@gmail.com>
%%% @copyright
%%% @doc
%%% application template
%%% @end
%%%-------------------------------------------------------------------

-module(app).

-behaviour(application).

%% application callbacks
-export([
         start/2,
         stop/1
        ]).


%% ===================================================================
%% application callbacks
%% ===================================================================

%% @private
%% @doc starts application's top supervisor
start(_StartType, StartArgs) ->
    case app_sup:start_link(StartArgs) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @private
%% @doc stops application
stop(_State) ->
    ok.
