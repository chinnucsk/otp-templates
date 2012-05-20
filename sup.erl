%%%-------------------------------------------------------------------
%%% @author Juraj Hlista <juro.hlista@gmail.com>
%%% @copyright
%%% @doc
%%% supervisor template
%%% @end
%%%-------------------------------------------------------------------

-module(sup).

-behaviour(supervisor).

%% Public API
-export([
         start_link/0
        ]).

%% supervisor callbacks
-export([
         init/1
        ]).

-define(SERVER, ?MODULE).


%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Starts supervisor
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% ===================================================================
%% supervisor callbacks
%% ===================================================================

%% @private
%% @doc
init([]) ->
    ChildProcess = {child_process, {child_process, start_link, []},
        permanent, 5000, worker, [child_process]},
    Children = [ChildProcess],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.
