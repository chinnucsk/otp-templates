%%%-------------------------------------------------------------------
%%% @author Juraj Hlista <juro.hlista@gmail.com>
%%% @copyright
%%% @doc
%%% Event manager template
%%% @end
%%%-------------------------------------------------------------------

-module(event_manager).

%% Public API
-export([
         start_link/0,
         add_handler/2,
         delete_handler/2,
         notify/1,
         call/2
        ]).

-define(SERVER, ?MODULE).


%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Starts supervisor
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%% @doc Adds event handler
add_handler(Module, Args) ->
    gen_event:add_handler(?SERVER, Module, Args).

%% @doc Deletes event handler
delete_handler(Module, Args) ->
    gen_event:delete_handler(?SERVER, Module, Args).

%% @doc
notify(Event) ->
    gen_event:notify(?SERVER, Event).

%% @doc
call(Handler, Request) ->
	gen_event:call(?SERVER, Handler, Request).
