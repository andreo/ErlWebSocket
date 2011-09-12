%%%-------------------------------------------------------------------
%%% File    : chat_app.erl
%%% Author  : andreo <andreo@andreo-laptop>
%%% Description : 
%%%
%%% Created : 25 Aug 2011 by andreo <andreo@andreo-laptop>
%%%-------------------------------------------------------------------
-module(websocket_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([test/0]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, Port) ->
    websocket_sup:start_link(Port).

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

test() ->
    application:load(websocket),
    application:start(websocket),
    toolbar:start().
