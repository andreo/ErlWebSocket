%%%-------------------------------------------------------------------
%%% File    : chat_sup.erl
%%% Author  : andreo <andreo@andreo-laptop>
%%% Description : 
%%%
%%% Created : 25 Aug 2011 by andreo <andreo@andreo-laptop>
%%%-------------------------------------------------------------------
-module(websocket_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([Port]) ->
    {ok, {{one_for_one,10,1}, [
                              %% {chat_sup,
                              %%  {client_manager_srv,start_link,[Port, []]},
                              %%  permanent,2000,worker,[client_manager_srv]},
                              {connection_sup,
                               {connection_sup, start_link,[Port]},
                               permanent,2000,supervisor,[client_connection_sup]},
                              {listen_manager_srv,
                               {listen_manager_srv, start_link, [Port]},
                               permanent,2000,worker,[listen_manager_srv]}
                             ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
