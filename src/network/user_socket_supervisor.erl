-module(user_socket_supervisor).
-export([start_link/0, new_socket/1]).

-behaviour(supervisor).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

new_socket(Socket) ->
    supervisor:start_child(?MODULE, [Socket]).

init(_Args) ->
    {ok, {
        {simple_one_for_one, 100, 1},
         [{user_socket, {user_socket, start_link, []},
           temporary, 2000, worker, [user_socket]}]
    }}.