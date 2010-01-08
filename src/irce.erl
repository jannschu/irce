-module(irce).
-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

%% starts the server
start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) -> ok.

init(_Args) ->
    MaxRestarts = 10,
    InTimeOf = 8,
    {ok, {
        {one_for_all, MaxRestarts, InTimeOf},
        [
            % the listener for incoming connections
            {socket_listener, {socket_listener, start_link, []},
             permanent, 1000, worker, [socket_listener]},
            % the socket supervisor
            {user_socket_supervisor, {user_socket_supervisor, start_link, []},
             permanent, infinity, supervisor, [user_socket_supervisor]},
            % the channel database
            {channel_supervisor, {ircchannels, start_link, []},
             permanent, 3000, worker, [channel_supervisor]},
            % the user database
            {user_database_server, {ircusers, start_link, []},
             permanent, 1000, worker, [ircusers]}
        ]
    }}.
