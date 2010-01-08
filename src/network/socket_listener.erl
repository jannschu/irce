-module(socket_listener).
-export([start_link/0]).

-export([init/0]).

start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

init() ->
    {ok, ListenSocket} = gen_tcp:listen(6667, [
            {packet, 0},
            {active, false},
            {keepalive, true},
            {nodelay, true},
            {packet_size, 512},
            {recbuf, 1024},
            {send_timeout, 30000},
            {send_timeout_close, true},
            {reuseaddr, true}
        ]),
    listen(ListenSocket).

listen(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    {ok, ChildPid} = user_socket_supervisor:new_socket(ClientSocket),
    gen_tcp:controlling_process(ClientSocket, ChildPid),
    listen(ListenSocket).