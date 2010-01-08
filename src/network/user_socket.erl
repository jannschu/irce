-module(user_socket).
-export([send_message/2, close_connection/1, get_socket/1]).

-behaviour(gen_server).
-export([start_link/1,
               init/1,
        handle_call/3,
        handle_cast/2,
          terminate/2,
        code_change/3,
        handle_info/2]).

-export([compute_messages/2]).

-include("ircmessage.hrl").
-include("ircuser.hrl").

%%----------------------------------------------------------------------------
%% public api methods
%%----------------------------------------------------------------------------

send_message(UserID, Message) ->
    gen_server:cast(UserID, {send, Message}).

close_connection(UserID) ->
    gen_server:cast(UserID, close).

get_socket(UserID) ->
    gen_server:call(UserID, get_socket).

%%----------------------------------------------------------------------------
%% gen_server methods
%%----------------------------------------------------------------------------
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
    UserID = ircusers:new(),
    inet:setopts(Socket, [{active, once}]),
    {ok, {UserID, Socket}}.

% synchronous
handle_call(get_socket, _From, State) ->
    {_, Socket} = State,
    {reply, Socket, State}.

% asynchronous
handle_cast({send, Message}, State) ->
    {_, Socket} = State,
    String = ircmessage:message_to_list(Message),
    gen_tcp:send(Socket, String),
    {noreply, State};

handle_cast(close, State) ->
    {_, Socket} = State,
    inet:setopts(Socket, [{exit_on_close, false}]),
    gen_tcp:close(Socket),
    {stop, connection_closed, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({tcp, Socket, Messages}, State) ->
    {UserID, _} = State,
    spawn(?MODULE, compute_messages, [Messages, UserID]),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    {ID, _} = State,
    quit_command:run_user(ID, ["Connection closed by client"]),
    {stop, connection_closed, State}.

terminate(_Signal, _State) ->
    ok.

%%----------------------------------------------------------------------------
%% helper methods
%%----------------------------------------------------------------------------

compute_messages(Messages, UserID) ->
    case string:str(Messages, "\r\n") of
        0 -> {error, malformed_message};
        N ->
            {Message, Rest} = lists:split(N + 1, Messages),
            spawn(?MODULE, compute_messages, [Rest, UserID]),
            case ircmessage:parse(Message) of
                {error, _} -> ok;
                #ircmessage{params = Params, command = Command} ->
                    command:run(Command, Params, UserID)
            end
    end.
