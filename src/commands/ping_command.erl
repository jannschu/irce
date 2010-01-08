-module(ping_command).
-export([run_user/2]).

-include("replies.hrl").

run_user(UserID, []) ->
    user_socket:send_message(UserID, ?ERR_NOORIGIN(?TARGET(UserID)));

run_user(UserID, [Server1]) ->
    case is_me(Server1) of
        true ->
            user_socket:send_message(UserID, ?RPL("PONG", [?SERVERHOST]));
        false ->
            user_socket:send_message(UserID, ?ERR_NOSUCHSERVER(?TARGET(UserID), Server1))
    end;

run_user(UserID, [_Server1, Server2]) ->
    % not implemented
    user_socket:send_message(UserID, ?ERR_NOSUCHSERVER(?TARGET(UserID), Server2)).

is_me(Host) ->
    {ok, ServerHost} = inet:gethostname(),
    case Host of
        ServerHost -> true;
        _ -> false
    end.