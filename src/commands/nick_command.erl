-module(nick_command).
-export([run_user/2]).

-include("replies.hrl").

run_user(UserID, []) ->
    user_socket:send_message(UserID, ?ERR_NONICKNAMEGIVEN(?TARGET(UserID)));

run_user(UserID, [Nick|_Rest]) ->
    % TODO: check for collisions, restricted etc.
    CurrentNick = ?TARGET(UserID),
    case check_nick(Nick, CurrentNick) of
        ok ->
            ircusers:update_user(UserID, fun(User) ->
                NewUser = User#ircuser{nick = Nick},
                case User of
                    #ircuser{user = A, nick = B} when A =:= nil orelse B =/= nil ->
                        ok;
                    #ircuser{nick = nil} ->
                        userregistration:for_user(NewUser)
                end,
                NewUser
            end);
        {error, Msg} ->
            user_socket:send_message(UserID, Msg)
    end.

check_nick(Nick, CurrentNick) ->
    check_nick(valid, Nick, CurrentNick).

check_nick(valid, Nick, CurrentNick) ->
    case is_valid_nick(Nick) of
        true -> check_nick(resource, Nick, CurrentNick);
        false ->
            SanitizedNick = user_helper:sanitize_nick(Nick),
            {error, ?ERR_ERRONEUSNICKNAME(CurrentNick, SanitizedNick)}
    end;

check_nick(resource, Nick, CurrentNick) ->
    % TODO: implement
    check_nick(collision, Nick, CurrentNick);

check_nick(collision, Nick, CurrentNick) ->
    case ircusers:get_by_nick(Nick) of
        {error, _} -> ok;
        {ok, _} -> {error, ?ERR_NICKNAMEINUSE(CurrentNick, Nick)}
    end.

is_valid_nick([]) -> false;

is_valid_nick(Nick) when length(Nick) > 9 -> false;

is_valid_nick([First|_]) when First =:= '-' orelse
                                (First >= $0 andalso First =< $9) ->
    false;

is_valid_nick(Nick) ->
    AllowedChars = [91, 92, 93, 94, 95, 96, 123, 124, 125] ++ "-0123456789" ++
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
    case string:span(Nick, AllowedChars) of
        N when N =:= length(Nick) -> true;
        _ -> false
    end.
    