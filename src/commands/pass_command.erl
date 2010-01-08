-module(pass_command).
-export([run_user/2]).

-include("replies.hrl").

run_user(UserID, []) ->
    user_socket:send_message(UserID, ?ERR_NEEDMOREPARAMS(?TARGET(UserID), "PASS"));

run_user(UserID, [Password|_Rest]) ->
    ircusers:update_user(UserID, fun(User) ->
        case User of
            #ircuser{nick = nil, user = nil, host = nil, real = nil} ->
                User#ircuser{pass = Password};
            _ ->
                user_socket:send_message(UserID, ?ERR_ALREADYREGISTRED(?TARGET(UserID))),
                User
        end
    end).
    