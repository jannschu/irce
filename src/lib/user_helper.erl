-module(user_helper).
-export([sanitize_nick/1, rpl_target/1]).

-include("ircuser.hrl").

sanitize_nick(A) -> misc:sanitize_msg_param(A).

rpl_target(UserID) ->
    case ircusers:get(UserID) of
        {ok, #ircuser{nick = Nick}} -> Nick;
        _ -> "*"
    end.