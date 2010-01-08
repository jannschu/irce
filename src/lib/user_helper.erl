-module(user_helper).
-export([sanitize_nick/1, rpl_target/1]).

-include("ircuser.hrl").

sanitize_nick(nil) -> "*";
sanitize_nick("") -> "*";
sanitize_nick([$: | Rest]) -> sanitize_nick([$* | Rest]);

sanitize_nick(Nick) ->
    case string:chr(Nick, $ ) of
        0 -> Nick;
        N ->
            {A, B} = lists:split(N - 1, Nick),
            sanitize_nick(A ++ [$_ | tl(B)])
    end.


rpl_target(UserID) ->
    case ircusers:get(UserID) of
        {ok, #ircuser{nick = Nick}} -> Nick;
        _ -> "*"
    end.