-module(misc).
-export([sanitize_msg_param/1]).

sanitize_msg_param(nil) -> "*";
sanitize_msg_param("") -> "*";
sanitize_msg_param([$: | Rest]) -> sanitize_msg_param([$* | Rest]);

sanitize_msg_param(Nick) ->
    case string:chr(Nick, $ ) of
        0 -> Nick;
        N ->
            {A, B} = lists:split(N - 1, Nick),
            sanitize_msg_param(A ++ [$_ | tl(B)])
    end.