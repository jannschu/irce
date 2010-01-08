-module(motd_command).

-export([run_user/2]).

-include("replies.hrl").

run_user(UserID, _) ->
    case file:open("MOTD", [read]) of
        {error, _} ->
            user_socket:send_message(UserID, ?ERR_NOMOTD(?TARGET(UserID)));
        {ok, IO} ->
            Target = ?TARGET(UserID),
            user_socket:send_message(UserID, ?RPL_MOTDSTART(Target)),
            send_motd(UserID, IO, Target),
            user_socket:send_message(UserID, ?RPL_ENDOFMOTD(Target))
    end.

send_motd(UserID, IO, Target) ->
    case file:read_line(IO) of
        {error, _Reason} -> ok;
        eof -> ok;
        {ok, Data} ->
            Line = lists:sublist(Data, length(Data) - 1),
            user_socket:send_message(UserID, ?RPL_MOTD(Target, Line)),
            send_motd(UserID, IO, Target)
    end.