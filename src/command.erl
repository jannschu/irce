-module(command).
-export([run/3]).

-include("replies.hrl").

-define(MAP_REG_COMMAND(Name, Module), run(Name, Params, UserID) -> Module:run_user(UserID, Params)).
-define(MAP_COMMAND(Name, Module),
run(Name, Params, UserID) ->
    case ircusers:get(UserID) of
        {ok, #ircuser{user = A, nick = N}} when A =:= nil orelse N =:= nil ->
            Nick = user_helper:sanitize_nick(N),
            user_socket:send_message(UserID, ?ERR_NOTREGISTERED(Nick));
            _ ->
                Module:run_user(UserID, Params)
    end).

?MAP_COMMAND("PRIVMSG", privmsg_command);

?MAP_REG_COMMAND("NICK", nick_command);
?MAP_COMMAND("PING", ping_command);

?MAP_COMMAND("JOIN", join_command);
?MAP_COMMAND("MODE", mode_command);

?MAP_COMMAND("MOTD", motd_command);
?MAP_COMMAND("PART", part_command);

?MAP_COMMAND("QUIT", quit_command);
?MAP_REG_COMMAND("USER", user_command);
?MAP_REG_COMMAND("PASS", pass_command);

run(Command, _Params, UserID) ->
    case ircusers:get(UserID) of
        {ok, #ircuser{user = A, nick = N}} when A =/= nil andalso N =/= nil ->
            user_socket:send_message(UserID, ?ERR_UNKNOWNCOMMAND(N, Command));
        _ ->
            ok
    end.
