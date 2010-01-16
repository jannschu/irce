-module(mode_command).
-export([run_user/2]).

-include("replies.hrl").

run_user(UserID, []) ->
    user_socket:send_message(UserID, ?ERR_NEEDMOREPARAMS(?TARGET(UserID), "JOIN"));

% Channel mode message
run_user(_UserID, [[$# | _Channel] | _Rest]) ->
    todo;

run_user(UserID, [UserName | Modes]) ->
    {ok, #ircuser{nick = Nick} = User} = ircusers:get(UserID),
    case UserName == Nick of
        false ->
            user_socket:send_message(UserID, ?ERR_USERSDONTMATCH(Nick));
        true ->
            change_umodes(UserID, Modes),
            send_umodes(User)
    end.

send_umodes(#ircuser{modes = Modes, nick = Target, socket_process = ID}) ->
    user_socket:send_message(ID, ?RPL_UMODEIS(Target, Modes)).

change_umodes(UserID, [ModeChange|Rest]) ->
    ircusers:update_user(UserID, fun(User) ->
        NewModes = case ModeChange of
            [$+|Modes] -> add_umodes(User#ircuser.modes, Modes);
            [$-|Modes] -> del_umodes(User#ircuser.modes, Modes)
        end,
        User#ircuser{modes = NewModes}
    end),
    change_umodes(UserID, Rest);

change_umodes(_UserID, []) -> ok.

add_umodes(CurrentModes, [ModeToAdd|Rest]) ->
    case string:chr(CurrentModes, ModeToAdd) of
        0 ->
            NewModes = case ModeToAdd of
                $a -> CurrentModes;
                $i -> [$i|CurrentModes];
                $w -> [$w|CurrentModes];
                $r -> [$r|CurrentModes];
                $o -> CurrentModes;
                $O -> CurrentModes;
                $s -> [$s|CurrentModes]
            end,
            add_umodes(NewModes, Rest);
        _ -> add_umodes(CurrentModes, Rest)
    end;

add_umodes(CurrentModes, _) -> CurrentModes.

del_umodes(CurrentModes, [ModeToAdd|Rest]) ->
    case string:chr(CurrentModes, ModeToAdd) of
        0 -> del_umodes(CurrentModes, Rest);
        1 ->
            NewModes = case ModeToAdd of
                $a -> CurrentModes;
                $i -> lists:delete($i, CurrentModes);
                $w -> lists:delete($w, CurrentModes);
                $r -> CurrentModes;
                $o -> lists:delete($o, CurrentModes);
                $O -> lists:delete($O, CurrentModes);
                $s -> lists:delete($s, CurrentModes)
            end,
            del_umodes(NewModes, Rest)
    end;

del_umodes(CurrentModes, _) -> CurrentModes.