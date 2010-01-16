-module(join_command).
-export([run_user/2]).

-include("replies.hrl").
-include("ircchannel.hrl").

run_user(UserID, []) ->
    user_socket:send_message(UserID, ?ERR_NEEDMOREPARAMS(?TARGET(UserID), "JOIN"));

run_user(UserID, ["0"]) ->
    case ircchannels:get_channel_with_user(UserID) of
        {ok, Channels} ->
            lists:foreach(fun(Channel) ->
                part_command:run_user(UserID, [Channel, "Leaving all channels"])
            end, Channels);
        {error, _} -> ok
    end;

run_user(UserID, [Channels]) ->
    run_user(UserID, [Channels, ""]);

run_user(UserID, [Chans, _Ks]) ->
    Channels = ircmessage:parse_argument(Chans),
    lists:foreach(fun(Channel) -> join_channel(UserID, Channel) end, Channels).

join_channel(UserID, ChannelName) ->
    %% TODO: everything
    case is_valid_channel_name(ChannelName) of
        false ->
            Msg = ?ERR_NOSUCHCHANNEL(?TARGET(UserID), misc:sanitize_msg_param(ChannelName)),
            user_socket:send_message(UserID, Msg);
        true ->
            {ok, Channel, _UserModes} = ircchannels:join(UserID, ChannelName),
            {ok, JoiningUser} = ircusers:get(UserID),
            Target = JoiningUser#ircuser.nick,
            ChannelTopic = Channel#ircchannel.topic,
            ChannelName = Channel#ircchannel.name,
            case ChannelTopic of
                nil ->
                    user_socket:send_message(UserID, ?RPL_NOTOPIC(Target, ChannelName));
                _ ->
                    user_socket:send_message(UserID, ?RPL_TOPIC(Target, ChannelName, ChannelTopic))
            end,
            %user_socket:send_message(UserID, ?RPL("MODE", [[$+|Modes]])),
            UserInChannel = dict:fetch_keys(Channel#ircchannel.users),
            send_names(Channel, JoiningUser, UserInChannel, [])
    end.

% send if reply has more than 370 chars
send_names(Channel, JoinUser, UserList, Sum) when length(Sum) >= 370 ->
    #ircuser{socket_process = ID, nick = Nick} = JoinUser,
    ChannelForRpl = format_channel_name_for_name_rpl(Channel),
    user_socket:send_message(ID, ?RPL_NAMREPLY(Nick, ChannelForRpl, Sum)),
    send_names(Channel, JoinUser, UserList, []);

% or send if no more users to send
send_names(Channel, JoinUser, [], Sum) ->
    #ircuser{socket_process = ID, nick = Nick} = JoinUser,
    case Sum of
        [] -> ok;
        _ ->
            ChannelForRpl = format_channel_name_for_name_rpl(Channel),
            user_socket:send_message(ID, ?RPL_NAMREPLY(Nick, ChannelForRpl, Sum))
    end,
    user_socket:send_message(ID, ?RPL_ENDOFNAMES(Nick, Channel#ircchannel.name));

% add user to reply and send JOIN notice
send_names(Channel, JoinUser, [MemberID|Rest], Sum) ->
    ChannelName = Channel#ircchannel.name,
    NewSum = case format_nick_for_name_rpl(MemberID, ChannelName) of
        {ok, NewNick} ->
            user_socket:send_message(MemberID, ?MSG_FROM(JoinUser, "JOIN", [ChannelName])),
            case Sum of
                [] -> NewNick;
                _ -> [NewNick] ++ [$ |Sum]
            end;
        error -> Sum
    end,
    send_names(Channel, JoinUser, Rest, NewSum).

format_nick_for_name_rpl(UserID, ChannelName) ->
    case ircusers:get(UserID) of
        {ok, #ircuser{nick = Nick, modes = UserModes}} ->
            {ChanOp, ChanVoice} =
                case ircchannels:get_user_modes(ChannelName, UserID) of
                    {ok, Modes} -> {has_mode($o, Modes), has_mode($v, Modes)};
                    _ -> {false, false}
                end,
            NewNick = case {has_mode($o, UserModes), ChanOp, ChanVoice} of
                {Op, OpC, _} when Op =:= true orelse OpC =:= true ->
                    [$@|Nick];
                {_, _, true} -> [$+|Nick];
                _ -> Nick
            end,
            {ok, NewNick};
        _ ->
            error
    end.

format_channel_name_for_name_rpl(Channel) ->
    Modes = Channel#ircchannel.modes,
    [case {has_channel_mode($s, Modes), has_channel_mode($p, Modes)} of
        {true, false} -> $@;
        {false, true} -> $*;
        {false, false} -> $=
    end|[$ |Channel#ircchannel.name]].

has_channel_mode(Mode, Modes) ->
    case dict:find(Mode, Modes) of
        {ok, _} -> true;
        error -> false
    end.

has_mode(Mode, Modes) ->
    case string:chr(Modes, Mode) of
        0 -> false;
        _ -> true
    end.

is_valid_channel_name(Name) ->
    case Name of
        [$#|Channel] -> string:span(Channel, [$ , 7, $,, $:]) == 0;
        _ -> false
    end.
