-module(privmsg_command).

-export([run_user/2]).

-include("replies.hrl").
-include("ircchannel.hrl").

run_user(UserID, []) ->
    user_socket:send_message(UserID, ?ERR_NORECIPIENT(?TARGET(UserID), "PRIVMSG"));

run_user(UserID, [_To]) ->
    user_socket:send_message(UserID, ?ERR_NOTEXTTOSEND(?TARGET(UserID)));

%% TODO: more targets, channel modes
run_user(UserID, [[$#|To], Text]) ->
    send_to_channel(UserID, [$#|To], Text);

run_user(UserID, [To, Text]) ->
    send_to_user(UserID, To, Text).

send_to_channel(UserID, ChannelName, Text) ->
    {ok, User} = ircusers:get(UserID),
    case ircchannels:get_channel(ChannelName) of
        error ->
            user_socket:send_message(UserID, ?ERR_NOSUCHCHANNEL(?TARGET(UserID), ChannelName));
        {ok, #ircchannel{users = Users, name = ChannelName2}} ->
            lists:foreach(fun
                (MemberID) when MemberID =/= UserID ->
                    Msg = ?MSG_FROM(User, "PRIVMSG", [ChannelName2, Text]),
                    user_socket:send_message(MemberID, Msg);
                (_Member) -> ok
            end, dict:fetch_keys(Users))
    end.

send_to_user(SenderID, Nick, Text) ->
    case ircusers:get_by_nick(Nick) of
        {error, _} ->
            Target = ?TARGET(SenderID),
            user_socket:send_message(SenderID, ?ERR_NOSUCHNICK(Target, Nick));
        {ok, #ircuser{socket_process = RecID}} ->
            {ok, Sender} = ircusers:get(SenderID),
            user_socket:send_message(RecID, ?MSG_FROM(Sender, "PRIVMSG", [Nick, Text]))
    end.