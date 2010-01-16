-module(quit_command).
-export([run_user/2]).

-include("replies.hrl").
-include("ircchannel.hrl").

run_user(UserID, []) ->
    run_user(UserID, [nil]);

run_user(UserID, [Msg]) ->
    broadcast_quit(UserID, Msg),
    user_socket:close_connection(UserID).

broadcast_quit(UserID, Msg) ->
    {ok, User} = ircusers:get(UserID),
    QuitMsg = case Msg of
        nil ->
            Nick = User#ircuser.nick,
            ?MSG_FROM(User, "QUIT", [Nick]);
        _ ->
            ?MSG_FROM(User, "QUIT", [Msg])
    end,
    case ircchannels:get_channel_with_user(UserID) of
        {ok, Channels} ->
            send_to_users_in_channel(Channels, QuitMsg, UserID);
        {error, not_found} -> ok
    end.

send_to_users_in_channel(Channels, QuitMsg, From) ->
    send_to_users_in_channel(Channels, QuitMsg, From, sets:new()).

send_to_users_in_channel([], _QuitMsg, _From, SentTo) -> SentTo;
send_to_users_in_channel([ChannelName|Channels], QuitMsg, From, SentTo) ->
    {ok, #ircchannel{users = Users}} = ircchannels:get_channel(ChannelName),
    NewUsers = lists:delete(From, dict:fetch_keys(Users)),
    NewSentTo = send_to_users(NewUsers, QuitMsg, SentTo),
    send_to_users_in_channel(Channels, QuitMsg, NewSentTo).

send_to_users([], _Msg, Sent) -> Sent;
send_to_users([UserID|Rest], QuitMsg, Sent) ->
    case sets:is_element(UserID, Sent) of
        true ->
            send_to_users(Rest, QuitMsg, Sent);
        false ->
            user_socket:send_message(UserID, QuitMsg),
            send_to_users(Rest, QuitMsg, sets:add_element(UserID, Sent))
    end.
    