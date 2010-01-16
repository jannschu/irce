-module(part_command).
-export([run_user/2]).

-include("replies.hrl").
-include("ircchannel.hrl").

run_user(UserID, []) ->
    user_socket:send_message(UserID, ?ERR_NEEDMOREPARAMS(?TARGET(UserID), "PART"));

run_user(UserID, [Channel]) ->
    run_user(UserID, [Channel, nil]);

run_user(UserID, [ChannelName, Msg]) ->
    {ok, User} = ircusers:get(UserID),
    Target = User#ircuser.nick,
    case ircchannels:get_channel(ChannelName) of
        {ok, #ircchannel{users = Users}} ->
            case dict:is_key(UserID, Users) of
                true ->
                    ircchannels:user_part(UserID, ChannelName),
                    broadcast_part(User, dict:fetch_keys(Users), ChannelName, Msg);
                false ->
                    user_socket:send_message(UserID, ?ERR_NOTONCHANNEL(Target, ChannelName))
            end;
        {error, _} ->
            user_socket:send_message(UserID, ?ERR_NOSUCHCHANNEL(Target, ChannelName))
    end.

broadcast_part(PartUser, Users, ChannelName, Msg) ->
    PartMsg = case Msg of
        nil ->
            ?MSG_FROM(PartUser, "PART", [ChannelName, PartUser#ircuser.nick]);
        _ ->
            ?MSG_FROM(PartUser, "PART", [ChannelName, Msg])
    end,
    lists:foreach(fun(User) ->
        user_socket:send_message(User, PartMsg)
    end, Users).
    