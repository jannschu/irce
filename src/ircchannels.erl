-module(ircchannels).
-export([join/2,
get_channel_with_user/1,
    get_channel_modes/1,
       get_user_modes/2,
          get_channel/1,
            user_part/2,
            user_quit/1]).

% -export([join/2, part/2, part/3, privmsg/3]).
% 
-behaviour(gen_server).
-export([start_link/0,
               init/1,
        handle_call/3,
        handle_cast/2,
          terminate/2,
        code_change/3,
        handle_info/2]).

-record(ircchannel_of_user, {userid, channels = sets:new()}).
-include("ircchannel.hrl").

%% TODO: maybe add event manager for data consistency
 
%%----------------------------------------------------------------------------
%% public api methods
%%----------------------------------------------------------------------------
 
%% Channel is the case insensitive name of the channel to join
%% returns: {ok, Channel, UserModes}
join(UserID, Channel) ->
    gen_server:call(?MODULE, {user_join, UserID, Channel}).

%% The user leaves all channels
user_quit(UserID) ->
    gen_server:cast(?MODULE, {user_quit, UserID}).

user_part(UserID, ChannelName) ->
    gen_server:cast(?MODULE, {user_part, UserID, ChannelName}).

% 
% part(UserID, Channel) ->
%     gen_server:cast(?MODULE, {user_part, UserID, Channel, nil}).
% 
% part(UserID, Channel, Reason) ->
%     gen_server:cast(?MODULE, {user_part, UserID, Channel, Reason}).
% 
% privmsg(UserID, Channel, Msg) ->
%     gen_server:cast(?MODULE, {user_privmsg, Channel, Msg}).
% 
% % has_mode(Channel, Mode) -> ok.

%% {ok, Channel} | error
get_channel(ChannelName) ->
    gen_server:call(?MODULE, {get_channel, ChannelName}).

get_channel_modes(ChannelName) ->
    gen_server:call(?MODULE, {get_mode, ChannelName}).

%% {ok, [mode, mode]} | {error, Reason}
get_user_modes(ChannelName, UserID) ->
    gen_server:call(?MODULE, {get_user_mode, ChannelName, UserID}).

%% {ok, [Channel]} | {error, _}
get_channel_with_user(UserID) ->
    gen_server:call(?MODULE, {get_channel_with_user, UserID}).

%%----------------------------------------------------------------------------
%% gen_server methods
%%----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    mnesia:create_table(ircchannel, [
        {attributes, record_info(fields, ircchannel)}
    ]),
    mnesia:create_table(ircchannel_of_user, [
        {attributes, record_info(fields, ircchannel_of_user)}
    ]),
    MonRefs = dict:new(),
    {ok, MonRefs}.

% synchronous
handle_call({get_mode, ChannelName}, _From, State) ->
    Reply = case mnesia:dirty_read({ircchannel, string:to_lower(ChannelName)}) of
        [#ircchannel{modes = Modes}] -> {ok, Modes};
        _ -> {error, not_found}
    end,
    {reply, Reply, State};

handle_call({get_channel, ChannelName}, _From, State) ->
    Reply = case mnesia:dirty_read({ircchannel, string:to_lower(ChannelName)}) of
        [Channel] -> {ok, Channel};
        _ -> error
    end,
    {reply, Reply, State};

handle_call({get_user_mode, ChannelName, UserID}, _From, State) ->
    Reply = case mnesia:dirty_read({ircchannel, string:to_lower(ChannelName)}) of
        [#ircchannel{users = Users}] ->
            case dict:find(UserID, Users) of
                {ok, Modes} -> {ok, Modes};
                error -> {error, not_found}
            end;
        _ -> {error, not_found}
    end,
    {reply, Reply, State};

handle_call({get_channel_with_user, UserID}, _From, State) ->
    Reply = read_channels_with_user(UserID),
    {reply, Reply, State};

handle_call({user_join, UserID, ChannelNameCaseIns}, _From, Refs) ->
    {Channel, UserModes} = case ensure_channel_exists(ChannelNameCaseIns) of
        {created, Chan} ->
            {Chan, "Oo"};
        {existed, Chan} ->
            {Chan, ""}
    end,
    ChannelWithUser = add_user_to_channel(UserID, UserModes, Channel),
    NewRef = monitor_user(UserID, Refs),
    {reply, {ok, ChannelWithUser, UserModes}, NewRef}.

% asynchronous
handle_cast({user_part, UserID, ChannelName}, State) ->
    remove_user_from_channel(UserID, ChannelName),
    {noreply, State};

handle_cast({user_quit, UserID}, Refs) ->
    NewRefs = demonitor_user(UserID, Refs),
    {ok, Channels} = read_channels_with_user(UserID),
    lists:foreach(fun(ChannelName) ->
        remove_user_from_channel(UserID, ChannelName)
    end, Channels),
    mnesia:dirty_delete({ircchannel_of_user, UserID}),
    {noreply, NewRefs}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({'DOWN', _Ref, process, UserID, _Reason}, State) ->
    user_quit(UserID),
    {noreply, State}.

terminate(_Signal, _State) ->
    mnesia:clear_table(ircchannel),
    mnesia:clear_table(ircchannel_of_user),
    ok.

%%----------------------------------------------------------------------------
%% private methods
%%----------------------------------------------------------------------------

read_channels_with_user(UserID) ->
    case mnesia:dirty_read({ircchannel_of_user, UserID}) of
        [User] ->
            {ok, sets:to_list(User#ircchannel_of_user.channels)};
        _ ->
            {error, not_found}
    end.

monitor_user(UserID, MonRefs) ->
    case dict:find(UserID, MonRefs) of
        {ok, _} -> MonRefs;
        error ->
            Ref = erlang:monitor(process, UserID),
            dict:store(UserID, Ref, MonRefs)
    end.

demonitor_user(UserID, MonRefs) ->
    case dict:find(UserID, MonRefs) of
        {ok, Ref} ->
            erlang:demonitor(Ref, [flush]),
            dict:erase(UserID, MonRefs);
        error ->
            MonRefs
    end.

remove_user_from_channel(UserID, ChannelName) ->
    case mnesia:dirty_read({ircchannel_of_user, UserID}) of
        [User] ->
            #ircchannel_of_user{channels = UserChannels} = User,
            NewChannelList = sets:del_element(ChannelName, UserChannels),
            NewUser = User#ircchannel_of_user{channels = NewChannelList},
            mnesia:dirty_write(NewUser);
        _ ->
            ok
    end,
    
    %% in table 'ircchannel'
    case mnesia:dirty_read({ircchannel, ChannelName}) of
        [Channel] ->
            #ircchannel{users = ChannelUsers} = Channel,
            NewUserList = dict:erase(UserID, ChannelUsers),
            case dict:fetch_keys(NewUserList) of
                [] ->
                    ChannelName = Channel#ircchannel.lower_name,
                    mnesia:dirty_delete({ircchannel, ChannelName});
                _ ->
                    mnesia:dirty_write(Channel#ircchannel{users = NewUserList})
            end;
        _ -> ok
    end.

add_user_to_channel(UserID, UserChannelModes, Channel) ->
    ChannelName = Channel#ircchannel.name,
    Entry = case mnesia:dirty_read({ircchannel_of_user, UserID}) of
        [User] -> User;
        _ ->
            #ircchannel_of_user{userid = UserID, channels = sets:new()}
    end,
    NewSet = sets:add_element(ChannelName, Entry#ircchannel_of_user.channels),
    mnesia:dirty_write(Entry#ircchannel_of_user{channels = NewSet}),
    
    %% now in table 'ircchannel'
    #ircchannel{users = ChannelUsers} = Channel,
    NewChannelList = dict:store(UserID, UserChannelModes, ChannelUsers),
    NewChannel = Channel#ircchannel{users = NewChannelList},
    mnesia:dirty_write(NewChannel),
    NewChannel.

ensure_channel_exists(Name) ->
    LowerName = string:to_lower(Name),
    case mnesia:dirty_read({ircchannel, LowerName}) of
        [Channel] ->
            {existed, Channel};
        _ ->
            Modes = dict:store($s, true, dict:store($n, true, dict:new())),
            Channel = #ircchannel{lower_name = LowerName, name = Name, modes = Modes},
            mnesia:dirty_write(Channel),
            {created, Channel}
    end.