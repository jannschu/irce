-module(ircusers).

-export ([new/0,
          get/1,
  get_by_nick/1,
  update_user/2,
  remove_user/1]).

-behaviour(gen_server).
-export([start_link/0,
               init/1,
        handle_call/3,
        handle_cast/2,
          terminate/2,
        code_change/3,
        handle_info/2]).

-include("ircuser.hrl").

%% TODO: maybe add event manager for data consistency

%%----------------------------------------------------------------------------
%% public api methods
%%----------------------------------------------------------------------------

new() -> 
    gen_server:call(?MODULE, new_user).

get_by_nick(Nick) ->
    gen_server:call(?MODULE, {get_user_by_nick, Nick}).

get(UserID) ->
    gen_server:call(?MODULE, {get_user, UserID}).

update_user(UserID, Fun) ->
    gen_server:cast(?MODULE, {update_user, UserID, Fun}).

remove_user(UserID) ->
    gen_server:cast(?MODULE, {remove_user, UserID}).

%%----------------------------------------------------------------------------
%% gen_server methods
%%----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    mnesia:create_table(ircuser, [
        {attributes, record_info(fields, ircuser)}, {index, [nick]}
    ]),
    MonitorRefs = dict:new(), % UserID -> Ref
    {ok, MonitorRefs}.

% synchronous
handle_call(new_user, {From, _Tag}, Refs) ->
    User = #ircuser{socket_process = From},
    Ref = erlang:monitor(process, From),
    NewRef = dict:store(From, Ref, Refs),
    mnesia:dirty_write(User),
    {reply, User#ircuser.socket_process, NewRef};

handle_call({get_user, UserID}, _From, State) ->
    Reply = case mnesia:dirty_read({ircuser, UserID}) of
        [User] -> {ok, User};
        _ -> {error, not_found}
    end,
    {reply, Reply, State};

handle_call({get_user_by_nick, Nick}, _Form, State) ->
    Reply = case mnesia:dirty_index_read(ircuser, Nick, nick) of
        [User] -> {ok, User};
        _ -> {error, not_found}
    end,
    {reply, Reply, State}.

% asynchronous
handle_cast({update_user, UserID, Fun}, State) ->
    case mnesia:dirty_read({ircuser, UserID}) of
        [User] ->
            NickBefore = User#ircuser.nick,
            NewUser = Fun(User),
            NickAfter = NewUser#ircuser.nick,
            case {NickBefore, NickAfter} of
                {A, B} when A =/= B andalso B =/= nil ->
                    case nick_exists(B) of
                        true -> error;
                        false -> mnesia:dirty_write(NewUser)
                    end;
                {_, _} -> % {nil, nil}, {"x", "x"}
                    mnesia:dirty_write(NewUser)
            end,
            {noreply, State};
        _ ->
            {noreply, State}
    end;

handle_cast({remove_user, UserID}, Dict) ->
    mnesia:dirty_delete({ircuser, UserID}),
    NewDict = case dict:find(UserID, Dict) of
        {ok, Ref} ->
            erlang:demonitor(Ref, [flush]),
            dict:erase(UserID, Dict);
        _ -> Dict
    end,
    {noreply, NewDict}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({'DOWN', _Ref, process, SocketProcess, _Reason}, State) ->
    remove_user(SocketProcess), % SocketProcess is UserID
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Signal, _State) -> 
    mnesia:clear_table(ircuser),
    ok.

%%----------------------------------------------------------------------------
%% private methods
%%----------------------------------------------------------------------------
nick_exists(Nick) ->
    case mnesia:dirty_index_read(ircuser, Nick, nick) of
        [_] -> true;
        _ -> false
    end.
    