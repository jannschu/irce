-module(user_command).

-export([run_user/2]).
-include("replies.hrl").

-include_lib("kernel/include/inet.hrl").

run_user(UserID, [UserName, Mode, _Unused, Real | _Rest]) ->
    ircusers:update_user(UserID, fun(User) ->
        case User of
            #ircuser{user = nil, host = nil, real = nil, nick = Nick} ->
                Socket = user_socket:get_socket(UserID),
                Host = get_host(Socket, UserName),
                NewUser = User#ircuser{user = UserName, host = Host, real = Real},
                case Nick of
                    nil -> ok;
                    _ -> userregistration:for_user(NewUser)
                end,
                NewUser;
            _ ->
                user_socket:send_message(UserID, ?ERR_ALREADYREGISTRED(?TARGET(UserID))),
                User
        end
    end);

run_user(UserID, _) ->
    user_socket:send_message(UserID, ?ERR_NEEDMOREPARAMS(?TARGET(UserID), "USER")).

get_host(Socket, UserName) ->
    case inet:peername(Socket) of
        {ok, {Address, _Port}} ->
            case inet:gethostbyaddr(Address) of
                {ok, #hostent{h_name = Host}} ->
                    Host;
                _ ->
                    UserName
            end;
        _ ->
            UserName
    end.
