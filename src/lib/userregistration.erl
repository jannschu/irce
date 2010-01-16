-module(userregistration).
-export([for_user/1, uptime_date/0]).

-include("replies.hrl").

for_user(#ircuser{user = User, host = UserHost, nick = Nick, socket_process = UserID}) ->
    user_socket:send_message(UserID, ?RPL_WELCOME(Nick, Nick, User, UserHost)),
    {ok, Version} = application:get_key(vsn),
    user_socket:send_message(UserID, ?RPL_YOURHOST(Nick, ?SERVERHOST, "irce-" ++ Version)),
    % it is not the time it was build, but started
    user_socket:send_message(UserID, ?RPL_CREATED(Nick, uptime_date())),
    spawn(motd_command, run_user, [UserID, []]).

uptime_date() ->
    {Mega, Secs, Micro} = erlang:now(),
    {UpTimeMilliseconds, _} = erlang:statistics(wall_clock),
    StartTime = normalize({Mega, Secs, Micro - UpTimeMilliseconds * 1000}),
    UTC = calendar:now_to_universal_time(StartTime),
    utc_to_list(UTC).

normalize({Mega, Secs, Micro}) when Micro < 0 orelse Micro >= 1000000 ->
    normalize({Mega, Secs + (Micro div 1000000), abs(Micro rem 1000000)});

normalize({Mega, Secs, Mirco}) when Secs < 0 orelse Secs >= 1000000 ->
    normalize({Mega + (Secs div 1000000), abs(Secs rem 1000000), Mirco});

normalize(Time) -> Time.

utc_to_list({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    [integer_to_list(Year), $-,
     string:right(integer_to_list(Month), 2, $0), $-,
     string:right(integer_to_list(Day), 2, $0), $ ,
     string:right(integer_to_list(Hour), 2, $0), $:,
     string:right(integer_to_list(Minute), 2, $0), $:,
     string:right(integer_to_list(Second), 2, $0), " UTC"].
     
    