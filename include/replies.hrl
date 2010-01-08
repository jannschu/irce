-include("ircmessage.hrl").
-include("ircuser.hrl").

-define(SERVERHOST, element(2, inet:gethostname())).
-define(TARGET(UserID), user_helper:rpl_target(UserID)).
-define(RPL(Nr, Params), #ircmessage{prefix = ?SERVERHOST, command = Nr, params = Params}).
-define(MSG_FROM(User, Command, Params), #ircmessage{
    prefix = [User#ircuser.nick, $!, User#ircuser.user, $@, User#ircuser.host],
    command = Command,
    params = Params
}).

%% client-server connection replies

-define(RPL_WELCOME(Target, Nick, User, Host),
    ?RPL("001", [Target, "Welcome to the Internet Relay Network " ++ Nick ++ "!" ++ User ++ "@" ++ Host])).
-define(RPL_YOURHOST(Target, Host, Version),
    ?RPL("002", [Target, "Your host is " ++ Host ++ ", running version " ++ Version])).
-define(RPL_CREATED(Target, Date),
    ?RPL("003", [Target, "This server was created " ++ Date])).
-define(RPL_MYINFO(Target, ServerName, Version, UserModes, ChannelModes),
    ?RPL("004", [string:join([ServerName, Version, UserModes, ChannelModes])])).

-define(RPL_NOTOPIC(Target, Channel), ?RPL("331", [Target, Channel, "No topic is set"])).
-define(RPL_TOPIC(Target, Channel, Topic), ?RPL("331", [Target, Channel, Topic])).

-define(RPL_NAMREPLY(Target, Channel, Nicks), ?RPL("353", [Target, Channel, Nicks])).
-define(RPL_ENDOFNAMES(Target, Channel), ?RPL("366", [Target, Channel, "End of NAMES list"])).

-define(RPL_MOTDSTART(Target), ?RPL("375", [Target, "- " ++ ?SERVERHOST ++ " Message of the day"])).
-define(RPL_MOTD(Target, Line), ?RPL("372", [Target, "- " ++ Line])).
-define(RPL_ENDOFMOTD(Target), ?RPL("376", [Target, "End of MOTD command"])).

%% error replies

-define(ERR_NOSUCHNICK(Target, Nick), ?RPL("401", [Target, Nick, "No such nick/channel"])).
-define(ERR_NOSUCHSERVER(Target, Server), ?RPL("402", [Target, Server, "No such server"])).
-define(ERR_NOSUCHCHANNEL(Target, Channel), ?RPL("403", [Target, Channel, "No such channel"])).

-define(ERR_NOORIGIN(Target), ?RPL("409", [Target, "No origin specified"])).

-define(ERR_NORECIPIENT(Target, Command), ?RPL("411", [Target, "No recipient given (", Command, ")"])).
-define(ERR_NOTEXTTOSEND(Target), ?RPL("412", [Target, "No text to send"])).

-define(ERR_UNKNOWNCOMMAND(Target, Command), ?RPL("421", [Target, Command, "Unknown command"])).
-define(ERR_NOMOTD(Target), ?RPL("422", [Target, "MOTD File is missing"])).


-define(ERR_NONICKNAMEGIVEN(Target), ?RPL("431", [Target, "No nickname given"])).
-define(ERR_ERRONEUSNICKNAME(Target, Nick), ?RPL("432", [Target, Nick, "Erroneous nickname"])).
-define(ERR_NICKNAMEINUSE(Target, Nick), ?RPL("432", [Target, Nick, "Nickname is already in use"])).

-define(ERR_NOTONCHANNEL(Target, Channel), ?RPL("442", [Target, Channel, "You're not on that channel"])).

-define(ERR_NOTREGISTERED(Target), ?RPL("451", [Target, "You have not registered"])).

-define(ERR_NEEDMOREPARAMS(Target, Command), ?RPL("461", [Target, Command, "Not enough parameters"])).
-define(ERR_ALREADYREGISTRED(Target), ?RPL("462", [Target, "Unauthorized command (already registered)"])).
