-module(ircmessage).
-export([parse/1, parse_argument/1, message_to_list/1]).
-include("ircmessage.hrl").

parse(Line) ->
    parse(message, Line, #ircmessage{command = nil}).

parse_argument(Arg) ->
    parse_argument(Arg, []).

message_to_list(#ircmessage{prefix = Prefix,
                            command = Command,
                            params = Params}) ->
    [if
        Prefix =:= "" ->
            Command;
       true ->
           [$: | [Prefix | [$  | Command]]]
    end | case params_to_list(Params) of
        "" -> "\r\n";
        ParamStr -> [[$  | ParamStr] | "\r\n"]
    end].

%% The BNF for a message
%%
%% message    =  [ ":" prefix SPACE ] command [ params ] crlf
%% prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
%% command    =  1*letter / 3digit
%% params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
%%            =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]
%% 
%% nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
%%                 ; any octet except NUL, CR, LF, " " and ":"
%% middle     =  nospcrlfcl *( ":" / nospcrlfcl )
%% trailing   =  *( ":" / " " / nospcrlfcl )
%% 
%% SPACE      =  %x20        ; space character
%% crlf       =  %x0D %x0A   ; "carriage return" "linefeed"

parse(message, [$: | Rest], Record) ->
    case string:chr(Rest, $ ) of
        0 -> {error, illegal_message_prefix};
        N -> 
            {Prefix, Msg} = lists:split(N - 1, Rest),
            NewRecord = Record#ircmessage{prefix = Prefix},
            parse(message, tl(Msg), NewRecord)
    end;

parse(message, [First | Rest], Record) ->
    if
        (First >= $0) andalso (First =< $9) ->
            parse(cmd_digit, [First | Rest], Record);
        true ->
            parse(cmd_string, [First | Rest], Record)
    end;

parse(cmd_digit, Line, Record) ->
    {Number, Rest} = lists:split(3, Line),
    case string:to_integer(Number) of
        {_N, []} ->
            parse(params, Rest, Record#ircmessage{command = Number});
        {error, _} ->
            {error, illegal_command}
    end;

parse(cmd_string, Line, Record) ->
    Length = string:span(Line, "abcdefghijklmnopqrstuvwxyz" ++
                               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    case Length of
        0 ->
            {error, illegal_command};
        N ->
            {Command, Rest} = lists:split(N, Line),
            UpperCommand = string:to_upper(Command),
            parse(params, Rest, Record#ircmessage{command = UpperCommand})
    end;

parse(params, [$  | Rest], Record) ->
    case Rest of
        [$  | _] -> {error, illegal_param};
        _ -> parse(params, Rest, Record)
    end;

%% parse a trailing 
parse(params, [$: | Rest], Record) ->
    ParamLength = string:span(Rest, [$: | [$  | nospcrlfcl()]]),
    {Param, EndMessage} = lists:split(ParamLength, Rest),
    NewParams = [Param | get_params(Record)],
    parse(crlf, EndMessage, Record#ircmessage{params = NewParams});

%% no params, parse terminating CR LF sequence
parse(params, "\r\n", Record) ->
    Record#ircmessage{params = lists:reverse(get_params(Record))};

%% parse a middle param
parse(params, RestLine, #ircmessage{params = Params} = Record)
    when length(Params) == 14 ->
        parse(params, [$: | RestLine], Record);

parse(params, RestLine, Record) ->
    ParamLength = string:span(RestLine, [$: | nospcrlfcl()]),
    {NewParam, Rest} = lists:split(ParamLength, RestLine),
    case NewParam of
        "" ->
            {error, illegal_param};
        _ ->
            NewParams = [NewParam | get_params(Record)],
            parse(params, Rest, Record#ircmessage{params = NewParams})
    end;

parse(crlf, "\r\n", Record) ->
    Record#ircmessage{params = lists:reverse(get_params(Record))};

parse(crlf, _Text, _Record) ->
    {error, illegal_message_termination}.

get_params(#ircmessage{params = Params}) ->
    Params.

nospcrlfcl() -> 
    [  1,   2,   3,   4,   5,   6,   7,   8,   9,  11,  12,  14,  15,  16,
      17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,
      31,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,
      46,  47,  48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  59,  60,
      61,  62,  63,  64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,
      75,  76,  77,  78,  79,  80,  81,  82,  83,  84,  85,  86,  87,  88,
      89,  90,  91,  92,  93,  94,  95,  96,  97,  98,  99, 100, 101, 102,
     103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
     117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130,
     131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144,
     145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158,
     159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172,
     173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186,
     187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200,
     201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214,
     215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228,
     229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242,
     243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255].

parse_argument([], Args) ->
    lists:reverse(Args);

parse_argument(Arg, Args) ->
    case string:chr(Arg, $,) of
        0 -> parse_argument([], [Arg|Args]);
        N ->
            {A, B} = lists:split(N - 1, Arg),
            parse_argument(tl(B), [A|Args])
    end.

params_to_list([]) ->
    [];

params_to_list([Param | []]) ->
    IncludeSpace = string:chr(Param, $ ) =/= 0,
    if
        IncludeSpace -> [$: | Param];
        true -> Param
    end;

params_to_list([Param | Rest]) ->
    [Param | [$  | params_to_list(Rest)]].