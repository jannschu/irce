-record(ircuser, {
    socket_process,
    nick = nil,
    % USER command
    user = nil,
    host = nil,
    real = nil,
    % PASS command
    pass = nil,
    modes = [] % [modeChar, modeChar]
}).