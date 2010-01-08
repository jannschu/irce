-record(ircchannel, {
    lower_name,
    name,
    users = dict:new(), %% userID -> [modeChar, modeChar]
    modes = dict:new(),
    topic = nil
}).