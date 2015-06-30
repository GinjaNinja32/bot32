
-define(VERSION, "Bot32 v0.2").
-define(TRANSPORT, ssl).

-record(state, {nick, prefix, permissions, ignore, commands, moduledata, modules}).
-record(user, {nick, username, host}).
