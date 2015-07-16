
-define(VERSION, "Bot32 v0.3").

-record(state, {nick, prefix, permissions, ignore, commands, moduledata, modules}).
-record(user, {nick, username, host}).
