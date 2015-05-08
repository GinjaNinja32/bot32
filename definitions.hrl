
-define(VERSION, "Bot32 v0.1").

-record(state, {nick, prefix, admins, ignore, commands, moduledata, modules}).
-record(user, {nick, username, host}).
