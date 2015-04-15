
-define(VERSION, "Bot32 v0.1").

-record(state, {nick, prefix, admins, ignore, commands, moduledata}).
-record(user, {nick, username, host}).
