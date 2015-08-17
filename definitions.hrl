
-define(VERSION, "Bot32 v0.4").

-record(state, {nick, prefix, permissions, aliases, commands, moduledata, modules}).
-record(user, {nick, username, host}).
