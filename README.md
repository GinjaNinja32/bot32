An IRC bot, built in Erlang.

# Configuration:
`!c <expression> [, <expression> [, <expression [...]]]`  
`<expression>` is either `<path>` or `<path> = <value>`, in Erlang syntax; paths are lists.

For example, to instruct the bot to join a list of channels at startup:  
`!c [bot,channels] = ["#channel", "#otherchannel", "#thirdchannel"]`

# Starting the bot:
`$ bot_start -s<server> -p<port> -t<transport>`  
Server, port, and transport default to the values in config/core.crl, which should be in the following format (entries can be skipped if desired):  
`[{port,6667}, {server,"irc.example.com"}, {transport,gen_tcp}].` - this is irc.example.com:6667, without SSL  
`[{port,6697}, {server,"irc.whatever.net"}, {transport,ssl}].` - irc.whatever.net:6697, with SSL

# Starting the bot from the shell:
```
1> c(loader).
{ok,loader}.
2> loader:load().
[compilation output]
ok
```
followed by EITHER of
- `3> common:start()` to use the server address/port/transport mode in the config file
- `3> common:start(Server, Transport, Port)` to provide these values and skip the configuration. `Server` and `Port` will be passed to the `Transport:connect()` call unchanged. `Transport` should either be the atom `ssl`, the atom `gen_tcp`, or the name of any module which is API-compatible with `gen_tcp` and sends messages of the same format as either `ssl` or `gen_tcp`.

# Licensing

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
