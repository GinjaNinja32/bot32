An IRC bot, built in Erlang.

Edit the config at `bot_config.erl` before starting the bot.
If either start method is used and any of a server, port, or transport are *not* supplied, the values in `core_config.crl` will be used; either ensure they are supplied, or edit that file too.

# Starting the bot:
`$ bot_start -s[SERVER] -p[PORT] -s[TRANSPORT]`  
SERVER should be the URL or IP of the server  
PORT should be a numeric port  
TRANSPORT should be `gen_tcp` (standard TCP connection) or `ssl` (SSL connection)

For example, to connect via SSL to irc.example.com:6697: `bot_start -sirc.example.com -p6697 -tssl`

# Starting the bot from the shell:
Compile all included modules, then call `common:start()` or `common:start(Server, Transport, Port)` (valid values for `Transport` are `ssl` and `gen_tcp`, or any module which is API-compatible with `gen_tcp`)

# Licensing

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
