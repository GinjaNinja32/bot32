-module(vote).
-compile(export_all).

get_commands() ->
	[
		{"vote", fun vote/1, user},
		{"votemake", fun votemake/1, vote},
		{"votecount", fun votecount/1, vote},
		{"voteresult", fun voteresult/1, vote},
		{"votedel", fun votedel/1, vote},
		{"voteinfo", fun voteinfo/1, user}
	].

vote(#{nick:=Nick, reply:=Reply, ping:=Ping, params:=Params}) ->
	LNick = string:to_lower(Nick),
	case Params of
		[ID|OptionTokens] ->
			Option = string:join(OptionTokens, " "),
			case config:get_value(data, [?MODULE, votes, ID]) of
				'$none' -> {irc, {msg, {Reply, [Ping, "Invalid vote id!"]}}};
				_ ->
					Options = config:get_value(data, [?MODULE, votes, ID, options]),
					case lists:member(Option, Options) of
						false -> {irc, {msg, {Reply, [Ping, "Invalid option; valid options: ", string:join(Options, "; ")]}}};
						true ->
							case config:get_value(data, [?MODULE, votes, ID, votes, LNick]) of
								'$none' ->
									config:set_value(data, [?MODULE, votes, ID, votes, LNick], Option),
									{irc, {msg, {Reply, [Ping, "Vote cast."]}}};
								_ ->
									config:set_value(data, [?MODULE, votes, ID, votes, LNick], Option),
									{irc, {msg, {Reply, [Ping, "Vote changed."]}}}
							end
					end
			end;
		_ -> {irc, {msg, {Reply, [Ping, "Provide a vote ID and an option to pick!"]}}}
	end.

ipow(B, P) -> ipow(B, P, 1).
ipow(_, 0, O) -> O;
ipow(B, P, O) -> ipow(B, P-1, O*B).

votemake(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	QnOptions = lists:map(fun string:strip/1, string:tokens(string:join(Params, " "), "|")),
	Question = hd(QnOptions),
	Options = tl(QnOptions),
	<<MD5:128>> = crypto:hash(md5, string:join(QnOptions, "|")),
	Num = MD5 rem ipow(36, 5),
	VoteKey = lists:flatten(io_lib:format("~5.36.0B", [Num])),
	case config:get_value(data, [?MODULE, votes, VoteKey]) of
		'$none' ->
			config:set_value(data, [?MODULE, votes, VoteKey, question], Question),
			config:set_value(data, [?MODULE, votes, VoteKey, options], Options),
			{irc, {msg, {Reply, [Ping, "Vote created; vote ID '", VoteKey, $']}}};
		_ ->
			{irc, {msg, {Reply, [Ping, "Hash collision. If you are sure this is a new poll, try changing one option slightly."]}}}
	end.

votecount(#{reply:=Reply, ping:=Ping, params:=[ID]}) ->
	case config:get_value(data, [?MODULE, votes, ID]) of
		'$none' -> {irc, {msg, {Reply, [Ping, "That vote does not exist."]}}};
		_ ->
			NVotes = length(config:get_value(data, [?MODULE, votes, ID, votes], [])),
			{irc, {msg, {Reply, [Ping, "Vote ", ID, " has ", integer_to_list(NVotes), " vote", util:s(NVotes), $.]}}}
	end.

voteresult(#{reply:=Reply, ping:=Ping, params:=[ID]}) ->
	case config:get_value(data, [?MODULE, votes, ID]) of
		'$none' -> {irc, {msg, {Reply, [Ping, "That vote does not exist."]}}};
		_ ->
			RawVotes = config:get_value(data, [?MODULE, votes, ID, votes], []),
			Options = config:get_value(data, [?MODULE, votes, ID, options]),
			Votes = lists:map(fun(Option) ->
					lists:sum(lists:map(fun({_,V}) when V==Option -> 1; (_) -> 0 end, RawVotes))
				end, Options),
			core ! {irc, {msg, {Reply, ["Vote ", ID, ": ", config:get_value(data, [?MODULE, votes, ID, question])]}}},
			lists:foreach(fun({Opt,Num}) ->
						core ! {irc, {msg, {Reply, [Opt, $:, $ , integer_to_list(Num)]}}}
					end, lists:reverse(lists:ukeysort(2, lists:zip(Options, Votes)))),
			ok
	end.

votedel(#{reply:=Reply, ping:=Ping, params:=[ID]}) ->
	case config:get_value(data, [?MODULE, votes, ID]) of
		'$none' -> {irc, {msg, {Reply, [Ping, "That vote does not exist."]}}};
		_ ->
			config:del_value(data, [?MODULE, votes, ID]),
			{irc, {msg, {Reply, [Ping, "Vote deleted"]}}}
	end.

voteinfo(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	case Params of
		[ID] ->
			case config:get_value(data, [?MODULE, votes, ID]) of
				'$none' -> {irc, {msg, {Reply, [Ping, "That vote does not exist."]}}};
				_ ->
					Question = config:get_value(data, [?MODULE, votes, ID, question]),
					Options = config:get_value(data, [?MODULE, votes, ID, options]),
					core ! {irc, {msg, {Reply, [Ping, "Vote ", ID, ": ", Question]}}},
					core ! {irc, {msg, {Reply, [Ping, "Options: ", string:join(Options, "; ")]}}},
					ok
			end;
		_ -> {irc, {msg, {Reply, [Ping, "Usage: voteinfo [id]"]}}}
	end.
