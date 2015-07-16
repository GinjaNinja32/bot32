% Simple defaults for data handling

get_data(#state{moduledata=M}) ->
	case orddict:find(?MODULE, M) of
		{ok, Value} -> Value;
		error -> default_data()	
	end.

set_data(S=#state{moduledata=M}, Data) ->
	S#state{moduledata=orddict:store(?MODULE, Data, M)}.

store_data(Data) ->
	bot ! {setkey, {?MODULE, Data}},
	ok.
