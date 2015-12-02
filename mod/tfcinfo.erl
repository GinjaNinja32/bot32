-module(tfcinfo).
-compile(export_all).

get_commands() ->
	[
		{"tfcm2o", generic(fun metal2ore/1), user},
		{"tfco2m", generic(fun ore2metal/1), user},
		{"tfcs2o", generic(fun stone2ore/1), user},
		{"tfco2s", generic(fun ore2stone/1), user},
		{"tfcs2t", generic(fun stone2type/1), user},
		{"tfct2s", generic(fun type2stone/1), user}
	].

generic(Func) ->
	fun(#{reply:=R, ping:=P, params:=Params}) ->
		Reply = lists:map(Func, Params),
		{irc, {msg, {R, [P, string:join(Reply, "; ")]}}}
	end.

metal2ore(T) ->
	case ores_of_metal(T) of
		[] -> ["? -> "|T];
		List -> [string:join(List,", ")," -> "|T]
	end.
ore2metal(T) ->
	case orddict:find(T, ore_list()) of
		error -> [T|" -> ?"];
		{ok,{Metal,_}} -> [T," -> "|Metal]
	end.
stone2ore(T) ->
	case ores_in_stone(T) of
		[] -> [T|" -> ?"];
		List -> [T," -> "|string:join(List,", ")]
	end.
ore2stone(T) ->
	case orddict:find(T, ore_list()) of
		error -> ["? -> "|T];
		{ok, {_,StoneTypes}} -> [string:join(StoneTypes,", ")," -> "|T]
	end.
stone2type(T) ->
	case orddict:find(T, stone_list()) of
		{ok, Type} -> [T," - "|Type];
		error -> [T|" - ?"]
	end.
type2stone(T) ->
	case string:join(orddict:fetch_keys(orddict:filter(fun
			(_,X)->X==T
		end, stone_list())), ", ") of
		[] -> ["? - "|T];
		List -> [List," - "|T]
	end.

% UTIL FUNCTIONS

ores_in_stone(Stone) ->
	case orddict:find(Stone, stone_list()) of
		{ok, StoneType} ->
			orddict:fetch_keys(orddict:filter(fun
					(_, {_, StoneList}) ->
						lists:member(StoneType, StoneList) orelse lists:member(Stone, StoneList)
				end, ore_list()));
		error -> []
	end.

ores_of_metal(Metal) ->
	orddict:fetch_keys(orddict:filter(fun
			(_, {M, _}) -> M == Metal
		end, ore_list())).

% DATA

ore_list() ->
	[
		{"Bismuthinite", {"Bismuth", ["IgneousExtrusive", "Sedimentary"]}},
		{"BituminousCoal", {"Coal", ["Sedimentary"]}},
		{"Borax", {"Flux", ["Rock Salt"]}},
		{"Cassiterite", {"Tin", ["IgneousIntrusive"]}},
		{"Cinnabar", {"Redstone", ["IgneousExtrusive", "Quartzite", "Shale"]}},
		{"Cryolite", {"Redstone", ["Granite"]}},
		{"Garnierite", {"Nickel", ["Gabbro"]}},
		{"Graphite", {"GraphitePowder", ["Gneiss", "Marble", "Quartzite", "Schist"]}},
		{"Hematite", {"Iron", ["IgneousExtrusive"]}},
		{"Kaolinite", {"KaolinitePowder", ["Sedimentary"]}},
		{"Kimberlite", {"Diamond", ["Gabbro"]}},
		{"LapisLazuli", {"LapisLazuli", ["Marble"]}},
		{"Lignite", {"Coal", ["Sedimentary"]}},
		{"Limonite", {"Iron", ["Sedimentary"]}},
		{"Magnetite", {"Iron", ["Sedimentary"]}},
		{"Malachite", {"Copper", ["Marble", "Limestone"]}},
		{"NativeCopper", {"Copper", ["IgneousExtrusive"]}},
		{"NativeGold", {"Gold", ["IgneousExtrusive", "IgneousIntrusive"]}},
		{"NativeSilver", {"Silver", ["Granite", "Gneiss"]}},
		{"Saltpeter", {"SaltpeterPowder", ["Sedimentary"]}},
		{"Sphalerite", {"Zinc", ["Metamorphic"]}},
		{"Sulfur", {"SulfurPowder", ["Lava"]}},
		{"Sylvite", {"Fertilizer", ["RockSalt"]}},
		{"Tetrahedrite", {"Copper", ["Metamorphic"]}}
	].
stone_list() ->
	[
		{"Andesite", "IgneousExtrusive"},
		{"Basalt", "IgneousExtrusive"},
		{"Chalk", "Sedimentary"},
		{"Chert", "Sedimentary"},
		{"Claystone", "Sedimentary"},
		{"Conglomerate", "Sedimentary"},
		{"Dacite", "IgneousExtrusive"},
		{"Dolomite", "Sedimentary"},
		{"Diorite", "IgneousIntrusive"},
		{"Gabbro", "IgneousIntrusive"},
		{"Gneiss", "Metamorphic"},
		{"Granite", "IgneousIntrusive"},
		{"Limestone", "Sedimentary"},
		{"Marble", "Metamorphic"},
		{"Phyllite", "Metamorphic"},
		{"Quartzite", "Metamorphic"},
		{"Rhyolite", "IgneousExtrusive"},
		{"RockSalt", "Sedimentary"},
		{"Schist", "Metamorphic"},
		{"Shale", "Sedimentary"},
		{"Slate", "Metamorphic"}
	].
