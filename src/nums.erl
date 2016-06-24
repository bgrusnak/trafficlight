%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(nums).
-include_lib("eunit/include/eunit.hrl").
-include("trafficlight.hrl").

-compile(export_all).


get_bits(Number) when Number==0 ->
	<<"1110111">>
;

get_bits(Number) when Number==1 ->
	<<"0010010">>
;

get_bits(Number) when Number==2 ->
	<<"1011101">>
;

get_bits(Number) when Number==3 ->
	<<"1011011">>
;

get_bits(Number) when Number==4 ->
	<<"0111010">>
;

get_bits(Number) when Number==5 ->
	<<"1101011">>
;

get_bits(Number) when Number==6 ->
	<<"1101111">>
;

get_bits(Number) when Number==7 ->
	<<"1010010">>
;

get_bits(Number) when Number==8 ->
	<<"1111111">>
;

get_bits(Number) when Number==9 ->
	<<"1111011">>
;

get_bits(Number) when is_list(Number) ->
	Nums=lists:map(fun(X) -> binary_to_list(get_bits(X-48)) end, Number), % fix char to num conversion
	list_to_binary(lists:flatten(Nums))
;

get_bits(Number) ->
	Great=get_bits(Number div 10),
	Least=get_bits(Number rem 10),
	list_to_binary(lists:append(binary_to_list(Great), binary_to_list(Least)))
.


all_bits() ->
	lists:map(fun(X) -> {X, get_bits(X)} end, lists:seq(0, 9))
.

all_bits(Count) ->
	all_bits(Count, length(integer_to_list(Count)))
.

all_bits(Number, Digits) when Number > -1 ->
	lists:append(all_bits(Number-1, Digits), [{Number, get_bits(string:right (integer_to_list(Number), Digits, $0))}])
;

all_bits(_, _) ->
	[]
.


bin_to_bits(Bins) when is_binary(Bins) ->
	bin_to_bits(binary_to_list (Bins) )
;

bin_to_bits([]) ->
	0
;

bin_to_bits(Bins) ->
	{ok, [B], _} = io_lib:fread("~2u", Bins),
	B
.



get_variants(Bits) when  is_binary(Bits) ->
	get_variants(bin_to_bits(Bits), lists:seq(1, 99))
;

get_variants(Bits) ->
	get_variants(Bits,  lists:seq(1, 99))
.

get_variants(Bits, Numbers) when  is_binary(Bits) ->
	get_variants(bin_to_bits(Bits), Numbers)
;

get_variants(Bits, Numbers) ->
	BL=hd (io_lib:format("~.2B", [Bits])),
	NC=(1 bsl length(BL)) -1,
	lists:foldl(fun(X, A) ->  
		{Num, B}=X, 
		case index_of(Num, Numbers) of
		notfound -> A;
		_ ->
			C=bin_to_bits(B), 
			D=C bxor NC,
			case (D band Bits) of
				0 -> lists:append(A, [Num]); 
				_ ->  A
			end
		end
	end, [], all_bits(99))
.

get_broken(Number, Bits, Digits) when  is_binary(Bits) ->
	get_broken(Number, bin_to_bits(Bits), Digits)
;

get_broken(Number, Bits, Digits)  ->
	Fs=string:right (integer_to_list(Number), Digits, $0),
	Correct=bin_to_bits(get_bits(Fs)),
	Rt=Correct bxor Bits,
	Rt
.

index_of(Value, List) ->
   Map = lists:zip(List, lists:seq(1, length(List))),
   case dict:find(Value, dict:from_list(Map)) of
      {ok, Index} -> Index;
      error -> notfound
   end.
  
  
splitlist(_, []) ->
	[]
;

splitlist(Len, List)  when Len < length (List)  ->
	{F,L}=lists:split(Len, List),
	[F|splitlist(Len, L)]
;

splitlist(_, List) ->
	[List]
.

bits_to_nums(Bits, Digits) ->
	U=lists:flatten (hd (io_lib:format("~.2.0B", [Bits]))),
	splitlist(7, string:right(U, Digits*7, $0))  % дополняем нулями до длины кратной 7*число знаков
.

%---------------------------- tests ---------------------------------

get_bits_test () ->
	?assertEqual(<<"11101111101011">>, get_bits("05")),
	?assertEqual(<<"101110111101111101011">>, get_bits("205")),
	?assertEqual(<<"111101110110111101011">>, get_bits(935)),
	?assertEqual(<<"10110111101011">>, get_bits(35)),
	?assertEqual(<<"1101011">>, get_bits(5))	
.

all_bits_test () ->
	?assertEqual(10, length (all_bits())),
	?assertEqual([], all_bits(-5)),
	?assertEqual(16, length (all_bits(15))),
	?assertEqual(all_bits(), all_bits(9))
.

bin_to_bits_test () ->
	?assertEqual(0, bin_to_bits([])),
	?assertEqual(0, bin_to_bits(<<"">>)),
	?assertEqual(bin_to_bits("1101111"), bin_to_bits(<<"1101111">>))
.
get_variants_test() ->
	?assertEqual([56,58,66,68,86,88,96,98], get_variants(<<"11010111101111">>)),
	?assertEqual([56,58], get_variants(<<"11010111101111">>, [50,51,52,53,54,55,56,57,58,59]))
.

get_broken_test () ->
	?assertEqual(0, get_broken(4, <<"11101110111010">>,2)),
	?assertEqual(4098, get_broken(4, <<"10101110111000">>,2))
.

splitlist_test() ->
	?assertEqual([], splitlist(3, [])),
	?assertEqual([[1,2]], splitlist(3, [1,2])),
	?assertEqual([[1,2,3]], splitlist(3, [1,2,3])),
	?assertEqual([[1,2,3],[4]], splitlist(3, [1,2,3,4]))
.

bits_to_nums_test() ->
	?assertEqual(["0000000", "0101100"], bits_to_nums(44, 2))
.
