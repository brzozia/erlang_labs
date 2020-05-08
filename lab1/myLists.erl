%%%-------------------------------------------------------------------
%%% @author Natalia
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. mar 2020 11:01
%%%-------------------------------------------------------------------
-module(myLists).
-author("Natalia").

%% API
-export([contains/2]).
-export([duplicateElements/1]).
-export([sumFloats/1]).
-export([sumFloats2/2]).

contains([Val|_], Val) ->
   true;
contains([_ | []], _) ->
  false;
contains([_ | T], Val) ->
  contains(T,Val).



duplicateElements([H | T]) ->
  [ H, H  | duplicateElements(T)];
duplicateElements([])->
  [].



sumFloats([H | []]) when is_float(H) ->
    H;
sumFloats([H | T]) when is_float(H) ->
  H + sumFloats(T);
sumFloats([_|[]]) ->
  0;
sumFloats([_ | T]) ->
  sumFloats(T).


sumFloats2([H | T], Acc) when is_float(H) ->
  sumFloats2(T, Acc+H);
sumFloats2([_ | T], Acc) ->
  sumFloats2(T, Acc);
sumFloats2([], Acc) ->
  Acc.


%%zrobic wzytskie zadanka bez case (rekurencja i pattern matching) - mozna zrobic case w 3 a nawet trzeba jakis jeden
%%miec ze soba uruchamialne zadanko, czasem moze sprawdzic czasem nie