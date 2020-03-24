%%%-------------------------------------------------------------------
%%% @author Natalia
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. mar 2020 12:17
%%%-------------------------------------------------------------------
-module(higherOrderFunctions).
-author("Natalia").

%% API
-export([map/2]).
-export([filter/2]).
-export([randomEles/3]).
-export([sum/1]).
-export([makeList/1]).

map(_,[])->[];
map(F,[H|T]) -> [F(H)|map(F,T)].

filter(X,L) -> [H || H<-L, X(H)==true].

makeList(0) ->[];
makeList(X) ->
  [X rem 10 | makeList(X div 10)].

sum(X) -> lists:foldl(fun(A,B) -> A + B end,0,makeList(X)).

randomEles(N,Min,Max) -> [rand:uniform(Max-Min)+Min || _<-lists:seq(1,N)].
