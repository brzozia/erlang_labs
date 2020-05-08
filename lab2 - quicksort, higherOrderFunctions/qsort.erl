%%%-------------------------------------------------------------------
%%% @author Natalia
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. mar 2020 10:20
%%%-------------------------------------------------------------------
-module(qsort).
-author("Natalia Brzozowska").

%% API
-export([lessThan/2]).
-export([grtEqThan/2]).
-export([qs/1]).
-export([randomEles/3]).
-export([compareSpeeds/3]).


lessThan(List, Arg) -> [X || X <-List,X<Arg].

grtEqThan(List,Arg) -> [X || X<-List, X>=Arg].

qs([])->[];
qs([Pivot|Tail]) -> qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot) ).

randomEles(N,Min,Max) -> [rand:uniform(Max-Min)+Min || _<-lists:seq(1,N)].

compareSpeeds(List,Fun1,Fun2)->
  {Fqs,_v}=timer:tc(Fun1,[List]),
  {Sqs,_v}=timer:tc(Fun2,[List]),
  io:format("Czas pierwszej funkcji: ~b Czas drugiej funkcji: ~b~n ",[Fqs,Sqs]).