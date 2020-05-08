%%%-------------------------------------------------------------------
%%% @author Natalia
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2020 01:45
%%%-------------------------------------------------------------------
-module(onpChange).
-author("Natalia").

%% API
-export([getSigns/2]).
-export([rpnCalc/5]).
-export([changeNotation/1]).
-export([makeList/2]).


changeNotation(L) ->
  W=rpnCalc(L,[],[],0,0),
  W.


getSigns(L,[]) ->
  [L,[]];
getSigns(L,[H |T]) when (H=:=40) ->
  [L,T];
getSigns(L, [H | T]) ->
  getSigns(L++[H]++[32],T).




makeList(0,List) ->
  List++[32];
makeList(Num,List)->
  case Num of
    0 ->  [48 | List];
    _ -> makeList(Num div 10,[(Num rem 10)+48 | List])
  end.


rpnCalc([H|T],Res,Sign,Inc,Num) when ((H=:=32) or (T=:=[])) and (Num=/=0) ->
  rpnCalc(T,Res++(makeList(Num,[])),Sign,Inc+1,0);

rpnCalc([32|T],Res,Sign,Inc,0)  ->
  rpnCalc(T,Res,Sign,Inc,0);

%%dodaj liczbe do Res
rpnCalc([H|T], Res,Sign,Inc,Num) when (H=/=40) and (H=/=41) and (H=/=42) and (H=/=43) and (H=/=45) and (H=/=47) ->
  rpnCalc(T,Res,Sign,Inc+1,Num*10 + H-48);

%%jesli byly dwie liczby i teraz nie bedziemy mnozyc alni dielic to dodaj do Res pierwszy znak
rpnCalc([H|T], Res,[S|Rest],Inc,Num) when (Inc=:=2) and (H=/=42) and (H=/=47)  ->
  rpnCalc([H|T],Res++[S]++[32],Rest,0,Num);

%%jesli trafilo sie '(' to dodaj do listy znakow
rpnCalc([H|T], Res,Sign,_,Num) when (H=:=40) ->
  rpnCalc(T,Res,[40 | Sign],0,Num);

%%jesli na stosie jest '*' albo '/' to sciagnij ze stosu znaki az do nawiasu '(' i dodaj do Res
rpnCalc(T,Res,[S|Rest],Inc,Num) when (Inc>=2) and ((S=:=42) or (S=:=47))  ->
  [P,K]=getSigns([],[S|Rest]),
  rpnCalc(T,Res++P,K,0,Num);

%% dodaj do listy znakow (na stos) znak * + - /
rpnCalc([H|T], Res,Sign,Inc,Num) when (H=:=42) or (H=:=43) or (H=:=45) or  (H=:=47)->
  rpnCalc(T,Res,[H | Sign],Inc,Num);

%%jesli trafilo sie ')' to sciagnij ze stosu az do nawiasu '(' znaki
rpnCalc([H|T], Res,Sign,_,Num) when (H=:=41) ->
  [P,K]=getSigns([],Sign),
  rpnCalc(T,Res++P,K,0,Num);

rpnCalc(_,Res,[],_,_) ->
  Res;
rpnCalc([],Res,[S|Rest],Inc,Num) ->
  rpnCalc( [], Res ++ [S]++[32], Rest,Inc,Num).
