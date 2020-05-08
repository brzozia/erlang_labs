%%%-------------------------------------------------------------------
%%% @author Natalia
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. mar 2020 14:40
%%%-------------------------------------------------------------------
-module(rnpCalculator).
-author("Natalia Brzozowska").

%% API

-export([rpnValue/2]).
-export([onp/1]).
-export([intOrFloat/3]).

%% 1 2 3 * + 4 5 / - 6 +
%% 1 2 + 3 + 4 + 5 + 6 7 * +
%% 4 7 + 3 / 2 19 - *
%% 17 31 4 + * 26 15 - 2 * 22 - / 1 -

intOrFloat(_,J,[]) ->
  J;
intOrFloat(H,_,D) when D=/=0 ->
  list_to_float(H).


onp(T) ->
  L=string:tokens(T, " "),
  rpnValue(L,[]).

%% head is an operation
rpnValue(["*" |T],[V,A |Lues]) ->
  rpnValue(T,[V*A|Lues]);
rpnValue(["+" |T],[V,A |Lues]) ->
  rpnValue(T,[V+A|Lues]);
rpnValue(["-"|T],[V,A |Lues]) ->
  rpnValue(T,[A-V|Lues]);
rpnValue(["/"|T],[V,A |Lues]) when (V=/=0) ->
  rpnValue(T, [A / V |Lues]);
rpnValue(["/"|_],[0,_ |_]) ->
  io:format("You cannot divide by zero.~n");
rpnValue(["sqrt"|T],[V|Alues]) ->
  rpnValue(T,[math:sqrt(V) | Alues]);
rpnValue(["pow"|T],[V,A|Lues]) ->
  rpnValue(T,[ math:pow(A,V) | Lues]);
rpnValue(["sin"|T],[V|Alues]) ->
  rpnValue(T,[math:sin(V) | Alues]);
rpnValue(["cos"|T],[V|Alues]) ->
  rpnValue(T,[math:cos(V) | Alues]);
rpnValue(["tan"|T],[V|Alues]) ->
  rpnValue(T,[math:tan(V) | Alues]);
rpnValue(["tanh"|T],[V|Alues]) ->
  rpnValue(T,[math:tanh(V) | Alues]);
rpnValue(["rev"|T],[V|Alues]) ->
  rpnValue(T,[-V | Alues]);
rpnValue(["idk"|T],[V,A|Lues]) ->
  rpnValue(T,[ ((V*A)-4)/(math:pow((A+V),2))| Lues]);

%% head is a number
rpnValue([H|T], Values) ->
  {J,D} = string:to_integer(H),
  rpnValue(T,[intOrFloat(H,J,D)| Values]);

rpnValue(_,[V | []]) ->
  V.


