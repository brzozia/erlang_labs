%%%-------------------------------------------------------------------
%%% @author Natalia
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. kwi 2020 01:31
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Natalia Brzozowska").

%% API
-export([start/0]).
-export([stop/0]).
-export([play/1]).
-export([loopPing/1]).
-export([loopPong/0]).

start() ->
  register(ping, spawn(pingpong,loopping,[0]) ),
  register(pong, spawn(pingpong,looppong,[])).

stop() ->
  ping ! stop,
  pong ! stop.

play(N) ->
  start(),
  ping ! N.

loopPing(Status)->
  receive
    stop -> ok;
    N  when N > 0 ->
      St=Status+N,
      io:format("ping: sending ~B, Status: ~B~n",[N-1,St]), timer:sleep(1000), pong ! N-1,loopPing(St);
    0 -> io:format("ping: end of fun, Status: ~B~n",[Status]), pong ! stop

  after
    20000 -> ok
  end.

loopPong() ->
  receive
    stop -> ok;
    N  when N > 0 -> io:format("pong: sending ~B~n",[N-1]), timer:sleep(1000), ping ! N-1, loopPong() ;
    0 -> io:format("pong: end of fun~n"), ping ! stop
  after
    20000 -> ok
  end.