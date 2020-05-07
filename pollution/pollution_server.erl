%%%-------------------------------------------------------------------
%%% @author Natalia Brzozowska
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. kwi 2020 11:42
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Natalia Brzozowska").

%% API
-export([start/0,stop/0, call/2]).
-export([init/0,loop/1]).

%% client

start()->
   try register(monitor,spawn(pollution_server,init, []))
   catch
     error:Error -> io:format("error occured during starting process - it has been started before - ~s~n",[Error])
   end.

stop()->
  try monitor ! {self(), stop} of
    _ ->waitForReply()
  catch
    error:Error -> io:format("error occured during stop execution - it this process has been stopped before - ~s~n",[Error])
  end.


call(Name, Args) ->
  monitor ! {self(), Name, Args},
  waitForReply().


waitForReply()->
  receive
    {reply, Msg} -> Msg;
    {error, Msg} -> {error,Msg};
    {_,Msg} -> Msg,io:format("received something strange ~n")
  end.

%% server

init() ->
  loop(pollution:createMonitor()).


loop(M) ->
  receive
    {Pid,addStation,[Name, Coordinates]}  ->
      {Atom, Msg,M2} = pollution:addStation(M,Name,Coordinates),
       Pid ! {Atom, Msg}, loop(M2);


    {Pid,addValue,[Id, Date, Type, Value]}  ->
      {Atom, Msg, M2}=pollution:addValue(M,Id,Date, Type,Value),
      Pid ! {Atom, Msg}, loop(M2);


    {Pid,removeValue,[ Id, Date, Type]}  ->
      {Atom, Msg, M2}=pollution:removeValue(M,Id,Date, Type),
      Pid ! {Atom, Msg}, loop(M2);


    {Pid,getOneValue, [Id, Date, Type]}  ->
      {Atom,Val}=pollution:getOneValue(M,Id,Date,Type),
      Pid ! {Atom, Val }, loop(M);


    {Pid,getStationMean, [Id, Type]}  ->
      {Atom,Val}=pollution:getStationMean(M,Id,Type),
      Pid ! {Atom, Val }, loop(M);


    {Pid,getDailyMean, [Day,Type]}  ->
      {Atom,Val}=pollution:getDailyMean(M,Day,Type),
        Pid ! {Atom, Val }, loop(M);


    {Pid,getWorstDay, [Id, Type]}  ->
      {Atom,Val}=pollution:getWorstDay(M,Id,Type),
      Pid ! {Atom, Val }, loop(M);



    {Pid,getWorstHourlyStation,[Day, Hour, Type]}  ->
    {Atom,Val}=pollution:getWorstHourlyStation(M,Day, Hour,Type),
      Pid ! {Atom, Val }, loop(M);


    {Pid,stop} ->
      Pid ! {reply, stopped},ok;

    {Pid,_} ->
      Pid ! {reply, 'unknown message'}, loop(M)
  after
    900000 ->
      io:format("how long server has to wait!~n"), ok
  end.


