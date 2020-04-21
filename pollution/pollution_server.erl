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
-export([start/0,stop/0, callAddStation/2,callAddValue/4,callGetDailyMean/2,callGetWorstDay/2,callGetWorstHourlyStation/3,callRemoveValue/3,callGetOneValue/3,callGetStationMean/2,waitForReply/0]).
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




callAddStation(Name, Coordinates)->
  monitor ! {self(), addStation, Name, Coordinates},
  waitForReply().
callAddValue(Id, Date, Type, Value) ->
  monitor ! {self(), addValue, Id, Date, Type, Value},
  waitForReply().
callRemoveValue(Id, Date, Type) ->
  monitor ! {self(), removeValue, Id, Date, Type },
  waitForReply().
callGetOneValue(Id, Date, Type) ->
  monitor ! {self(), getOneValue, Id, Date, Type },
  waitForReply().
callGetStationMean(Id, Type) ->
  monitor ! {self(), getStationMean, Id, Type },
  waitForReply().
callGetDailyMean( Day,Type) ->
  monitor ! {self(), getDailyMean,Day,Type },
  waitForReply().
callGetWorstDay(Id, Type) ->
  monitor ! {self(), getWorstDay, Id, Type },
  waitForReply().
callGetWorstHourlyStation(Day, Hour, Type) ->
  monitor ! {self(), getWorstHourlyStation,Day, Hour, Type },
  waitForReply().


waitForReply()->
  receive
    {reply, Msg} -> Msg;
    {error, Msg, Error} -> io:format("~s~n",[Msg]), Error;
    _ -> io:format("received something strange")
  end.

%% server

init() ->
  loop(pollution:createMonitor()).


loop(M) ->
  receive
    {Pid,addStation,Name, Coordinates}  ->
      try pollution:addStation(M,Name,Coordinates) of
        M2 ->Pid ! {reply, 'station added'}, loop(M2)
      catch
        error: same_station_arrributes ->Pid ! {error, "error occured during adding station",same_station_arrributes}, loop(M);
        error:Error -> Pid ! {error, "error occured during adding station",Error}, loop(M)
      end;


    {Pid,addValue,Id, Date, Type, Value}  ->
      try pollution:addValue(M,Id,Date, Type,Value) of
        M2 ->Pid !  {reply, 'value added'}, loop(M2)
      catch
        error: same_values_to_station -> Pid ! {error, "error occured during adding value",same_values_to_station}, loop(M);
        error:Error -> Pid ! {error, "error occured during adding value",Error}, loop(M)
      end;


    {Pid,removeValue, Id, Date, Type}  ->
      try pollution:removeValue(M,Id,Date, Type) of
        M2 ->Pid ! {reply, 'value removed'}, loop(M2)
      catch
        error:no_such_value -> Pid ! {error, "there is no such value in monitor, so I cannot be remove it", badmatch}, loop(M);
        error:Error -> Pid ! {error, "error during remove",Error}, loop(M)
      end;


    {Pid,getOneValue, Id, Date, Type}  ->
      try pollution:getOneValue(M,Id,Date,Type) of
        Val -> Pid ! {reply, Val }, loop(M)
      catch
        error:no_such_value -> Pid ! {error, "there is no such value in monitor, so I cannot get it", badmatch}, loop(M);
        error:Error -> Pid ! {error, "error during getting value",Error}, loop(M)
      end;


    {Pid,getStationMean, Id, Type}  ->
      try pollution:getStationMean(M,Id,Type) of
        Val -> Pid ! {reply, Val }, loop(M)
      catch
        error:no_such_value -> Pid ! {error, "there is no such value in monitor, so I cannot get it", badmatch}, loop(M);
        error:Error -> Pid ! {error, "error during getting station's mean",Error}, loop(M)
      end;


    {Pid,getDailyMean, Day,Type}  ->
      try pollution:getDailyMean(M,Day,Type) of
        Val -> Pid ! {reply, Val }, loop(M)
      catch
        error:no_such_value -> Pid ! {error, "there is no such values in monitor, so I cannot get it", badmatch}, loop(M);
        error:Error -> Pid ! {error, "error during getting daily mean",Error}, loop(M)
      end;


    {Pid,getWorstDay, Id, Type}  ->
      try pollution:getWorstDay(M,Id,Type) of
        Val -> Pid ! {reply, Val }, loop(M)
      catch
        error:no_such_value-> Pid ! {error, "there is no such values in monitor, so I cannot get it", badmatch}, loop(M);
        error:Error -> Pid ! {error, "error during getting worst day",Error}, loop(M)
      end;


    {Pid,getWorstHourlyStation, Day, Hour, Type}  ->
      try pollution:getWorstHourlyStation(M,Day, Hour,Type) of
        Val -> Pid ! {reply, Val }, loop(M)
      catch
        error:no_such_value -> Pid ! {error, "there is no such values in monitor, so I cannot get it", badmatch}, loop(M);
        error:Error -> Pid ! {error, "error during getting worst day",Error}, loop(M)
      end;


    {Pid,stop} ->
      Pid ! {reply, stopped},ok;

    {Pid,_} ->
      Pid ! {reply, 'unknown message'}, loop(M)
  after
    900000 ->
      io:format("how long server has to wait!~n"), ok
  end.


