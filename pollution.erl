%%%-------------------------------------------------------------------
%%% @author Natalia Brzozowska
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. kwi 2020 04:57
%%%-------------------------------------------------------------------
-module(pollution).
-author("Natalia Brzozowska").

%% API
-export([createMonitor/0, addStation/3, reallyAddStation/3,addValue/5,removeValue/4,getOneValue/4, getStationMean/3,getDailyMean/3,getTuples/5]).
%%, getDailyMean/0, getOneValue/0, getStationMean/0]).

%%-----------------------data structure---------------------------
%%
%% 'monitor' is a record which contains two dictionaries:
%% -> first one - 'stations' - contains stations' data: key is a station's coordinates, and value is a station's name
%% -> second one - 'stationsData' - is a main dictionary - key is station's name and value is a record 'stationData', containing station's coordinates and list of measurements
%% list of measurements contains 'measurement' records
%%

-record(monitor, {stations, stationsData}).
-record(stationData, {coordinates, measurements=[]}).
-record(measurement, {type, value, date}).

%%-----------------------functions---------------------------------
%%-----------createMonitor----------

createMonitor() -> #monitor{stations=dict:new(), stationsData=dict:new()}.


%%------------addStation------------

addStation(Monitor, Name, Coordinates)  ->
  case ( dict:is_key(Name,  Monitor#monitor.stationsData) or dict:is_key(Coordinates,  Monitor#monitor.stations)) of
    true ->  io:put_chars(standard_error, "There already exists a station with this features\n");
    false -> reallyAddStation(Monitor, Name, Coordinates)
  end.

reallyAddStation(Monitor, Name, Coordinates) ->
  StationsDict = dict:store(Coordinates, Name, Monitor#monitor.stations),
  StationsDataDict = dict:store(Name, #stationData{coordinates = Coordinates, measurements = []}, Monitor#monitor.stationsData),
  #monitor{stations=StationsDict, stationsData=StationsDataDict}.


%%------------addValue------------

addValue(Monitor, Id, Date, Type, Value ) ->
  Measure = #measurement{type=Type, value =Value, date=Date},
  Name = getStationName(Monitor,Id),
  #monitor{stations= Monitor#monitor.stations,
    stationsData=dict:update(Name,
    fun(Old) -> #stationData{coordinates = Id, measurements = Old#stationData.measurements ++[Measure]} end,
    Monitor#monitor.stationsData)}.

getStationName(Monitor, Id) ->
  case is_tuple(Id) of
    true -> {_,Name} = dict:find(Id, Monitor#monitor.stations);
    false -> Name=Id
  end,
  Name.

%%------------removeValue------------

removeValue(Monitor, Id, Date, Type) ->
  Name = getStationName(Monitor,Id),
  #monitor{stations= Monitor#monitor.stations,
    stationsData=dict:update(Name,
      fun(Old) -> #stationData{coordinates = Id, measurements = lists:delete(#measurement{type=Type,date=Date, value=getOneValue(Monitor, Id, Date, Type)},Old#stationData.measurements) } end,
      Monitor#monitor.stationsData)}.

%%------------getOneValue------------

getOneValue(Monitor, Id, Date, Type) ->
  Name = getStationName(Monitor,Id),
  {_,_, Measurements}= dict:fetch(Name, Monitor#monitor.stationsData),
  [{_,_,Val,_}] = lists:filter(
    fun
      (Elem) when (Elem#measurement.type==Type) and (Elem#measurement.date==Date) -> true;
      (_)->false end,Measurements),
  Val.

%%------------getStationMean------------

getStationMean(Monitor, Id, Type) ->
  Name = getStationName(Monitor,Id),
  {_,_, Measurements}= dict:fetch(Name, Monitor#monitor.stationsData),
  TypeList = lists:filter(
    fun
      (Elem) when (Elem#measurement.type==Type) -> true;
      (_)->false end, Measurements),
  lists:foldl(fun(X, Sum) -> X#measurement.value + Sum end, 0, TypeList) / lists:foldl(fun(_, Sum) -> 1 + Sum end, 0, TypeList).

%%------------getDailyMean------------

getDailyMean(Monitor, Date,Type) ->
  Stations = dict:fetch_keys(Monitor#monitor.stationsData),
  TuplesList = getTuples(Monitor, Stations, Date, Type, []),
  lists:foldl(fun(X, Sum) -> X#measurement.value + Sum end, 0, TuplesList) / lists:foldl(fun(_, Sum) -> 1 + Sum end, 0, TuplesList).

getTuples(_, [], _, _, TuplesList) ->
  TuplesList;
getTuples(Monitor, [H | Stations], DateTime={DateV, _}, Type, TuplesList) ->
  {_,_, Measurements}= dict:fetch(H, Monitor#monitor.stationsData),
  NewTuplesList = TuplesList ++ lists:filter(
      fun
      (Elem) when (Elem#measurement.type==Type) -> {Date, _} = Elem#measurement.date, Date==DateV;
      (_)->false end,Measurements),
  getTuples(Monitor,Stations, DateTime, Type, NewTuplesList).


%%M = pollution:createMonitor().
%%M2 = pollution:addStation(M, "Krakow", {31.23,45.67}).
%%M3 = pollution:addValue(M2, {31.23,45.67}, calendar:local_time(), "mjg", 23.4).
%%M4 = pollution:addValue(M3, {31.23,45.67}, calendar:local_time(), "mjgt", 23.9).
%%M32 = pollution:removeValue(M4,{31.23,45.67}, calendar:local_time(), "mjg" ).
%%pollution:getOneValue(M32,"Krakow", calendar:local_time(), "mjgt").
%%pollution:getStationMean(M32,"Krakow","mjgt").
%%M7 = pollution:addValue(M32, "Krakow", calendar:local_time(), "mjg", 9).
%%pollution:getStationMean(M7,"Krakow","mjg").