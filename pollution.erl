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
-export([createMonitor/0, addStation/3, reallyAddStation/3,addValue/5]).
%%, removeValue/0, getDailyMean/0, getOneValue/0, getStationMean/0]).

%%-----------------------data structure---------------------------
%%
%% 'monitor' is a record which contains two dictionaries:
%% -> first one - 'stations' - contains stations' data: key is a station's coordinates, and value is a station's name
%% -> second one - 'stationsData' - is a main dictionary - key is station's name and value is a record 'stationData', containing station's coordinates and list of measurements
%% list of measurements contains 'measurement' records

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
addValue(Monitor, Id, Date, Type, Value ) when is_tuple(Id) ->
  Measure = #measurement{type=Type, value =Value, date=Date},
  case is_tuple(Id) of
    true -> {_,Name} = dict:find(Id, Monitor#monitor.stations);
    false -> Name=Id
  end,

  #monitor{stations= Monitor#monitor.stations,
    stationsData=dict:update(Name,
    fun(Old) -> #stationData{coordinates = Id, measurements = Old#stationData.measurements ++[Measure]} end,
    Monitor#monitor.stationsData)}.


%%------------removeValue------------
removeValue()



%%getOneValue()
%%getStationMean()
%%getDailyMean()
