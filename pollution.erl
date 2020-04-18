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
-export([createMonitor/0, addStation/3, reallyAddStation/3,addValue/5,removeValue/4,getOneValue/4, getStationMean/3,getDailyMean/3,getTuples/5, existMeasure/3, getWorstDay/3,compareMeasurements/3]).


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
%% 'createMonitor' is a function which creates monitor containing two dictionaries.

createMonitor() -> #monitor{stations=dict:new(), stationsData=dict:new()}.


%%------------addStation------------
%% 'addStation' is a function which firstly checks whether station already exist in monitor, and if not
%% calls 'reallyAddStation'  which adds station's data to both monitor's dictionaries, and returns new updated monitor.

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
%% 'addValue' is a function, which adds value to monitor to given dictionary key, after checking whether there already is such measurement in monitor's dictionary. It adds Measure to measurements' list
%% It calls functions 'existMeasure' and 'getStationName'.

addValue(Monitor, Id, Date, Type, Value ) ->
  Measure = #measurement{type=Type, value =Value, date=Date},
  Name = getStationName(Monitor,Id),

  case existMeasure(Monitor,Name,Measure) of
    true -> io:put_chars(standard_error, "There already exists a measurement with this features\n");
    false -> #monitor{stations= Monitor#monitor.stations,
                      stationsData=dict:update(Name,
                        fun(Old) -> #stationData{coordinates = Id, measurements = Old#stationData.measurements ++[Measure]} end,
                        Monitor#monitor.stationsData)}
  end.

%% Function 'existMeasure' check whether the measure, which we want to add already exist in monitor.

existMeasure(Monitor,Name,Measure) ->
  {_,_, Measurements}= dict:fetch(Name, Monitor#monitor.stationsData),
  List = lists:filter(
    fun
      (Elem) when Elem==Measure -> true;
      (_)->false end, Measurements),
  case List of
    [] -> false;
    _ -> true
  end.

%% Function 'getStationName' returns name of station basing on given Id. Id can be a station's coordinates or name.

getStationName(Monitor, Id) when is_tuple(Id) ->
  case dict:is_key(Id,Monitor#monitor.stations) of
    true -> {_,Name} = dict:find(Id, Monitor#monitor.stations), Name; %% if Station does not exist in dict, dict:find returns error
    false ->  io:put_chars(standard_error, "There is no such Station")
  end;
getStationName(_, Id) ->
  Name=Id,
  Name.

%%------------removeValue------------
%% This function removes a value connected to given station's Id and specified Date and Type of measurement. It updates the dictionary, removing value from measurements' list.

removeValue(Monitor, Id, Date, Type) ->
  Name = getStationName(Monitor,Id),
  #monitor{stations= Monitor#monitor.stations,
    stationsData=dict:update(Name,
      fun(Old) -> #stationData{coordinates = Id, measurements = lists:delete(#measurement{type=Type,date=Date, value=getOneValue(Monitor, Id, Date, Type)},Old#stationData.measurements) } end,
      Monitor#monitor.stationsData)}.

%%------------getOneValue------------
%% Returns value from specified measurement of specified station, basing on station's Id. It gets list of measurements of the station and then filters this list.

getOneValue(Monitor, Id, Date, Type) ->
  Name = getStationName(Monitor,Id),
  {_,_, Measurements}= dict:fetch(Name, Monitor#monitor.stationsData),
  [{_,_,Val,_}] = lists:filter(
    fun
      (Elem) when (Elem#measurement.type==Type) and (Elem#measurement.date==Date) -> true;
      (_)->false end,Measurements),
  Val.

%%------------getStationMean------------
%% Function 'getStationMean' returns mean of all measurements of specified type of given station. Firstly it gets list of measurements of the station and then filters this list. After that it uses foldl to compute the mean.

getStationMean(Monitor, Id, Type) ->
  Name = getStationName(Monitor,Id),
  {_,_, Measurements}= dict:fetch(Name, Monitor#monitor.stationsData),
  TypeList = lists:filter(
    fun
      (Elem) when (Elem#measurement.type==Type) -> true;
      (_)->false end, Measurements),
  lists:foldl(fun(X, Sum) -> X#measurement.value + Sum end, 0, TypeList) / lists:foldl(fun(_, Sum) -> 1 + Sum end, 0, TypeList).


%%------------getDailyMean------------
%% Day should be a date() tuple - {year,month,day}
%% Function returns a mean of all measurements of specified type and day from all stations. Firstly it gets all stations' names and then, using 'getTuples' all tuples containing that day's measurements. After computes mean using foldl.

getDailyMean(Monitor, Day,Type) ->
  Stations = dict:fetch_keys(Monitor#monitor.stationsData),
  TuplesList = getTuples(Monitor, Stations, Day, Type, []),
  lists:foldl(fun(X, Sum) -> X#measurement.value + Sum end, 0, TuplesList) / lists:foldl(fun(_, Sum) -> 1 + Sum end, 0, TuplesList).


%% 'getTuples' returns a list of tuples, which contains measurements from given day. Function search whole monitor dictionary to find that tuples and adds them to final list.

getTuples(_, [], _, _, TuplesList) ->
  TuplesList;
getTuples(Monitor, [H | Stations], DateV, Type, TuplesList) ->
  {_,_, Measurements}= dict:fetch(H, Monitor#monitor.stationsData),
  NewTuplesList = TuplesList ++ lists:filter(
      fun
      (Elem) when (Elem#measurement.type==Type) -> {Date, _} = Elem#measurement.date, Date==DateV;
      (_)->false end,Measurements),
  getTuples(Monitor,Stations, DateV, Type, NewTuplesList).


%%------------getWorstDay------------
%% Function 'getWorstDay' returns the worst daytime and value of all measurements from given type and given station. It makes list of measurements of given type and from given station and calls 'compareMeasurements' with this list.

getWorstDay(Monitor, Id, Type) ->
  Name = getStationName(Monitor,Id),
  {_,_, Measurements}= dict:fetch(Name, Monitor#monitor.stationsData),
  TypeMeasurements = lists:filter(
    fun
      (Elem) when (Elem#measurement.type==Type) -> true;
      (_)->false end,Measurements),
  compareMeasurements(TypeMeasurements,{}, 0 ).


%% 'compareMeasurements' compares measurements in given list. It returns datetime of the measurement with the highest value and this value.

compareMeasurements([],WorstDate, WorstVal) ->
  {WorstDate, WorstVal};
compareMeasurements([H | Tail],WorstDate, WorstVal) ->
  case H#measurement.value > WorstVal of
    true -> compareMeasurements(Tail,H#measurement.date, H#measurement.value);
    false -> compareMeasurements(Tail,WorstDate, WorstVal)
  end.




%%test:

%%M = pollution:createMonitor().
%%M2 = pollution:addStation(M, "Krakow", {31.23,45.67}).
%%M3 = pollution:addValue(M2, {31.23,45.67}, calendar:local_time(), "mjg", 23.4).
%%M4 = pollution:addValue(M3, {31.23,45.67}, calendar:local_time(), "mjgt", 23.9).
%%M32 = pollution:removeValue(M4,{31.23,45.67}, calendar:local_time(), "mjg" ).
%%pollution:getOneValue(M32,"Krakow", calendar:local_time(), "mjgt").
%%pollution:getStationMean(M32,"Krakow","mjgt").
%%M7 = pollution:addValue(M32, "Krakow", calendar:local_time(), "mjg", 9).
%%pollution:getStationMean(M7,"Krakow","mjg").
%%M8 = pollution:addValue(M7, {31.23,45.67}, calendar:local_time(), "mjg", 23.4).
%%M9 = pollution:addValue(M8, {31.23,45.67}, calendar:local_time(), "mjg", 23.4).
%%pollution:getWorstDay(M8,"Krakow", "mjg").