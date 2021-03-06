%%%-------------------------------------------------------------------
%%% @author Natalia Brzozowska
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. kwi 2020 20:18
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("Natalia Brzozowska").

-include_lib("eunit/include/eunit.hrl").


addStation_test() ->
  M = pollution:createMonitor(),
  {reply,_,M2} = pollution:addStation(M,"Kra",{123.234,23.23}),

  ?assert(is_tuple(pollution:addStation(M,"Kra",{123.234,23.23}) )),
  ?assertEqual({error,same_station_attributes,M2},pollution:addStation(M2,"Bronowice",{123.234,23.23}) ), %% same coordinates as before
  ?assertEqual({error,same_station_attributes,M2}, pollution:addStation(M2,"Kra",{187.234,29.23}) ).   %% same name as before


addValueAndGetOneValue_test() ->
  M = pollution:createMonitor(),

  {reply,_,M2} = pollution:addStation(M, "Bronowice", {31.23,45.67}),
  {reply,_,M5} = pollution:addValue(M2,{31.23,45.67},  {{2020,04,21},{11,11,11}}, "PM2.5", 40.4),
  {reply,_,M6} = pollution:addValue(M5, "Bronowice",  {{2020,04,21},{11,11,13}}, "Temp", 15),

  ?assertEqual({error,same_values_to_station,M6},pollution:addValue(M6, "Bronowice",  {{2020,04,21},{11,11,13}}, "Temp", 15) ), %% same values as up
  ?assert(is_tuple(pollution:addValue(M2, "Bronowice",  {{2020,04,21},{11,11,12}}, "PM2.5", 40.4) )), %% returns new monitor - tuple
  ?assertEqual({error,wrong_station_name,M6},pollution:addValue(M6, "Zabierzow",  {{2020,04,21},{11,11,13}}, "Temp", 15) ), %% gets value from station that does not exist

  ?assertEqual({reply,40.4},pollution:getOneValue(M6,"Bronowice", {{2020,04,21},{11,11,11}}, "PM2.5")), %% getOneValue test
  ?assertEqual({error,wrong_station_name},pollution:getOneValue(M6,"Zabierzow", {{2020,04,21},{11,11,11}}, "PM2.5")), %% wrong name
  ?assertEqual({error,no_such_value},pollution:getOneValue(M6,"Bronowice", {{2020,04,21},{11,11,11}}, "Temp")). %% wrong date


removeValueAndOther_test() ->
  M = pollution:createMonitor(),

  {reply,_,M2} = pollution:addStation(M, "Bronowice", {31.23,45.67}),
  {reply,_,M5} = pollution:addValue(M2, {31.23,45.67},  {{2020,04,21},{11,11,11}}, "PM2.5", 40.4),
  {reply,_,M6} = pollution:addValue(M5, "Bronowice",  {{2020,04,21},{11,11,13}}, "Temp", 15),
  {reply,_,M7} = pollution:removeValue(M6,"Bronowice",{{2020,04,21},{11,11,11}}, "PM2.5"),

  ?assertEqual({error,no_such_value},pollution:getOneValue(M7,"Bronowice",{{2020,04,21},{11,11,11}}, "PM2.5")), %% checks deleted value
  ?assert(is_tuple(pollution:removeValue(M6, "Bronowice",  {{2020,04,21},{11,11,13}}, "Temp") )), %% remove returns new monitor - tuple
  ?assertEqual({error,no_such_value,M7},pollution:removeValue(M7, "Bronowice",  {{2020,04,21},{11,11,12}}, "PM2.5")). %% tries to remove this same value again - error occurs

getOneValue_getMean_andOther_test() ->
  M = pollution:createMonitor(),

  {reply,_,M2} = pollution:addStation(M, "Bronowice", {31.23,45.67}),
  {reply,_,M4} = pollution:addValue(M2, {31.23,45.67}, {{2020,04,21},{11,11,11}}, "PM10", 23.4),
  {reply,_,M5} = pollution:addValue(M4, "Bronowice", {{2020,04,22},{11,11,11}}, "PM2.5", 40.4),
  {reply,_,M6} = pollution:addValue(M5, "Bronowice", {{2020,04,23},{11,11,11}}, "Temp", 15),

  {reply,_,M7} = pollution:addStation(M6, "Azory", {50.2345, 18.3445}),
  {reply,_,M8} = pollution:addValue(M7, {50.2345, 18.3445}, {{2020,04,21},{11,11,11}}, "PM10", 50.7),
  {reply,_,M9} = pollution:addValue(M8, "Azory", {{2020,04,22},{11,11,11}}, "PM2.5", 72.4),
  {reply,_,M10} = pollution:addValue(M9, "Azory", {{2020,04,23},{11,11,11}}, "Temp", 16.1),
  {reply,_,M15} = pollution:addValue(M10, {31.23,45.67}, {{2020,04,21},{16,11,11}}, "PM10", 10.1),
  {reply,_,M16} = pollution:addValue(M15, "Bronowice", {{2020,04,22},{11,51,11}}, "PM2.5", 34.45),
  {reply,_,M17} = pollution:addValue(M16, "Bronowice", calendar:local_time(), "Temp", 15.1),
  {reply,_,M18} = pollution:addValue(M17, {50.2345, 18.3445}, {{2020,04,21},{18,11,11}}, "PM10", 50.0),
  {reply,_,M19} = pollution:addValue(M18, "Azory", {{2020,04,22},{11,18,11}}, "PM2.5", 43.3),
  {reply,_,M23} = pollution:addValue(M19, "Azory", calendar:local_time(), "Temp", 15.8),

  %%getOneValue
  ?assertEqual({reply,50.7},pollution:getOneValue(M10,{50.2345, 18.3445}, {{2020,04,21},{11,11,11}}, "PM10")), %% correct
  ?assertEqual({reply,23.4},pollution:getOneValue(M10,"Bronowice", {{2020,04,21},{11,11,11}}, "PM10")),
  ?assertEqual({reply,15},pollution:getOneValue(M6,"Bronowice", {{2020,04,23},{11,11,11}}, "Temp")),

  ?assertEqual({error,no_such_value},pollution:getOneValue(M10,{50.2345, 18.3445}, {{2020,04,21},{11,11,51}}, "PM10" )), %%wrong date
  ?assertEqual({error,wrong_station_name},pollution:getOneValue(M10,"Kielce", {{2020,04,21},{11,11,51}}, "PM10" )), %% wrong station name
    %%getStationMean
  ?assertEqual({reply,16.75}, pollution:getStationMean(M23,"Bronowice","PM10")),
  ?assertEqual({reply,57.85},pollution:getStationMean(M23,{50.2345, 18.3445},"PM2.5")),
  ?assertEqual({error,wrong_station_name},pollution:getStationMean(M23,"Zabierzow","PM2.5")), %% wrong station name
  ?assertEqual({reply,0.0},pollution:getStationMean(M23,"Bronowice","PM222222")), %% wrong type
    %%getDailyMean
  ?assertEqual({reply,33.55}, pollution:getDailyMean(M23,{2020,04,21},"PM10")),
  ?assertEqual({reply,47.6375},pollution:getDailyMean(M23,{2020,04,22},"PM2.5")),
  ?assertEqual({reply,0.0},pollution:getDailyMean(M23,"Zabierzow","PM2.5")), %% wrong station name
  ?assertEqual({reply,0.0},pollution:getDailyMean(M23,"Bronowice","PM222222")), %% wrong type
    %%getWorstDay
  ?assertEqual({reply,{{{2020,04,21},{11,11,11}},23.4}},pollution:getWorstDay(M23,"Bronowice","PM10")),
  ?assertEqual({reply,{{{2020,04,21},{11,11,11}},50.7}},pollution:getWorstDay(M23,"Azory","PM10")),
  ?assertEqual({error,wrong_station_name},pollution:getWorstDay(M23,"Zabierzow","PM2.5")),
    %%getWorstHourlyStation
  ?assertEqual({reply,{"Azory",50.7}},pollution:getWorstHourlyStation(M23,{2020,04,21},11,"PM10")),
  ?assertEqual({reply,{0,0}},pollution:getWorstHourlyStation(M23,{2020,04,21},11,"PM2.5564")),
  ?assertEqual({reply,{0,0}},pollution:getWorstHourlyStation(M23,{2020,04,21},11,"PM2.5")). %% no measures such type that day
