%%%-------------------------------------------------------------------
%%% @author Natalia Brzozowska
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. kwi 2020 20:17
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-author("Natalia Brzozowska").

-include_lib("eunit/include/eunit.hrl").
-export([start_test/0,stop_test/0,addStation_test/0,addValue_test/0,getOneValue_test/0,getStationAndDailyMean_test/0,removeValueAndOther_test/0]).


start_test() ->
  ?assert(pollution_server:start()),
  ?assertNotEqual(true,pollution_server:start()).

stop_test() ->
  ?assertEqual(stopped,pollution_server:stop()),
  ?assertNotEqual(stopped,pollution_server:stop()).


%% those tests checks receiving and sending messages between processes (but also checks a little of pollution module logic)

addStation_test() ->
  pollution_server:start(),
  ?assertEqual('station added', pollution_server:call(addStation,["Kra",{123.234,23.23}])),
  ?assertEqual({error,same_station_attributes}, pollution_server:call(addStation,["Bronowice",{123.234,23.23}]) ), %% same coordinates as before
  ?assertEqual({error,same_station_attributes}, pollution_server:call(addStation,["Kra",{187.234,29.23}]) ),   %% same name as before
  pollution_server:stop().

addValue_test() ->
  pollution_server:start(),
  pollution_server:call(addStation,["Kra",{123.234,23.23}]),
  ?assertEqual('value added', pollution_server:call(addValue,["Kra", calendar:local_time(),"PM10", 123]) ),
  ?assertEqual('value added',pollution_server:call(addValue,[{123.234,23.23}, {{2020,04,21},{11,11,11}},"PM2.5", 10])),
  ?assertEqual({error,same_values_to_station},pollution_server:call(addValue,["Kra", {{2020,04,21},{11,11,11}},"PM2.5", 10]) ), %% same values as before - cannot add
  ?assertEqual({error,wrong_station_name},pollution_server:call(addValue,["Zabierzow", calendar:local_time(),"PM2.5", 13]) ), %% adds value to unavailable station - cannot
  pollution_server:stop().

removeValueAndOther_test() ->
  pollution_server:start(),
  pollution_server:call(addStation,["Kra",{123.234,23.23}]),
  ?assertEqual('value added', pollution_server:call(addValue,["Kra", {{2020,04,21},{11,11,11}},"Temp", 10]) ),
  ?assertEqual('value removed',pollution_server:call(removeValue,["Kra", {{2020,04,21},{11,11,11}},"Temp"]) ), %% removes just added value
  ?assertEqual({error,no_such_value},pollution_server:call(getOneValue,["Kra", {{2020,04,21},{11,11,11}},"Temp"])), %% checks whether the value is in monitor - it is not
  ?assertEqual({error,no_such_value},pollution_server:call(removeValue,["Kra", {{2020,04,21},{11,11,11}},"Temp"]) ), %% tries again to remove this value - cannot
  pollution_server:stop().

getOneValue_test() ->
  pollution_server:start(),
  pollution_server:call(addStation,["Kra",{123.234,23.23}]),
  pollution_server:call(addValue,["Kra", {{2020,04,21},{11,11,11}},"PM10", 123]),
  pollution_server:call(addValue,["Kra", {{2020,04,21},{11,11,12}},"PM2.5", 10]),
  pollution_server:call(addStation,[ "Bronowice", {31.23,45.67}]),
  pollution_server:call(addValue,[ "Bronowice", {{2020,04,22},{11,11,11}}, "Temp", 15]),

  ?assertEqual(123,pollution_server:call(getOneValue,["Kra", {{2020,04,21},{11,11,11}},"PM10"])), %% gets correct values
  ?assertEqual(10,pollution_server:call(getOneValue,["Kra", {{2020,04,21},{11,11,12}},"PM2.5"])),
  ?assertEqual(15,pollution_server:call(getOneValue,["Bronowice", {{2020,04,22},{11,11,11}}, "Temp"])),

  ?assertEqual({error,no_such_value},pollution_server:call(getOneValue,["Kra", {{2020,04,21},{11,11,10}},"PM10"])), %% gets not available data - cannot
  ?assertNotEqual({error,no_such_value},pollution_server:call(getOneValue,["Bronowice", {{2020,04,21},{11,11,11}},"Temp"])),
  ?assertNotEqual({error,wrong_station_name},pollution_server:call(getOneValue,["Zabierzow", {{2020,04,21},{11,11,11}},"Temp"])), %% gets data from station which does not exist
  pollution_server:stop().

getStationAndDailyMean_test() ->
  pollution_server:start(),
  pollution_server:call(addStation,[ "Bronowice", {31.23,45.67}]),
  pollution_server:call(addValue,[ {31.23,45.67}, {{2020,04,21},{11,11,10}}, "PM10", 20]),
  pollution_server:call(addValue,["Bronowice", {{2020,04,21},{11,11,10}}, "PM10", 10]),
  pollution_server:call(addStation,["Kra",{123.234,23.23}]),
  pollution_server:call(addValue,["Kra", {{2020,04,21},{11,11,10}},"PM10", 3]),
  pollution_server:call(addValue,["Kra", {{2020,04,21},{11,11,10}},"Temp", 10]),

  ?assertEqual(15.0,pollution_server:call(getStationMean,["Bronowice","PM10"])),
  ?assertEqual(11.0,pollution_server:call(getDailyMean,[{2020,04,21},"PM10"])),
  ?assertNotEqual(15.0,pollution_server:call(getStationMean,["Bronowice","PM2.5"])),
  ?assertNotEqual(15.0,pollution_server:call(getStationMean,["Kra","Temp"])),

  %% getWorstDay test
  pollution_server:call(addValue,[ {31.23,45.67}, {{2020,04,22},{11,11,10}}, "PM10", 25]),
  pollution_server:call(addValue,[ "Bronowice", {{2020,04,22},{11,11,10}}, "PM10", 10]),
  ?assertEqual({{{2020,04,22},{11,11,10}},25},pollution_server:call(getWorstDay,["Bronowice","PM10"])),
  ?assertNotEqual({{{{2020,04,22},{11,11,10}}},25.0},pollution_server:call(getWorstDay,["Bronowice","Temp"])),

  %% getWorstHourlyStation test
  ?assertEqual({"Bronowice", 25},pollution_server:call(getWorstHourlyStation,[{2020,04,22},11,"PM10"])),
  pollution_server:stop().


