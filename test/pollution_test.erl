%%%-------------------------------------------------------------------
%%% @author krzysztof
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. kwi 2020 13:45
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("krzysztof").

-include_lib("eunit/include/eunit.hrl").
-record(monitor, {stations, data}).

createMonitor_test() ->
  ?assertMatch(#monitor{stations = #{}, data = #{}}, pollution:createMonitor()).

addStation_test() ->
  M1 = pollution:addStation("Kraków", {20, 20}, pollution:createMonitor()),
  ?assert(#monitor{stations = #{"Kraków" => {20, 20}}, data = #{}} == M1),

  ?assert(pollution:addStation("Kraków", {21, 50}, M1) == {error,"Station already exists"}),
  ?assert(pollution:addStation("Lubiąż", {20, 20}, M1) == {error, "Station already exists"}).

addValue_test() ->
  M1 = pollution:addStation("Kraków", {20, 20}, pollution:createMonitor()),

  ?assert(pollution:addValue("Kraków", {2020, 1, 1}, "PM 2.5", 10, M1) ==
    #monitor{stations = #{"Kraków" => {20, 20}}, data = #{{"Kraków", {2020, 1, 1}, "PM 2.5"} => 10}}),

  ?assert(pollution:addValue({20, 20}, {2020, 1, 1}, "PM 2.5", 10, M1) ==
    #monitor{stations = #{"Kraków" => {20, 20}}, data = #{{"Kraków", {2020, 1, 1}, "PM 2.5"} => 10}}),

  M2 = pollution:addValue({20, 20}, {2020, 1, 1}, "PM 2.5", 10, M1),
  ?assert(pollution:addValue({20, 20}, {2020, 1, 1}, "PM 2.5", 20, M2) == {error,"Value already exists"}).

removeValue_test() ->
  M1 = pollution:addStation("Kraków", {20, 20}, pollution:createMonitor()),
  M2 = pollution:addValue({20, 20}, {2020, 1, 1}, "PM 2.5", 10, M1),

  ?assert(pollution:removeValue("Kraków", {2020, 1, 1}, "PM 2.5", M2) == #monitor{stations = #{"Kraków" => {20, 20}}, data = #{}}),
  ?assert(pollution:removeValue("Kraków", {2020, 1, 1}, "PM 2.5", M1) == {error, "Value does not exist"}).

getOneValue_test() ->
  M1 = pollution:addStation("Kraków", {20, 20}, pollution:createMonitor()),
  M2 = pollution:addValue({20, 20}, {2020, 1, 1}, "PM 2.5", 10, M1),
  ?assert(pollution:getOneValue("Kraków", {2020, 1, 1}, "PM 2.5", M2) == 10),
  ?assert(pollution:getOneValue("Kraków", {2020, 1, 1}, "PM 2.5", M1) == {error, "Value does not exist"}).

getStationMean_test() ->
  M1 = pollution:addStation("Kraków", {20, 20}, pollution:createMonitor()),
  M2 = pollution:addValue({20, 20}, {2020, 1, 1}, "PM 2.5", 10, M1),
  M3 = pollution:addValue({20, 20}, {2020, 1, 2}, "PM 2.5", 20, M2),
  M4 = pollution:addValue({20, 20}, {2020, 1, 3}, "PM 2.5", 30, M3),
  ?assert(pollution:getStationMean("Kraków", "PM 2.5", M4) == 20).

getDailyMean_test() ->
  M1 = pollution:addStation("Kraków", {20, 20}, pollution:createMonitor()),
  M2 = pollution:addValue({20, 20}, {2020, 1, 1}, "PM 2.5", 10, M1),
  M3 = pollution:addStation("Czerwone Maki", {21, 20}, M2),
  M4 = pollution:addValue({21, 20}, {2020, 1, 1}, "PM 2.5", 20, M3),
  M5 = pollution:addValue({21, 20}, {2020, 1, 2}, "PM 2.5", 100, M4),
  ?assert(pollution:getDailyMean("PM 2.5", {2020, 1, 1}, M5) == 15).

getDailyOverLimit_test() ->
  M1 = pollution:addStation("Kraków", {20, 20}, pollution:createMonitor()),
  M2 = pollution:addValue({20, 20}, {2020, 1, 1}, "PM 2.5", 10, M1),
  M3 = pollution:addStation("Czerwone Maki", {21, 20}, M2),
  M4 = pollution:addValue({21, 20}, {2020, 1, 1}, "PM 2.5", 20, M3),
  M5 = pollution:addValue({21, 20}, {2020, 1, 2}, "PM 2.5", 100, M4),

  ?assert(pollution:getDailyOverLimit("PM 2.5", {2020, 1, 1}, 9, M5) == 2),
  ?assert(pollution:getDailyOverLimit("PM 2.5", {2020, 1, 1}, 15, M5) == 1),
  ?assert(pollution:getDailyOverLimit("PM 2.5", {2020, 1, 1}, 99, M5) == 0).

getYearlyMean_test() ->
  M1 = pollution:addStation("Kraków", {20, 20}, pollution:createMonitor()),
  M2 = pollution:addValue({20, 20}, {2020, 12, 1}, "PM 2.5", 10, M1),
  M3 = pollution:addStation("Czerwone Maki", {21, 20}, M2),
  M4 = pollution:addValue({21, 20}, {2020, 1, 1}, "PM 2.5", 20, M3),
  M5 = pollution:addValue({21, 20}, {2018, 1, 2}, "PM 2.5", 100, M4),
  ?assert(pollution:getYearlyMean("PM 2.5", 2020, M5) == 15).







