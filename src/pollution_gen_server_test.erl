%%%-------------------------------------------------------------------
%%% @author krzysztof
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. maj 2020 19:41
%%%-------------------------------------------------------------------
-module(pollution_gen_server_test).
-author("krzysztof").

-include_lib("eunit/include/eunit.hrl").

addStation_test() ->
  ?assertEqual(ok, pollution_gen_server:addStation("Kraków", {20, 20})),
  ?assertEqual(ok, pollution_gen_server:addStation("Czerwone Maki", {21, 20})).

addValue_test() ->
  ?assertEqual(ok, pollution_gen_server:addValue("Kraków", {2020, 1, 1}, "PM 10", 10)),
  ?assertEqual(ok, pollution_gen_server:addValue({20, 20}, {2020, 1, 2}, "PM 10", 20)).

getOneValue_test() ->
  ?assertEqual(10, pollution_gen_server:getOneValue("Kraków", {2020, 1, 1}, "PM 10")),
  ?assertEqual(10, pollution_gen_server:getOneValue({20, 20}, {2020, 1, 1}, "PM 10")),
  ?assertMatch({error, _}, pollution_gen_server:getOneValue("Łękołody", {2020, 1, 1}, "PM 10")).

removeValue_test() ->
  pollution_gen_server:addValue("Kraków", {2020, 07, 07}, "PM 2.5", 10),
  ?assertMatch(ok, pollution_gen_server:removeValue("Kraków", {2020, 07, 07}, "PM 2.5")).

getStationMean_test() ->
  ?assertEqual(15.0, pollution_gen_server:getStationMean("Kraków", "PM 10")),
  ?assertMatch({error, _}, pollution_gen_server:getStationMean("Szczebrzeszyn", "PM 10")).

getDailyMean_test() ->
  pollution_gen_server:addValue("Czerwone Maki", {2020, 1, 1}, "PM 10", 50),
  ?assertEqual(30.0, pollution_gen_server:getDailyMean("PM 10", {2020, 1, 1})),
  ?assertMatch({error, _}, pollution_gen_server:getDailyMean("PM 10", {2020, 312321, 373})).

getDailyOverLimit_test() ->
  ?assertEqual(0, pollution_gen_server:getDailyOverLimit("PM 10", {2020, 1, 1}, 100)),
  ?assertEqual(1, pollution_gen_server:getDailyOverLimit("PM 10", {2020, 1, 1}, 49)),
  ?assertEqual(2, pollution_gen_server:getDailyOverLimit("PM 10", {2020, 1, 1}, 9)),
  ?assertMatch({error, _}, pollution_gen_server:getDailyOverLimit("PM 10", {2020, 9921, 3799}, 9)).

getYearlyMean_test() ->
  pollution_gen_server:addValue("Czerwone Maki", {2020, 2, 1}, "PM 10", 10),
  ?assertEqual(22.5, pollution_gen_server:getYearlyMean("PM 10", 2020)).

crash_test() ->
  pollution_gen_server:crash(),
  ?assert(lists:member(pollution_gen_server, registered())).

