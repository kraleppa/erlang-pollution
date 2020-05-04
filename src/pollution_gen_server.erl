%%%-------------------------------------------------------------------
%%% @author krzysztof
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0, addStation/2, addValue/4, removeValue/3, showMonitor/0, getOneValue/3, getStationMean/2, getDailyMean/2, getDailyOverLimit/3, getYearlyMean/2, stop/0, crash/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, pollution:createMonitor(), []).
init(Monitor) -> {ok, Monitor}.

showMonitor() -> gen_server:call(?MODULE, showMonitor).

crash() -> gen_server:cast(?MODULE, gen_server).
addStation(Name, Cords) -> gen_server:cast(?MODULE, {addStation, [Name, Cords]}).
addValue(Station, Date, Type, Value) -> gen_server:cast(?MODULE, {addValue, [Station, Date, Type, Value]}).
removeValue(Station, Date, Type) ->  gen_server:cast(?MODULE, {removeValue, [Station, Date, Type]}).
stop() -> gen_server:cast(?MODULE, stop).

handle_cast({addStation, [Name, Cords]}, Monitor) -> handle_cast_result(pollution:addStation(Name, Cords, Monitor), Monitor);
handle_cast({addValue, [Station, Date, Type, Value]}, Monitor) -> handle_cast_result(pollution:addValue(Station, Date, Type, Value, Monitor), Monitor);
handle_cast({removeValue, [Station, Date, Type]}, Monitor) -> handle_cast_result(pollution:removeValue(Station, Date, Type, Monitor), Monitor);
handle_cast(crash, Monitor) -> 1 / 0, {noreply, Monitor};
handle_cast(stop, Monitor) -> {stop, normal, Monitor}.

getOneValue(Station, Date, Type) -> gen_server:call(?MODULE, {getOneValue, [Station, Date, Type]}).
getStationMean(Station, Type) -> gen_server:call(?MODULE, {getStationMean, [Station, Type]}).
getDailyMean(Type, Date) -> gen_server:call(?MODULE, {getDailyMean, [Type, Date]}).
getDailyOverLimit(Type, Date, Limit) -> gen_server:call(?MODULE, {getDailyOverLimit, [Type, Date, Limit]}).
getYearlyMean(Type, Year) -> gen_server:call(?MODULE, {getYearlyMean, [Type, Year]}).

handle_call({getOneValue, [Station, Date, Type]}, _From, Monitor) -> {reply, pollution:getOneValue(Station, Date, Type, Monitor), Monitor};
handle_call({getStationMean, [Station, Type]}, _From, Monitor) -> {reply, pollution:getStationMean(Station, Type, Monitor), Monitor};
handle_call({getDailyMean, [Type, Date]}, _From, Monitor) -> {reply, pollution:getDailyMean(Type, Date, Monitor), Monitor};
handle_call({getDailyOverLimit, [Type, Date, Limit]}, _From, Monitor) -> {reply, pollution:getDailyOverLimit(Type, Date, Limit, Monitor), Monitor};
handle_call({getYearlyMean, [Type, Year]}, _From, Monitor) -> {reply, pollution:getYearlyMean(Type, Year, Monitor), Monitor};
handle_call(showMonitor, _From, Monitor) -> {reply, Monitor, Monitor}.

handle_cast_result({error, Text}, Monitor) -> erlang:display({error, Text}), {noreply, Monitor};
handle_cast_result(NewMonitor, _) -> {noreply, NewMonitor}.

handle_info(_, Monitor) -> {noreply, Monitor}.

terminate(Reason, Monitor) -> erlang:display(Monitor), Reason.