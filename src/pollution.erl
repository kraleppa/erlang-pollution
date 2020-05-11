%%%-------------------------------------------------------------------
%%% @author krzysztof
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. mar 2020 15:46
%%%-------------------------------------------------------------------
-module(pollution).
-author("krzysztof").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getDailyOverLimit/4, getYearlyMean/3, getStationByCords/2]).

-record(monitor, {stations, data}).

%% inicjalizuje monitor
createMonitor() -> #monitor{stations = #{}, data = #{}}.

%% sprawdza czy dana stacja istnieje
stationExists(Name, Cords, Monitor) ->
  Map1 = maps:filter(fun(Station, _) -> Station == Name end, Monitor#monitor.stations),
  Map2 = maps:filter(fun(_, KeyCords) -> KeyCords == Cords end, Monitor#monitor.stations),
  (maps:size(Map1) /= 0) or (map_size(Map2) /= 0).

stationExists(Name, Monitor) ->
  Map1 = maps:filter(fun(Station, _) -> Station == Name end, Monitor#monitor.stations),
  maps:size(Map1) /= 0.

%% dodaje stacje
addStation(Name, {X, Y}, Monitor) when is_list(Name) and is_number(X) and is_number(Y) and is_record(Monitor, monitor)->
  case stationExists(Name, {X, Y}, Monitor) of
    false -> #monitor{stations = maps:put(Name, {X, Y}, Monitor#monitor.stations), data = Monitor#monitor.data};
    _ -> {error, "Station already exists"}
  end.

%% sprawdza czy dany pomiar zostal juz zapisany
valueExists(Tuple, Monitor) ->
  Map = maps:filter(fun(Key, _) -> Key == Tuple end, Monitor#monitor.data),
  maps:size(Map) /= 0.


getStationByCords(Tuple, Monitor) when is_tuple(Tuple) and is_record(Monitor, monitor) ->
  Map = maps:filter(fun(_, Cords) -> Cords == Tuple end, Monitor#monitor.stations),
  List = maps:keys(Map),
  case length(List) of
    1 -> [Head | _] = List, Head;
    _ -> "Station not Found"
  end.


%% dodaje pomiar
addValue(Tuple, Date, Type, Value, Monitor) when is_tuple(Tuple) and is_list(Type) and is_number(Value) and is_record(Monitor, monitor)->
  Name = getStationByCords(Tuple, Monitor),
  addValue(Name, Date, Type, Value, Monitor);

addValue(Name, Date, Type, Value, Monitor) when is_list(Name) and is_list(Type) and is_number(Value) and is_record(Monitor, monitor)->
  case not valueExists({Name, Date, Type}, Monitor) of
    true -> #monitor{stations = Monitor#monitor.stations, data = maps:put({Name, Date, Type}, Value, Monitor#monitor.data)};
    _ -> {error, "Value already exists"}
  end.


%% ususwa pomiar
removeValue(Tuple, Date, Type, Monitor) when is_tuple(Tuple) and is_list(Type) and is_record(Monitor, monitor) ->
  Name = getStationByCords(Tuple, Monitor),
  removeValue(Name, Date, Type, Monitor);

removeValue(Name, Date, Type, Monitor) when is_list(Name) and is_list(Type) and is_record(Monitor, monitor) ->
  case valueExists({Name, Date, Type}, Monitor) of
    true -> #monitor{stations = (Monitor#monitor.stations), data = maps:remove({Name, Date, Type}, Monitor#monitor.data)};
    _ -> {error, "Value does not exist"}
  end.



%% pobiera wartosc pomiaru
getOneValue(Tuple, Date, Type, Monitor) when is_tuple(Tuple) and is_list(Type) and is_record(Monitor, monitor) ->
  Name = getStationByCords(Tuple, Monitor),
  getOneValue(Name, Date, Type, Monitor);

getOneValue(Name, Date, Type, Monitor) when is_list(Name) and is_list(Type) and is_record(Monitor, monitor) ->
  case stationExists(Name, Monitor) of
    false -> {error, "Station does not exist"};
    _ ->
      case valueExists({Name, Date, Type}, Monitor) of
        true -> maps:get({Name, Date, Type}, Monitor#monitor.data);
        _ -> {error, "Value does not exist"}
      end
  end.

%% pobiera srednia wartosc danego pomiaru na stacji
getStationMean(Tuple, Type, Monitor) when is_tuple(Tuple) and is_list(Type) ->
  Name = getStationByCords(Tuple, Monitor),
  getStationMean(Name, Type, Monitor);

getStationMean(Name, Type, Monitor) when is_list(Name) and is_list(Type) ->
  case stationExists(Name, Monitor) of
    false -> {error, "Station does not exist"};
    _ ->
      F = fun({KeyName, _, KeyType}, _) -> (Name == KeyName) and (Type == KeyType) end,
      Map = maps:filter(F, (Monitor#monitor.data)),
      Sum = maps:fold(fun(_, Value, Acc) -> Acc + Value end, 0, Map),
      Sum / maps:size(Map)
  end.


%% pobiera srednia danego pomiaru danego dnia
getDailyMean(Type, Date, Monitor) when is_list(Type) ->
  case is_tuple(Date) of
    true ->
      Map = maps:filter(fun({_, {KeyDate, _}, KeyType}, _) -> (Date == KeyDate) and (Type == KeyType) end, Monitor#monitor.data),
      Sum = maps:fold(fun(_, Value, Acc) -> Acc + Value end, 0, Map),
      Sum / maps:size(Map);
    false -> {error, "Incorrect Date"}
  end.

%% zwraca ilosc pomiarow ktore danego dnia wyszly poza norme
getDailyOverLimit(Type, Date, Limit, Monitor) when is_list(Type) and is_number(Limit) ->
  case is_tuple(Date) of
    true ->
      F = fun({_, {KeyDate, _}, KeyType}, Value) -> (Value > Limit) and (KeyType == Type) and (Date == KeyDate) end,
      Map = maps:filter(F, (Monitor#monitor.data)),
      maps:size(Map);
    false -> {error, "Incorrect Date"}
  end.

%% zwraca roczna srednia
getYearlyMean(Type, Year, Monitor) when is_list(Type) and is_number(Year) ->
  F = fun({_, {{KeyYear, _, _}, _}, KeyType}, _) -> (Year == KeyYear) and (Type == KeyType) end,
  Map = maps:filter(F, (Monitor#monitor.data)),
  Sum = maps:fold(fun(_, Value, Acc) -> Acc + Value end, 0, Map),
  Sum / maps:size(Map).
