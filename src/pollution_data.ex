defmodule PollutionData do
  @moduledoc false

  def import_lines_from_csv(), do:
    File.read!("pollution.csv") |> String.split("\n")

  def parse(line) do
    [date, time, x, y, value] = String.split(line, ",")
    date = String.split(date, "-") |> Enum.reverse |>  Enum.map(&(Integer.parse(&1) |> elem(0))) |> :erlang.list_to_tuple()
    time = String.split(time, ":") |> Enum.map(&(Integer.parse(&1) |> elem(0))) |> :erlang.list_to_tuple()
    location = {Float.parse(x) |> elem(0), Float.parse(y) |> elem(0)}
    value = Integer.parse(value) |> elem(0)

    %{:datetime => {date, time}, :location => location, :pollutionLevel => value}
  end

  def identify_stations(list), do:
    Enum.uniq_by(list, &(&1.location))


  def add_stations([station | tail]) do
    name = 'station_#{station.location |> elem(0)}_#{station.location |> elem(1)}'
    :pollution_gen_server.addStation(name, station.location)
    add_stations(tail)
  end
  def add_stations([]), do: :ok

  def add_values([value | tail]) do
    name = 'station_#{value.location |> elem(0)}_#{value.location |> elem(1)}'
    :pollution_gen_server.addValue(name, value.datetime, 'PM 10', value.pollutionLevel)
    add_values(tail)
  end
  def add_values([]), do: :ok

  def load_data() do
    :pollution_sup.start_link()
    exec_station = fn () -> Enum.map(import_lines_from_csv(), &(parse(&1))) |> identify_stations() |> add_stations() end
    exec_values = fn () -> Enum.map(import_lines_from_csv(), &(parse(&1))) |> add_values() end

    IO.puts("#{ exec_station |> :timer.tc([]) |> elem(0)}")
    IO.puts("#{ exec_values |> :timer.tc([]) |> elem(0)}")
  end

  def analyze() do
    {time, value} = (&:pollution_gen_server.getStationMean/2) |> :timer.tc(['station_20.06_49.986', 'PM 10'])
    IO.puts("#{time}, #{value}")
    {time, value} = (&:pollution_gen_server.getDailyMean/2) |> :timer.tc(['PM 10', {2017, 5, 3}])
    IO.puts("#{time}, #{value}")
  end
end
