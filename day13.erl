-module(day13).
-export([run/0]).
-import(utils, [bool/3, puts/0, puts/1]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  { T, Buses } = case Input of
      [X, Line] -> { list_to_integer(X), lists:filter(fun is_integer/1, parse(Line)) }
    end,
  Xs = maps:from_list([ { Bus - (T rem Bus), Bus } || Bus <- Buses ]),
  { W, Bus, _ } = maps:next(maps:iterator(Xs)),
  Ans = W * Bus,
  puts(Ans).

run2(Input) ->
  Buses = parse(lists:nth(2, Input)),
  IBuses = lists:zip(Buses, lists:seq(0, length(Buses) - 1)),
  Ans = find(0, IBuses),
  puts(Ans).

find(T, IBuses) ->
  Pred = fun({ Bus, I }) ->
      case Bus of
        x -> true;
        _ ->  (T + I) rem Bus =:= 0
      end
    end,
  Xs = lists:filter(Pred, IBuses),
  case length(Xs) =:= length(IBuses) of
    false ->
      Ys = [ Bus || { Bus, _ } <- Xs, Bus /= x ],
      Step = lists:foldr(fun(Z, Acc) -> Z * Acc end, 1, Ys),
      find(T + Step, IBuses);
    true -> T
  end.
  

parse(Line) ->
  F = fun(X) ->
    case X of
      "x" -> [x];
      _ -> [list_to_integer(X)]
    end
  end,
  lists:concat(lists:map(F, string:split(Line, ",", all))).
  
input() ->
  { ok, Device } = file:open("day13-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
