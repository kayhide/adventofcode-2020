-module(day16).
-export([run/0]).
-import(utils, [puts/0, puts/1]).

run() ->
  puts(),
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  { Info, _, Nearby } = parse(Input),
  ValidNums = sets:from_list(lists:concat(maps:values(Info))),
  Res = [X || T <- Nearby, X <- T, not sets:is_element(X, ValidNums)],
  Ans = lists:sum(Res),
  puts(Ans).

run2(Input) ->
  { Info, My, Nearby } = parse(Input),
  ValidNums = sets:from_list(lists:concat(maps:values(Info))),
  ValidTickets = [T || T <- Nearby, lists:all(fun(X) -> sets:is_element(X, ValidNums) end, T)],
  InfoSet = maps:map(fun(_, V) -> sets:from_list(V) end, Info),
  
  Step = fun(Possibilities) ->
      { _, Res } = lists:foldl(fun go/2, { InfoSet, Possibilities }, ValidTickets),
      Res
    end,
    
  Possibilities = [maps:keys(Info) || _ <- lists:seq(1, length(My))],
  Res = fix(Step, Possibilities),

  Keys = [K || [K] <- Res],
  Deps = [N || { K, N } <- lists:zip(Keys, My), lists:prefix("departure", K)],
  Ans = lists:foldr(fun(X, Acc) -> X * Acc end, 1, Deps),
  puts(Ans).

fix(Step, State) ->
  case Step(State) of
    State -> State;
    Next -> fix(Step, Next)
  end.

go(Ticket, { InfoSet, Possibilities }) ->
  Next = [
    [ P || P <- Ps, sets:is_element(N, maps:get(P, InfoSet)) ]
    || { N, Ps } <- lists:zip(Ticket, Possibilities)
    ],
  { InfoSet, sweep(Next) }.

sweep(Possibilities) ->
  Singles = lists:concat([P || P <- Possibilities, length(P) =:= 1]),
  lists:map(
    fun(P) ->
      case P of
        [_] -> P;
        _ -> P -- Singles
      end
    end
    , Possibilities).

parse(Input) ->
  [Info, [_ | My], [_ | Nearby]] = chunks(Input),
  { parse_info(Info),
    [list_to_integer(X) || X <- string:split(My, ",", all)],
    [ [list_to_integer(X) || X <- string:split(T, ",", all) ] || T <- Nearby ]
  }.

parse_info(Items) ->
  KVs = lists:map(
    fun(Item) ->
      [K, V] = string:split(Item,": "),
      { K,
        lists:concat([parse_integer_range(X) || X <- string:split(V, " ", all), X /= "or"])
      }
    end,
    Items),
  maps:from_list(KVs).

parse_integer_range(Str) ->
  [Min, Max] = string:split(Str, "-"),
  lists:seq(list_to_integer(Min), list_to_integer(Max)).
  
chunks(Input) ->
  F = fun(X, { Chunks, Current }) -> case X of
      "" -> { Chunks ++ [Current], [] };
      _ -> { Chunks, Current ++ [X] }
      end
    end,
  { Chunks, Current } = lists:foldl(F, { [], [] }, Input),
  case Current of
    "" -> Chunks;
    _ -> Chunks ++ [Current]
  end.

input() ->
  { ok, Device } = file:open("day16-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
