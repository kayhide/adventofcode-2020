-module(day04).
-export([run/0]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  Chunks = chunks(Input),
  Ans = length(lists:filter(fun is_valid/1, Chunks)),
  io:format("~B~n", [Ans]).
  
run2(Input) ->
  Chunks = chunks(Input),
  Ans = length(lists:filter(fun is_valid_strict/1, Chunks)),
  io:format("~B~n", [Ans]).
  
is_valid(Chunk) ->
  % [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid" ]
  Required = [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ],
  lists:all(fun(K) -> maps:is_key(K, Chunk) end, Required).

is_valid_strict(Chunk) ->
  is_valid(Chunk)
    andalso is_between(1920, 2002, maps:get("byr", Chunk))
    andalso is_between(2010, 2020, maps:get("iyr", Chunk))
    andalso is_between(2020, 2030, maps:get("eyr", Chunk))
    andalso ( 
      is_between(150, 193, strip_unit("cm", maps:get("hgt", Chunk)))
      orelse is_between(59, 76, strip_unit("in", maps:get("hgt", Chunk)))
    )
    andalso is_color(maps:get("hcl", Chunk))
    andalso lists:member(maps:get("ecl", Chunk), ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
    andalso is_valid_pid(maps:get("pid", Chunk))
.

is_valid_pid(Str) ->
  case read_integer(Str) of
    err -> false;
    _ -> length(Str) == 9
  end.

strip_unit(Unit, Str) ->
  case string:split(Str, Unit) of
    [X, []] -> X;
    _ -> err
  end.

is_between(Min, Max, X) ->
  case read_integer(X) of
    err -> false;
    I -> (Min =< I) and (I =< Max)
  end.

read_integer(Str) ->
  try list_to_integer(Str) of
    X -> X
  catch
    _:_ -> err
  end.

is_color(Str) ->
  case lists:split(1, Str) of
    {"#", X} -> try list_to_integer(X, 16) of
        _ -> true
      catch
        _:_ -> false
      end;
    _ -> false
  end.
  

chunks(Input) ->
  F = fun(X, { Chunks, Current }) -> case X of
      "" -> { Chunks ++ [Current], #{} };
      _ -> { Chunks, maps:merge(Current, parse(X)) }
      end
    end,
  { Chunks, Current } = lists:foldr(F, { [], #{} }, Input),
  case Current of
    "" -> Chunks;
    _ -> Chunks ++ [Current]
  end.

parse(Line) ->
  Xs = string:split(Line, " ", all),
  KVs = lists:map(fun(X) -> case string:split(X, ":") of [K, V] -> {K, V} end end, Xs),
  maps:from_list(KVs).
    

input() ->
  { ok, Device } = file:open("day04-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
