-module(day14).
-export([run/0]).
-import(utils, [bool/3, puts/1]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  Insts = parse(fun parse_mask_and_or/1, Input),
  F =
    fun(Inst, { Mask, Mem }) ->
      case Inst of
        { mask, X } -> { X, Mem };
        { mem, { I, V } } -> { Mask, maps:put(I, apply_mask(Mask, V), Mem) }
      end
    end,
  { _, Mem } = lists:foldl(F, {nil, #{}}, Insts),
  Ans = lists:sum(maps:values(Mem)),
  puts(Ans).

run2(Input) ->
  Insts = parse(fun parse_mask_floating/1, Input),
  F =
    fun(Inst, { Mask, Mem }) ->
      case Inst of
        { mask, X } -> { X, Mem };
        { mem, { I, V } } -> { Mask, apply_mask(Mask, I, V, Mem) }
      end
    end,
  { _, Mem } = lists:foldl(F, {nil, #{}}, Insts),
  Ans = lists:sum(maps:values(Mem)),
  puts(Ans).

parse(ParseMask, Input) ->
  F =
    fun(Line) ->
      case string:split(Line, " = ") of
        [ "mask", V ] -> { mask, ParseMask(V) };
        [ K, V ] -> { mem, { parse_mem_key(K), list_to_integer(V) } }
      end
    end,
  [ F(Line) || Line <- Input ].

apply_mask({ And, Or }, X) -> X band And bor Or.

apply_mask({ And, Or, Floatings }, I, V, Mem) ->
  Addrs = [apply_mask({ And, Or }, I) + lists:sum(Bits) || Bits <- perms(Floatings)],
  lists:foldr(fun(X, Acc) -> maps:put(X, V, Acc) end, Mem, Addrs).

parse_mask_and_or(Str) ->
  And = tr(Str, "X", "1"),
  Or = tr(Str, "X", "0"),
  { list_to_integer(And, 2), list_to_integer(Or, 2) }.

parse_mask_floating(Str) ->
  And = tr(Str, "0X", "10"),
  Or = tr(Str, "X", "0"),
  Xs = tr(Str, "1X", "01"),
  N = list_to_integer(Xs, 2),
  Floatings = lists:concat([ bool([], [1 bsl I], 0 < ((1 bsl I) band N)) || I <- lists:seq(0, 35) ]),
  { list_to_integer(And, 2), list_to_integer(Or, 2), Floatings }.


perms([]) -> [[]];
perms([X| Xs])  -> perms(Xs) ++ [[X|T] || T <- perms(Xs)].  

tr(Str, Ss, Ts) ->
  lists:foldl(
    fun ({S, T}, Acc) -> lists:concat(string:replace(Acc, [S], [T], all)) end,
    Str,
    lists:zip(Ss, Ts)
  ).

parse_mem_key(Str) ->
  Pred = fun(C) -> ($0 =< C) and (C =< $9) end,
  N = lists:filter(Pred, Str),
  list_to_integer(N).
       
input() ->
  { ok, Device } = file:open("day14-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
