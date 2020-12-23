-module(day22).
-export([run/0]).
-import(utils, [chunks/1, puts/0, puts/1, input/1]).

run() ->
  puts(),
  Input = input("day22-input.txt"),
  run1(Input),
  run2(Input).

run1(Input) ->
  [ P1, P2 ] = [ parse_block(Block) || Block <- blocks(Input) ],
  Pred = fun (X, Y, _) -> X < Y end,
  { _, Ns } = play(Pred, { P1, P2 }),
  Ans = score(Ns),
  puts(Ans).

run2(Input) ->
  [ P1, P2 ] = [ parse_block(Block) || Block <- blocks(Input) ],
  { _, Ns } = play(fun recursive_pred/3, { P1, P2 }),
  Ans = score(Ns),
  puts(Ans).

score(Xs) ->
  lists:sum([ X * Y || { X, Y } <- lists:zip(lists:reverse(Xs), lists:seq(1, length(Xs))) ]).

recursive_pred(X, Y, { Xs, Ys }) ->
  case (X =< length(Xs)) andalso (Y =< length(Ys)) of
    false -> X < Y;
    true ->
      { Xs1, _ } = lists:split(X, Xs),
      { Ys1, _ } = lists:split(Y, Ys),
      case play(fun recursive_pred/3, { Xs1, Ys1 }) of
        { p1_win, _ } -> false;
        _ -> true
      end
  end.


play(Pred, Cards) -> play(Pred, Cards, { sets:new(), sets:new() }).

play(_, { Xs, [] }, _) -> { p1_win, Xs };
play(_, { [], Ys }, _) -> { p2_win, Ys };
play(Pred, { XCards = [X | Xs], YCards = [Y | Ys] }, { XHistory, YHistory }) ->
  case sets:is_element(XCards, XHistory) orelse sets:is_element(YCards, YHistory) of
    false ->
      History1 = { sets:add_element(XCards, XHistory), sets:add_element(YCards, YHistory) },
      case Pred(X, Y, { Xs, Ys }) of
        false -> play(Pred, { Xs ++ [X, Y], Ys }, History1);
        true -> play(Pred, { Xs, Ys ++ [Y, X] }, History1)
      end;
    true ->
      { p1_win, XCards }
  end.

parse_block([_ | Xs]) -> lists:map(fun list_to_integer/1, Xs).

blocks(Input) ->
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
