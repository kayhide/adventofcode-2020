-module(day11).
-export([run/0]).

run() ->
  Input = input(),
  run1(Input),
  run2(Input).

run1(Input) ->
  Board = to_board(Input),
  Positions = positions(Board),
  { Size, _ } = Board,
  Step = fun(B) ->
    Neighbors = array:map(fun(_, P) -> { cell_at(P, B), neighbors(P, B) } end, Positions),
    F = fun(_, { C, Ns }) ->
      case { C, count($#, Ns) } of
        { "L", 0 } -> $#;
        { "#", N } -> bool($#, $L, 4 =< N);
        _ -> hd(C)
      end
    end,
    { Size, array:map(F, Neighbors) }
  end,
  { _, Data } = go(Step, to_board(Input)),
  Ans = count($#, array:to_list(Data)),
  puts(Ans).

run2(Input) ->
  Board = to_board(Input),
  Positions = positions(Board),
  { Size, _ } = Board,
  Step = fun(B) ->
    Neighbors = array:map(fun(_, P) -> { cell_at(P, B), visibles(P, B) } end, Positions),
    F = fun(_, { C, Ns }) ->
      case { C, count($#, Ns) } of
        { "L", 0 } -> $#;
        { "#", N } -> bool($#, $L, 5 =< N);
        _ -> hd(C)
      end
    end,
    { Size, array:map(F, Neighbors) }
  end,
  { _, Data } = go(Step, to_board(Input)),
  Ans = count($#, array:to_list(Data)),
  puts(Ans).

display({ { W, _ }, Data }) ->
  [ puts(Line) || Line <- chunks(W, array:to_list(Data)) ],
  puts().


to_board(Input = [ Line | _ ]) ->
  W = length(Line),
  H = length(Input),
  { { W, H }, array:fix(array:from_list(lists:concat(Input))) }.

go(Step, Board) ->
  case Step(Board) of
    Board -> Board;
    Next -> go(Step, Next)
  end.

count(X, Xs) ->
  F = fun(Y, Acc) ->
    case Y of
      X -> Acc + 1;
      _ -> Acc
    end
  end,
  lists:foldr(F, 0, Xs).

positions({ { W, _ }, Data }) ->
  array:fix(array:from_list(
  [ { I rem W, I div W } || I <- lists:seq(0, array:size(Data) - 1) ]
  )).

neighbors({ X, Y }, Board) ->
  lists:concat(
    [ cell_at({ X - 1, Y - 1 }, Board)
    , cell_at({ X,     Y - 1 }, Board)
    , cell_at({ X + 1, Y - 1 }, Board)
    , cell_at({ X - 1, Y     }, Board)
    , cell_at({ X + 1, Y     }, Board)
    , cell_at({ X - 1, Y + 1 }, Board)
    , cell_at({ X    , Y + 1 }, Board)
    , cell_at({ X + 1, Y + 1 }, Board)
    ]
  ).

visibles(Pos, Board) ->
  lists:concat(
    [ visible_in(Pos, {  -1, -1 }, Board)
    , visible_in(Pos, {   0, -1 }, Board)
    , visible_in(Pos, {  +1, -1 }, Board)
    , visible_in(Pos, {  -1,  0 }, Board)
    , visible_in(Pos, {  +1,  0 }, Board)
    , visible_in(Pos, {  -1, +1 }, Board)
    , visible_in(Pos, {   0, +1 }, Board)
    , visible_in(Pos, {  +1, +1 }, Board)
    ]
  ).

visible_in({ X, Y }, Dir = { Dx, Dy }, Board) ->
  case cell_at({ X + Dx, Y + Dy }, Board) of
    "" -> "";
    "." -> visible_in({ X + Dx, Y + Dy }, Dir, Board);
    C -> C
  end.

cell_at({ X, Y }, { { W, H }, Data })
  when 0 =< X, X < W, 0 =< Y, Y < H -> [array:get((W * Y) + X, Data)];
cell_at(_, _) -> "".

input() ->
  { ok, Device } = file:open("day11-input.txt", [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.

bool(F, _T, false) -> F;
bool(_F, T, true) -> T.

puts() -> io:format("~n").
puts(X) when is_list(X) -> io:format("~s~n", [X]);
puts(X) -> io:format("~p~n", [X]).

chunks(_N, []) -> [];
chunks(N, Xs) ->
  case lists:split(N, Xs) of
    { Chunk, Ys } -> [Chunk | chunks(N, Ys)]
  end.
