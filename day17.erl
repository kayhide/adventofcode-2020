-module(day17).
-export([run/0]).
-import(utils, [bool/3, echo/1, puts/0, puts/1, input/1]).

run() ->
  puts(),
  Input = input("day17-input.txt"),
  run1(Input),
  run2(Input).

run1(Input) ->
  Space = to_space(Input),
  { _, Data} = iterate(6, fun step/1, Space),
  Ans = count($#, array:to_list(Data)),
  puts(Ans).

run2(Input) ->
  Space = to_hyperspace(Input),
  { _, Data} = iterate(6, fun step/1, Space),
  Ans = count($#, array:to_list(Data)),
  puts(Ans).

step(Space) ->
  Space1 = { Size1, _ } = expand($., Space),
  F = fun({ C, Ns }) ->
    case { C, count($#, Ns) } of
      { ".", 3 } -> $#;
      { ".", _ } -> $.;
      { "#", 2 } -> $#;
      { "#", 3 } -> $#;
      { "#", _ } -> $.
    end
  end,
  Next = [ F({ cell_at(P, Space1), neighbors(P, Space1) }) || P <- positions(Space1)],
  { Size1, array:from_list(Next) }.

iterate(0, _Step, State) ->
  % display(State),
  State;
iterate(I, Step, State) ->
  % display(State),
  iterate(I - 1, Step, Step(State)).

count(X, Xs) ->
  F = fun(Y, Acc) ->
    case Y of
      X -> Acc + 1;
      _ -> Acc
    end
  end,
  lists:foldr(F, 0, Xs).


display({ { W, H, _ }, Data }) ->
  [ display_plane(W, Plane) 
  || Plane <- chunks(W * H, array:to_list(Data))
  ],
  puts(),
  puts();

display({ { W, H, D, _ }, Data }) ->
  [ display({{ W, H, D }, array:from_list(Volume)}) 
  || Volume <- chunks(W * H * D, array:to_list(Data))
  ],
  puts(),
  puts().

display_plane(W, Plane) ->
  [ echo(Line) || Line <- chunks(W, Plane) ],
  puts().

to_space(Input = [ Line | _ ]) ->
  W = length(Line),
  H = length(Input),
  { { W, H, 1 }, array:fix(array:from_list(lists:concat(Input))) }.

to_hyperspace(Input = [ Line | _ ]) ->
  W = length(Line),
  H = length(Input),
  { { W, H, 1, 1 }, array:fix(array:from_list(lists:concat(Input))) }.

expand(C, { { W, H, D }, Data }) ->
  EmptyLine = [ C || _ <- lists:seq(1, W + 2) ],
  EmptyPlane = [ C || _ <- lists:seq(1, (W + 2) * (H + 2)) ],
  Data1 =
    lists:flatten(
      [EmptyPlane] ++
      lists:concat(
      [ [EmptyLine] ++ [ [C] ++ Line ++ [C] || Line <- chunks(W, Plane) ] ++ [EmptyLine]
        || Plane <- chunks(W * H, array:to_list(Data))
        ])
        ++ [EmptyPlane]
        ),
  { { W + 2, H + 2, D + 2 }, array:from_list(Data1) };

expand(C, { { W, H, D, E }, Data }) ->
  EmptyLine = [ C || _ <- lists:seq(1, W + 2) ],
  EmptyPlane = [ C || _ <- lists:seq(1, (W + 2) * (H + 2)) ],
  EmptyVolume = [ C || _ <- lists:seq(1, (W + 2) * (H + 2) * (D + 2)) ],
  Data1 =
    lists:flatten(
      [EmptyVolume] ++
      [ [EmptyPlane] ++
        [ [EmptyLine] ++
          [ [C] ++ Line ++ [C]
            || Line <- chunks(W, Plane)
          ] ++
          [EmptyLine]
          || Plane <- chunks(W * H, Volume)
        ] ++
        [EmptyPlane]
        || Volume <- chunks(W * H * D, array:to_list(Data))
      ] ++
      [EmptyVolume]
    ),
  { { W + 2, H + 2, D + 2, E + 2 }, array:from_list(Data1) }.
  
  
positions({ { W, H, _ }, Data }) ->
  [ { I rem W, (I div W) rem H, I div (W * H) } || I <- lists:seq(0, array:size(Data) - 1) ];

positions({ { W, H, D, _ }, Data }) ->
  [ { I rem W, (I div W) rem H, I div (W * W) rem D, I div (W * H * D) } || I <- lists:seq(0, array:size(Data) - 1) ].

neighbors({ X, Y, Z }, Space) ->
  lists:flatten(
    [ [ [ cell_at({ X + I, Y + J, Z + K }, Space)
            || I <- lists:seq(-1, 1), ((I /= 0) or (J /= 0) or (K /= 0))
        ] || J <- lists:seq(-1, 1)
      ] || K <- lists:seq(-1, 1)
    ]
  );

neighbors({ X, Y, Z, W }, Space) ->
  lists:flatten(
    [ [ [ [ cell_at({ X + I, Y + J, Z + K, W + L }, Space)
              || I <- lists:seq(-1, 1), ((I /= 0) or (J /= 0) or (K /= 0) or (L /= 0))
          ] || J <- lists:seq(-1, 1)
        ] || K <- lists:seq(-1, 1)
      ] || L <- lists:seq(-1, 1)
    ]
  ).
  
cell_at({ X, Y, Z }, { { W, H, D }, Data })
  when 0 =< X, X < W, 0 =< Y, Y < H, 0 =< Z, Z < D -> [array:get((W * H * Z) + (W * Y) + X, Data)];
cell_at({ X, Y, Z, W }, { { DX, DY, DZ, DW }, Data })
  when 0 =< X, X < DX, 0 =< Y, Y < DY, 0 =< Z, Z < DZ, 0 =< W, W < DW
  -> [array:get(DX * (DY * (DZ * W + Z) + Y) + X, Data)];
cell_at(_, _) -> "".


chunks(_N, []) -> [];
chunks(N, Xs) ->
  case lists:split(N, Xs) of
    { Chunk, Ys } -> [Chunk | chunks(N, Ys)]
  end.
