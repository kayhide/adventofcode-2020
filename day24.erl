-module(day24).
-export([run/0]).
-import(utils, [bool/3, puts/0, puts/1, input/1]).

run() ->
  puts(),
  Input = input("day24-input.txt"),
  run1(Input),
  run2(Input).

run1(Input) ->
  % puts(pos_from_dirs(parse("esew"))),
  % puts(pos_from_dirs(parse("nwwswee"))),
  Floor = initial_floor(Input),
  Ans = sets:size(Floor),
  puts(Ans).

run2(Input) ->
  Floor = initial_floor(Input),
  Floor1 = iterate(100, fun step/1, Floor),
  Ans = sets:size(Floor1),
  puts(Ans).

step(Floor) ->
  Whitens = [ P || P <- sets:to_list(Floor), is_whitening(P, Floor) ],
  Blackens = [ P || PB <- sets:to_list(Floor), P <- white_adjacents(PB, Floor), is_blackening(P, Floor) ],
  sets:union(sets:from_list(Blackens), sets:subtract(Floor, sets:from_list(Whitens))).

is_whitening(P, Floor) ->
  N = length(black_adjacents(P, Floor)),
  N =:= 0 orelse 2 < N.

is_blackening(P, Floor) ->
  N = length(black_adjacents(P, Floor)),
  N =:= 2.

black_adjacents(Pos, Floor) ->
  lists:filtermap(fun(P) -> sets:is_element(P, Floor) end, adjacents_of(Pos)).

white_adjacents(Pos, Floor) ->
  lists:filtermap(fun(P) -> not sets:is_element(P, Floor) end, adjacents_of(Pos)).

adjacents_of({X, Y}) ->
  [ {X + 1, Y}
  , {X,     Y + 1}
  , {X - 1, Y + 1}
  , {X - 1, Y}
  , {X,     Y - 1}
  , {X + 1, Y - 1}
  ].
  
pos_from_dirs(Dirs) ->
  lists:foldl(
    fun({Dx, Dy}, {X, Y}) ->
      {X + Dx, Y + Dy}
    end, {0, 0}, Dirs).

initial_floor(Input) ->
  Poss = [ pos_from_dirs(parse(Line)) || Line <- Input ],
  lists:foldl(
    fun(P, Ps) ->
      bool(sets:add_element(P, Ps), sets:del_element(P, Ps), sets:is_element(P, Ps))
    end, sets:new(), Poss).

parse(Line) ->
  case Line of
    [] -> [];
    [$e | Line1] -> [ {1, 0} | parse(Line1) ];
    [$s, $e | Line1] -> [ {0, 1} | parse(Line1) ];
    [$s, $w | Line1] -> [ {-1, 1} | parse(Line1) ];
    [$w | Line1] -> [ {-1, 0} | parse(Line1) ];
    [$n, $w | Line1] -> [ {0, -1} | parse(Line1) ];
    [$n, $e | Line1] -> [ {1, -1} | parse(Line1) ]
  end.

iterate(0, _, X) -> X;
iterate(N, F, X) ->
  iterate(N - 1, F, F(X)).
