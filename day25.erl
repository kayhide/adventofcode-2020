-module(day25).
-export([run/0]).
-import(utils, [puts/0, puts/1, input/1]).

run() ->
  puts(),
  Input = input("day25-input.txt"),
  % Input = ["5764801", "17807724"],
  run1(Input).

run1(Input) ->
  [CardPub, DoorPub] = [ list_to_integer(X) || X <- Input ],

  Divider = 20201227,
  CardLoop = find_loop_size(make_step_function(7, Divider), CardPub),
  DoorLoop = find_loop_size(make_step_function(7, Divider), DoorPub),
  EncKey = iterate(CardLoop, make_step_function(DoorPub, Divider), 1),
  EncKey = iterate(DoorLoop, make_step_function(CardPub, Divider), 1),
  Ans = EncKey,
  puts(Ans).

make_step_function(Sub, Divider) ->
  fun(X) -> (X * Sub) rem Divider end.

find_loop_size(F, Pub) -> find_loop_size(F, Pub, 1, 1).
find_loop_size(F, Pub, X, N) ->
  case F(X) of
    Pub -> N;
    X1 -> find_loop_size(F, Pub, X1, N + 1)
  end.
  
iterate(0, _, X) -> X;
iterate(N, F, X) -> iterate(N - 1, F, F(X)).
