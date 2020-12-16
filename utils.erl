-module(utils).
-compile(export_all).

fst({ F, _ }) -> F.
snd({ _, S }) -> S.

bool(F, _T, false) -> F;
bool(_F, T, true) -> T.

chunks(_N, []) -> [];
chunks(N, Xs) ->
  case lists:split(N, Xs) of
    { Chunk, Ys } -> [Chunk | chunks(N, Ys)]
  end.

puts() -> io:format("~n").
puts(X) -> io:format("~p~n", [X]).

echo(X) -> io:format("~s~n", [X]).

input(Filename) ->
  { ok, Device } = file:open(Filename, [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:chomp(Line) | get_all_lines(Device)]
  end.
