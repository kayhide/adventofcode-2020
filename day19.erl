-module(day19).
-export([run/0]).
-import(utils, [bool/3, puts/0, puts/1, input/1]).

run() ->
  puts(),
  Input = input("day19-input.txt"),
  run1(Input),
  run2(Input).

run1(Input) ->
  [ RulesPart, Messages ] = chunks(Input),
  Rules = maps:from_list([ parse_rule(Line) || Line <- RulesPart ]),
  Ans = length(lists:filter(fun(Msg) -> is_valid(Rules, Msg) end, Messages)),
  puts(Ans).

run2(Input) ->
  [ RulesPart, Messages ] = chunks(Input),
  Rules = update_rules(maps:from_list([ parse_rule(Line) || Line <- RulesPart ])),
  Ans = length(lists:filter(fun(Msg) -> is_valid(Rules, Msg) end, Messages)),
  puts(Ans).

parse_rule(Line) ->
  [K, Body] = string:split(Line, ": "),
  case Body of
    [ $", C, $" ] -> { list_to_integer(K), { char, C } };
    _ ->
      Elems = [
        [ list_to_integer(X) || X <- string:split(Part, " ", all) ]
          || Part <- string:split(Body, " | ", all)
        ],
      { list_to_integer(K), { ref, Elems } }
  end.

update_rules(Rule) ->
  maps:merge(
    Rule,
    #{
      8 => { ref, [[42], [42, 8]] },
      11 => { ref, [[42, 31], [42, 11, 31]] }
    }
  ).

is_valid(Rules, Str) ->
  case validate(Rules, Str) of
    [] -> false;
    Res -> lists:any(fun(X) -> X =:= ok end, Res)
  end.
  
validate(Rules, Str) -> validate(Rules, 0, Str).

validate(Rules, I, Str) ->
  case maps:get(I, Rules) of
    { char, C } -> case Str of
      [ C ] -> [ok];
      [ C | Str1 ] -> [Str1];
      _ -> []
    end;
    { ref, Ptns } -> validate_patterns(Rules, Ptns, Str)
  end.

validate_patterns(Rules, Ptns, Str) ->
  lists:foldl(
    fun(Ptn, Acc) ->
      case validate_pattern(Rules, Ptn, Str) of
        fail -> Acc;
        Strs -> Acc ++ Strs
      end
    end, [], Ptns
  ).
  
validate_pattern(Rules, Ptn, Str) ->
  case length(Ptn) =< string:length(Str)  of
    false -> fail;
    true ->
      lists:foldl(
        fun(P, Acc) ->
          case Acc of
            _ ->
              lists:concat([ validate(Rules, P, bool(S, "", S =:= ok)) || S <- Acc ])
          end
        end, [Str], Ptn
      )
  end.
   
 
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
