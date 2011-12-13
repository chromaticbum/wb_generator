-module(wb_grid).

-include("wb_grid.hrl").

-export([
    create_letter_grid/2,
    random_letter/0,
    random_vowel/0,
    value/3,
    set_value/4,
    rotate_position/5,
    word_count/1,
    compact_string/1
  ]).

compact_string(#grid{matrix = Matrix}) ->
  CompactList = lists:map(
    fun(Row) -> tuple_to_list(Row) end,
    tuple_to_list(Matrix)
  ),
  string:join(CompactList, "/").

create_letter_grid(Rows, Columns) ->
  Matrix = create_letter_matrix(Rows, Columns),
  #grid{type = letter, rows = Rows, columns = Columns, matrix = Matrix}.

create_letter_matrix(Rows, Columns) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),

  create_matrix(Rows, Columns, fun() -> random_letter() end).

create_matrix(Rows, Columns, Generate) ->
  list_to_tuple(lists:map(
    fun(_Index) -> create_row(Columns, Generate) end,
    lists:seq(1, Rows)
  )).

create_row(Columns, Generate) ->
  list_to_tuple(lists:map(
    fun(_Index) -> Generate() end,
    lists:seq(1, Columns)
  )).

rotate_position(#grid{rows = Rows, columns = Columns}, Row, Column, RotRow, RotColumn) ->
  {((Row + RotRow - 1) rem Rows) + 1, ((Column + RotColumn - 1) rem Columns) + 1}.

value(#grid{matrix = Matrix}, Row, Column) ->
  element(Column, element(Row, Matrix)).

set_value(#grid{matrix = Matrix} = Grid, Row, Column, Value) ->
  Grid#grid{matrix = setelement(Row, Matrix, setelement(Column, element(Row, Matrix), Value))}.

word_count(#grid{type = letter, rows = Rows, columns = Columns} = Grid) ->
  Info = ets:new(info, [set]),
  Count = lists:foldl(
    fun(Row, WordCount) ->
      WordCount + lists:foldl(
        fun(Column, WordCountRow) ->
            WordCountRow + word_count(Grid, 0, Row, Column, 0, Info)
        end, 0, lists:seq(1, Columns)
      )
    end, 0, lists:seq(1, Rows)
  ),

  ets:delete(Info),
  Count.

is_selected(Info, Id) ->
  ets:member(Info, Id).

set_selected(Info, Id) ->
  ets:insert(Info, {Id}).

is_used(Info, Row, Column) ->
  ets:member(Info, {Row, Column}).

set_used(Info, Row, Column) ->
  ets:insert(Info, {{Row, Column}}).

set_unused(Info, Row, Column) ->
  ets:delete_object(Info, {{Row, Column}}).

word_count(#grid{rows = Rows, columns = Columns} = Grid, Id, Row, Column, Length, Info) ->
  case (Row >= 1) andalso (Column >= 1) andalso (Row =< Rows) andalso (Column =< Columns) andalso(not is_used(Info, Row, Column)) of
    true ->
      Letter = value(Grid, Row, Column),

      case trie:get_node(Id, Letter) of
        {{Id, Letter}, Id2} ->
          set_used(Info, Row, Column),
          Count =
            word_count(Grid, Id2, Row + 1, Column, Length + 1, Info) +
            word_count(Grid, Id2, Row - 1, Column, Length + 1, Info) +
            word_count(Grid, Id2, Row, Column + 1, Length + 1, Info) +
            word_count(Grid, Id2, Row, Column - 1, Length + 1, Info) +
            word_count(Grid, Id2, Row + 1, Column + 1, Length + 1, Info) +
            word_count(Grid, Id2, Row - 1, Column - 1, Length + 1, Info) +
            word_count(Grid, Id2, Row - 1, Column + 1, Length + 1, Info) +
            word_count(Grid, Id2, Row + 1, Column - 1, Length + 1, Info),
          set_unused(Info, Row, Column),

          case (not is_selected(Info, Id2)) andalso trie:is_terminal(Id2) andalso (Length >= 3) of
            true ->
              set_selected(Info, Id2),
              Count +1;
            false -> Count
          end;
        not_found -> 0
      end;
    false -> 0
  end.

random_letter() ->
  $a + random:uniform(26) - 1.

random_vowel() ->
  case random:uniform(5) of
    1 -> $a;
    2 -> $e;
    3 -> $i;
    4 -> $o;
    5 -> $u
  end.
