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

create_used_grid(Rows, Columns) ->
  Matrix = create_matrix(Rows, Columns, fun() -> false end),
  #grid{type = used, rows = Rows, columns = Columns, matrix = Matrix}.

rotate_position(#grid{rows = Rows, columns = Columns}, Row, Column, RotRow, RotColumn) ->
  {((Row + RotRow - 1) rem Rows) + 1, ((Column + RotColumn - 1) rem Columns) + 1}.

value(#grid{matrix = Matrix}, Row, Column) ->
  element(Column, element(Row, Matrix)).

set_value(#grid{matrix = Matrix} = Grid, Row, Column, Value) ->
  Grid#grid{matrix = setelement(Row, Matrix, setelement(Column, element(Row, Matrix), Value))}.

word_count(#grid{type = letter, rows = Rows, columns = Columns} = Grid) ->
  RootNode = wb_tree:get_node(0),
  Used = create_used_grid(Rows, Columns),
  Info = ets:new(info, [set]),

  Count = lists:foldl(
    fun(Row, WordCount) ->
      WordCount + lists:foldl(
        fun(Column, WordCountRow) ->
          WordCountRow + word_count(Grid, Row, Column, RootNode, Used, Info)
        end, 0, lists:seq(1, Columns)
      )
    end, 0, lists:seq(1, Rows)
  ),
  ets:delete(Info),
  Count.

word_count(#grid{type = letter} = Grid, Row, Column, Node, Used, Info) ->
  word_count(Grid, Row, Column, Node, Used, 1, [], Info).

word_count(#grid{type = letter, rows = Rows, columns = Columns} = Grid, Row, Column, {_Id, _Terminal, Ids}, Used, Length, Word, Info) ->
  case (Row >= 1) andalso (Column >= 1) andalso (Row =< Rows) andalso (Column =< Columns) andalso (not value(Used, Row, Column)) of
    true ->
      Letter = value(Grid, Row, Column),
      Word2 = [Letter | Word],
      Id = element(Letter - $a + 1, Ids),

      case wb_tree:get_node(Id) of
        not_found -> 0;
        Node ->
          Used2 = set_value(Used, Row, Column, true),

          case ets:lookup(Info, Word) of
            [{Word}] -> Selected = true;
            [] -> Selected = false
          end,

          {Id, Terminal, _Ids2} = Node,
          Count = word_count(Grid, Row + 1, Column, Node, Used2, Length + 1, Word2, Info)
            + word_count(Grid, Row - 1, Column, Node, Used2, Length + 1, Word2, Info)
            + word_count(Grid, Row, Column + 1, Node, Used2, Length + 1, Word2, Info)
            + word_count(Grid, Row, Column - 1, Node, Used2, Length + 1, Word2, Info)
            + word_count(Grid, Row + 1, Column + 1, Node, Used2, Length + 1, Word2, Info)
            + word_count(Grid, Row - 1, Column - 1, Node, Used2, Length + 1, Word2, Info)
            + word_count(Grid, Row - 1, Column + 1, Node, Used2, Length + 1, Word2, Info)
            + word_count(Grid, Row + 1, Column - 1, Node, Used2, Length + 1, Word2, Info),

          case (not Selected) andalso Terminal andalso (Length >= 3) of
            true ->
              ets:insert(Info, {Word}),
              1 + Count;
            false -> Count
          end
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
