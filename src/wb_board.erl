-module(wb_board).

-include("wb_board.hrl").

-export([
    create_board/1, create_board/2,
    random_letter/0,
    letter/3,
    rotate_position/5,
    word_count/1
  ]).

create_board({Rows, Columns}) ->
  create_board(Rows, Columns).

create_board(Rows, Columns) ->
  Matrix = create_letter_matrix(Rows, Columns),
  #board{rows = Rows, columns = Columns, matrix = Matrix}.

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

create_used_matrix(Rows, Columns) ->
  create_matrix(Rows, Columns, fun() -> false end).

rotate_position(#board{rows = Rows, columns = Columns}, Row, Column, RotRow, RotColumn) ->
  {((Row + RotRow - 1) rem Rows) + 1, ((Column + RotColumn - 1) rem Columns) + 1}.

letter(#board{matrix = Matrix}, Row, Column) ->
  element(Column, element(Row, Matrix)).

word_count(#board{rows = Rows, columns = Columns} = Board) ->
  RootNode = wb_tree:get_node(0),
  Used = create_used_matrix(Rows, Columns),

  lists:foldl(
    fun(Row, WordCount) ->
      WordCount + lists:foldl(
        fun(Column, WordCountRow) ->
          WordCountRow + word_count(Board, Row, Column, RootNode, Used)
        end, 0, lists:seq(1, Columns)
      )
    end, 0, lists:seq(1, Rows)
  ).

word_count(Board, Row, Column, Node, Used) ->
  word_count(Board, Row, Column, Node, Used, 1).

word_count(#board{rows = Rows, columns = Columns} = Board, Row, Column, {_Id, _Terminal, Ids}, Used, Length) ->
  case (Row >= 1) andalso (Column >= 1) andalso (Row =< Rows) andalso (Column =< Columns) andalso (not element(Column, element(Row, Used))) of
    true ->
      Letter = letter(Board, Row, Column),
      Id = element(Letter - $a + 1, Ids),
      wb_tree:get_node(Id),

      case wb_tree:get_node(Id) of
        not_found -> 0;
        Node ->
          UsedRow = setelement(Column, element(Row, Used), true),
          Used2 = setelement(Row, Used, UsedRow),

          {_Id2, Terminal, _Ids2} = Node,
          Count = word_count(Board, Row + 1, Column, Node, Used2, Length + 1)
            + word_count(Board, Row - 1, Column, Node, Used2, Length + 1)
            + word_count(Board, Row, Column + 1, Node, Used2, Length + 1)
            + word_count(Board, Row, Column - 1, Node, Used2, Length + 1)
            + word_count(Board, Row + 1, Column + 1, Node, Used2, Length + 1)
            + word_count(Board, Row - 1, Column - 1, Node, Used2, Length + 1)
            + word_count(Board, Row - 1, Column + 1, Node, Used2, Length + 1)
            + word_count(Board, Row + 1, Column - 1, Node, Used2, Length + 1),

          case Terminal and (Length >= 3) of
            true -> 1 + Count;
            false -> Count
          end
      end;
    false -> 0
  end.

random_letter() ->
  $a + random:uniform(26) - 1.

