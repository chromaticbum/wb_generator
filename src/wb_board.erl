-module(wb_board).

-include("wb_board.hrl").

-export([
    create_board/1, create_board/2,
    random_letter/0,
    position/2, position/3,
    letter/2, letter/3,
    rotate_position/4,
    word_count/1
  ]).

create_board({Rows, Columns}) ->
  create_board(Rows, Columns).

create_board(Rows, Columns) ->
  Matrix = create_matrix(Rows, Columns),
  #board{rows = Rows, columns = Columns, matrix = Matrix}.

create_matrix(Rows, Columns) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),

  list_to_tuple(lists:map(
    fun(_Index) -> random_letter() end, lists:seq(1, Rows * Columns)
  )).

create_used_matrix(Rows, Columns) ->
  list_to_tuple(lists:map(
    fun(_Index) -> false end, lists:seq(1, Rows * Columns)
  )).

position(#board{columns = Columns}, Index) ->
  {(Index - 1) div Columns + 1, ((Index - 1) rem Columns) + 1}.

position(#board{columns = Columns}, Row, Column) ->
  (Row - 1) * Columns + Column.

rotate_position(#board{rows = Rows, columns = Columns} = Board, Index, RotRow, RotColumn) ->
  {Row, Column} = wb_board:position(Board, Index),
  {Row2, Column2} = {((Row + RotRow - 1) rem Rows) + 1, ((Column + RotColumn - 1) rem Columns) + 1},

  position(Board, Row2, Column2).

letter(#board{matrix = Matrix}, Index) ->
  element(Index, Matrix).

letter(Board, Row, Column) ->
  letter(Board, position(Row, Column)).

word_count(#board{rows = Rows, columns = Columns} = Board) ->
  Node = wb_tree:get_node(0),
  Used = create_used_matrix(Rows, Columns),

  lists:foldl(
    fun(Index, WordCount) ->
      WordCount + word_count(Board, position(Board, Index), Node, Used)
    end, 0, lists:seq(1, Rows * Columns)
  ).

word_count(Board, Position, Node, Used) ->
  word_count(Board, Position, Node, Used, 1).

word_count(#board{rows = Rows, columns = Columns} = Board, {Row, Column}, {_Id, _Terminal, Ids}, Used, Length) ->
  Index = position(Board, Row, Column),
  case (Row >= 1) andalso (Column >= 1) andalso (Row =< Rows) andalso (Column =< Columns) andalso (not element(Index, Used)) of
    true ->
      Letter = letter(Board, Index),
      Id = element(Letter - $a + 1, Ids),
      wb_tree:get_node(Id),

      case wb_tree:get_node(Id) of
        not_found -> 0;
        Node ->
          Used2 = setelement(Index, Used, true),
          {_Id2, Terminal, _Ids2} = Node,
          Count = word_count(Board, {Row + 1, Column}, Node, Used2, Length + 1)
            + word_count(Board, {Row - 1, Column}, Node, Used2, Length + 1)
            + word_count(Board, {Row, Column + 1}, Node, Used2, Length + 1)
            + word_count(Board, {Row, Column - 1}, Node, Used2, Length + 1)
            + word_count(Board, {Row + 1, Column + 1}, Node, Used2, Length + 1)
            + word_count(Board, {Row - 1, Column - 1}, Node, Used2, Length + 1)
            + word_count(Board, {Row - 1, Column + 1}, Node, Used2, Length + 1)
            + word_count(Board, {Row + 1, Column - 1}, Node, Used2, Length + 1),

          case Terminal and (Length >= 3) of
            true -> 1 + Count;
            false -> Count
          end
      end;
    false -> 0
  end.

random_letter() ->
  $a + random:uniform(26) - 1.

