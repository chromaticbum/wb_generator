-module(wb_board).

-include("wb_board.hrl").

-export([
    create_board/1, create_board/2,
    random_letter/0,
    position/2, position/3,
    rotate_position/4
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

position(#board{columns = Columns}, Index) ->
  {(Index - 1) div Columns + 1, ((Index - 1) rem Columns) + 1}.

position(#board{columns = Columns}, Row, Column) ->
  (Row - 1) * Columns + Column.

rotate_position(#board{rows = Rows, columns = Columns} = Board, Index, RotRow, RotColumn) ->
  Size = Rows * Columns,

  {Row, Column} = wb_board:position(Board, Index),
  {Row2, Column2} = {((Row + RotRow - 1) rem Rows) + 1, ((Column + RotColumn - 1) rem Columns) + 1},

  position(Board, Row2, Column2).

random_letter() ->
  $a + random:uniform(26) - 1.

