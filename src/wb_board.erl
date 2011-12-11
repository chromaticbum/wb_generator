-module(wb_board).

-include("wb_board.hrl").

-export([
    create_board/1, create_board/2,
    random_letter/0
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

random_letter() ->
  $a + random:uniform(26) - 1.

