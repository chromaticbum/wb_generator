-module(wb_perturb).

-include("wb_perturb.hrl").
-include("wb_grid.hrl").

-export([
    perturb_grid/1,
    perturb1/1,
    perturb2/1,
    perturb3/1,
    perturb4/1
  ]).

perturb_grid(Grid) ->
  perturb_random(Grid).

perturb_random(Grid) ->
  case random:uniform(4) of
    1 -> perturb1(Grid);
    2 -> perturb2(Grid);
    3 -> perturb3(Grid);
    4 -> perturb4(Grid)
  end.

perturb1(#grid{type = letter, rows = Rows, columns = Columns, matrix = Matrix}) ->
  Size = Rows * Columns,
  Count = random:uniform(Size),

  lists:foldl(
    fun(_, Matrix2) ->
        setelement(random:uniform(Size), Matrix2, wb_board:random_letter())
    end, Matrix, lists:seq(1, Count)
  ).

perturb2(#grid{rows = Rows, columns = Columns, matrix = Matrix}) ->
  Size = Rows * Columns,
  Count = random:uniform(Size),

  lists:foldl(
    fun(_, Matrix2) ->
      swap(Matrix2, random:uniform(Size), random:uniform(Size))
    end, Matrix, lists:seq(1, Count)
  ).

perturb3(#grid{rows = Rows, columns = Columns, matrix = Matrix}) ->
  Size = Rows * Columns,
  Count = random:uniform(Size),

  lists:foldl(
    fun(_, Matrix2) ->
      swap_random_adjacent(Matrix2, Rows, Columns)
    end, Matrix, lists:seq(1, Count)
  ).

perturb4(#grid{rows = Rows, columns = Columns, matrix = Matrix} = Board) ->
  RotRow = random:uniform(Rows - 1),
  RotColumn = random:uniform(Columns - 1),

  Size = Rows * Columns,

  lists:foldl(
    fun(Index, Matrix2) ->
      setelement(Index, Matrix2, element(wb_board:rotate_position(Board, Index, RotRow, RotColumn), Matrix))
    end, Matrix, lists:seq(1, Size)
  ).

swap(Matrix, Index1, Index2) ->
  Letter1 = element(Index1, Matrix),
  Matrix2 = setelement(Index1, Matrix, element(Index2, Matrix)),
  setelement(Index2, Matrix2, Letter1).

swap_random_adjacent(Matrix, Rows, Columns) ->
  Row1 = random:uniform(Rows),
  Column1 = random:uniform(Columns),

  {Row2, Column2} = case random:uniform(8) of
    1 -> {Row1 + 1, Column1};
    2 -> {Row1 - 1, Column1};
    3 -> {Row1, Column1 + 1};
    4 -> {Row1, Column1 - 1};
    5 -> {Row1 + 1, Column1 + 1};
    6 -> {Row1 - 1, Column1 - 1};
    7 -> {Row1 - 1, Column1 + 1};
    8 -> {Row1 + 1, Column1 - 1}
  end,

  case (Row2 >= 1) and (Column2 >= 1) and (Row2 =< Rows) and (Column2 =< Columns) of
    true -> swap(Matrix, Row1 * Column1, Row2 * Column2);
    false -> swap_random_adjacent(Matrix, Rows, Columns)
  end.


