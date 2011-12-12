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
    1 -> perturb2(Grid);
    2 -> perturb2(Grid);
    3 -> perturb3(Grid);
    4 -> perturb4(Grid)
  end.

perturb1(#grid{type = letter, rows = Rows, columns = Columns} = Grid) ->
  Count = random:uniform(Rows * Columns),

  lists:foldl(
    fun(_, Grid2) ->
      wb_grid:set_value(Grid2, random:uniform(Rows), random:uniform(Columns), wb_grid:random_letter())
    end, Grid, lists:seq(1, Count)
  ).

perturb2(#grid{rows = Rows, columns = Columns} = Grid) ->
  Count = random:uniform(Rows * Columns),

  lists:foldl(
    fun(_, Grid2) ->
      swap(Grid2, random:uniform(Rows), random:uniform(Columns), random:uniform(Rows), random:uniform(Columns))
    end, Grid, lists:seq(1, Count)
  ).

perturb3(#grid{rows = Rows, columns = Columns} = Grid) ->
  Count = random:uniform(Rows * Columns),

  lists:foldl(
    fun(_, Grid2) ->
      swap_random_adjacent(Grid2, Rows, Columns)
    end, Grid, lists:seq(1, Count)
  ).

perturb4(#grid{rows = Rows, columns = Columns} = Grid) ->
  RotRow = random:uniform(Rows - 1),
  RotColumn = random:uniform(Columns - 1),

  lists:foldr(
    fun(Row, Grid2) ->
      lists:foldr(
        fun(Column, Grid3) ->
          {Row2, Column2} = wb_grid:rotate_position(Grid, Row, Column, RotRow, RotColumn),
          wb_grid:set_value(Grid3, Row2, Column2, wb_grid:value(Grid, Row, Column))
        end, Grid2, lists:seq(1, Columns)
      )
    end, Grid, lists:seq(1, Rows)
  ).

swap(Grid, Row1, Column1, Row2, Column2) ->
  Letter1 = wb_grid:value(Grid, Row1, Column1),
  Grid2 = wb_grid:set_value(Grid, Row1, Column1, wb_grid:value(Grid, Row2, Column2)),
  wb_grid:set_value(Grid2, Row2, Column2, Letter1).

swap_random_adjacent(Grid, Rows, Columns) ->
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
    true -> swap(Grid, Row1, Column1, Row2, Column2);
    false -> swap_random_adjacent(Grid, Rows, Columns)
  end.


