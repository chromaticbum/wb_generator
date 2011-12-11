-module(wb_perturb).

-include("wb_perturb.hrl").
-include("wb_board.hrl").

-export([
    perturb_board/1,
    replace/2,
    swap/2
  ]).

perturb_board(Board) ->
  Perturb = generate_random(Board),
  #perturb{method = Method} = Perturb,

  wb_perturb:Method(Board, Perturb).

generate_random(Board) ->
  case random:uniform(2) of
    1 -> generate_perturb1(Board);
    2 -> generate_perturb2(Board);
    3 -> generate_perturb3(Board)
  end.

generate_perturb1(#board{rows = Rows, columns = Columns}) ->
  Size = Rows * Columns,
  Count = random:uniform(Size),

  Changes = lists:map(
    fun(_) -> {random:uniform(Size), wb_board:random_letter()} end,
    lists:seq(1, Count)
  ),

  #perturb{method = replace, changes = Changes}.

generate_perturb2(#board{rows = Rows, columns = Columns}) ->
  Size = Rows * Columns,
  Count = random:uniform(Size),

  Changes = lists:map(
    fun(_) -> {random:uniform(Size), random:uniform(Size)} end,
    lists:seq(1, Count)
  ),

  #perturb{method = swap, changes = Changes}.

generate_perturb3(#board{rows = Rows, columns = Columns} = Board) ->
  Size = Rows * Columns,
  Count = random:uniform(Size),

  Changes = lists:map(
    fun(_) -> generate_perturb3_change(Board) end,
    lists:seq(1, Count)
  ),

  #perturb{method = swap, changes = Changes}.

generate_perturb3_change(#board{rows = Rows, columns = Columns} = Board) ->
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
    true -> {Row1 * Column1, Row2 * Column2};
    false -> generate_perturb3_change(Board)
  end.

% replace letters with new letters
replace(#board{matrix = Matrix}, #perturb{changes = Changes}) ->
  lists:foldl(
    fun({Index, Letter}, Matrix2) -> setelement(Index, Matrix2, Letter) end,
    Matrix, Changes
  ).

% swap letters
swap(#board{matrix = Matrix}, #perturb{changes = Changes}) ->
  lists:foldl(
    fun({Index1, Index2}, Matrix2) ->
      Letter = element(Index1, Matrix2),
      Matrix3 = setelement(Index1, Matrix2, element(Index2, Matrix2)),
      setelement(Index2, Matrix3, Letter)
    end,
    Matrix, Changes
  ).

