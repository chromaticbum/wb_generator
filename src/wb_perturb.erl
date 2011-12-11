-module(wb_perturb).

-include("wb_perturb.hrl").
-include("wb_board.hrl").

-export([
    perturb_board/1,
    perturb1/2
  ]).

perturb_board(Board) ->
  Perturb = generate_random(Board),
  #perturb{method = Method} = Perturb,

  wb_perturb:Method(Board, Perturb).

generate_random(Board) ->
  case random:uniform(1) of
    1 -> generate_perturb1(Board)
  end.

generate_perturb1(#board{rows = Rows, columns = Columns}) ->
  Size = Rows * Columns,
  Count = random:uniform(Size div 3),

  Changes = lists:map(
    fun(_) -> {random:uniform(Size), wb_board:random_letter()} end,
    lists:seq(1, Count)
  ),

  #perturb{method = perturb1, changes = Changes}.

perturb1(#board{matrix = Matrix}, #perturb{changes = Changes}) ->
  lists:foldl(
    fun({Index, Letter}, Matrix2) -> setelement(Index, Matrix2, Letter) end,
    Matrix, Changes
  ).

