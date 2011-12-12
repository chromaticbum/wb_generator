-module(wb_generator).

-export([
    generate_board/4
  ]).

-record(scored_grid, {
    score,
    grid
  }).

perform_rounds(ScoredGrids, _GridCount, 0) ->
  ScoredGrids;
perform_rounds(ScoredGrids, GridCount, Count) ->
  PerturbedGrids = lists:map(
    fun(#scored_grid{grid = Grid}) ->
      Grid2 = wb_perturb:perturb_grid(Grid),
      #scored_grid{score = wb_grid:word_count(Grid2), grid = Grid2}
    end, ScoredGrids
  ),

  {_LowScores, Grids} = lists:split(GridCount, 
    lists:sort(
      fun(#scored_grid{score = Score1}, #scored_grid{score = Score2}) ->
        Score1 =< Score2
      end, lists:append(ScoredGrids, PerturbedGrids)
    )
  ),

  case (Count rem 50) of
    0 ->
      lists:foreach(
        fun(#scored_grid{score = Score,grid = Grid}) ->
          io:format("~s:~p~n", [wb_grid:compact_string(Grid), Score])
        end, Grids
      ),
      io:format("~n");
    _ -> ok
  end,

  perform_rounds(Grids, GridCount, Count - 1).

init_grids(Rows, Columns, GridCount) ->
  lists:map(
    fun(_) ->
      Grid = wb_grid:create_letter_grid(Rows, Columns),
      #scored_grid{score = wb_grid:word_count(Grid), grid = Grid}
    end,
    lists:seq(1, GridCount)
  ).

generate_board(Rows, Columns, GridCount, Rounds) ->
  ScoredGrids = init_grids(Rows, Columns, GridCount),
  perform_rounds(ScoredGrids, GridCount, Rounds).
