-module(wb_generator).

-export([
    generate_boards/4,
    print_boards/4,
    init/0
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
      #scored_grid{score = wb_grid:word_count(Grid2, wb_generator), grid = Grid2}
    end, ScoredGrids
  ),

  List = lists:usort(lists:sort(
    fun(#scored_grid{score = Score1}, #scored_grid{score = Score2}) ->
      Score1 =< Score2
    end, lists:append(ScoredGrids, PerturbedGrids)
  )),
  {_LowScores, Grids} = lists:split(length(List) - GridCount, List),

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

init() ->
  ets:new(wb_generator, [duplicate_bag, public, named_table]).

init_grids(Rows, Columns, GridCount) ->
  lists:map(
    fun(_) ->
      Grid = wb_grid:create_letter_grid(Rows, Columns),
      #scored_grid{score = wb_grid:word_count(Grid, wb_generator), grid = Grid}
    end,
    lists:seq(1, GridCount)
  ).

generate_boards(Rows, Columns, GridCount, Rounds) ->
  ScoredGrids = init_grids(Rows, Columns, GridCount),
  perform_rounds(ScoredGrids, GridCount, Rounds).

print_boards(Rows, Columns, GridCount, Rounds) ->
  lists:foreach(
    fun(#scored_grid{score = Score, grid = Grid}) -> io:format("~s:~p~n", [wb_grid:compact_string(Grid), Score]) end,
    generate_boards(Rows, Columns, GridCount, Rounds)
  ).
