-module(wb_generator).

-include("wb_grid.hrl").

-export([
    generate_boards/1,
    print_boards/1
  ]).

-define(DEFAULT_SIZE, 5).
-define(DEFAULT_GRID_COUNT, 5).
-define(DEFAULT_ROUNDS, 100).
-define(DEFAULT_TARGET_SCORE, 100).
-define(DEFAULT_REPEATS, 2).

perform_rounds(ScoredGrids, _GridCount, 0) ->
  ScoredGrids;
perform_rounds(ScoredGrids, GridCount, Count) ->
  PerturbedGrids = lists:map(
    fun(#scored_grid{grid = Grid}) ->
      Grid2 = wb_perturb:perturb_grid(Grid),
      #scored_grid{score = wb_grid:word_count(Grid2), grid = Grid2}
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

init_grids(Rows, Columns, GridCount) ->
  lists:map(
    fun(_) ->
      Grid = wb_grid:create_letter_grid(Rows, Columns),
      #scored_grid{score = wb_grid:word_count(Grid), grid = Grid}
    end,
    lists:seq(1, GridCount)
  ).

generate_boards(Config) ->
  Rows = proplists:get_value(rows, Config, ?DEFAULT_SIZE),
  Columns = proplists:get_value(columns, Config, ?DEFAULT_SIZE),
  GridCount = proplists:get_value(grid_count, Config, ?DEFAULT_GRID_COUNT),
  Rounds = proplists:get_value(rounds, Config, ?DEFAULT_ROUNDS),
  TargetScore = proplists:get_value(target_score, Config, ?DEFAULT_TARGET_SCORE),
  Repeats = proplists:get_value(repeats, Config, ?DEFAULT_REPEATS),

  generate_boards(Rows, Columns, GridCount, Rounds, TargetScore, Repeats).

generate_boards(_Rows, _Columns, _GridCount, _Rounds, _TargetScore, 0) ->
  failed;
generate_boards(Rows, Columns, GridCount, Rounds, TargetScore, Repeat) ->
  ScoredGrids = init_grids(Rows, Columns, GridCount),
  ScoredGrids2 = lists:filter(
    fun(#scored_grid{score = Score}) ->
      Score >= TargetScore
    end,
    perform_rounds(ScoredGrids, length(ScoredGrids), Rounds)
  ),

  case length(ScoredGrids2) > 0 of
    true -> ScoredGrids2;
    false -> generate_boards(Rows, Columns, GridCount, Rounds, TargetScore, Repeat - 1)
  end.

print_boards(Config) ->
  case generate_boards(Config) of
    failed -> io:format("Could not reach the target score.~n");
    Boards ->
      io:format("Found the following boards matching your criteria.~n"),
      lists:foreach(
        fun(#scored_grid{score = Score, grid = Grid}) -> io:format("~s:~p~n", [wb_grid:compact_string(Grid), Score]) end,
        Boards
      )
  end.
