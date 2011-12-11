-module(wb_generator_server).
-behavior(gen_server).

% Behavior exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).

% API exports
-export([
    start_link/1,
    generate_grid/1,
    perturb_grids/1
  ]).

-define(GRID_COUNT, 5).
-define(PERTURBATION_COUNT, 5).

-record(state, {
    info,
    grid_spec
  }).

-record(scored_grid, {
    score,
    grid
  }).

scored_grids(#state{info = Info}) ->
  [{scored_grids, ScoredGrids}] = ets:lookup(Info, scored_grids),
  ScoredGrids.

start_link(GridSpec) ->
  gen_server:start_link(?MODULE, [GridSpec], []).

init([GridSpec]) ->
  Info = ets:new(info, [set]),
  {ok, #state{info = Info, grid_spec = GridSpec}}.

terminate(_Reason, _State) ->
  ok.

handle_call(perturb_grids, _From, State) ->
  ScoredGrids = scored_grids(State),

  PerturbedGrids = lists:map(
    fun(#scored_grid{grid = Grid}) ->
      Grid2 = wb_perturb:perturb_grid(Grid),
      #scored_grid{score = wb_grid:word_count(Grid2), grid = Grid2}
    end, ScoredGrids
  ),

  error_logger:info_msg("Grids: ~p~n", [ScoredGrids]),
  error_logger:info_msg("PerturbedGrids: ~p~n", [PerturbedGrids]),

  {reply, ok, State};
handle_call({init_grids, GridCount}, _From, #state{grid_spec = {Rows, Columns}, info = Info} = State) ->
  ScoredGrids = lists:map(
    fun(_) ->
      Grid = wb_grid:create_letter_grid(Rows, Columns),
      #scored_grid{score = wb_grid:word_count(Grid), grid = Grid}
    end,
    lists:seq(1, GridCount)
  ),

  ets:insert(Info, {scored_grids, ScoredGrids}),
  {reply, ok, State};

handle_call(_Args, _From, State) ->
  {reply, ok, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

generate_grid(Pid) ->
  init_grids(Pid, ?GRID_COUNT),
  perturb_grids(Pid),
  ok.

init_grids(Pid, GridCount) ->
  gen_server:call(Pid, {init_grids, GridCount}).

perturb_grids(Pid) ->
  gen_server:call(Pid, perturb_grids).
