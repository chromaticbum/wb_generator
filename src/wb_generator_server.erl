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
    generate_board/1,
    perturb_boards/1
  ]).

-define(SERVER, ?MODULE).

-define(BOARD_COUNT, 5).
-define(PERTURBATION_COUNT, 5).

-record(state, {
    info,
    board_spec
  }).

boards(#state{info = Info}) ->
  [{boards, Boards}] = ets:lookup(Info, boards),
  Boards.

start_link(BoardSpec) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [BoardSpec], []).

init([BoardSpec]) ->
  Info = ets:new(info, [set]),
  {ok, #state{info = Info, board_spec = BoardSpec}}.

terminate(_Reason, _State) ->
  ok.

handle_call(perturb_boards, _From, State) ->
  Boards = boards(State),

  PerturbedBoards = lists:map(
    fun(Board) ->
      wb_perturb:perturb_board(Board)
    end, Boards
  ),

  error_logger:info_msg("Boards: ~p~n", [Boards]),
  error_logger:info_msg("PerturbedBoards: ~p~n", [PerturbedBoards]),

  {reply, ok, State};
handle_call({init_boards, BoardCount}, _From, #state{board_spec = BoardSpec, info = Info} = State) ->
  Boards = lists:map(
    fun(_) -> wb_board:create_board(BoardSpec) end,
    lists:seq(1, BoardCount)
  ),

  ets:insert(Info, {boards, Boards}),
  {reply, ok, State};

handle_call(_Args, _From, State) ->
  {reply, ok, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

generate_board(Pid) ->
  init_boards(Pid, ?BOARD_COUNT),
  perturb_boards(Pid),
  ok.

init_boards(Pid, BoardCount) ->
  gen_server:call(Pid, {init_boards, BoardCount}).

perturb_boards(Pid) ->
  gen_server:call(Pid, perturb_boards).
