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
    start_link/1
  ]).

-define(SERVER, ?MODULE).

-record(board, {
    rows,
    columns,
    matrix
  }).

-record(state, {
    board
  }).

start_link(BoardSpec) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [BoardSpec], []).

init([{rows, columns}]) ->
  {ok, #state{board = create_board(rows, columns)}}.

create_board(Rows, Columns) ->
  Matrix = create_matrix(Rows, Columns),
  #board{rows = Rows, columns = Columns, matrix = Matrix}.

create_matrix(Rows, Columns) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),

  lists:map(
    fun(_Index) ->
      $a + rand:uniform(26)
    end, lists:seq(1, Rows * Columns)
  ).

terminate(_Reason, _State) ->
  ok.

handle_call(_Args, _From, State) ->
  {reply, ok, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

