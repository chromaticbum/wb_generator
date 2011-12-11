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
    start_link/0,
    generate_board/1
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

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

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

generate_board({Rows, Columns}) ->
  Board = create_board(Rows, Columns),
  io:format("Board: ~p~n", [Board]).

create_board(Rows, Columns) ->
  Matrix = create_matrix(Rows, Columns),
  #board{rows = Rows, columns = Columns, matrix = Matrix}.

create_matrix(Rows, Columns) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),

  list_to_tuple(lists:map(
    fun(_Index) ->
      $a + random:uniform(26) - 1
    end, lists:seq(1, Rows * Columns)
  )).
