-module(wb_generator_sup).
-behavior(supervisor).

-export([
    start_link/0,
    start_child/1
  ]).

-export([
    init/1
  ]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(BoardSpec) ->
  supervisor:start_child(?SERVER, [BoardSpec]).

init([]) ->
  GeneratorServer = {wb_generator_server, {wb_generator_server, start_link, []},
    temporary, 2000, worker, [wb_generator_server]},
  Children = [GeneratorServer],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
