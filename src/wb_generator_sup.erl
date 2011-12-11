-module(wb_generator_sup).
-behavior(supervisor).

-export([
    start_link/0,
    start_child/0
  ]).

-export([
    init/1,
    do_stuff/0
  ]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
  supervisor:start_child(?SERVER, []).

init([]) ->
  GeneratorServer = {wb_generator_server, {wb_generator_server, start_link, []},
    temporary, 2000, worker, [wb_generator_server]},
  Children = [GeneratorServer],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

do_stuff() ->
  {ok, Pid} = start_child(),

  wb_generator_server:generate_board({5, 5}).
