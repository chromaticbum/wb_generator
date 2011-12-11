-module(wb_generator_app).
-behavior(application).

-export([
    start/2,
    stop/1
  ]).

start(_Type, _Args) ->
  wb_tree:start_link(),
  spawn(wb_tree, load_file, [code:priv_dir(wb_generator) ++ "/dictionary"]),

  wb_generator_sup:start_link().

stop(_S) ->
  ok.

