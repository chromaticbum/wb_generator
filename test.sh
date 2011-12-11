rebar compile
erl -pa ../wb_generator/ebin -eval "application:start(wb_generator), wb_generator_sup:do_stuff()."
