rebar compile
# erl -pa ../wb_generator/ebin -eval "application:start(wb_generator), B = wb_grid:create_letter_grid(5,5), wb_grid:word_count(B)."
erl -pa ../wb_generator/ebin -eval "application:start(wb_generator)."
