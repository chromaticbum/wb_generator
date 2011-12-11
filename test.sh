rebar compile
erl -pa ../wb_generator/ebin -eval "application:start(wb_generator), B = wb_board:create_board(5,5), wb_board:word_count(B)."
