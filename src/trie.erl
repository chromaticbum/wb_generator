-module(trie).

% API exports
-export([
    init/0,
    get_node/2,
    load_file/1,
    is_word/1,
    is_terminal/1,
    add_tree_word/2
  ]).

init() ->
  ets:new(trie, [set, public, named_table, {read_concurrency, true}]).

is_word(Word) ->
  is_word(Word, 0).

is_word([], Id) ->
  is_terminal(Id);
is_word([Letter | Word], Id) ->
  case get_node(Id, Letter) of
    {{Id, Letter}, Id2} ->
      is_word(Word, Id2);
    not_found -> false
  end.

load_file(FileName) ->
  case file:open(FileName, [read, read_ahead]) of
    {ok, FileIO} ->
      error_logger:info_msg("Beginning to read dictionary: ~s~n", [FileName]),
      read_word(FileIO, 1),
      error_logger:info_msg("Done reading dictionary~n");
    {error, Reason} ->
      error_logger:error_msg("Could not open dictionary(~s): ~p~n", [FileName, Reason])
  end.

read_word(FileIO, Count) ->
  case file:read_line(FileIO) of
    {ok, Data} ->
      Count2 = add_word(Data, Count),
      read_word(FileIO, Count2);
    eof ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

add_word(Data, Count) ->
  Word = string:substr(Data, 1, length(Data) - 1),
  add_tree_word(Word, Count).

add_tree_word(Word, Count) ->
  {Id, Count2} = lists:foldl(
    fun(Letter, {Id2, Count3}) ->
      add_node(Id2, Letter, Count3)
    end, {0, Count}, Word
  ),

  ets:insert(trie, {{Id, terminal}, true}),
  Count2.

add_node(Id, Letter, Count) ->
  case get_node(Id, Letter) of
    {{Id, Letter}, Id2} ->
      {Id2, Count};
    not_found ->
      ets:insert(trie, {{Id, Letter}, Count}),
      {Count, Count + 1}
  end.

get_node(Id, Letter) ->
  case ets:lookup(trie, {Id, Letter}) of
    [Node] -> Node;
    [] -> not_found
  end.

is_terminal(Id) ->
  case ets:lookup(trie, {Id, terminal}) of
    [{{Id, terminal}, true}] -> true;
    _ -> false
  end.

