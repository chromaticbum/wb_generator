-module(wb_tree).
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
    load_file/1,
    add_node/2,
    get_node/1
  ]).

-record(state, {
    node_count
}).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

node_tuple() ->
  list_to_tuple(lists:map(
    fun(_) ->
      -1
    end, lists:seq(1, 26)
  )).

init([]) ->
  ets:new(wb_tree, [set, public, named_table, {read_concurrency, true}]),
  ets:insert(wb_tree, {0, false, node_tuple()}),
  {ok, #state{node_count = 1}}.

terminate(_Reason, _State) ->
  ok.

load_file(FileName) ->
  case file:open(FileName, [read, read_ahead]) of
    {ok, FileIO} ->
      error_logger:info_msg("Beginning to read dictionary: ~s~n", [FileName]),
      read_word(FileIO),
      error_logger:info_msg("Done reading dictionary~n");
    {error, Reason} ->
      error_logger:error_msg("Could not open dictionary(~s): ~p~n", [FileName, Reason])
  end.

read_word(FileIO) ->
  case file:read_line(FileIO) of
    {ok, Data} ->
      add_word(Data),
      read_word(FileIO);
    eof ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

add_word(Data) ->
  Word = string:substr(Data, 1, length(Data) - 1),
  add_tree_word(Word).

add_tree_word(Word) ->
  Id = lists:foldr(
    fun(Letter, Node) ->
      add_node(Node, Letter)
    end, 0, Word
  ),

  {Id, _Terminal, Ids} = get_node(Id),
  ets:insert(wb_tree, {Id, true, Ids}).

add_node(Node, Letter) ->
  gen_server:call(?SERVER, {add_node, Node, Letter}).

get_node(Id) ->
  case ets:lookup(wb_tree, Id) of
    [Node] -> Node;
    [] -> not_found
  end.

handle_call({add_node, Id, Letter}, _From, #state{node_count = Count} = State) ->
  {Id, Terminal, Ids} = get_node(Id),
  Index = Letter - $a + 1,

  case element(Index, Ids) of
    Id2 when Id2 >= 0 ->
      {reply, Id2, State};
    _ ->
      ets:insert(wb_tree, {Count, false, node_tuple()}),
      ets:insert(wb_tree, {Id, Terminal, setelement(Index, Ids, Count)}),
      {reply, Count, State#state{node_count = Count + 1}}
  end.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
