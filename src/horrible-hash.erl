-module('horrible-hash').

%% API exports
-export([new/1, delete/1, get/2, set/3, exists/2, delete/2]).
-export([keys/1, values/1, each/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec new(Name::atom()) -> true.
new(Name) when is_atom(Name) ->
  Pid = erlang:spawn(fun loop/0),
  erlang:register(Name, Pid).

-spec delete(Name::atom()) -> boolean().
delete(Name) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      unregister(Name),
      Ref = monitor(process, Pid),
      erlang:send(Pid, quit),
      receive
        {'DOWN', Ref, process, _, normal} -> true
      end
  end.

-spec get(Name::atom(), Key::any()) -> Value::any() | false.
get(Name, Key) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      Ref = make_ref(),
      erlang:send(Pid, {get, {self(), Ref}, Key}),
      receive
        {Ref, Value} -> Value
      end
  end.

-spec set(Name::atom(), Key::any(), Value::any()) -> boolean().
set(Name, Key, Value) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      {set, Key, Value} == erlang:send(Pid, {set, Key, Value})
  end.

-spec exists(Name::atom(), Key::any()) -> boolean().
exists(Name, Key) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      Ref = make_ref(),
      erlang:send(Pid, {exists, {self(), Ref}, Key}),
      receive
        {Ref, Boolean} -> Boolean
      end
  end.

-spec delete(Name::atom(), Key::any()) -> boolean().
delete(Name, Key) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      {delete, Key} == erlang:send(Pid, {delete, Key})
  end.

-spec keys(Name::atom()) -> [Key::any()] | false.
keys(Name) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      Ref = make_ref(),
      erlang:send(Pid, {{self(), Ref}, keys}),
      receive
        {Ref, Keys} -> Keys
      end
  end.

-spec values(Name::atom()) -> [Value::any()] | false.
values(Name) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      Ref = make_ref(),
      erlang:send(Pid, {{self(), Ref}, values}),
      receive
        {Ref, Values} -> Values
      end
  end.

-spec each(Name::atom()) -> [{Key::any(), Value::any()}] | [] | false.
each(Name) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      Ref = make_ref(),
      erlang:send(Pid, {{self(), Ref}, each}),
      receive
        {Ref, Iteration} -> Iteration
      end
  end.

%%====================================================================
%% Internal functions
%%====================================================================

loop() ->
  loop(undefined).

loop(Iterator) ->
  receive
    {get, {From, Ref}, Key} ->
      Value = erlang:get(Key),
      erlang:send(From, {Ref, Value}),
      loop(Iterator);
    {set, Key, Value} ->
      erlang:put(Key, Value),
      loop(Iterator);
    {exists, {From, Ref}, Key} ->
      erlang:send(From, {Ref, undefined /= erlang:get(Key)}),
      loop(Iterator);
    {delete, Key} ->
      erlang:erase(Key),
      loop(Iterator);
    {{From, Ref}, keys} ->
      Keys = [Key || {Key, _} <- erlang:get()],
      erlang:send(From, {Ref, Keys}),
      loop(Iterator);
    {{From, Ref}, values} ->
      Values = [Value || {_, Value} <- erlang:get()],
      erlang:send(From, {Ref, Values}),
      loop(Iterator);
    {{From, Ref}, each} ->
      KVs = erlang:get(),
      NewIterator = maybe_start_iterator(Iterator, KVs),
      case is_pid(NewIterator) andalso is_process_alive(NewIterator) of
        true ->
          erlang:send(NewIterator, {{From, Ref}, each}),
          loop(NewIterator);
        false ->
          erlang:send(From, {Ref, []}),
          loop()
        end;
    quit ->
      ok
  after
    infinity -> end_of_universe
  end.

maybe_start_iterator(undefined, KVs) ->
  spawn(fun() -> iterator(KVs) end);
maybe_start_iterator(Iterator, _) ->
  Iterator.

iterator([]) ->
  ok;
iterator([Iteration | Rest]) ->
  receive
    {{From, Ref}, each} ->
      erlang:send(From, {Ref, [Iteration]}),
      iterator(Rest)
  after
    infinity -> end_of_universe
  end.
