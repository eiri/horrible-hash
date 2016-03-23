%% @doc horrible-hash is implementation of (perl) hash as an erlang process
%% @version 0.1
%% @reference <a href="https://github.com/eiri/horrible-hash">https://github.com/eiri/horrible-hash</a>
%% @author Eric Avdey <eiri@eiri.ca>
%% @copyright 2016 Eric Avdey

-module('horrible-hash').

%% API exports
-export([new/1, delete/1, get/2, set/3, exists/2, delete/2]).
-export([keys/1, values/1, each/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Creates a new hash with a given name.
%% Internally creates a registred process with hash name
%% that uses process dictionary to hold provided values.
-spec new(Name::atom()) -> true.
new(Name) when is_atom(Name) ->
  Pid = erlang:spawn(fun loop/0),
  erlang:register(Name, Pid).

%% @doc Delete existing hash.
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

%% @doc Gets a value for a given key.
%% If no value exists returns atom undefined.
-spec get(Name::atom(), Key::any()) -> Value::any() | undefined.
get(Name, Key) ->
  case whereis(Name) of
    undefined -> undefined;
    Pid ->
      Ref = make_ref(),
      erlang:send(Pid, {get, {self(), Ref}, Key}),
      receive
        {Ref, Value} -> Value
      end
  end.

%% @doc Sets a value for a given key.
%% If the key already has a value, overrides it.
-spec set(Name::atom(), Key::any(), Value::any()) -> boolean().
set(Name, Key, Value) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      {set, Key, Value} == erlang:send(Pid, {set, Key, Value})
  end.

%% @doc Retrns true is a given key associated with a value in a hash.
%% Otherwise returns false.
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

%% @doc Removes a given key from a hash.
%% Returns true even if the hash doesn't have the key.
-spec delete(Name::atom(), Key::any()) -> boolean().
delete(Name, Key) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      {delete, Key} == erlang:send(Pid, {delete, Key})
  end.

%% @doc Returns a list of all the keys allocated in a hash.
%% Order not guaranteed.
-spec keys(Name::atom()) -> [Key::any()].
keys(Name) ->
  case whereis(Name) of
    undefined -> [];
    Pid ->
      Ref = make_ref(),
      erlang:send(Pid, {{self(), Ref}, keys}),
      receive
        {Ref, Keys} -> Keys
      end
  end.

%% @doc Returns a list of all the values stored in a hash.
%% Order not guaranteed.
-spec values(Name::atom()) -> [Value::any()].
values(Name) ->
  case whereis(Name) of
    undefined -> [];
    Pid ->
      Ref = make_ref(),
      erlang:send(Pid, {{self(), Ref}, values}),
      receive
        {Ref, Values} -> Values
      end
  end.

%% @doc Returns iterator for a hash.
%% On each call returns a tuple of {Key, Value} until end is reached.
%% At which moment returns an empty list. Repeated call will initiate
%% a new iterator.
-spec each(Name::atom()) -> [{Key::any(), Value::any()}].
each(Name) ->
  case whereis(Name) of
    undefined -> [];
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
