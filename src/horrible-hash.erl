-module('horrible-hash').

%% API exports
-export([new/1, delete/1, get/2, set/3, exists/2, delete/2]).
-export([keys/1, values/1, each/1]).

%%====================================================================
%% API functions
%%====================================================================

new(Name) when is_atom(Name) ->
  Pid = erlang:spawn(fun loop/0),
  erlang:register(Name, Pid).

delete(Name) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      erlang:exit(Pid, kill),
      unregister(Name)
  end.

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

set(Name, Key, Value) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      {set, Key, Value} == erlang:send(Pid, {set, Key, Value})
  end.

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

delete(Name, Key) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      {delete, Key} == erlang:send(Pid, {delete, Key})
  end.

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
    Unknown ->
      io:format("* ~p~n", [Unknown]),
      loop(Iterator)
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

%%====================================================================
%% EUnit
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

public_api_test_() ->
  {setup,
    fun() ->
      true = 'horrible-hash':new('$hash'),
      '$hash'
    end,
    fun(Name) ->
      true = 'horrible-hash':delete(Name)
    end,
    fun(Name) ->
      {inorder, [
        {"set", ?_assert('horrible-hash':set(Name, key, value))},
        {"exists", ?_assert('horrible-hash':exists(Name, key))},
        {"get", ?_assertEqual(value, 'horrible-hash':get(Name, key))},
        {"keys", ?_assertEqual([key], 'horrible-hash':keys(Name))},
        {"values", ?_assertEqual([value], 'horrible-hash':values(Name))},
        {"delete", ?_assert('horrible-hash':delete(Name, key))},
        {"exists", ?_assertNot('horrible-hash':exists(Name, key))}
      ]}
    end
  }.

each_test_() ->
  {setup,
    fun() ->
      true = 'horrible-hash':new('$hash'),
      lists:foreach(fun(I) ->
        'horrible-hash':set('$hash', I, I + 100)
      end, lists:seq(1, 5)),
      '$hash'
    end,
    fun(Name) ->
      true = 'horrible-hash':delete(Name)
    end,
    fun(Name) ->
      {inorder, [
        ?_assertEqual([{5, 105}], 'horrible-hash':each(Name)),
        ?_assertEqual([{4, 104}], 'horrible-hash':each(Name)),
        ?_assertEqual([{3, 103}], 'horrible-hash':each(Name)),
        ?_assertEqual([{2, 102}], 'horrible-hash':each(Name)),
        ?_assertEqual([{1, 101}], 'horrible-hash':each(Name)),
        ?_assertEqual([], 'horrible-hash':each(Name))
      ]}
    end
  }.

-endif.
