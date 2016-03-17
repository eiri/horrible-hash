-module('horrible-hash').

%% API exports
-export([new/1, delete/1, get/2, set/3, exists/2, delete/2]).
-export([keys/1, values/1, each/1]).

%%====================================================================
%% API functions
%%====================================================================

new(Name) when is_atom(Name) ->
  Pid = erlang:spawn_link(fun loop/0),
  erlang:register(Name, Pid).

delete(Name) ->
  case whereis(Name) of
    undefined -> false;
    Pid -> erlang:exit(Pid, kill)
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
        {Ref, {Key, Value}} -> {Key, Value}
      end
  end.

%%====================================================================
%% Internal functions
%%====================================================================

loop() ->
  receive
    {get, {From, Ref}, Key} ->
      Value = erlang:get(Key),
      erlang:send(From, {Ref, Value}),
      loop();
    {set, Key, Value} ->
      erlang:put(Key, Value),
      loop();
    {exists, {From, Ref}, Key} ->
      erlang:send(From, {Ref, undefined /= erlang:get(Key)}),
      loop();
    {delete, Key} ->
      erlang:erase(Key),
      loop();
    {{From, Ref}, keys} ->
      Keys = [Key || {Key, _} <- erlang:get()],
      erlang:send(From, {Ref, Keys}),
      loop();
    {{From, Ref}, values} ->
      Values = [Value || {_, Value} <- erlang:get()],
      erlang:send(From, {Ref, Values}),
      loop();
    {{From, Ref}, each} ->
      %% placeholder
      erlang:send(From, {Ref, hd(erlang:get())}),
      loop();
    Unknown ->
      io:format("* ~p~n", [Unknown]),
      loop()
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
        {"each", ?_assertEqual({key, value}, 'horrible-hash':each(Name))},
        {"delete", ?_assert('horrible-hash':delete(Name, key))}
      ]}
    end
  }.

-endif.
