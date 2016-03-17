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
      erlang:send(Pid, {get, Key}),
      value
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
      erlang:send(Pid, {exists, Key}),
      true
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
      erlang:send(Pid, keys),
      []
  end.

values(Name) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      erlang:send(Pid, values),
      []
  end.

%% iterator
each(Name) ->
  case whereis(Name) of
    undefined -> false;
    Pid ->
      erlang:send(Pid, each),
      make_ref()
  end.

%%====================================================================
%% Internal functions
%%====================================================================

loop() ->
  receive
    Anything ->
      io:format("* ~p~n", [Anything]),
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
      [
        {"set", ?_assert('horrible-hash':set(Name, key, value))},
        {"exists", ?_assert('horrible-hash':exists(Name, key))},
        {"get", ?_assertEqual(value, 'horrible-hash':get(Name, key))},
        {"keys", ?_assertEqual([], 'horrible-hash':keys(Name))},
        {"values", ?_assertEqual([], 'horrible-hash':values(Name))},
        {"each", ?_assert(is_reference('horrible-hash':each(Name)))},
        {"delete", ?_assert('horrible-hash':delete(Name, key))}
      ]
    end
  }.

-endif.
