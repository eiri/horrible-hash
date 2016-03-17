-module('horrible-hash').

%% API exports
-export([new/1, delete/1, get/2, set/3, exists/2, delete/2]).
-export([keys/1, values/1, each/1]).

%%====================================================================
%% API functions
%%====================================================================

new(_Name) ->
  ok.

delete(_Name) ->
  ok.

get(_Name, _Key) ->
  value.

set(_Name, _Key, _Value) ->
  ok.

exists(_Name, _Key) ->
  true.

delete(_Name, _Key) ->
  ok.

keys(_Name) ->
  [].

values(_Name) ->
  [].

%% iterator
each(_Name) ->
  fun() -> ok end.

%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% EUnit
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

public_api_test_() ->
  [
    {"new", ?_assertEqual(ok, 'horrible-hash':new('$hash'))},
    {"delete", ?_assertEqual(ok, 'horrible-hash':delete('$hash'))},
    {"get", ?_assertEqual(value, 'horrible-hash':get('$hash', key))},
    {"set", ?_assertEqual(ok, 'horrible-hash':set('$hash', key, value))},
    {"exists", ?_assert('horrible-hash':exists('$hash', key))},
    {"delete", ?_assertEqual(ok, 'horrible-hash':delete('$hash', key))},
    {"keys", ?_assertEqual([], 'horrible-hash':keys('$hash'))},
    {"values", ?_assertEqual([], 'horrible-hash':values('$hash'))},
    {"each", ?_assert(is_function('horrible-hash':each('$hash')))}
  ].

-endif.
