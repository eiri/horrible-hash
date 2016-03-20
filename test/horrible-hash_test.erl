-module('horrible-hash_test').

-include_lib("eunit/include/eunit.hrl").

create_delete_test_() ->
  [
    {"create a hash",
    ?_assert('horrible-hash':new('$hash'))},
    {"delete the hash",
    ?_assert('horrible-hash':delete('$hash'))},
    {"try to delete the deleted hash",
    ?_assertNot('horrible-hash':delete('$hash'))},
    {"try to delete a non-existing hash",
    ?_assertNot('horrible-hash':delete('$none'))}
  ].

get_set_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    fun(Name) ->
      {inorder, [
        {"set a value",
        ?_assert('horrible-hash':set(Name, key, value))},
        {"set a value to a none-existing hash",
        ?_assertNot('horrible-hash':set('$none', key, value))},
        {"get an existing value",
        ?_assertEqual(value, 'horrible-hash':get(Name, key))},
        {"try to get a non-existing value",
        ?_assertEqual(undefined, 'horrible-hash':get(Name, ball))},
        {"try to get a value from a none-existing hash",
        ?_assertNot('horrible-hash':get('$none', key))}
      ]}
    end
  }.

exists_delete_test_() ->
  {setup,
    fun() ->
      Name = setup(),
      'horrible-hash':set(Name, a, 1),
      'horrible-hash':set(Name, b, 2),
      Name
    end,
    fun teardown/1,
    fun(Name) ->
      {inorder, [
        {"check an existance of an existing key",
        ?_assert('horrible-hash':exists(Name, a))},
        {"check an existance of a non-existing key",
        ?_assertNot('horrible-hash':exists(Name, z))},
        {"check an existance of a second existing key",
        ?_assert('horrible-hash':exists(Name, b))},
        {"delete one of the keys",
        ?_assert('horrible-hash':delete(Name, a))},
        {"cornfirm that deleted key is now non-exists",
        ?_assertNot('horrible-hash':exists(Name, a))},
        {"confirm that the second key still exists",
        ?_assert('horrible-hash':exists(Name, b))},
        {"delete the second key",
        ?_assert('horrible-hash':delete(Name, b))},
        {"cofirm that deletion is an indepotent operation",
        ?_assert('horrible-hash':delete(Name, a))},
        {"check an existance of a key in a none-existing hash",
        ?_assertNot('horrible-hash':exists('$none', a))},
        {"check a deletion of a key in a none-existing hash",
        ?_assertNot('horrible-hash':delete('$none', a))}
      ]}
    end
  }.

keys_test_() ->
  {setup,
    fun setup_filled/0,
    fun teardown/1,
    fun({Name, Keys, _}) ->
      {inorder, [
        {"pull keys from a hash",
        ?_assertEqual(Keys, 'horrible-hash':keys(Name))},
        {"get a proper keys' list after deletion of one of the keys",
        ?_test(begin
          true = 'horrible-hash':delete(Name, 5),
          ?assertEqual(tl(Keys), 'horrible-hash':keys(Name))
        end)},
        {"try to pull keys from a non-existing hash",
        ?_assertNot('horrible-hash':keys('$none'))},
        {"get an empty list from a hash after deletion of all the keys",
        ?_test(begin
          ['horrible-hash':delete(Name, K) || K <- lists:seq(1, 4)],
          ?assertEqual([], 'horrible-hash':keys(Name))
        end)}
      ]}
    end
  }.

values_test_() ->
  {setup,
    fun setup_filled/0,
    fun teardown/1,
    fun({Name, _, Values}) ->
      {inorder, [
        {"pull list of values from a hash",
        ?_assertEqual(Values, 'horrible-hash':values(Name))},
        {"get a proper values' list after deletion of one of the keys",
        ?_test(begin
          true = 'horrible-hash':delete(Name, 5),
          ?assertEqual(tl(Values), 'horrible-hash':values(Name))
        end)},
        {"try to pull the values from a non-existing hash",
        ?_assertNot('horrible-hash':values('$none'))},
        {"get an empty list from a hash after deletion of all the keys",
        ?_test(begin
          ['horrible-hash':delete(Name, K) || K <- lists:seq(1, 4)],
          ?assertEqual([], 'horrible-hash':values(Name))
        end)}
      ]}
    end
  }.

each_test_() ->
  {setup,
    fun setup_filled/0,
    fun teardown/1,
    fun({Name, _, _}) ->
      {inorder, [
        each_generator(Name, 5),
        {"last each call returns empty list",
        ?_assertEqual([], 'horrible-hash':each(Name))},
        {"try to call each on a non-existing hash",
        ?_assertNot('horrible-hash':each('$none'))}
      ]}
    end
  }.

%%====================================================================
%% Fixtures
%%====================================================================

setup() ->
  Name = '$hash',
  true = 'horrible-hash':new(Name),
  Name.

setup_filled() ->
  Name = setup(),
  lists:foreach(fun(I) ->
    'horrible-hash':set(Name, I, I + 100)
  end, lists:seq(1, 5)),
  Keys = lists:reverse(lists:seq(1, 5)),
  Values = lists:reverse(lists:seq(101, 105)),
  {Name, Keys, Values}.

teardown({Name, _, _}) ->
  teardown(Name);
teardown(Name) ->
  true = 'horrible-hash':delete(Name).

each_generator(Name, N) ->
  {generator,
  fun() ->
    if N >= 1 ->
      [make_each_callback(Name, N) | each_generator(Name, N - 1)];
    true ->
      []
    end
  end}.

make_each_callback(Name, N) ->
  {"each call returns key " ++ integer_to_list(N),
  ?_assertEqual([{N, N + 100}], 'horrible-hash':each(Name))}.
