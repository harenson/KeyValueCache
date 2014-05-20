-module(cache_tests).
-include_lib("eunit/include/eunit.hrl").

cache_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun test_cache/1}.

start() ->
    {ok, Pid} = cache:start_link(),
    Pid.

stop(_) ->
    ok = cache:stop().

test_cache(_) ->
    [?_assertMatch({ok, stored, {foo, 123, _Expires}}, cache:store(foo, 123)),
    ?_assertMatch({error, key_already_exist, {foo, 123, _Expires}},
                  cache:store(foo, 456)),
    ?_assertEqual({ok, updated, {foo, 456}}, cache:update(foo, 456)),
    ?_assertEqual({error, {key_not_found, bar}}, cache:lookup(bar)),
    ?_assertMatch({ok, 456, _Expires}, cache:lookup(foo)),
    ?_assertNotMatch([{foo,456,_Expires}], cache:list()),
    ?_assertMatch({ok, [{foo,456,_Expires}]}, cache:list())].
