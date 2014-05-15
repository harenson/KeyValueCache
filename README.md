# {Key, Value} cache
### TODO
1. Add option for changing the value of an existing key.
2. ~~Abstract messages away with the help of functions dealing with receiving and sending them:~~ ** % Done using gen_server**
      * start/0 -> () -> avoid user do this `Cache = spawn(cache, process, [[]]).`
      * store/2 -> (Pid, {NewKey, NewValue})
      * update/2 -> (Pid, {ExistingKey, NewValue}) (first point must be done first)
      * lookup/2 -> (Pid, {ExistingKey})
      * list/1 -> (Pid)
3. Add Unit Tests

### How to tests cache.erl module (manually)
```
1> c(cache).
{ok,cache}
2> cache:start_link().
{ok,<0.41.0>}
3> gen_server:call(cache, {store, {foo, 123}}).
{stored,{foo,123}}
4> gen_server:call(cache, {store, {bar, 456}}).
{stored,{bar,456}}
5> gen_server:call(cache, {store, {baz, 789}}).
{stored,{baz,789}}
6> gen_server:call(cache, {store, {baz, 789}}).
{already_exist,{baz,789}}
7> gen_server:call(cache, {lookup, {bar}}).
{ok,456}
8> gen_server:call(cache, {lookup, {foo}}).
{ok,123}
9> gen_server:call(cache, {lookup, {baz}}).
{ok,789}
10> gen_server:call(cache, unknown_request).
{error,{unknown_request,unknown_request}}
11> gen_server:call(cache, my_request).
{error,{unknown_request,my_request}}
12> gen_server:cast(cache, list).
Cache content: [{baz,789},{bar,456},{foo,123}]
ok
```
