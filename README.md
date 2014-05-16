# {Key, Value} cache
### TODO
1. ~~Add option for changing the value of an existing key.~~
2. ~~Abstract messages away with the help of functions dealing with receiving and sending them:~~ **% Done using gen_server**
      * ~~start_link/0 -> () -> avoid user do this `Cache = spawn(cache, process, [[]]).`~~
      * ~~store/2 -> (NewKey, NewValue)~~
      * ~~update/2 -> (ExistingKey, NewValue) (first point must be done first)~~
      * ~~lookup/1 -> (ExistingKey)~~
      * ~~list/0 -> ()~~
3. Add Unit Tests

### How to tests cache.erl module (manually)
```
1> c(cache).
{ok,cache}
2> cache:start_link().
{ok,<0.41.0>}
3> cache:store(foo, 123).
{ok,stored,{foo,123}}
4> cache:store(bar, 456).
{ok,stored,{bar,456}}
5> cache:list().
Cache content: [{bar,456},{foo,123}]
ok
6> cache:update(bar, 789).
{ok,updated,{bar,789}}
7> cache:list().
Cache content: [{bar,789},{foo,123}]
ok
8> cache:lookup(foo).
{ok,123}
9> cache:lookup(foobar).
{error,{key_not_found,foobar}}
```
