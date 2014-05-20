# {Key, Value} cache
### TODO
1. ~~Add option for changing the value of an existing key.~~
2. ~~Abstract messages away with the help of functions dealing with receiving and sending them:~~ **% Done using gen_server**
      * ~~start_link/0 -> () -> avoid user do this `Cache = spawn(cache, process, [[]]).`~~
      * ~~store/2 -> (NewKey, NewValue)~~
      * ~~update/2 -> (ExistingKey, NewValue) (first point must be done first)~~
      * ~~lookup/1 -> (ExistingKey)~~
      * ~~list/0 -> ()~~
3. ~~Add Unit Tests~~
4. ~~Add time management~~

### How to tests cache.erl module (manually)
```
1> c(cache).
{ok,cache}
2> cache:start_link().
{ok,{interval,#Ref<0.0.0.2067>}}
3> cache:store(foo, 123).
{ok,stored,{foo,123,1400610207}}
4> cache:store(bar, 456).
{ok,stored,{bar,456,1400610217}}
5> cache:list().
{ok,[{bar,456,1400610217},{foo,123,1400610207}]}
6> cache:update(bar, 789).
{ok,updated,{bar,789}}
7> cache:list().
{ok,[{bar,789,1400610217},{foo,123,1400610207}]}
8> cache:lookup(foo).
{ok,123,1400610207}
9> cache:lookup(foobar).
{error,{key_not_found,foobar}}
10> cache:lookup(foo).   
{error,{key_not_found,foo}} %% Cache time expired (comment added manually)
```
