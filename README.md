# {Key, Value} cache
### TODO
1. Add option for changing the value of an existing key.
2. Abstract messages away with the help of functions dealing with receiving and sending them:
      * start/0 -> () -> avoid user do this `Cache = spawn(cache, process, [[]]).`
      * store/2 -> (Pid, {NewKey, NewValue})
      * update/2 -> (Pid, {ExistingKey, NewValue}) (first point must be done first)
      * lookup/2 -> (Pid, {ExistingKey})
      * list/1 -> (Pid)
3. Add Unit Tests

### How to tests cache.erl module (manually)
```
39> c(cache).
{ok,cache}
40> f().
ok
41> Cache = spawn(cache, process, [[]]).
<0.110.0>
42> Cache ! {self(), store, {foo, 123}}.
{<0.33.0>,store,{foo,123}}
43> flush().
Shell got {stored,{foo,123}}
ok
44> Cache ! {self(), store, {foo, 123}}.
{<0.33.0>,store,{foo,123}}
45> flush().
Shell got already_exist
Shell got {foo,123}
ok
46> Cache ! {self(), lookup, {foo}}.
{<0.33.0>,lookup,{foo}}
47> flush().
Shell got 123
ok
48> Cache ! {self(), whatever}.
{<0.33.0>,whatever}
49> flush().
Shell got unknown_request
ok
50> Cache ! {self(), list}.
{<0.33.0>,list}
51> flush().
Shell got [{foo,123}]
ok
```
