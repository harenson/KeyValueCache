-module(cache).
-export([process/1]).

process(CacheList) ->
    receive
        {Pid, store, {Key, Value}} ->
            case lists:keyfind(Key, 1, CacheList) of
                false ->
                    NewCacheList = [{Key, Value} | CacheList],
                    Pid ! {stored, {Key, Value}},
                    process(NewCacheList);
                {ExistingKey, ExistingValue} ->
                    Pid ! already_exist,
                    Pid ! {ExistingKey, ExistingValue},
                    process(CacheList)
            end;
        {Pid, lookup, {Key}} ->
            Pid ! case lists:keyfind(Key, 1, CacheList) of
                {Key, Value} -> Value;
                false -> not_found
            end,
            process(CacheList);
        {Pid, list} ->
            Pid ! CacheList,
            process(CacheList);
        {Pid, _} ->
            Pid ! unknown_request,
            process(CacheList)
    end.
