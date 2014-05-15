-module(cache).
-behavior(gen_server).
-export([start_link/0]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(CacheList) ->
    {ok, CacheList}.

handle_call({store, {Key, Value}}, _From, CacheList) ->
    case lists:keyfind(Key, 1, CacheList) of
        false ->
            NewCacheList = [{Key, Value} | CacheList],
            Reply = {stored, {Key, Value}},
            {reply, Reply, NewCacheList};
        {ExistingKey, ExistingValue} ->
            Reply = {already_exist, {ExistingKey, ExistingValue}},
            {reply, Reply, CacheList}
    end;

handle_call({lookup, {Key}}, _From, CacheList) ->
    Reply = case lists:keyfind(Key, 1, CacheList) of
        {Key, Value} -> Value;
        false -> not_found
    end,
    {reply, {ok, Reply}, CacheList};

handle_call(_Unknown, _From, CacheList) ->
    Reply = {unknown_request, _Unknown},
    {reply, {error, Reply}, CacheList}.

handle_cast(list, CacheList) ->
    io:format("Cache content: ~w~n", [CacheList]),
    {noreply, CacheList}.

handle_info(_Request, CacheList) ->
    {noreply, CacheList}.

terminate(_Reason, _CacheList) ->
    {ok, _Reason}.

code_change(_OldVsn, CacheList, _Extra) ->
    {ok, CacheList}.
