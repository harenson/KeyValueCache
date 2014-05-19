-module(cache).
-behavior(gen_server).
%% API
-export([start_link/0,
         store/2,
         update/2,
         lookup/1,
         list/0,
         stop/0]).
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

handle_call({store, {Key, Value}, Overwrite}, _From, CacheList) ->
    case lists:keyfind(Key, 1, CacheList) of
        false ->
            NewCacheList = [{Key, Value} | CacheList],
            Reply = {ok, stored, {Key, Value}},
            {reply, Reply, NewCacheList};
        {ExistingKey, ExistingValue} ->
            if
                Overwrite =:= true -> % Updates an existing Key with a new Value
                    TmpList = lists:delete({ExistingKey, ExistingValue}, CacheList),
                    NewCacheList = [{Key, Value} | TmpList],
                    Reply = {ok, updated, {Key, Value}},
                    {reply, Reply, NewCacheList};
                true ->
                    Reply = {error, key_already_exist, {ExistingKey, ExistingValue}},
                    {reply, Reply, CacheList}
            end
    end;

handle_call({lookup, {Key}}, _From, CacheList) ->
    Reply = case lists:keyfind(Key, 1, CacheList) of
        {Key, Value} -> {ok, Value};
        false -> {error, {key_not_found, Key}}
    end,
    {reply, Reply, CacheList};

handle_call(stop, _From, CacheList) ->
    {stop, normal, ok, CacheList};

handle_call(Unknown, _From, CacheList) ->
    Reply = {unknown_request, Unknown},
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


%% API

% Store a new {Key, Value} pair in the cache.
store(Key, Value) ->
    gen_server:call(?MODULE, {store, {Key, Value}, false}).

% Update and existing {Key, Value} pair with the NewValue or
% create a new one if {Key, NewValue} pair doesn't exist
update(Key, NewValue) ->
    gen_server:call(?MODULE, {store, {Key, NewValue}, true}).

% Lookup for Key in the cache and return its value.
lookup(Key) ->
    gen_server:call(?MODULE, {lookup, {Key}}).

% List the cache content.
list() ->
    gen_server:cast(?MODULE, list).

% Stop the process
stop() ->
    gen_server:call(?MODULE, stop).
