-module(cache).
-behavior(gen_server).
%% API
-export([start_link/0,
         store/2,
         update/2,
         lookup/1,
         list/0,
         stop/0]).
%% Internal functions
-export([clean_cache/0]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    %% Calls ?MODULE:clean_cache/0 function every x milliseconds.
    timer:apply_interval(2000, ?MODULE, clean_cache, []).

init(CacheList) ->
    {ok, CacheList}.

handle_call({store, {Key, Value}, Overwrite}, _From, CacheList) ->
    case lists:keyfind(Key, 1, CacheList) of
        %% Key doesn't exists in the cache yet.
        false ->
            %% Sets Expires time for this
            %% {Key, Value} pair in the cache.
            Expires = timestamp() + 60,
            NewCacheList = [{Key, Value, Expires} | CacheList],
            Reply = {ok, stored, {Key, Value, Expires}},
            {reply, Reply, NewCacheList};
        %% Key already exists in the cache.
        {ExistingKey, ExistingValue, ExistingExpires} ->
            Now = timestamp(),
            if
                Now > ExistingExpires ->
                    Reply = {error,
                             expired,
                             {ExistingKey, ExistingValue, ExistingExpires}},
                    {reply, Reply, CacheList};
                %% Updates an existing Key with a new Value.
                Overwrite =:= true ->
                    TmpList = lists:delete({ExistingKey,
                                            ExistingValue,
                                            ExistingExpires},
                                           CacheList),
                    NewCacheList = [{Key, Value, ExistingExpires} | TmpList],
                    Reply = {ok, updated, {Key, Value}},
                    {reply, Reply, NewCacheList};
                true ->
                    Reply = {error,
                             key_already_exist,
                             {ExistingKey, ExistingValue, ExistingExpires}},
                    {reply, Reply, CacheList}
            end
    end;

handle_call({lookup, {Key}}, _From, CacheList) ->
    Reply = case lists:keyfind(Key, 1, CacheList) of
        {Key, Value, Expires} -> {ok, Value, Expires};
        false -> {error, {key_not_found, Key}}
    end,
    {reply, Reply, CacheList};

handle_call(stop, _From, CacheList) ->
    {stop, normal, ok, CacheList};

handle_call(list, _From, CacheList) ->
    Reply = {ok, CacheList},
    {reply, Reply, CacheList};

handle_call(Unknown, _From, CacheList) ->
    Reply = {unknown_request, Unknown},
    {reply, {error, Reply}, CacheList}.

handle_cast(clean_cache, CacheList) ->
    %% {K, V, E} = {Key, Value, Expires}
    %io:format("[~p] Cleaning cache~n", [timestamp()]), %% Debug
    Now = timestamp(),
    CleanedCacheList = [{K, V, E} || {K, V, E} <- CacheList, E > Now],
    {noreply, CleanedCacheList};

handle_cast(_, CacheList) ->
    {noreply, CacheList}.

handle_info(_Request, CacheList) ->
    {noreply, CacheList}.

terminate(_Reason, _CacheList) ->
    {ok, _Reason}.

code_change(_OldVsn, CacheList, _Extra) ->
    {ok, CacheList}.


%%% API

%% Store a new {Key, Value} pair in the cache.
store(Key, Value) ->
    gen_server:call(?MODULE, {store, {Key, Value}, false}).

%% Update and existing {Key, Value} pair with the NewValue or
%% create a new one if {Key, NewValue} pair doesn't exist.
update(Key, NewValue) ->
    gen_server:call(?MODULE, {store, {Key, NewValue}, true}).

%% Lookup for Key in the cache and return its value.
lookup(Key) ->
    gen_server:call(?MODULE, {lookup, {Key}}).

%% List the cache content.
list() ->
    gen_server:call(?MODULE, list).

%% Stop the process.
stop() ->
    gen_server:call(?MODULE, stop).


%%% Internal functions

%% Returns the current date and time in seconds.
timestamp() ->
    {MegaSecs, Secs, _} = os:timestamp(),
    {NowTimestamp, _} = string:to_integer(lists:concat([MegaSecs, Secs])),
    NowTimestamp.

%% Removes expired entries from the cache list.
clean_cache() ->
    gen_server:cast(?MODULE, clean_cache).
