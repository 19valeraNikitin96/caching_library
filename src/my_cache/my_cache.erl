%%%-------------------------------------------------------------------
%%% @author erlang
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2020 12:44
%%%-------------------------------------------------------------------
-module(my_cache).
-author("erlang").
-include("for_macro.hrl").
%% API
-export([create/0, insert/3, lookup/1, insert/2, insert/4, delete_obsolete/0, all/0]).

-spec create() -> ok.
create() ->
  ID = get(ets_id),
  case ID /= ?NULL of
    true -> ets:delete(ID);
    false -> ok
  end,
  put(ets_id, ets:new(?CACHENAME, [duplicate_bag, public])),
  ok
.

-spec insert(_Key, Value) -> ok when
  Value :: term()
.
insert(Key, Value)->
  insert(Key, Value, #lifetime{initial_time = erlang:system_time(?SEC), value = 60, unit = ?SEC})
.

-spec insert(_Key, Value, TimeValue, Unit) -> ok when
  Value :: term(),
  TimeValue :: non_neg_integer(),
  Unit :: atom()
.
insert(Key, Value, TimeValue, Unit)->
  insert(Key, Value, #lifetime{initial_time = erlang:system_time(?SEC), value = TimeValue, unit = Unit})
.

-spec insert(_Key, Value, LifeTime) -> ok when
  Value :: term(),
  LifeTime :: tuple() | non_neg_integer()
.
insert(Key, Value, TimeValue) when is_integer(TimeValue)->
  insert(Key, Value, #lifetime{initial_time = erlang:system_time(?SEC), value = TimeValue, unit = ?SEC})
;
insert(Key, Value, LifeTime) ->
  case lists:member(LifeTime#lifetime.unit, ?TIME_UNITS) of
    true -> ets:insert(get(ets_id), {Key, Value, LifeTime});
    false -> ?NULL
  end
.

%% my_cache:insert(t1,"Valera",{lifetime,erlang:system_time('second'),10,'second'}).
lookup(Key)->
  Result = ets:lookup(get(ets_id),Key),
  CurrentTime = erlang:system_time(?SEC),
  lists:filter(
    fun(X)-> {_,_,{lifetime,InitialTime,Value,_Unit}}=X,
      (CurrentTime < InitialTime+Value) end,
    Result)
.
-spec delete_obsolete() -> ok.
delete_obsolete()->
  ID = get(ets_id),
  delete_obsolete(ets:tab2list(ID), ID)
.
delete_obsolete([], _Ets_ID)-> ok;
delete_obsolete([{_,_,{_,InitialTime, Value, _Unit}}=H|T], Ets_ID) ->
  case erlang:system_time(?SEC) > InitialTime+Value of
    true -> ets:delete_object(Ets_ID, H), delete_obsolete(T, Ets_ID);
    false -> delete_obsolete(T, Ets_ID)
  end
.
%%TODO must be removed
all()->
  ets:tab2list(get(ets_id))
.