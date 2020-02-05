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
-export([create/0, insert/3, lookup/1, insert/2, insert/4, delete_obsolete/0, all/0, analyze/0]).

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
    true -> ets:insert(get(ets_id), {Key, Value, LifeTime}),ok;
    false -> ?NULL
  end
.

-spec lookup(_Key) -> {ok, _}.
%% my_cache:insert(t1,"Valera",{lifetime,erlang:system_time('second'),10,'second'}).
lookup(Key)->
  Result = ets:lookup(get(ets_id),Key),
  CurrentTime = erlang:system_time(?SEC),
  Res =lists:filter(
    fun(X)-> {_,_,{lifetime,InitialTime,Value,_Unit}}=X,
      (CurrentTime < InitialTime+Value) end,
    Result),
  case Res == [] of
    true -> {ok, []};
    false -> {ok, lists:flatmap(fun(X)->{_,Val,_}=X, [Val] end, Res)}
  end
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

analyze() ->
  {T1, _} =   timer:tc(my_cache,insert,[123, 999]),
  {T2, _} =   timer:tc(my_cache,insert,[256, "999"]),
  {T3, _} =   timer:tc(my_cache,insert,[123, <<"999">>]),
  {T4, _} =   timer:tc(my_cache,insert,[123, [1,2,3,4,5,6,7,8,9,10]]),
  {T5, _} =   timer:tc(my_cache,insert,[123, [<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>]]),
  {T6, _} =   timer:tc(my_cache,insert,[123, {<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>}]),
  {T7, _} =   timer:tc(my_cache,insert,[123, #lifetime{initial_time = erlang:system_time(?SEC), value = 120, unit = ?SEC}]),

  {T8, _} =   timer:tc(my_cache,insert,["key", 999]),
  {T9, _} =   timer:tc(my_cache,insert,["key", "999"]),
  {T10, _} =   timer:tc(my_cache,insert,["key", <<"999">>]),
  {T11, _} =   timer:tc(my_cache,insert,["key", [1,2,3,4,5,6,7,8,9,10]]),
  {T12, _} =   timer:tc(my_cache,insert,["key", [<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>]]),
  {T13, _} =   timer:tc(my_cache,insert,["key", {<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>}]),
  {T14, _} =   timer:tc(my_cache,insert,["key", #lifetime{initial_time = erlang:system_time(?SEC), value = 120, unit = ?SEC}]),

  {T15, _} =   timer:tc(my_cache,insert,[[1,2,3,4,5,6,7,8,9,10], 999]),
  {T16, _} =   timer:tc(my_cache,insert,[[1,2,3,4,5,6,7,8,9,10], "999"]),
  {T17, _} =   timer:tc(my_cache,insert,[[1,2,3,4,5,6,7,8,9,10], <<"999">>]),
  {T18, _} =   timer:tc(my_cache,insert,[[1,2,3,4,5,6,7,8,9,10], [1,2,3,4,5,6,7,8,9,10]]),
  {T19, _} =   timer:tc(my_cache,insert,[[1,2,3,4,5,6,7,8,9,10], [<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>]]),
  {T20, _} =   timer:tc(my_cache,insert,[[1,2,3,4,5,6,7,8,9,10], {<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>}]),
  {T21, _} =   timer:tc(my_cache,insert,[[1,2,3,4,5,6,7,8,9,10], #lifetime{initial_time = erlang:system_time(?SEC), value = 120, unit = ?SEC}]),

  {T22, _} =   timer:tc(my_cache,insert,[{<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>}, 999]),
  {T23, _} =   timer:tc(my_cache,insert,[{<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>}, "999"]),
  {T24, _} =   timer:tc(my_cache,insert,[{<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>}, <<"999">>]),
  {T25, _} =   timer:tc(my_cache,insert,[{<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>}, [1,2,3,4,5,6,7,8,9,10]]),
  {T26, _} =   timer:tc(my_cache,insert,[{<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>}, [<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>]]),
  {T27, _} =   timer:tc(my_cache,insert,[{<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>}, {<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>}]),
  {T28, _} =   timer:tc(my_cache,insert,[{<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>}, #lifetime{initial_time = erlang:system_time(?SEC), value = 120, unit = ?SEC}]),

  {T29, _} =   timer:tc(my_cache,insert,[#lifetime{initial_time = erlang:system_time(?SEC), value = 120, unit = ?SEC}, 999]),
  {T30, _} =   timer:tc(my_cache,insert,[#lifetime{initial_time = erlang:system_time(?SEC), value = 120, unit = ?SEC}, "999"]),
  {T31, _} =   timer:tc(my_cache,insert,[#lifetime{initial_time = erlang:system_time(?SEC), value = 120, unit = ?SEC}, <<"999">>]),
  {T32, _} =   timer:tc(my_cache,insert,[#lifetime{initial_time = erlang:system_time(?SEC), value = 120, unit = ?SEC}, [1,2,3,4,5,6,7,8,9,10]]),
  {T33, _} =   timer:tc(my_cache,insert,[#lifetime{initial_time = erlang:system_time(?SEC), value = 120, unit = ?SEC}, [<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>]]),
  {T34, _} =   timer:tc(my_cache,insert,[#lifetime{initial_time = erlang:system_time(?SEC), value = 120, unit = ?SEC}, {<<"111">>,<<"222">>,<<"333">>,<<"444">>,<<"555">>,<<"666">>,<<"777">>,<<"888">>,<<"999">>}]),
  {T35, _} =   timer:tc(my_cache,insert,[#lifetime{initial_time = erlang:system_time(?SEC), value = 120, unit = ?SEC}, #lifetime{initial_time = erlang:system_time(?SEC), value = 120, unit = ?SEC}]),

  io:format("~w~n", [{T1, int_int}]),
  io:format("~w~n", [{T2, int_string}]),
  io:format("~w~n", [{T3, int_bin}]),
  io:format("~w~n", [{T4, int_listOfInt}]),
  io:format("~w~n", [{T5, int_listOfBinaries}]),
  io:format("~w~n", [{T6, int_tupleOfBinaries}]),
  io:format("~w~n", [{T7, int_record}]),
  io:format("~w~n", [{T8,  string_int}]),
  io:format("~w~n", [{T9,  string_string}]),
  io:format("~w~n", [{T10, string_bin}]),
  io:format("~w~n", [{T11, string_listOfInt}]),
  io:format("~w~n", [{T12, string_listOfBinaries}]),
  io:format("~w~n", [{T13, string_tupleOfBinaries}]),
  io:format("~w~n", [{T14, string_record}]),
  io:format("~w~n", [{T15, listOfInt_int}]),
  io:format("~w~n", [{T16, listOfInt_string}]),
  io:format("~w~n", [{T17, listOfInt_bin}]),
  io:format("~w~n", [{T18, listOfInt_listOfInt}]),
  io:format("~w~n", [{T19, listOfInt_listOfBinaries}]),
  io:format("~w~n", [{T20, listOfInt_tupleOfBinaries}]),
  io:format("~w~n", [{T21, listOfInt_record}]),
  io:format("~w~n", [{T22, tupleOfBinaries_int}]),
  io:format("~w~n", [{T23, tupleOfBinaries_string}]),
  io:format("~w~n", [{T24, tupleOfBinaries_bin}]),
  io:format("~w~n", [{T25, tupleOfBinaries_listOfInt}]),
  io:format("~w~n", [{T26, tupleOfBinaries_listOfBinaries}]),
  io:format("~w~n", [{T27, tupleOfBinaries_tupleOfBinaries}]),
  io:format("~w~n", [{T28, tupleOfBinaries_record}]),
  io:format("~w~n", [{T29, record_int}]),
  io:format("~w~n", [{T30, record_string}]),
  io:format("~w~n", [{T31, record_bin}]),
  io:format("~w~n", [{T32, record_listOfInt}]),
  io:format("~w~n", [{T33, record_listOfBinaries}]),
  io:format("~w~n", [{T34, record_tupleOfBinaries}]),
  io:format("~w~n", [{T35, record_record}])


.



%%  io:format("~w~n", [{T1, <<"Int - Int">>}]),
%%io:format("~w~n", [{T2, <<"Int - String">>}]),
%%io:format("~w~n", [{T3, <<"Int - Bin">>}]),
%%io:format("~w~n", [{T4, <<"Int - List of Int">>}]),
%%io:format("~w~n", [{T5, <<"Int - List of binaries">>}]),
%%io:format("~w~n", [{T6, <<"Int - Tuple of binaries">>}]),
%%io:format("~w~n", [{T7, <<"Int - Record">>}]),
%%io:format("~w~n", [{T8,  <<"String - Int">>}]),
%%io:format("~w~n", [{T9,  <<"String - String">>}]),
%%io:format("~w~n", [{T10, <<"String - Bin">>}]),
%%io:format("~w~n", [{T11, <<"String - List of Int">>}]),
%%io:format("~w~n", [{T12, <<"String - List of binaries">>}]),
%%io:format("~w~n", [{T13, <<"String - Tuple of binaries">>}]),
%%io:format("~w~n", [{T14, <<"String - Record">>}]),
%%io:format("~w~n", [{T15, <<"List of Int - Int">>}]),
%%io:format("~w~n", [{T16, <<"List of Int - String">>}]),
%%io:format("~w~n", [{T17, <<"List of Int - Bin">>}]),
%%io:format("~w~n", [{T18, <<"List of Int - List of Int">>}]),
%%io:format("~w~n", [{T19, <<"List of Int - List of binaries">>}]),
%%io:format("~w~n", [{T20, <<"List of Int - Tuple of binaries">>}]),
%%io:format("~w~n", [{T21, <<"List of Int - Record">>}]),
%%io:format("~w~n", [{T22, <<"Tuple of binaries - Int">>}]),
%%io:format("~w~n", [{T23, <<"Tuple of binaries - String">>}]),
%%io:format("~w~n", [{T24, <<"Tuple of binaries - Bin">>}]),
%%io:format("~w~n", [{T25, <<"Tuple of binaries - List of Int">>}]),
%%io:format("~w~n", [{T26, <<"Tuple of binaries - List of binaries">>}]),
%%io:format("~w~n", [{T27, <<"Tuple of binaries - Tuple of binaries">>}]),
%%io:format("~w~n", [{T28, <<"Tuple of binaries - Record">>}]),
%%io:format("~w~n", [{T29, <<"Record - Int">>}]),
%%io:format("~w~n", [{T30, <<"Record - String">>}]),
%%io:format("~w~n", [{T31, <<"Record - Bin">>}]),
%%io:format("~w~n", [{T32, <<"Record - List of Int">>}]),
%%io:format("~w~n", [{T33, <<"Record - List of binaries">>}]),
%%io:format("~w~n", [{T34, <<"Record - Tuple of binaries">>}]),
%%io:format("~w~n", [{T35, record_record}])