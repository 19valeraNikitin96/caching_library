%%%-------------------------------------------------------------------
%%% @author erlang
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2020 15:32
%%%-------------------------------------------------------------------
-module(my_cache_test).
-author("erlang").

-include_lib("eunit/include/eunit.hrl").

first_test()->
 ?assert(my_cache:create()==ok)
.

second_test()->
  my_cache:create(),
  ?assert(my_cache:insert(123, 555)==ok)
.

third_test()->
  my_cache:create(),
  my_cache:insert(123, 555, 5),
  ?assertEqual({ok,[555]},my_cache:lookup(123))
.

fourth_test()->
  my_cache:create(),
  my_cache:insert(123, 555, 0),
  my_cache:delete_obsolete(),
  ?assertEqual({ok,[]},my_cache:lookup(123))
.

fifth_test()->
  my_cache:create(),
  ?assertEqual(undefined,my_cache:insert(123, 555, 60, some_atom))
.