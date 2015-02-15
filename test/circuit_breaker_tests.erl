%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright 2012 Klarna AB
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%
%%% @doc Circuit breaker tests
%%% @copyright 2012 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(circuit_breaker_tests).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("circuit_breaker/include/circuit_breaker.hrl").

%%%_* Defines ==========================================================
-define(SERVICE, service).
-define(APP,     circuit_breaker).

%%%_* Tests ============================================================
circuit_breaker_test_() ->
  {"Tests the circuit breaker functionality",
   { foreach
   , fun start/0             % setup
   , fun stop/1              % teardown
   , [ fun undefined/1
     , fun call_timeout_block/1
     , fun timeout_block/1
     , fun error_block/1
     , fun manual_block/1
     , fun manual_deblock/1
     , fun manual_clear/1
     , fun returns/1
     , fun reset/1
     , fun ignore_errors/1
     , fun info/1
     ]}}.

start() ->
  case application:start(?APP) of
    ok                            ->
      ok = application:set_env(?APP, event_handler, circuit_breaker_event_test);
    {error, {already_started, _}} ->
      circuit_breaker:clear(?SERVICE),
      already_started
  end.

stop(already_started) -> ok;
stop(_)               -> application:stop(?APP).

undefined(_Setup) ->
  [ ?_assertEqual({error, undefined}, circuit_breaker:block(?SERVICE))
  , ?_assertEqual({error, undefined}, circuit_breaker:deblock(?SERVICE))
  , ?_assertEqual({error, undefined}, circuit_breaker:clear(?SERVICE))
  , ?_assertEqual(false, circuit_breaker:disabled(?SERVICE))
  , ?_assertEqual(false, circuit_breaker:blocked(?SERVICE))
  ].

call_timeout_block(_Setup) ->
  call(),
  {_, 5} = loop_call(fun() -> timer:sleep(200) end),
  [ ?_assertEqual(false, circuit_breaker:blocked(?SERVICE))
  , ?_assert(circuit_breaker:disabled(?SERVICE))
  , ?_assertEqual({error, {circuit_breaker, ?CIRCUIT_BREAKER_CALL_TIMEOUT}},
                  call())
  ].

timeout_block(_Setup) ->
  call(),
  {_, 5} = loop_call(fun() -> {error, timeout} end),
  [ ?_assertEqual(false, circuit_breaker:blocked(?SERVICE))
  , ?_assert(circuit_breaker:disabled(?SERVICE))
  , ?_assertEqual({error, {circuit_breaker, ?CIRCUIT_BREAKER_TIMEOUT}},
                  call())
  ].

error_block(_Setup) ->
  call(),
  {_, 5} = loop_call(fun() -> {error, crash} end),
  [ ?_assertEqual(false, circuit_breaker:blocked(?SERVICE))
  , ?_assert(circuit_breaker:disabled(?SERVICE))
  , ?_assertEqual({error, {circuit_breaker, ?CIRCUIT_BREAKER_ERROR}},
                  call())
  ].

manual_block(_Setup) ->
  call(),
  circuit_breaker:block(?SERVICE),
  [ ?_assert(circuit_breaker:blocked(?SERVICE))
  , ?_assert(circuit_breaker:disabled(?SERVICE))
  , ?_assertEqual({error, {circuit_breaker, ?CIRCUIT_BREAKER_BLOCKED}}, call())
  ].

manual_deblock(_Setup) ->
  call(),
  circuit_breaker:block(?SERVICE),
  circuit_breaker:deblock(?SERVICE),
  [ ?_assertEqual(false, circuit_breaker:blocked(?SERVICE))
  , ?_assertEqual(false, circuit_breaker:disabled(?SERVICE))
  , ?_assert(call())
  ].

manual_clear(_Setup) ->
  call(),
  {_, 5} = loop_call(fun() -> {error, crash} end),
  circuit_breaker:clear(?SERVICE),
  [ ?_assertEqual(false, circuit_breaker:blocked(?SERVICE))
  , ?_assertEqual(false, circuit_breaker:disabled(?SERVICE))
  , ?_assert(call())
  ].

returns(_Setup) ->
  [ ?_assert(call())
  , ?_assertEqual({error, timeout}, call(fun() -> timer:sleep(200) end))
  , ?_assertEqual({error, timeout}, call(fun() -> {error, timeout} end))
  , ?_assertEqual({error, some_error}, call(fun() -> {error, some_error} end))
  , ?_assertException(exit, reason, call(fun() -> exit(reason) end))
  ].

reset(_Setup) ->
  {_, 5} = loop_call(fun() -> {error, timeout} end),
  [ ?_assert(retry(fun() ->
                       false =:= circuit_breaker:disabled(?SERVICE)
                   end, 1000, 10))
  ].

retry(F, T, N) when N > 0 ->
  case F() of
    Res when Res =:= false
           ; Res =:= error
           ; element(1, Res) =:= error ->
      timer:sleep(T),
      retry(F, T, N-1);
    Res ->
      Res
  end;
retry(_F, _T, 0) -> false.

ignore_errors(_Setup) ->
  call(),
  ok = loop_call(fun() -> {error, some_error} end),
  [ ?_assertEqual(false, circuit_breaker:disabled(?SERVICE))
  ].

info(_Setup) ->
  call(),
  [ ?_assertEqual(ok, circuit_breaker:info())
  ].

%%%_* Helpers ==========================================================
call() -> call(fun() -> true end).

%% Use custom values to test that these are used
call(F) ->
  circuit_breaker:call( ?SERVICE, F, 100, fun() -> true end, 2000
                      , [ {n_error,           5}
                        , {n_timeout,         5}
                        , {n_call_timeout,    5}
                        , {ignore_errors,     [some_error]}
                        ]
                      ).

loop_call(F) -> loop_call(F, 0).

loop_call(_F, 20) -> ok;
loop_call(F, N)   ->
  case call(F) of
    {error, {circuit_breaker, _}} = Error -> {Error, N};
    _                                     -> loop_call(F, N + 1)
  end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
