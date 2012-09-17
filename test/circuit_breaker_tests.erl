%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
     , fun reset/1
     , fun ignore_errors/1
     ]}}.

start() -> application:start(circuit_breaker).

stop(_Setup) -> application:stop(circuit_breaker).

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
  , ?_assertEqual(true, circuit_breaker:disabled(?SERVICE))
  , ?_assertEqual({error, {circuit_breaker, ?CIRCUIT_BREAKER_CALL_TIMEOUT}},
                  call())
  ].

timeout_block(_Setup) ->
  call(),
  {_, 5} = loop_call(fun() -> {error, timeout} end),
  [ ?_assertEqual(false, circuit_breaker:blocked(?SERVICE))
  , ?_assertEqual(true, circuit_breaker:disabled(?SERVICE))
  , ?_assertEqual({error, {circuit_breaker, ?CIRCUIT_BREAKER_TIMEOUT}},
                  call())
  ].

error_block(_Setup) ->
  call(),
  {_, 5} = loop_call(fun() -> {error, crash} end),
  [ ?_assertEqual(false, circuit_breaker:blocked(?SERVICE))
  , ?_assertEqual(true, circuit_breaker:disabled(?SERVICE))
  , ?_assertEqual({error, {circuit_breaker, ?CIRCUIT_BREAKER_ERROR}},
                  call())
  ].

manual_block(_Setup) ->
  call(),
  circuit_breaker:block(?SERVICE),
  [ ?_assertEqual(true, circuit_breaker:blocked(?SERVICE))
  , ?_assertEqual(true, circuit_breaker:disabled(?SERVICE))
  , ?_assertEqual({error, {circuit_breaker, ?CIRCUIT_BREAKER_BLOCKED}}, call())
  ].

manual_deblock(_Setup) ->
  call(),
  circuit_breaker:block(?SERVICE),
  circuit_breaker:deblock(?SERVICE),
  [ ?_assertEqual(false, circuit_breaker:blocked(?SERVICE))
  , ?_assertEqual(false, circuit_breaker:disabled(?SERVICE))
  , ?_assertEqual(true, call())
  ].

manual_clear(_Setup) ->
  call(),
  {_, 5} = loop_call(fun() -> {error, crash} end),
  circuit_breaker:clear(?SERVICE),
  [ ?_assertEqual(false, circuit_breaker:blocked(?SERVICE))
  , ?_assertEqual(false, circuit_breaker:disabled(?SERVICE))
  , ?_assertEqual(true, call())
  ].

reset(_Setup) ->
  {_, 5} = loop_call(fun() -> {error, timeout} end),
  timer:sleep(3000),
  [ ?_assertEqual(false, circuit_breaker:disabled(?SERVICE))
  ].

ignore_errors(_Setup) ->
  call(),
  ok = loop_call(fun() -> {error, some_error} end),
  [ ?_assertEqual(false, circuit_breaker:disabled(?SERVICE))
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
