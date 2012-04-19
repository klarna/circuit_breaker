%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Circuit breaker
%%%
%%% Generic circuit breaker that can be used to break any service that
%%% isn't fully functional. A service can be manually blocked/cleared as well.
%%% The service will be executed in a spawned process that will continue
%%% execution even after a specified call timeout in order to be able
%%% to complete a request even if a response is not sent to the client.
%%% NOTE: It's important that the service can store it's result even
%%% if the result is not returned to the client.
%%%
%%% Information regarding current services under circuit breaker
%%% control can be displayed by: circuit_breaker:info/0.
%%%
%%% The circuit breaker generates an error event if a service is
%%% automatically blocked due to too many errors/timeouts.
%%% An info event is sent when the service is automatically cleared again.
%%%
%%% If several services are used to provide functionallity it's
%%% outside the scope of this server to take care (e.g. send
%%% a critical event) in the case that all used services are
%%% blocked.
%%%
%%% The heuristics/thresholds are configurable per service.
%%%
%%% @author Magnus Fröberg <magnus@klarna.com>
%%% @author Christian Rennerskog <christian.r@klarna.com>
%%% @copyright 2012 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(circuit_breaker).

%%%_* Behaviour ========================================================
-behaviour(gen_server).

%%%_* Exports ==========================================================
%% API
-export([ start_link/0
        , call/2
        , call/5
        , call/6
        , clear/1
        , block/1
        , deblock/1
        , disabled/1
        , blocked/1
        , info/0
        ]).

%% Gen server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%%%_* Includes =========================================================
-include("circuit_breaker.hrl").
-include_lib("tulib/include/bit.hrl").
-include_lib("tulib/include/prelude.hrl").

%%%_* Records ==========================================================
-record(state, {}).
-record(circuit_breaker,
        { service                            % Term, e.g. {IP|Host, Port}
        , flags        = ?CIRCUIT_BREAKER_OK % Status, ?CIRCUIT_BREAKER_*
        , timeout      = 0                   % {N, gnow()} | 0
        , call_timeout = 0                   % {N, gnow()} | 0
        , error        = 0                   % {N, gnow()} | 0
        , reset_fun                          % Fun to check for up status
        , ref                                % Timer reference
        }).

%%%_* Defines ==========================================================
-define(SERVER,            ?MODULE).
-define(TABLE,             ?MODULE).
-define(CALL_TIMEOUT,      10 * 1000).      % 10 seconds
-define(RESET_TIMEOUT,     10 * 60 * 1000). % 10 minutes
-define(RESET_FUN,         fun() -> true end).
-define(N_ERROR,           10).
-define(N_TIMEOUT,         10).
-define(N_CALL_TIMEOUT,    10).
-define(TIME_ERROR,        60 * 1000).     % 1 minute
-define(TIME_TIMEOUT,      60 * 1000).     % 1 minute
-define(TIME_CALL_TIMEOUT, 5 * 60 * 1000). % 5 minutes
-define(IGNORE_ERRORS,     [no_hit, not_found]).
-define(THRESHOLDS,        [ {n_error,           ?N_ERROR}
                           , {n_timeout,         ?N_TIMEOUT}
                           , {n_call_timeout,    ?N_CALL_TIMEOUT}
                           , {time_error,        ?TIME_ERROR}
                           , {time_timeout,      ?TIME_TIMEOUT}
                           , {time_call_timeout, ?TIME_CALL_TIMEOUT}
                           , {ignore_errors,     ?IGNORE_ERRORS}
                           ]).

%%%_* API ==============================================================
-spec start_link() -> {ok, Pid::pid()} | ignore | {error, Reason::term()}.
%% @doc Start circuit_breaker.
%% @end
start_link() -> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

-spec call(Service::term(), CallFun::function()) -> ok.
%% @doc Call Service with default parameters.
%% @end
call(Service, CallFun) ->
  call(Service, CallFun, ?CALL_TIMEOUT, ?RESET_FUN, ?RESET_TIMEOUT).

-spec call(Service::term(), CallFun::function(),
           CallTimeout::integer(), ResetFun::function(),
           ResetTimeout::integer()) -> ok.
%% @doc Call Service with custom parameters.
%% @end
call(Service, CallFun, CallTimeout, ResetFun, ResetTimeout) ->
  call(Service, CallFun, CallTimeout, ResetFun, ResetTimeout, ?THRESHOLDS).

-spec call(Service::term(), CallFun::function(),
           CallTimeout::integer(), ResetFun::function(),
           ResetTimeout::integer(), Thresholds::list()) -> ok.
%% @doc Call Service with custom parameters.
%% @end
call(Service, CallFun, CallTimeout, ResetFun, ResetTimeout, Thresholds) ->
  case read(Service) of
    R when (R#circuit_breaker.flags > ?CIRCUIT_BREAKER_WARNING) ->
      {error, {circuit_breaker, R#circuit_breaker.flags}};
    _ -> do_call(Service, CallFun, CallTimeout, ResetFun,
                 ResetTimeout, Thresholds)
  end.

-spec block(Service::term()) -> ok | {error, undefined}.
%% @doc Block Service.
%% @end
block(Service) ->
  case try_read(Service) of
    {ok, _} -> change_status(Service, block);
    false   -> {error, undefined}
  end.

-spec deblock(Service::term()) -> ok | {error, undefined}.
%% @doc Deblock Service.
%% @end
deblock(Service) ->
  case try_read(Service) of
    {ok, _} -> change_status(Service, deblock);
    false   -> {error, undefined}
  end.

-spec blocked(Service::term()) -> boolean().
%% @doc Check if Service is manually blocked.
%% @end
blocked(Service) ->
  case try_read(Service) of
    {ok, R} when ?bit_is_set(R#circuit_breaker.flags,
                             ?CIRCUIT_BREAKER_BLOCKED) -> true;
    _                                                  -> false
  end.

-spec disabled(Service::term()) -> boolean().
%% @doc Check if Service is disabled.
%% @end
disabled(Service) ->
  case try_read(Service) of
    {ok, R} -> R#circuit_breaker.flags >= ?CIRCUIT_BREAKER_BLOCKED;
    false   -> false
  end.

-spec clear(Service::term()) -> ok | {error, undefined}.
%% @doc Clear Service (except for manually blocked serices).
%% @end
clear(Service) ->
  case try_read(Service) of
    {ok, _} -> change_status(Service, clear);
    false   -> {error, undefined}
  end.

-spec info() -> ok.
%% @doc Print information regarding services.
%% @end
info() ->
  format("Service", "Status", "Error", "Timeout", "CallTimeout"),
  format(dup(31, $-), dup(15, $-), dup(8, $-), dup(8, $-), dup(8, $-)),
  ets:foldl(fun do_info/2, ok, ?TABLE).

%%%_* Gen server callbacks =============================================
%% @hidden
init([]) ->
  ?TABLE = ets:new(?TABLE, [named_table, {keypos, #circuit_breaker.service}]),
  {ok, #state{}}.

%% @hidden
handle_call({init, Service}, _From, State) ->
  do_init(Service),
  {reply, ok, State};
handle_call({change_status, Service, What}, _From, State) ->
  do_change_status(Service, What),
  {reply, ok, State}.

%% @hidden
handle_cast(_Msg, State) -> {noreply, State}.

%% @hidden
handle_info({reset, Service, ResetTimeout}, State) ->
  reset_service(Service, ResetTimeout),
  {noreply, State};

%% @hidden
handle_info(_Info, State) -> {noreply, State}.

%% @hidden
terminate(_Reason, _State) -> ok.

%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%_* Internal =========================================================
do_call(Service, CallFun, CallTimeout, ResetFun, ResetTimeout, Thresholds) ->
  Self   = self(),
  Pid    = proc_lib:spawn(fun() -> called(Self, CallFun(), Service) end),
  MonRef = erlang:monitor(process, Pid),
  receive
    {Pid, Result, Service} ->
      erlang:demonitor(MonRef, [flush]),
      handle_result({got, Result}, Service, ResetFun, ResetTimeout, Thresholds);
    {'DOWN', MonRef, process, Pid, Reason} ->
      handle_result({'EXIT', Reason}, Service, ResetFun,
                    ResetTimeout, Thresholds)
  after CallTimeout ->
      %% Let CallFun/0 continue in order to finish work,
      %% but return to client now.
      Pid ! {Self, call_timeout},
      erlang:demonitor(MonRef, [flush]),
      handle_result({error, call_timeout, Pid}, Service, ResetFun,
                    ResetTimeout, Thresholds)
  end.

called(Parent, Result, Service) ->
  receive
    {Parent, call_timeout} -> exit(normal) % Call did timeout, no return
  after 0 -> Parent ! {self(), Result, Service}
  end.

handle_result({got, Result}, Service, ResetFun, ResetTimeout, Thresholds) ->
  case Result of
    {error, timeout}                         ->
      timeout(Service, ResetFun, ResetTimeout, Thresholds);
    Result when element(1, Result) =:= error ->
      error(Service, Result, ResetFun, ResetTimeout, Thresholds);
    _ -> ok(Service)
  end,
  Result;
handle_result({error, call_timeout, Pid}, Service, ResetFun,
              ResetTimeout, Thresholds) ->
  call_timeout(Pid, Service, ResetFun, ResetTimeout, Thresholds),
  {error, call_timeout};
handle_result({'EXIT', Reason} = Exit, Service, ResetFun,
              ResetTimeout, Thresholds) ->
  error(Service, Exit, ResetFun, ResetTimeout, Thresholds),
  %% Keep behavior as if CallFun/0 was executed in same process context.
  exit(Reason).

error(Service, {_, Reason} = Error, ResetFun, ResetTimeout, Thresholds) ->
  case lists:member(Reason, ignore_errors(Thresholds)) of
    true  -> ok(Service);
    false ->
      event(?EVENT_ERROR, Service, Error),
      change_status(Service, {error, ResetFun, ResetTimeout, Thresholds})
  end.

call_timeout(Pid, Service, ResetFun, ResetTimeout, Thresholds) ->
  event(?EVENT_CALL_TIMEOUT, Service, {call_timeout, Pid}),
  change_status(Service, {call_timeout, ResetFun, ResetTimeout, Thresholds}).

timeout(Service, ResetFun, ResetTimeout, Thresholds) ->
  event(?EVENT_TIMEOUT, Service, timeout),
  change_status(Service, {timeout, ResetFun, ResetTimeout, Thresholds}).

ok(Service) ->
  case try_read(Service) of
    {ok, R} when R#circuit_breaker.flags =/= ?CIRCUIT_BREAKER_OK ->
      change_status(Service, ok);
    {ok, _R} -> ok;
    false    -> init_circuit_breaker(Service)
  end.

init_circuit_breaker(Service) ->
  gen_server:call(?SERVER, {init, Service}, infinity).

change_status(Service, What) ->
  gen_server:call(?SERVER, {change_status, Service, What}, infinity).

do_info(R, _Acc) ->
  Service           = io_lib:format("~p", [R#circuit_breaker.service]),
  Status            = fmt_flags(R#circuit_breaker.flags),
  {Errors, _}       = get_data(R, error),
  {Timeouts, _}     = get_data(R, timeout),
  {CallTimeouts, _} = get_data(R, call_timeout),
  format(Service, Status, ?i2l(Errors), ?i2l(Timeouts), ?i2l(CallTimeouts)).

fmt_flags(0) -> [pif(?CIRCUIT_BREAKER_OK)];
fmt_flags(F) -> lists:flatten(string:join(fmt_flags(0, F, []), ", ")).

fmt_flags(Pos, Flags, Acc) when Flags < (1 bsl Pos) ->
  lists:reverse(Acc);
fmt_flags(Pos, Flags, Acc) when ?bit_is_set(Flags, (1 bsl Pos)) ->
  fmt_flags(Pos + 1, Flags, [pif(1 bsl Pos)|Acc]);
fmt_flags(Pos, Flags, Acc) ->
  fmt_flags(Pos + 1, Flags, Acc).

pif(?CIRCUIT_BREAKER_OK)           -> "OK";
pif(?CIRCUIT_BREAKER_WARNING)      -> "WARNING";
pif(?CIRCUIT_BREAKER_BLOCKED)      -> "BLOCKED";
pif(?CIRCUIT_BREAKER_CALL_TIMEOUT) -> "CALL TIMEOUT";
pif(?CIRCUIT_BREAKER_TIMEOUT)      -> "TIMEOUT";
pif(?CIRCUIT_BREAKER_ERROR)        -> "ERROR".

dup(N, Char) -> lists:duplicate(N, Char).

format(A1, A2, A3, A4, A5) ->
  io:format("~-31s ~-15s ~-8s ~-8s ~-8s~n", [A1, A2, A3, A4, A5]).

do_init(Service) ->
  case exists(Service) of
    false -> write(#circuit_breaker{service = Service});
    true  -> ok
  end.

do_change_status(Service, What) ->
  do_change_status(read(Service), Service, What).

%% What = {error, ResetFun, ResetTimeout, Thresholds} |
%%        {call_timeout, ResetFun, ResetTimeout, Thresholds} |
%%        {timeout, ResetFun, ResetTimeout, Thresholds} |
%%        ok |
%%        block |
%%        deblock |
%%        clear
do_change_status(R, Service, {Type, ResetFun, ResetTimeout, Thresholds}) ->
  fault_status(R, Service, Type, ResetFun, ResetTimeout, Thresholds);
do_change_status(R, _Service, ok) ->
  decrease_counter(R);
do_change_status(R0, _Service, block) ->
  %% Manually blocked
  event(?EVENT_MANUALLY_BLOCKED, R0),
  R = maybe_cancel_timer(R0),
  write(?bit_set_rec(R#circuit_breaker, ?CIRCUIT_BREAKER_BLOCKED));
do_change_status(R0, Service, deblock) ->
  %% Manually deblocked; create new default #circuit_breaker{}.
  event(?EVENT_MANUALLY_DEBLOCKED, R0),
  _R = maybe_cancel_timer(R0),
  write(#circuit_breaker{service = Service});
do_change_status(R0, Service, clear) ->
  %% Manually cleared errors; do not clear CIRCUIT_BREAKER_BLOCKED
  event(?EVENT_MANUALLY_CLEARED, R0),
  R = maybe_cancel_timer(R0),
  Flags =
    if
      ?bit_is_set(R#circuit_breaker.flags,
                  ?CIRCUIT_BREAKER_BLOCKED) -> ?CIRCUIT_BREAKER_BLOCKED;
      true                                  -> ?CIRCUIT_BREAKER_OK
    end,
  write(#circuit_breaker{ service = Service
                        , flags   = Flags
                        }).

fault_status(R0, _Service, Type, ResetFun, ResetTimeout, Thresholds) ->
  Now           = gnow(),
  {N, LastNow}  = get_data(R0, Type),
  NThreshold    = n_threshold(Thresholds, Type),
  TimeThreshold = time_threshold(Thresholds, Type),
  if
    N + 1 =:= NThreshold,
    Now - LastNow < TimeThreshold        ->
      event(?EVENT_AUTOMATICALLY_BLOCKED, R0, Type),
      Flag  = flag(Type),
      R1    = maybe_cancel_timer(R0),
      R2    = set_data(R1, Type, {N + 1, Now}),
      R     = start_timer(R2, ResetTimeout),
      Flags = ?bit_clr(R#circuit_breaker.flags, ?CIRCUIT_BREAKER_WARNING),
      write(R#circuit_breaker{ flags     = ?bit_set(Flags, Flag)
                             , reset_fun = ResetFun
                             });
    Now - LastNow < TimeThreshold, N > 0 ->
      write(set_data(R0, Type, {N + 1, Now}));
    true                                 ->
      R = set_data(R0, Type, {1, Now}),
      write(R#circuit_breaker{flags = ?bit_set(R#circuit_breaker.flags,
                                               ?CIRCUIT_BREAKER_WARNING)})
  end.

%% An ok request; decrease a counter in "severity" order.
decrease_counter(R0) ->
  Types = [error, timeout, call_timeout],
  case decrease_counter(Types, R0, false, 1) of
    {WarnP, R} when WarnP =:= false ->
      write(R#circuit_breaker{flags = ?bit_clr(R#circuit_breaker.flags,
                                               ?CIRCUIT_BREAKER_WARNING)});
    {_WarnP, R} -> write(R)
  end.

decrease_counter([Type|Types], R, WarnP, M) when M > 0   ->
  case get_data(R, Type) of
    {0, _} -> decrease_counter(Types, R, WarnP, M);
    {1, _} -> decrease_counter(Types, set_data(R, Type, 0), WarnP, M - 1);
    {N, T} -> decrease_counter(Types,
                               set_data(R, Type, {N - 1, T}), true, M - 1)
  end;
decrease_counter([Type|Types], R, WarnP, M) when M =:= 0 ->
  case get_data(R, Type) of
    {0, _} -> decrease_counter(Types, R, WarnP, M);
    _      -> decrease_counter(Types, R, true, M)
  end;
decrease_counter([], R, WarnP, _M)                       ->
  {WarnP, R}.

reset_service(Service, ResetTimeout) ->
  R = read(Service),
  case catch (R#circuit_breaker.reset_fun)() of
    true when ?bit_is_set(R#circuit_breaker.flags,
                          ?CIRCUIT_BREAKER_BLOCKED) ->
      write(#circuit_breaker{ service = Service
                            , flags   = ?CIRCUIT_BREAKER_BLOCKED
                            });
    true -> event(?EVENT_AUTOMATICALLY_CLEARED, R),
            write(#circuit_breaker{service = Service});
    _    -> write(start_timer(R, ResetTimeout))
  end.

%%%_* Setters/getters --------------------------------------------------
%% Set/get fault data (error, timeout and call_timeout)
get_data(R, Type) ->
  case do_get_data(R, Type) of
    0    -> {0, gnow()};
    Data -> Data
  end.

do_get_data(R, error)           -> R#circuit_breaker.error;
do_get_data(R, timeout)         -> R#circuit_breaker.timeout;
do_get_data(R, call_timeout)    -> R#circuit_breaker.call_timeout.

set_data(R, error, Data)        -> R#circuit_breaker{error = Data};
set_data(R, timeout, Data)      -> R#circuit_breaker{timeout = Data};
set_data(R, call_timeout, Data) -> R#circuit_breaker{call_timeout = Data}.

n_threshold(L, error)        ->
  proplists:get_value(n_error, L, ?N_ERROR);
n_threshold(L, timeout)      ->
  proplists:get_value(n_timeout, L, ?N_TIMEOUT);
n_threshold(L, call_timeout) ->
  proplists:get_value(n_call_timeout, L, ?N_CALL_TIMEOUT).

time_threshold(L, error)        ->
  proplists:get_value(time_error, L, ?TIME_ERROR);
time_threshold(L, timeout)      ->
  proplists:get_value(time_timeout, L, ?TIME_TIMEOUT);
time_threshold(L, call_timeout) ->
  proplists:get_value(time_call_timeout, L, ?TIME_CALL_TIMEOUT).

ignore_errors(L) -> proplists:get_value(ignore_errors, L, ?IGNORE_ERRORS).

flag(error)        -> ?CIRCUIT_BREAKER_ERROR;
flag(timeout)      -> ?CIRCUIT_BREAKER_TIMEOUT;
flag(call_timeout) -> ?CIRCUIT_BREAKER_CALL_TIMEOUT.

%%%_* Timer ------------------------------------------------------------
%% Clear timer if existing
maybe_cancel_timer(R) when R#circuit_breaker.ref =/= undefined ->
  timer:cancel(R#circuit_breaker.ref),
  flush_reset(R),
  R#circuit_breaker{ref = undefined};
maybe_cancel_timer(R) -> R.

flush_reset(R) ->
  receive
    {reset, Service, _ResetTimeout}
      when Service =:= R#circuit_breaker.service -> ok
  after 0 -> ok
  end.

start_timer(R, ResetTimeout) ->
  {ok, Ref} =
    timer:send_after(ResetTimeout, self(),
                     {reset, R#circuit_breaker.service, ResetTimeout}),
  R#circuit_breaker{ref = Ref}.

%%%_* ETS operations ---------------------------------------------------
read(Service) ->
  case ets:lookup(?TABLE, Service) of
    [R] -> R;
    []  -> #circuit_breaker{service = Service}
  end.

try_read(Service) ->
  case ets:lookup(?TABLE, Service) of
    [R] -> {ok, R};
    []  -> false
  end.

exists(Service) -> ets:lookup(?TABLE, Service) =/= [].

write(#circuit_breaker{} = R) -> ets:insert(?TABLE, R).

%%%_* Event ------------------------------------------------------------
event(EventType, #circuit_breaker{} = R, Error) ->
  event(EventType, [ {error, Error}
                   , {stacktrace, get_stacktrace()}
                   | extract(R)
                   ]);
event(EventType, Service, Error)   ->
  event(EventType, read(Service), Error).

event(EventType, #circuit_breaker{} = R)            ->
  event(EventType, [{stacktrace, get_stacktrace()}|extract(R)]);
event(EventType, EventInfo) when is_list(EventInfo) ->
  case application:get_env(circuit_breaker, event_handler) of
    {ok, Module} when is_atom(Module) -> Module:event(EventType, EventInfo);
    _                                 -> ok
  end.

extract(#circuit_breaker{ service      = Service
                        , flags        = Flags
                        , timeout      = NTimeout
                        , call_timeout = NCallTimeout
                        , error        = NError
                        }) ->
  [ {service, Service}
  , {flags, Flags}
  , {n_timeout, NTimeout}
  , {n_call_timeout, NCallTimeout}
  , {n_error, NError}
  ].

get_stacktrace() ->
  try throw(get_stacktrace)
  catch throw:get_stacktrace -> erlang:get_stacktrace()
  end.

%%%_* Time -------------------------------------------------------------
gnow() ->
  case application:get_env(time_handler) of
    {ok, {Module, Function}}
      when is_atom(Module) andalso is_atom(Function) ->
      Module:Function();
    undefined                                        ->
      calendar:datetime_to_gregorian_seconds(calendar:local_time())
  end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
