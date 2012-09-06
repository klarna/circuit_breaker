%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Circuit breaker
%%% @copyright 2012 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Defines ==========================================================
%% #circuit_breaker.flags
-define(CIRCUIT_BREAKER_OK,           0).
-define(CIRCUIT_BREAKER_WARNING,      (1 bsl 0)).
-define(CIRCUIT_BREAKER_BLOCKED,      (1 bsl 1)).
-define(CIRCUIT_BREAKER_CALL_TIMEOUT, (1 bsl 2)).
-define(CIRCUIT_BREAKER_TIMEOUT,      (1 bsl 3)).
-define(CIRCUIT_BREAKER_ERROR,        (1 bsl 4)).

%% Event types
-define(EVENT_MANUALLY_BLOCKED,      event_manually_blocked).
-define(EVENT_MANUALLY_DEBLOCKED,    event_manually_deblocked).
-define(EVENT_MANUALLY_CLEARED,      event_manually_cleared).
-define(EVENT_AUTOMATICALLY_BLOCKED, event_automatically_blocked).
-define(EVENT_AUTOMATICALLY_CLEARED, event_automatically_cleared).
-define(EVENT_ERROR,                 event_error).
-define(EVENT_TIMEOUT,               event_timeout).
-define(EVENT_CALL_TIMEOUT,          event_call_timeout).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
