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

%% Events
-define(manually_blocked,      manually_blocked).
-define(manually_deblocked,    manually_deblocked).
-define(manually_cleared,      manually_cleared).
-define(automatically_blocked, automatically_blocked).
-define(automatically_cleared, automatically_cleared).
-define(error,                 error).
-define(timeout,               timeout).
-define(call_timeout,          call_timeout).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
