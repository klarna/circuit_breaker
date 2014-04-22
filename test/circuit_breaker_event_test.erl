%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Circuit breaker event test
%%% @copyright 2012 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(circuit_breaker_event_test).

%%%_* Behaviour ========================================================
-beahviour(circuit_breaker_event).

%%%_* Exports ==========================================================
%% Circuit breaker callback
-export([ handle/2
        ]).

%%%_* Callback =========================================================
%% @see circuit_breaker_event:handle/2
handle(_Type, _Info) -> ok.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
