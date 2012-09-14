%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Circuit breaker event behaviour
%%%
%%% Handles events passed from the circuit breaker.
%%%
%%% @copyright 2012 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(circuit_breaker_event).

%%%_* Exports ==========================================================
%% Behaviour definition
-export([ behaviour_info/1
        ]).

%% API
-export([ handle/2
        ]).

%%%_* Types ============================================================
-type(type() :: manually_blocked
              | manually_deblocked
              | manually_cleared
              | automatically_cleared
              | automatically_blocked
              | error
              | call_timeout
              | timeout).

-export_type([ type/0
             ]).

%%%_* Behaviour definition =============================================
behaviour_info(callbacks) ->
  [ {handle, 2}
  ];
behaviour_info(_Other)    ->
  undefined.

%%%_* API ==============================================================
-spec handle(Type::type(), Info::list()) -> any().
%% @doc Called when circuit breaker generates an event.
%% @end
handle(Type, _Info) ->
  error_logger:info_msg("Received ~w from circuit breaker", [Type]).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
