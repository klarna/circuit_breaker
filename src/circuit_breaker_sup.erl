%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Circuit breaker supervisor
%%% @copyright 2012 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(circuit_breaker_sup).

%%%_* Behaviour ========================================================
-behaviour(supervisor).

%%%_* Exports ==========================================================
%% API
-export([ start_link/0
        ]).

%% Supervisor callbacks
-export([ init/1
        ]).

%%%_* Defines ==========================================================
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type),
        {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%_* API ==============================================================
-spec start_link() -> {ok, pid()} | ignore | {error, Reason::term()}.
%% @doc Creates a supervisor process as part of a supervision tree.
%% The function will, among other things, ensure that the supervisor
%% is linked to the calling process (its supervisor). The created
%% supervisor process calls Module:init/1
%% @end
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%_* Supervisor callbacks =============================================
%% @hidden
init([]) ->
  {ok, {{one_for_one, 10, 20},
        [ ?CHILD(circuit_breaker, worker)
        ]}}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
