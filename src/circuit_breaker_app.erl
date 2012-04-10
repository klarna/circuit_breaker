%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Circuit breaker application
%%% @author Christian Rennerskog <christian.r@klarna.com>
%%% @author Magnus Fröberg <magnus@klarna.com>
%%% @copyright 2012 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(circuit_breaker_app).

%%%_* Behaviour ========================================================
-behaviour(application).

%%%_* Exports ==========================================================
%% Application callbacks
-export([ start/2
        , stop/1
        ]).

%%%_* Application callbacks ============================================
-spec start(StartType::normal | {takeover, node()} | {failover, node()},
            StartArgs::term()) ->
               {ok, Pid::pid()}
             | {ok, Pid::pid(), State::term()}
             | {error, Reason::term()}.
%% @doc This function is called whenever an application is started using
%% application:start/1,2, and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the top
%% supervisor of the tree.
%% @end
start(_StartType, _StartArgs) -> circuit_breaker_sup:start_link().

-spec stop(State::term()) -> ok.
%% @doc This function is called whenever an application has stopped. It is
%% intended to be the opposite of Module:start/2 and should do any
%% necessary cleaning up.
%% @end
stop(_State) -> ok.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
