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
%%% @author Christian Rennerskog <christian.r@klarna.com>
%%% @author Magnus Fröberg <magnus@klarna.com>
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
        , active/1
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

%%%_* Records ==========================================================
-record(state, {}).
-record(circuit_breaker
        { service                            % Term, e.g. {IP|Host, Port}
        , flags        = ?CIRCUIT_BREAKER_OK % Status flags, ?CIRCUIT_BREAKER_*
        , timeout      = 0                   % {N, gnow()} | 0
        , call_timeout = 0                   % {N, gnow()} | 0
        , error        = 0                   % {N, gnow()} | 0
        , reset_fun                          % Fun to check for up status
        , ref                                % Timer reference
        }).

%%%_* Defines ==========================================================
-define(SERVER,        ?MODULE).
-define(TABLE,         ?MODULE).
-define(CALL_TIMEOUT,  10 * 1000).         % 10 seconds
-define(RESET_TIMEOUT, 10 * 60 * 1000).    % 10 minutes.
-define(RESET_FUN,     fun() -> true end).

%%%_* API ==============================================================
-spec start_link() -> {ok, Pid::pid()} | ignore | {error, Reason::term()}.
%% @doc Start circuit_breaker.
%% @end
start_link() -> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
