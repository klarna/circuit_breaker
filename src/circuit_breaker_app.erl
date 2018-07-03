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
%%%
%%% @doc Circuit breaker application
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
