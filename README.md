[![Build Status](https://travis-ci.org/klarna/circuit_breaker.svg?branch=master)](https://travis-ci.org/klarna/circuit_breaker)

Generic circuit breaker that can be used to break any service that
isn't fully functional. A service can be manually blocked/cleared as well.

See doc/overview.edoc for more info.

Building
========

    rebar3 compile
    rebar3 doc
    rebar3 eunit

Using
=====

```erlang
%% Circuit-breaking
call(State, SomeId) ->
    circuit_breaker:call(
        % service identifier for the circuit
        {myservice, SomeId},

        % function being monitored
        fun() -> some_call(State) end,

        % call timeout
        timer:minutes(1),

        % reset fun, just returns true
        fun() -> true end,

        % timeout before reset in case of break
        timer:minutes(5),

        %% Options
        [
            % Number of errors tolerated per unit of time.
            {n_error, 3},
            {time_error, timer:minutes(30)},

            %% number of calls returning 'timeout' tolerated in a given
            %% period of time.
            {n_timeout, 3},
            % interval to happen
            {time_timeout, timer:minutes(30)},

            %% number of calls not responding in due time tolerated, and
            %% the time interval for the N failures to happen
            {n_call_timeout, 3},
            {time_call_timeout, timer:minutes(25)},

            %% When failing, the {'EXIT', Reason} tuple will be returned from
            %% the call. This list of `Reason's allows to avoid counting specific
            %% errors towards thresholds that can break a circuit.
            {ignore_errors, []}
        ]
    ).
```

test
