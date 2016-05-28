Generic circuit breaker that can be used to break any service that
isn't fully functional. A service can be manually blocked/cleared as well.

See doc/overview.edoc for more info.

Building
========

    rebar compile
    rebar doc
    rebar eunit

Using
=====

```erlang
%% Circuit-breaking
call(State, SomeId) ->
    circuit_breaker:call(
        {myservice, SomeId}, % service identifier for the circuit
        fun() -> some_call(State) end, % function being monitored
        timer:minutes(1), % call timeout,
        fun() -> true end, % reset fun, just returns true
        timer:minutes(5), % timeout before reset in case of break
        [% Number of errors tolerated per unit of time.
         {n_error, 3}
        ,{time_error, timer:minutes(30)}
        %% number of calls returning 'timeout' tolerated in a given
        %% period of time.
        ,{n_timeout, 3}
        ,{time_timeout, timer:minutes(30)} % interval to happen
        %% number of calls not responding in due time tolerated, and
        %% the time interval for the N failures to happen
        ,{n_call_timeout, 3}
        ,{time_call_timeout, timer:minutes(25)}
        %% When failing, the {'EXIT', Reason} tuple will be returned from
        %% the call. This list of `Reason's allows to avoid counting specific
        %% errors towards thresholds that can break a circuit.
        ,{ignore_errors, []}
        ]
    ).
```
