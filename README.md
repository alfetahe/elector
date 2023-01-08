# Elector

## Running the application:
`rebar3 shell`
`erl -pa ebin -pa ./_build/default/lib/elector`

## Build the application:
`rebar3 compile`

## Run tests:
`rebar3 compile && ct_run -dir test -logdir test_logs -pa ./_build/default/lib/elector/ebin`