# Elector

## Running the application:
`rebar3 shell`
`erl -pa ebin`

## Build the application:
`rebar3 compile`

## Run tests:
`ct_run -dir test -logdir test_logs -pa ./_build/default/lib/elector/ebin`
`ct_run -dir test -logdir test_logs -pa ./_build/default/lib/elector/ebin -refresh_logs`