# Elector

## Running the application:
`rebar3 shell`
`erl -sname local -pa ./_build/default/lib/elector/ebin -eval "application:start(elector)"`

## Build the application:
`rebar3 compile`

## Run tests:
`rebar3 compile && ct_run -dir test -logdir test_logs -pa ./_build/default/lib/elector/ebin`

## To debug in tests:
`ct:pal("MyVar: ~p~n", [MyVar]).`