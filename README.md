# Elector

![example workflow](https://github.com/alfetahe/elector/actions/workflows/erlang.yml/badge.svg)

## Description
Elector is Erlang application that automatically detects all nodes inside the Erlang cluster and choose the leader node.
The election are started automatically when the Elector application is started or when node joins/leaves the cluster.
Elector also allows to run pre and post hooks that will be triggered when the election process is started and finished.

The default election strategy is to choose the node with the highest runtime.

Elector supports the following configurations:
- `election_delay` - The delay in milliseconds before the new election starts. This value is used automatic election is triggered either by node join/leave or startup. Default value is 1 second(1000).
- `sync_start` - If true the election will start synchronously on start up. Set it `false` if start up should be async. Default is `true`.
- `strategy_module` - The module that is used for the election strategy
implementation. Default is `runtime_high_strategy` which chooses the node with the highest runtime.
Available options are: `runtime_high_strategy` and `runtime_low_strategy`. Feel free to write your own strategy module that implements the `strategy_behaviour` module.
`pre_election_hooks` - A list of hooks/function calls that will be triggered exactly before the node is starting the election. Expects
a list of tuples with the following format: `{Module, Function, Args}`. Default is `[]`.
`post_election_hooks` - A list of hooks/function calls that will be triggered exactly after the election process. Expects
a list of tuples with the following format: `{Module, Function, Args}`. Default is `[]`.
- `startup_hooks_enabled`- If true the `pre_election_hooks` and `post_election_hooks` will be triggered on startup. Default is `true`.

## Guides

### Installation for Elixir application
Add `{:elector, "~> 1.0"}` under the deps in the `mix.exs` file: 
```
defp deps do
    [
        {:elector, "~> 1.0"}
    ]
end
```
Add `elector` under the extra_applications in the `mix.exs` file:
```
def application do
    [
        extra_applications: [:elector],
        mod: {MyApp, []}
    ]
end
```

### Installation for Erlang application
Add `elector` to the deps in the `rebar.config` file: 
- `{deps, [{elector, {git, "git://github.com/alfetahe/elector.git", {tag, "v0.1.0"}}}]}.`.
- Next start the `elector` inside your .app start function: `application:start(elector).`
'''

### Start election manually
Elixir: `:elector.elect_sync()` or `:elector.elect()`

Erlang: `elector:elect_sync()` or `elector:elect()`

### Get current leader
```
> alias :elector, as Elector
> Elector.get_leader()
{:ok, :example_node}
```

```
> elector:get_leader().
{ok, example_node}
```

See the `elector` module for more.

## For contributors:

### Setup the elector locally and running the application:
- `docker-compose up -d`
- `docker exec -it elector_elector_1 sh`
- `rebar3 compile`
- `erl -sname local -pa ./_build/default/lib/elector/ebin -eval "application:start(elector)"`

### Run tests:
- `rebar3 compile && ct_run -dir test -logdir test_logs -pa ./_build/default/lib/elector/ebin`

### Generate documentation:
- `erl -noshell -run edoc_run files '["src/elector.erl"]' ' [{dir, "docs"}]'`