# Elector

![example workflow](https://github.com/alfetahe/elector/actions/workflows/erlang.yml/badge.svg)

## Description
Elector is an Erlang application that automatically detects all nodes inside the distributed Erlang cluster and chooses the leader node.
The elections are started automatically when the Elector application is started or when a node joins or leaves the cluster.
Elector also allows you to run pre- and post-election hooks that will be triggered when the election process is started and finished.

The default election strategy is to choose the node with the highest uptime.

## Features

- Automatic election process on startup or when node joins/leaves the cluster
- Ability to configure pre and post election hooks that will be called before and after the election process. **These hooks must return within 1 second to avoid timeouts**.
- Provides 4 built in election strategies (time based) and allows you to define your own strategy implementation (See `elector_strategy_behaviour` module for reference)
- Provides quorum option to detect split brain scenarios
- Provides option to define the election delay in milliseconds before the election process is started automatically and queue the election calls

## Configuration

Elector supports the following configurations:

- `election_delay` - The delay in milliseconds before the new election starts. This value is used when automatic election is triggered either by node join/leave or startup. Default value is 1 second (1000).

- `strategy_module` - The module that is used for the election strategy implementation. Available options are: `elector_rt_high_strategy` and `elector_rt_low_strategy`. Feel free to write your own strategy module that implements the `elector_strategy_behaviour` module. Default value is `elector_rt_high_strategy` which chooses the node with the highest runtime.

- `pre_election_hooks` - A list of hooks/function calls that will be triggered exactly before the node is starting the election. Expects a list of tuples with the following format: `{Module, Function, Args}`. Default value is `[]`.

- `post_election_hooks` - A list of hooks/function calls that will be triggered exactly after the election process. Expects a list of tuples with the following format: `{Module, Function, Args}`. Default value is `[]`.

- `quorum_size` - The number of nodes (including the local node) that should be available in the cluster before the election process is started. Do not set it to `0` as it will disable the election process, leave empty or `1` if you want to run the election process even if there are no other nodes in the cluster. Default value is `1`.

- `candidate_node` - Boolean indicating if this node should be a candidate for the leader election. Default value is `true`.

- `hooks_execution` - Atom indicating if the pre and post election hooks should be executed on the node starting the election process or on all nodes. The election process will be started automatically by the global commission process. The node who starts the commission process is not known and can be any node in the cluster. Available values are: `local` and `global`. Default value is `global`.

- `automatic_elections` - Boolean indicating if the automatic election process should be started when node joins or leaves the cluster. Default value is `true`.

- `candidate_cache_refresh_interval` - The interval in seconds for periodic candidate cache refresh. Set to `0` to disable periodic refresh (recommended - uses only event-based updates on node join/leave). Default value is `0`.

Keep in mind to use the same configuration for all nodes in the cluster!

## Guides

### Installation for Elixir application

1. Add `{:elector, "~> 0.3.2"}` under the deps in the `mix.exs` file:

```elixir
defp deps do
    [
        {:elector, "~> 0.3.2"}
    ]
end
```

2. Add `elector` under the extra_applications in the `mix.exs` file:

```elixir
def application do
    [
        extra_applications: [:elector],
        mod: {MyApp, []}
    ]
end
```

### Installation for Erlang application

1. Add `elector` to the deps in the `rebar.config` file:

```erlang
{deps, [{"elector", "0.3.2"}]}.
```

2. Add `elector` to the `applications` list in the `myapp.app.src` file:

```erlang
{applications, [elector]}.
```

## Examples

### Start election manually

#### Elixir
```elixir
> alias :elector, as Elector
> Elector.elect_sync() # Start election synchronously
{:ok, :election_finished}
> Elector.elect() # Start election asynchronously
{:ok, :election_started}
```
#### Erlang
```erlang
> elector:elect_sync(). % Start election synchronously
{ok, election_finished}
> elector:elect(). % Start election asynchronously
{ok, election_started}
```

### Get current leader

#### Elixir
```elixir
> alias :elector, as Elector
> Elector.get_leader()
{:ok, :example_node}
```

#### Erlang
```erlang
> elector:get_leader().
{ok, example_node}
```

See the `elector` module for more.

https://hexdocs.pm/elector/


## High level overview
Elector is a distributed application that utilizes the Erlang global module to spawn a singleton process named
`elector_commission`. The commission process is responsible for starting the election process and keeping track of the nodes in the cluster.
The election process is started by the commission process, which calls the `elect/1` function from the strategy module.
This function gathers the necessary information from the cluster and decides which node should become the leader.

Each node spawns the following local processes:
- `elector_overviewer` process is responsible for starting the commission and monitor it across the cluster.
- `elector_candidate` process holding the candidate information that describes if the node is a candidate for the leader election.
- `elector_state` process holding the selected leader information.

All processes are supervised by the `elector_sup` supervisor.


## Setup Elector locally

### Setup the elector locally and run the application:
- `docker-compose up -d`
- `docker exec -it elector_elector_1 sh`
- `rebar3 compile`
- `erl -sname local -setcookie cookievalue -pa ./_build/default/lib/elector/ebin -eval "application:start(elector)"`

### Run tests:
- `rebar3 compile && ct_run -dir test -logdir test_logs -pa ./_build/default/lib/elector/ebin -setcookie cookievalue`

### Generate documentation:
- `rebar3 ex_doc`