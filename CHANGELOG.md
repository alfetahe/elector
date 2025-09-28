# Change log

## v0.3.1 - 2025.09.28

### Changed:
- `hooks_execution` - Default value changed from `global` to `local`. This means that by default, the pre and post election hooks will now be executed only on the node that initiates the election process. This change aims to reduce unnecessary execution of hooks on all nodes, which can be particularly beneficial in larger clusters where such operations might lead to increased load and potential performance issues.

### Fixed:
- Fixed `:undef` error in `candidate_nodes/0` function by replacing anonymous function with named exported function for remote calls via `erpc`
- Improved error handling in `candidate_nodes/0` to gracefully handle missing `elector_candidate` processes on remote nodes
- Added proper timeout and try-catch blocks for better resilience in distributed scenarios

### Improved:
- Migrated documentation from edoc to hex docs for better user experience
- Enhanced module and function documentation with better examples and formatting
- Updated build system to use `rebar3 ex_doc` instead of `rebar3 edoc`


## v0.3.0 - 2023.07.28
The existing API functions did not change but the overall architecture did change meaning it
is recommended to restart the elector application on all nodes after upgrading 
to this version.

### Update rebar.config deps list: 
```{deps, [{elector, "0.3.0"}]}.```

### Changed:
- The previous implementation started the election process on all nodes. The new version utilizes a global process(singleton) named 'commission' which will initiate the election and gathers information from the cluster. Based on this information, the 'commission' process will determine the leader node.
The selected node information will then be propagated to all other nodes in the cluster. This significant change grants us better control over the election process and helps reduce bandwidth usage.
- Default election strategy is now `elector_ut_high_strategy` which selects the node with the highest uptime as the leader.

### Added
- `candidate_node` configuration option which will allow us to leave the node out of the election process. Default value is `true`.
- `hooks_execution` - configuration option which defines if the hooks should be executed on all nodes or only on the commission node. Default value is `global`.
- `automatic_elections` configuration option which will allow us to disable the automatic election process started by the commission when node joins or leaves the cluster. Default value is `true`.
- `elector_config_handler:add_pre_election_hook/3` - function to add new pre election hook.
- `elector_config_handler:add_post_election_hook/3` - function to add new post election hook.
- `elector_config_handler:rem_pre_election_hook/3` - function to remove pre election hook.
- `elector_config_handler:rem_post_election_hook/3` - function to remove post election hook.
- 2 new strategies that are based on node uptime.

### Removed
- `startup_hooks_enabled` configuration option

## v0.2.2 - 2023.04.02
### Update rebar.config deps list: 
```{deps, [{elector, "0.2.2"}]}.```

### Changed:
- Calling manually elect functions will trigger the election globally
on all nodes
- Doc directory to git
- Test logs directory to git
- Updated documentation and readme file

### Added
- New API function clear_leader/0 with test suite
- Added more assertions for elector:elect and elector:elect_sync functions

### Fixed
- Fixed typo on the elect cast function gen_server's name.

## v0.2.1 - 2023.03.19
### Update rebar.config deps list: 
```{deps, [{elector, "0.2.1"}]}.```

### Changed:
- Made post and pre election hooks synchronous
- Updated github CI settings
- Updated documentation

### Fixed:
- Renamed modules to avoid collisions with other applications
- Post election hooks were not running after the election fix


## v0.2.0 - 2023.03.16
### Update rebar.config deps list: 
```{deps, [{elector, "0.2.0"}]}.```

### Added:
- New configuration option `quorum_size` to set the minimum number of nodes that should be available in the cluster before the election process is started.

### Changed:
- Updated Erlang version inside the docker-compose.yml
- Set the user and group for the docker container to `1000:1000` to match the host user and group.
- Updated documentation
- Enabled all features for the BEAM using the `ERL_AFLAGS=-enable-feature all` inside the docker-compose.yml file.

### Fixed:
- Fixed running the ct_run command inside the docker container
- Removed duplicate `project_plugins` from the rebar.config file

## v0.1.1 - 2023.01.29
### Updated edocs overview section.