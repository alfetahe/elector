# Change log

## v0.1.1 - 2023.01.29
### Updated edocs overview section.

## v0.2.0 - 2023.03.16
### Update rebar.config deps list: 
```{deps, [{elector, "2.0"}]}.```

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