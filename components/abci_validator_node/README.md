# Tendermint consensus integration
The Findora ledger, replicated on multiple consensus nodes each with the same RESTful APIs and local state as a standalone ledger, can be run with the tendermint consensus protocol.
* To set up, [use these files](https://github.com/findoraorg/service-dockers/tree/master/tendermint_validator).

You should be able to use the scripts to control it.
It requires docker and docker-compose to be installed.
```
# Prepare directories for the dockers
./manage-tendermint init
# Start dockers
./manage-tendermint start
# Shutdown dockers and clean up
./manage-tendermint stop
# Do a clean stop and restart
./manage-tendermint restart
# Destroy everything and start from scratch
./manage-tendermint reset
```

Note for MacOS X developers testing locally: by default, docker on darwin does not recognize `/var` as an exported mount point.
To use these commands successfully on MacOS X, add the line `export TM_DOCKER_ROOT=$HOME/var` (or any other valid mount point) to your `.profile`.

If you are testing a locally built instance, you'll want to put your version of `abci_validator_node` in each container.
The easiest way to do this would be build the docker using the dockerfile included in the above files.
Just change the abci_validator_node copy directive to pull the file from your system.

```
#Change from
COPY --from=platform /app/abci_validator_node /usr/bin/abci_validator_node
#to
COPY /path/to/your/abci_validator_node /usr/bin/abci_validator_node
```

If you want to test with a four node cluster then you can use [this docker compose](https://github.com/findoraorg/findora-admin/blob/master/configuration-management/salt/tendermint/docker-compose.yml)
