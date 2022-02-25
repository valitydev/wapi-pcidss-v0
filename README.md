# wapi-pcidss

PCI DSS part of Wallet API written in Erlang
## Building

To build the project, run the following command:

```bash
$ make compile
```

## Running

To enter the [Erlang shell][1] with the project running, run the following command:

```bash
$ make rebar-shell
```

## Development environment

### Run in a docker container

You can run any of the tasks defined in the Makefile from inside of a docker container (defined in `Dockerfile.dev`) by prefixing the task name with `wc-`. To successfully build the dev container you need `Docker BuildKit` enabled. This can be accomplished by either installing [docker-buildx](https://docs.docker.com/buildx/working-with-buildx/) locally, or exporting the `DOCKER_BUILDKIT=1` environment variable.

#### Example

* This command will run the `compile` task in a docker container:
```bash
$ make wc-compile
```

## Documentation

This service implements parts of the [swag-wallets](https://github.com/valitydev/swag-wallets) OpenAPI specification which handle sensitive data. It is designed to be deployed in a PCI DSS compliant zone of the cluster and interacts with other services in this zone (like [cds](https://github.com/valitydev/cds-proto)) to process, store and tokenize various types of payment tools.

@TODO Write more documentation

[1]: http://erlang.org/doc/man/shell.html
