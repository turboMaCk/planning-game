# HOSTING

Official guide to self-hosting this project.

## Important Note

This project doesn't have any dependency on any type of database.
All state is hold in memory to minimize deployment and runtime overhead
(no addition things to setup and maintain).
**Anyway this also means that all the state of app is lost during restart
of application** so please be aware that restarts and redeploys might
affect currently running sessions.

At the moment sessions and tables data are only being added and hold in memory.
This means that memory usage should and will grow over time.
There is a plan to implement process that would be cleaning old data but
this is not implemented yet.

## Configuration

Default configuration can be changed using **ENVIRONMENT VARIABLES**.
Here is a table of all variables with description.

| Var Name                | Default Value | Description                                                |
| ----------------------- | ------------- | ---------------------------------------------------------- |
| `PORT`                  | `3000`        | Port to which server binds after start.                    |
| `GC_EVERY_MIN`          | `30`          | Frequency of Garbage Collection for server state (in mins) |
| `GC_TABLE_MIN_LIFE_MIN` | `120`         | Minimal life of table from creating (in mins)              |

Implementation can be found in [AgilePoker.hs](https://github.com/turboMaCk/planning-game/blob/master/server/AgilePoker.hs)

### Understanding Garbage Collection

Garbage Collector (GC) in terms of [configuration](#configuration) doesn't mean
GHC's run-time garbage collection but Garbage Collection for state of a server.
This application holds all its state in memory in a way this state is shared
between all threads handling HTTP requests and Web Socket connections.
This means state of Sessions and Tables is hold in a data structure.
Garbage Collector is another GHC thread spawn during start of an application
which periodically cleans data in this shared data structure by following rules:

- If table exists for longer than `GC_TABLE_MIN_LIFE_MIN` it can't be garbage collected
- If there is (active) Web Socket connection to the table it can't be garbage collected
- All other tables are removed from state by Garbage Collector
- Sessions are never GCed at the moment (Every session is ByteString hold in Memory)

## Docker

There is [official docker image](https://cloud.docker.com/u/turbomack/repository/docker/turbomack/planning-game)
publish on docker hub.

```
$ docker pull turbomack/planning-game
```

It should be very simple to plug this docker container
into your existing container orchestration tool.

## NixOS

Server runtime depends of presence of `public` dictionary.
Public dictionary can be build by building the `client` attribute.

```
$ nix-build --attr client --out-link www
$ nix-build --attr server
$ export PATH=$(readlink -f result)/bin:$PATH
$ cd www
$ planning-game
```

Alternatively you can build docker image using nix dockerTools
and run it as usual.

```
$ nix-build --attr docker
$ docker load -i result
```

**Contributions improving Nix builds are welcomed**

## Developement

Prefered way of hacking on this app is using [nix package manager](https://nixos.org/nix/).

with Nix installed on your machine you can simply enter shell

```
$ nix-shell
$ ./build-client.sh # builds client
$ cabal build # build server
```

## Performance Tweaks

TL;DR One can try to pass those flags to squeeze some extra runtime performance.

```
$ planning-game +RTS -ki8K -qg
```

*Changing default GHC memory might lead to improvement in space usage*.
By Default GHC increases memory for threads in 32Kb steps which is probably
too big step for application which uses a lot of inexpensive threads like this one.
By passing `+RTS -ki8K` flag threads will be started with 8Kb which should be enough
memory for most of the process.
[more informations](https://github.com/jaspervdj/websockets/issues/72#issuecomment-51074420)

*Disabling parallel GC of GHC can lead to reduction of CPU load.* This can be done by passing
`-qg` flag.
