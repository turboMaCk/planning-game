# HOSTING

Official guide to selfhosting this project.

## Important Note

This project doesn't have any dependency on any type of database.
All state is hold in memmory to minimize deployment and runtime overhead
(no addition things to setup and maitain).
**Anyway this also means that all the state of app is lost during restart
of application** so please be aware that restarts and redeploys might
affect currently running sessions.

At the moment sessions and tables data are only being added and hold in memmory.
This means that memmory usage should and will grow over time.
There is a plan to implement process that would be cleaning old data but
this is not implemented yet.

## Configuration

The only configuration available at this moment is to change port
server is listening on. This can be done by changing setting
`PORT` environment variable. Default port is `3000`.

## Docker

There is [official docker image](https://cloud.docker.com/u/turbomack/repository/docker/turbomack/agile-poker)
publish on docker hub.

```
$ docker pull turbomack/agile-poker
```

It should be very simple to plug this docker container
into your existing container orchestration tool.

## NixOS

Currently billder for client is unpure as it uses `elm` to fetch dependecies from
packages.elm-lang.org. Because of this `nix-build --attr client` doesn't work on
NixOS. It should be possible to simply build this package for NixOS but
one must follow those steps:

- start `nix-shell`
- build client using `./build-client.sh` within nix shell
- build server using `nix-build --attr server`
- make sure `public` directory is available

**Contributions improving Nix builds are welcomed**
