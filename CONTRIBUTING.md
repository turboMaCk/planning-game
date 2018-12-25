# Contributing to Project

The only officialy supported installation is via NIX package manager.
NIX takes care of installing all necessary dependencies including
compilers required to build the project.

## Pre requirements

Please make sure you have [nix](https://nixos.org/) installed on your system using instractions
from official website.

On most systems nix can be installed using:

```
$ curl https://nixos.org/nix/install | sh
```

You also need global `cabal` installation. This can be cabal
you already globally have in your system or alternatively you
can install it via nix.

```
$ nix-env --install cabal-install
```

Don't forget to update;)

```
$ cabal update
```

## Building

Builds for client and server are sparate attributes
in `default.nix`.

**Build server**

```
$ nix-build --attr server
```

**Build client**

```
$ nix-build --attr client
```

## Interactive Developement

After clonning this project you need to run `nix-shell` within project directory.

```shell
$ nix-shell
```

this will should build all the dependencies and start BASH in NIX environment.

Within the nix shell you then should be able to run following commands:


**Start prebuilded server:**

```shell
$ agile-poker
```

**Build server to binary:**

```shell
$ cabal build
```

**Build and start server:**

```shell
$ cabal run
```

**Run GHCi:**

```shell
$ cabal shell
```

**Run GHCID**

```shell
$ ghcid
```

**Build front-end**

```shell
$ ./build-client.sh
```

## Contributing

If you're thinking about contributing to this project but don't know where to start
feel free to open an issue.
