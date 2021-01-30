<div align="center">
    <h1>Planning Game</h1>
    <p>Planning tool for remote teams.</p>
    <!-- Badges -->
    <a href="https://travis-ci.org/turboMaCk/planning-game">
        <img src="https://travis-ci.org/turboMaCk/planning-game.svg?branch=master" alt="build">
    </a>
    <a href="https://www.gnu.org/licenses/agpl-3.0.en.html">
        <img src="https://img.shields.io/badge/license-AGPLv3-brightgreen.svg" alt="AGPLv3">
    <a>
    <p><a href="http://planning-game.com">planning-game.com</a></p>
</div>

![screenshot](docs/screenshot.png)

Simple voting poker implementation build with efficiency,
flexibility and easy deployment in mind.
Enabling remote teams to effectively identify complexity
and unclear requirements in collaborative way.

## Quick Start

Use free hosted service at [planning-game.com](http://planning-game.com)

### Using Container

Run official container locally:

```shell
$ docker run -p 3000:3000 --rm -it turbomack/planning-game
```

### Using Nix

**Optionaly** one can use [Cachix cache of the project](https://app.cachix.org/cache/planning-game):

```shell
$ cachix use planning-game
```

Install server binary:

```
$ nix-env -iA server -f https://github.com/turboMaCk/planning-game/archive/master.tar.gz
```

Server serves client side assets from the file system so it should ran within directory
where client files are present. To provide them one can use nix-build:

```
$ nix-build -A client https://github.com/turboMaCk/planning-game/archive/master.tar.gz
```

Nix build will create `result` symlink pointing to nix-store containing all the required client side assets.
You can then just run server within this directory:

```
$ cd result; planning-game
```

Server will run on https://localhost:3000

## Links

- [Cookies Usage](docs/COOKIES.md)
- [Contributing Guidelines](CONTRIBUTING.md)
- [Self Hosting](docs/HOSTING.md)

## License

This work contains **derivation** of [Scrum-poker-cards](https://github.com/redbooth/Scrum-poker-cards)
by [redbooth](https://redbooth.com/) used under [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/).

Planning Game is Free Software released under [AGPLv3](https://www.gnu.org/licenses/agpl-3.0.en.html).
