# Advent of Code 2020 in Haskell

## Quick Start

Each puzzle has two parts, and each part will be represented by one `.hs` file. You can run each file like this:

```sh
$ nix-shell
[nix-shell:/data/private/aoc2020]$ echo "input" | runghc ./day_one_part_one.hs
```

For on-the-fly type checking use `ghcid`

```sh
$ nix-shell
[nix-shell:/data/private/aoc2020]$ ghcid ./day_one_part_one.hs
```

To install packages update `aoc2020.cabal` and run `cabal2nix . > project.nix`. Since `shell.nix` uses `project.nix` this will update environment in the Nix shell.

There are also a few other tools installed for you, such as `ormolu` for formatting, `hlint` for linting and `fast-tags` to generate a `tags` file which Neovim understands.
