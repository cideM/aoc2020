# :christmas_tree: Advent of Code 2020 :santa:

## Preparation

Start `cat` and wait for input

```shell
$ cat > inputs/d2.txt
```

Reload and run

```shell
$ echo "d2.hs" | entr -sc "cat inputs/d2.txt | runghc d2.hs"
```

Skip type checking since you'll get type errors from `runghc` anyway

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

To install packages update `shell.nix`, specifically the `pkgs: [ INSERT_PKGS_HERE ]` part. Package names are like in Hackage but packages come from Nixpkgs.

There are also a few other tools installed for you, such as `ormolu` for formatting and `hlint` for linting.

## Progress (2/25)

|     | Done    |
| --- | ------- |
| 1   | :bell:  |
| 2   | :bell:  |
| 3   | :zzz:   |
| 4   | :zzz:   |
| 5   | :zzz:   |
| 6   | :zzz:   |
| 7   | :zzz:   |
| 8   | :zzz:   |
| 9   | :zzz:   |
| 10  | :zzz:   |
| 11  | :zzz:   |
| 12  | :zzz:   |
| 13  | :zzz:   |
| 14  | :zzz:   |
| 15  | :zzz:   |
| 16  | :zzz:   |
| 17  | :zzz:   |
| 18  | :zzz:   |
| 19  | :zzz:   |
| 20  | :zzz:   |
| 21  | :zzz:   |
| 22  | :zzz:   |
| 23  | :zzz:   |
| 24  | :zzz:   |
| 25  | :zzz:   |

## Compiling

If you're in the Nix shell GHC knows where to find all dependencies.

`$ ghc d1.hs`
