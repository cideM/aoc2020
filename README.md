# :christmas_tree: Advent of Code 2020 :santa:

## Intro

This repository contains my solutions for Advent of Code 2020 in whatever languages I feel like. Every solution is a single file which you can just run like this `./d2/d2.hs` or `./d2/d2.lua`. The code always starts with a shebang line that runs the code in a Nix shell. That means you don't need to install anything, it's all temporary and self-contained.

There are also a few other tools installed for you in the base Nix shell, such as `ormolu` for formatting Haskell and `hlint` for linting, and so on.

## Preparation

Start `cat` and wait for input

```shell
$ mkdir d2
$ cat > d2/input.txt
```

Reload and run

```shell
$ echo "d2/d2.hs" | entr -sc "cat d2/input.txt | ./d2/d2.hs"
```

## Progress (6/25)

|     | Haskell | Lua     |
| --- | ------- | ------- |
| 1   | :bell:  | :bell:  |
| 2   | :bell:  | :bell:  |
| 3   | :bell:  | :bell:  |
| 4   | :bell:  | :bell:  |
| 5   | :bell:  | :zzz:   |
| 6   | :bell:  | :zzz:   |
| 7   | :bell:  | :zzz:   |
| 8   | :zzz:   | :zzz:   |
| 9   | :zzz:   | :zzz:   |
| 10  | :zzz:   | :zzz:   |
| 11  | :zzz:   | :zzz:   |
| 12  | :zzz:   | :zzz:   |
| 13  | :zzz:   | :zzz:   |
| 14  | :zzz:   | :zzz:   |
| 15  | :zzz:   | :zzz:   |
| 16  | :zzz:   | :zzz:   |
| 17  | :zzz:   | :zzz:   |
| 18  | :zzz:   | :zzz:   |
| 19  | :zzz:   | :zzz:   |
| 20  | :zzz:   | :zzz:   |
| 21  | :zzz:   | :zzz:   |
| 22  | :zzz:   | :zzz:   |
| 23  | :zzz:   | :zzz:   |
| 24  | :zzz:   | :zzz:   |
| 25  | :zzz:   | :zzz:   |

## Misc

Post code on reddit (use `pbcopy` on MacOS)

```
$ cat code | sed 's/^/    /' | xsel -b`
```
