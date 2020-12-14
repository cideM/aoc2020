# :christmas_tree: Advent of Code 2020 :santa:

## Intro

This repository contains my solutions for Advent of Code 2020 in whatever languages I feel like. Every solution is a single file which you can just run like this `./d2/d2.hs` or `./d2/d2.lua`. The code always starts with a shebang line that runs the code in a Nix shell. That means you don't need to install anything, it's all temporary and self-contained.

The Clojure is to be run by opening the file in your favorite editor and interactively evaluating things. No shebang here.

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

## Progress (14/25)

|     | Haskell | Lua     | Clojure |
| --- | ------- | ------- | ------- |
| 1   | :bell:  | :bell:  | :zzz:   |
| 2   | :bell:  | :bell:  | :zzz:   |
| 3   | :bell:  | :bell:  | :zzz:   |
| 4   | :bell:  | :bell:  | :zzz:   |
| 5   | :bell:  | :zzz:   | :zzz:   |
| 6   | :bell:  | :zzz:   | :zzz:   |
| 7   | :bell:  | :zzz:   | :zzz:   |
| 8   | :bell:  | :zzz:   | :zzz:   |
| 9   | :zzz:   | :bell:  | :zzz:   |
| 10  | :bell:  | :zzz:   | :zzz:   |
| 11  | :bell:  | :zzz:   | :zzz:   |
| 12  | :bell:  | :zzz:   | :zzz:   |
| 13  | :zzz:   | :zzz:   | :bell:  |
| 14  | :zzz:   | :zzz:   | :bell:  |
| 15  | :zzz:   | :zzz:   | :zzz:   |
| 16  | :zzz:   | :zzz:   | :zzz:   |
| 17  | :zzz:   | :zzz:   | :zzz:   |
| 18  | :zzz:   | :zzz:   | :zzz:   |
| 19  | :zzz:   | :zzz:   | :zzz:   |
| 20  | :zzz:   | :zzz:   | :zzz:   |
| 21  | :zzz:   | :zzz:   | :zzz:   |
| 22  | :zzz:   | :zzz:   | :zzz:   |
| 23  | :zzz:   | :zzz:   | :zzz:   |
| 24  | :zzz:   | :zzz:   | :zzz:   |
| 25  | :zzz:   | :zzz:   | :zzz:   |

## Misc

Post code on reddit (use `pbcopy` on MacOS)

```
$ cat code | sed 's/^/    /' | xsel -b`
```
