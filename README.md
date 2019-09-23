# `remarks` — A DSL for marking student work

[![License: BSD 3-Clause](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](LICENSE)
[![Travis CI (Linux + macOS) Status](https://travis-ci.org/DIKU-EDU/remarks.svg)](https://travis-ci.org/DIKU-EDU/remarks)
[![AppVeyor (Windows) Status"](https://ci.appveyor.com/api/projects/status/g0hviw442o2bl8yi?svg=true)](https://ci.appveyor.com/project/DIKU-EDU/remarks)

When judging student performance, it is useful to have both small, composable,
quantitative judgements, and qualitative remarks. This makes both spreadsheets
and mere text-files ill-suited for marking student work.  Although
[org-mode](http://orgmode.org/) can solve this problem to a great extent, it
becomes a heavy tool in the light of having to mark hundreds of students in a
distributed fashion. With org-mode, everything is in one file, while global,
intra-student statistics are not needed until all the students have been fully
marked.

## Design Goals

  1. One, or several, human-readable/editable file(s) _per student_.
  2. git-friendly file format.
  3. Export options to spreadsheet-formats.
  4. Synchronization options with Dropbox and/or Google Drive.

Goal 4 is not necessarily related to `remarks`, but is related to marking
student work with external examiners, who are not always willing to use more
explicit version-control systems, such as git.

## Usage

User features:

* `remarks check [<file>]` checks that the file system structure is
  well-formed. For instance, checks that all explicitly stated point sums
  are correct.
* `remarks show [<file>]` checks the file system structure as above, and shows
  the overall judgement for each given argument.
* `remarks summary [--depth <depth>] [<file>]` checks and summarizes the points.
  Depth 0 (default) lists just the top-level judgements.
* `remarks pending [--depth <depth>] [<file>]` shown the corrections that has not
  been completed. Can be cut at a given depth; depth 0 lists just the top-level judgements.
* `remarks export [--format "<format>"] [<file>]` exports corrections to a semicolon separated 
  list. The format is a semicolon separated string of properties.
* `remarks exportHTML [<file>]` exports all corrections to a dynamic html-table.
Developer features:

* `remarks parse [<file>]` parses the given files and shows their ASTs.

## Installation

`remarks` is written in Haskell. If you are new to Haskell, we can recommend
installing either [Haskell Platform](https://www.haskell.org/platform/) or [The
Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/). Follow
the instructions below to then install `remarks`.

`remarks` is on [Hackage](http://hackage.haskell.org/package/remarks), so you
can use [Cabal](https://www.haskell.org/cabal/):

```
$ cabal install remarks
```

If you clone the repository, or [download the
sources](https://github.com/DIKU-EDU/remarks/releases), you get two further
options:

* If you are using the purely functional package manager
  [Nix](https://nixos.org/nix/), you can do this:

  ```
  $ nix-build
  ```

  This will create a symlink `result`, pointing to the directory in your Nix
  store, containing the binary.

* Or, if using [Stack](https://docs.haskellstack.org/en/stable/README/), you
  can do this:

  ```
  $ stack build
  $ stack install
  ```

## Syntax

A `.mrk` file is a list of judgements.

A judgement starts with a header mark (a sequence of `#`), a title (followed by
a `:`), given points (followed by `/`), and maximum points (followed by a line
break). The number of `#` determines the _depth_ of the header, and every file
_must_ start at depth 1, but may have multiple depth 1 judgements. Headings may
be arbitrarily nested, but must sum up correctly. For instance, here is a file
containing only quantitative remarks:

```
# Theory: 27/50
## Question 1: 10/10
## Question 2: 10/20
## Question 3: 7/20
# Practice: 35/50
## Task 1: 20/25
## Task 2: 15/25
```

It is possible to indicate that a student did not submit an answer with writing `-` instead of a number points. This will be treated like 0 points. For instance:

```
# Theory: 17/50
## Question 1: 10/10
## Question 2: -/20
## Question 3: 7/20
```

The header of a judgement may be followed by qualitative remarks. Remarks begin
with an indent (two spaces), and a mood mark:

  * `*` for neutral/structural remarks;
  * `+` for positive remarks;
  * `-` for negative remarks;
  * `~` for mixed remarks;
  * `?` for impartial remarks.

Impartial remarks are good for judgements where the mood is left to be judged
by a higher authority. For instance, when a teaching assistant is uncertain,
and would like to let the teacher decide, or the solution cannot be used to
make a judgement about this point.

Structural remarks are good for listing things to look for. For instance, a
(template) judgement for an operating systems exercise might look something
like this:

```
## T1: /15

### Formal requirements: /5
  * Has code
  * Has XML in a reloadable/testable form
  * Has an explanation

### Quality assessment: /10
  * Understands semaphores
  * Understands deadlocks
  * Understands starvation
  * Avoids deadlocks
  * Avoids starvation
  * Avoids race conditions
  * Has a good degree of multiprogramming
  * Solves the problem

### Bonus: +0
  * Has an accompanying implementation / simulation
```

Once filled in by a teaching assistant, this may look something like this:

```
## T1: 9/15

### Formal requirements: 5/5
  * Has code
    + Yes.
  * Has XML in a reloadable/testable form
    + Yes.
  * Has an explanation
    + Yes.

### Quality assessment: 2/10
  * Understands semaphores
    - Seems to treat them as counters. Report doesn't talk about procuring or
      vacating at all.
    - Checks semaphore value directly.
    - Seems to have mixed up procure and vacate.
  * Understands deadlocks
    - If more than 5 cars arrive, then only 5 cars will be allowed to drive
      through, and everything will then deadlock.
  * Understands starvation
    ? Seems to, but this is hard to judge.
  * Avoids deadlocks
    ? See above.
  * Avoids starvation
    ? Can't judge this.
  * Avoids race conditions
    - Uses a busy loop in attempt to synchronize.
    - There is a race condition after the busy loop.
  * Has a good degree of multiprogramming
    + It's a ticket system, so it could be okay.
  * Solves the problem
    + In a complicated way, but yes.

### Bonus: +2
  * Has an accompanying implementation / simulation
    + Yes!
```

Bonus-judgements _must_ have the title `Bonus`, but they don't have to be there
if you don't like them. The presence of bonus judgments also allows points to
sum up to more than the given maximum. This is regarded as a feature.

## Points and Sums

You do not need to manually enter sums. Points are only required for the
bottom-most judgements. Given the template above you can do a `remarks check`
to check if all necessary points have been given. This makes a "pointless"
template a rather useful starting point for grading.

Points can be integers or half-points, that is, the number may end in `.5`.
Please note, `.5` is exactly representable in an IEEE-754 (the internal
representation of points), so no numerical imprecision occurs when dealing with
half-points. Maximum points may only be integers (the internal representation
of maximum points is still an IEEE-754 double to avoid numerical conversions).

## Files and Directories

The file-format is kept "git-friendly" by keeping it comprehensible in
plain-text, and allowing for independent marking by splitting the remarks for a
student into multiple files.

The simplest setup is to have one `.mrk` file per student (e.g.
[basic.mrk](samples/organization/basic.mrk)).

To support more exotic setups, `remarks` can also work with directories:

  * If supplied with a directory path, `remarks` looks for files ending in
    `.mrk` inside that directory, and comprehends the files as above, in
    lexicographic filename order (e.g.,
    [directory-with-mrk-files](samples/organization/directory-with-mrk-files)).

  * If there exists a directory `<basename>` for any `<basename>.mrk`,
    `<basename>` is recursively searched for further `.mrk` files. Their
    contents is appended, in lexicographic filename order, to the last
    top-level judgement of `<basename>.mrk`
    [mixed-directory](samples/organization/mixed-directory)).

See the [organization samples](samples/organization) for some examples of how
judgements may be structured using files and directories.

```
├── basic.mrk
├── directory-with-mrk-files
│   ├── 01-theory.mrk
│   └── 02-practice.mrk
└── mixed-directory
    ├── 01-theory.mrk
    ├── 02-practice
    │   ├── Task1.mrk
    │   └── Task2.mrk
    └── 02-practice.mrk
```

`basic.mrk`, `directory-with-mrk-files`, and `mixed-directory` all parse to the
same judgements. In particular, the output from the following `remarks`
commands is identical:

```
$ cd samples/organization/
$ remarks parse basic.mrk
$ remarks parse directory-with-mrk-files
$ remarks parse mixed-directory
```
