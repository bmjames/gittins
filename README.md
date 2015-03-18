# Gittins

Gittins is a simple tool for managing multiple Git repositories.

## Requirements

Building Gittins requires [Cabal][1]. Running it requires [Git][2].

## Usage

Gittins for the impatient: `gittins --help`, `gittins <command> --help`

Gittins for the patient is not written yet.

## Parallelism

Git processes are run in the `ParIO` monad from [monad-par][3]. You can control the level of parallelism with GHC's SMP parallelism options. For example, `gittins pull +RTS -N4` will run `git pull` for all of your registered repositories, using a maximum of 4 workers at a time.

## Credits

Gittins is inspired by [myrepos][4], but has much less functionality. On the other hand, Gittins is not written in Perl, so I can enjoy hacking on it.

[1]: http://www.haskell.org/cabal
[2]: http://git-scm.com
[3]: http://hackage.haskell.org/package/monad-par
[4]: http://myrepos.branchable.com
