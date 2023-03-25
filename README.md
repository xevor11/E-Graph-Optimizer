## Haskell with Cabal

This template makes it easy for you to use Haskell with Cabal on Repl.it. 

## Setup

- Fork this repl
- Open the shell (control/cmd + K) 
- Run `cabal init`
- Run `cabal update`
- Add packages to your `.cabal` file as needed, see more at https://cabal.readthedocs.io/en/stable/getting-started.html#creating-a-new-application
- Hit the run button!

## Usage

- Your entrypoint is Main.hs
- Run is linked to `cabal run`, so just hitting run would work!
- To add a new dependency simply add it to Cabal-example.cabal
- To install dependencies make sure to open the shell (control/cmd + K) and run `cabal update && cabal install --only-dependencies`

That's it! This template has `titlecase` as a dependency just for the example.
