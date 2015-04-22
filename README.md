csh-eval
==========
An evaluations system for the Computer Science House at RIT.

## Project Goals
- Maintainablility: Should be easy to refactor with confidence.
- Extensibility: Should be easy to interface with
- Usability: site should have intelligent design decisions with real use cases
             in mind

## Setup for development
- install postgreSQL (we have been using the latest stable: 9.4)
- initialize the db: `db/recreatedb.sh`
- initialize your cabal sandbox: `cabal sandbox init`
- install the dependencies (with tests) `cabal install --enable-tests --only-dependencies`
- build. `cabal build`
- test. `cabal test`
