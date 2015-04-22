csh-eval
==========
An evaluations system for the Computer Science House at RIT.

## Project Goals
- Maintainablility: Should be easy to refactor with confidence.
- Extensibility: Should be easy to extend or modify existing features and
                 integrate with other services.
- Usability: Site should have intelligent design decisions with real use cases
             in mind.

## Setup for development
- Install PostgreSQL (we have been using the latest stable: 9.4)
- Initialize the database: `db/recreatedb.sh`
- Initialize your cabal sandbox: `cabal sandbox init`
- Install the dependencies (with tests) `cabal install --enable-tests --only-dependencies`
- `cabal build`
- `cabal test`
