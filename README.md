# `moon`

Basic Haskell web framework for building applications.

It offers file and memory sessions, SQL (MySQL) database integration, and the most basic HTTP routing system.

It's not a full-fledged framework, but it should give you the basis for something useful to build on.

The `Main.hs` file contains some example code.

### Installation

First, enter the project directory:

    $ cd moon

To run with Cabal:

    $ cabal build
    $ cabal run myproject-exe

To run with Stack:

    $ stack build
    $ stack run myproject-exe

Visit `http://localhost:3000` to see the application.
