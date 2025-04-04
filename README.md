# `moon`

Basic Haskell web framework for building applications. (Fully qualified class names are used to enhance code clarity)

It offers file and memory sessions, SQL (MySQL) database integration, and the most basic HTTP routing system.

It's not a full-fledged framework, but it should give you the basis for something useful to build on.

The `Main.hs` file contains some example code.

⚠️ The `Moon.Routing.Router.route` method can capture URL path parameters. In the moon framework, these parameters are not sanitized, which leaves the system vulnerable to malicious attacks, such as XSS injections.

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
