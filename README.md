## Sniper

An interface for watching Handshake auctions. Uses [Namebase]'s APIs, so
linking back to their servers.

### Install/Run

On Ubuntu, install [Elm] and run the reactor in this directory:

    npm install elm
    elm reactor

That will run it without css :shrug:

To run with css, compiling every 5 seconds:

    watch -n 5 elm make src/Main.elm --debug --output=sniper.js


### Build

    elm make src/Main.elm --optimize --output=sniper.js

The two files that need to be deployed are `index.html` and `sniper.js`.


[Elm]: https://elm-lang.org/
[Namebase]: https://www.namebase.io/
