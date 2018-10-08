# tic-tac-toe-clojure

A Tic Tac Toe game written in Clojure.

- [x] Human VS Human
- [x] Computer VS Human
- [x] Computer VS Computer

This game can be played in two different languages (English and French) and there is in-built replay functionality. You can play on any size board as long as it is larger than 0x0. However, the game is currently only optimised for 4x4 boards and under, so if you want to play 5x5 or beyond, you may be there for a couple of centuries...

## Set up
Make sure you have Java, Clojure and Leiningen installed on your machine. These can all be installed via Homewbrew if that's your jam (there are some instructions [here](https://medium.com/@codeidoscope/keep-your-friends-close-and-your-enemies-clojure-523cf6e8de15))

## Running the programme

You can play Tic Tac Toe by running `lein run` from the command line.

## Running the test suite

You can run the tests by running the command  `lein spec` (or `lein spec -a` if you want the test to reload automatically after updating them).
