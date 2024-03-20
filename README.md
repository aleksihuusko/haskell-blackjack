# Haskell Blackjack

A simple command-line Blackjack game implemented in Haskell. Experience the classic casino game with a twist of functional programming, allowing you to play against the dealer, make bets, and manage your bankroll. Whether you're new to Haskell or looking to see how a functional programming paradigm handles a traditional game, this project offers insights and entertainment.

## Prerequisites

Before you run the game, make sure you have the following installed on your system:
- [Glasgow Haskell Compiler (GHC)](https://www.haskell.org/ghc/)
- [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/) or [Cabal](https://www.haskell.org/cabal/), depending on your preference for managing Haskell projects.

This game also uses the `random-shuffle` package for shuffling the deck of cards.

## Installation

1. Clone this repository to your local machine using:

```bash
git clone https://github.com/aphuus/haskell-blackjack.git
cd haskell-blackjack
```

2. Ensure you have the `random-shuffle` package installed. You can do this by running:

For Stack users:
```bash
stack install random-shuffle
```

For Cabal users:
```bash
cabal update
cabal install random-shuffle
```

## Running the Game

To compile and run the game, follow the steps based on your tool of choice (Stack or Cabal).

### Using Stack

1. Compile the game using Stack:
```bash
stack build
```

2. Run the compiled game:
```bash
stack exec haskell-blackjack-exe
```

### Using Cabal

1. Compile the game with Cabal:
```bash
cabal build
```

2. Run the compiled game:
```bash
cabal run
```

## Gameplay

After starting the game, you will be prompted to place your bet and decide whether to hit or stand during each round, just like in traditional Blackjack. The game continues until you decide to stop or run out of money.

Enjoy playing Blackjack in Haskell and may the odds be in your favor!

## License

Distributed under the MIT License. See `LICENSE` for more information.
