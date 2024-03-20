# Blackjack using Haskell

Blackjack game developed with Haskell

The implementation of a simple Blackjack game in Haskell showcases several features of the Haskell language and its functional programming paradigm. This documentation outlines how various aspects of Haskell and functional programming concepts were leveraged to design and develop the game, focusing on the execution model, state, types, abstractions, data, and control flow.

### Execution Model

Haskell is a purely functional language, emphasizing immutability and expressions over statements. The game utilizes Haskell's lazy evaluation strategy, where expressions are not evaluated until their values are needed. This is particularly useful in the `shuffleDeck` function, where the deck is shuffled lazily, and cards are drawn as needed without prematurely evaluating the entire deck.

### State

In functional programming, state changes are handled differently than in imperative languages. Since Haskell lacks mutable state, the game manages state changes (like the player's bankroll or the deck of cards) through the return of new state values from functions, rather than modifying variables in place. For instance, after each turn, `playerTurn` and `dealerTurn` functions return updated hands and decks, reflecting the new game state without altering the original deck or hand directly.

### Types

Haskell’s strong, static type system plays a crucial role in the game's development. Custom data types `Suit`, `Rank`, and `Card` define the structure of a card, while type synonyms `Deck` and `Hand` represent collections of cards. These types make the code more readable and prevent errors, such as mixing up cards with other lists or values.

### Abstractions

The game leverages Haskell’s higher-order functions and abstractions extensively. Functions like `map`, `filter`, and `sum` are used to calculate scores and adjust for Aces, abstracting away the loop and conditional logic found in imperative languages. This leads to concise and expressive code that closely represents the domain logic without the boilerplate of iteration and state management.

### Data

Data in the game is represented using algebraic data types (ADTs) for cards, suits, and ranks, showcasing Haskell’s capability to model complex data structures in a succinct and type-safe manner. The use of ADTs, combined with pattern matching in functions like `calculateScore` and `adjustForAces`, allows for clear and concise processing of game data according to the rules of Blackjack.

### Control Flow

Control flow in Haskell is expressed through recursion and functional composition, rather than loops and conditional branching found in imperative languages. The game loop is managed recursively with the `gameLoop` function calling itself to continue the game until the player decides to stop. Conditionals are expressed through pattern matching and guards, providing a declarative approach to control flow that aligns with the functional paradigm.

### Analysis

Haskell’s features and the functional paradigm supported the development process by offering concise syntax, strong type safety, and clear abstractions that closely model the game’s logic. The immutability and function purity facilitated reasoning about the code and ensured predictable outcomes without side effects. However, managing state and control flow in a purely functional style presented challenges, particularly for developers accustomed to imperative and object-oriented paradigms. The need to pass and return updated state explicitly can be verbose and require careful design to keep the code clean and readable.

In conclusion, Haskell and its functional programming concepts provided a robust foundation for implementing the Blackjack game, demonstrating the strengths of the language in modeling complex logic with high-level abstractions and type safety. While the paradigm offers significant benefits in terms of expressiveness and code safety, it also demands a shift in thinking about state management and control flow, which could be seen as a hindrance or a valuable learning opportunity.
