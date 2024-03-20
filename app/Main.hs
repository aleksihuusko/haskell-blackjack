module Main where

import System.Random.Shuffle (shuffleM)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Control.Monad (when)

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Enum, Bounded, Eq)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Enum, Bounded, Eq)
data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq)

instance Show Card where
    show (Card rankVal suitVal) = show rankVal ++ " of " ++ show suitVal

type Deck = [Card]
type Hand = [Card]

allSuits :: [Suit]
allSuits = [minBound .. maxBound]

allRanks :: [Rank]
allRanks = [minBound .. maxBound]

fullDeck :: Deck
fullDeck = [Card rankVal suitVal | suitVal <- allSuits, rankVal <- allRanks]

shuffleDeck :: Deck -> IO Deck
shuffleDeck = shuffleM

dealCard :: Deck -> (Maybe Card, Deck)
dealCard [] = (Nothing, [])
dealCard (x:xs) = (Just x, xs)

calculateScore :: Hand -> Int
calculateScore hand = sum $ map cardValue hand
    where
        cardValue (Card rankVal _) = case rankVal of
            Ace -> 11
            King -> 10
            Queen -> 10
            Jack -> 10
            _ -> fromEnum rankVal + 2

adjustForAces :: Hand -> Int
adjustForAces hand
    | score > 21 && numAces > 0 = adjustForAces (filter ((/= Ace) . rank) hand) + score - 10 * numAces
    | otherwise = score
    where
        score = calculateScore hand
        numAces = length $ filter ((== Ace) . rank) hand

playerDecision :: IO String
playerDecision = do
  putStrLn "\nDo you want to (h)it or (s)tand?"
  getLine

getBetAmount :: Int -> IO Int
getBetAmount bankroll = do
  putStrLn $ "\nYour bankroll is €" ++ show bankroll
  putStrLn "How much would you like to bet?"
  betStr <- getLine
  let bet = read betStr :: Int
  if bet > 0 && bet <= bankroll then return bet
  else do
    putStrLn "Invalid bet amount. Please enter a valid amount."
    getBetAmount bankroll

playerTurn :: Deck -> Hand -> IO (Hand, Deck)
playerTurn deck hand = do
  decision <- playerDecision
  case decision of
    "h" -> do
      let (maybeCard, newDeck) = dealCard deck
      case maybeCard of
        Just card -> do
          let newHand = card : hand
          putStrLn "\nYou drew:"
          printHand [card]
          let score = adjustForAces newHand
          putStrLn $ "Your score: " ++ show score
          if score > 21 then do
            putStrLn "Bust! You lose."
            return (newHand, newDeck)
          else
            playerTurn newDeck newHand
        Nothing -> do
          putStrLn "The deck was unexpectedly empty."
          return (hand, deck)
    "s" -> do
      putStrLn "\nYou chose to stand."
      return (hand, deck)
    _ -> do
      putStrLn "Invalid input. Please enter 'h' to hit or 's' to stand."
      playerTurn deck hand

printHand :: Hand -> IO ()
printHand hand = putStrLn $ intercalate ", " (map show hand)

dealerTurn :: Deck -> Hand -> IO (Hand, Deck)
dealerTurn deck hand = do
  let score = adjustForAces hand
  putStrLn "\nDealer's turn..."
  if score < 17 then do
    putStrLn "Dealer draws a card."
    let (maybeCard, newDeck) = dealCard deck
    case maybeCard of
      Just card -> do
        let newHand = card : hand
        putStrLn "Dealer drew:"
        printHand [card]
        dealerTurn newDeck newHand
      Nothing -> do
        putStrLn "The deck was unexpectedly empty. Dealer stands."
        return (hand, deck)
  else do
    putStrLn "Dealer stands."
    return (hand, deck)

determineWinner :: Hand -> Hand -> Int -> IO Int
determineWinner playerHand dealerHand bet = do
    putStrLn "\n--- Game Result ---"
    let playerScore = adjustForAces playerHand
    let dealerScore = adjustForAces dealerHand
    putStrLn $ "Dealer's final score: " ++ show dealerScore
    putStrLn $ "Your final score: " ++ show playerScore
    if playerScore > 21 then do
        putStrLn "You bust! Dealer wins."
        return (-bet)
    else if dealerScore > 21 || playerScore > dealerScore then do
        putStrLn "You win!"
        return bet
    else if playerScore < dealerScore then do
        putStrLn "Dealer wins."
        return (-bet)
    else do
        putStrLn "It's a draw!"
        return 0

gameLoop :: Int -> IO ()
gameLoop bankroll
    | bankroll <= 0 = putStrLn "\nYou're out of money! Game over."
    | otherwise = do
        putStrLn "\n--- New Round ---"
        bet <- getBetAmount bankroll
        shuffledDeck <- shuffleDeck fullDeck
        let (maybePlayerCard1, deckAfterFirstDeal) = dealCard shuffledDeck
        case maybePlayerCard1 of
            Just playerCard1 -> do
                let (maybePlayerCard2, deckAfterSecondDeal) = dealCard deckAfterFirstDeal
                case maybePlayerCard2 of
                    Just playerCard2 -> do
                        let initialPlayerHand = [playerCard1, playerCard2]
                        putStrLn "\nYour initial hand is:"
                        printHand initialPlayerHand
                        (finalPlayerHand, deckAfterPlayerTurn) <- playerTurn deckAfterSecondDeal initialPlayerHand
                        let (dealerCard1, deckAfterDealerFirstCard) = dealCard deckAfterPlayerTurn
                        let (dealerCard2, finalDeck) = dealCard deckAfterDealerFirstCard
                        let initialDealerHand = [dealerCard1, dealerCard2] >>= maybeToList
                        putStrLn "\nDealer's initial hand is:"
                        printHand initialDealerHand
                        (finalDealerHand, _) <- dealerTurn finalDeck initialDealerHand
                        betResult <- determineWinner finalPlayerHand finalDealerHand bet
                        let newBankroll = bankroll + betResult
                        putStrLn $ "\nYour new bankroll is: €" ++ show newBankroll
                        playAgainPrompt newBankroll
                    Nothing -> putStrLn "The deck was unexpectedly empty."
            Nothing -> putStrLn "The deck was unexpectedly empty."

playAgainPrompt :: Int -> IO ()
playAgainPrompt bankroll = do
    putStrLn "Would you like to play another round? (yes/no)"
    decision <- getLine
    when (decision == "yes" || decision == "y") $ gameLoop bankroll
    when (decision /= "yes" && decision /= "y") $ putStrLn "Thanks for playing!"

main :: IO ()
main = do
    putStrLn "\nWelcome to Haskell Blackjack!"
    putStrLn "Starting with a bankroll of €100"
    gameLoop 100
