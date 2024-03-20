module Main where

import System.Random.Shuffle (shuffleM)
import Data.List (intercalate)
import Data.Maybe (maybeToList)

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
  putStrLn "Do you want to (h)it or (s)tand?"
  getLine

playerTurn :: Deck -> Hand -> IO (Hand, Deck)
playerTurn deck hand = do
  decision <- playerDecision
  case decision of
    "h" -> do
      let (maybeCard, newDeck) = dealCard deck
      case maybeCard of
        Just card -> do
          let newHand = card : hand
          putStrLn "You drew:"
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
    "s" -> return (hand, deck)
    _ -> do
      putStrLn "Invalid input. Please enter 'h' to hit or 's' to stand."
      playerTurn deck hand

printHand :: Hand -> IO ()
printHand hand = putStrLn $ intercalate ", " (map show hand)

dealerTurn :: Deck -> Hand -> IO (Hand, Deck)
dealerTurn deck hand = do
  let score = adjustForAces hand
  if score < 17 then do
    putStrLn "Dealer draws a card."
    let (maybeCard, newDeck) = dealCard deck
    case maybeCard of
      Just card -> do
        let newHand = card : hand
        printHand [card]
        dealerTurn newDeck newHand
      Nothing -> do
        putStrLn "The deck was unexpectedly empty. Dealer stands."
        return (hand, deck)
  else do
    putStrLn "Dealer stands."
    return (hand, deck)

-- Function to determine the winner
determineWinner :: Hand -> Hand -> IO ()
determineWinner playerHand dealerHand = do
  let playerScore = adjustForAces playerHand
  let dealerScore = adjustForAces dealerHand
  putStrLn $ "Dealer's final score: " ++ show dealerScore
  putStrLn $ "Your final score: " ++ show playerScore
  case compare playerScore dealerScore of
    GT -> putStrLn "You win!"
    LT -> putStrLn "Dealer wins!"
    EQ -> putStrLn "It's a draw!"

main :: IO ()
main = do
    shuffledDeck <- shuffleDeck fullDeck
    putStrLn "Dealing initial hands..."
    let (maybePlayerCard1, deckAfterFirstDeal) = dealCard shuffledDeck
    case maybePlayerCard1 of
        Just playerCard1 -> do
            let (maybePlayerCard2, deckAfterSecondDeal) = dealCard deckAfterFirstDeal
            case maybePlayerCard2 of
                Just playerCard2 -> do
                    let initialPlayerHand = [playerCard1, playerCard2]
                    putStrLn "Your initial hand is:"
                    printHand initialPlayerHand
                    let initialPlayerScore = adjustForAces initialPlayerHand
                    putStrLn $ "Your initial score: " ++ show initialPlayerScore
                    if initialPlayerScore == 21 then
                      putStrLn "Blackjack! You win!"
                    else do
                      (finalPlayerHand, deckAfterPlayerTurn) <- playerTurn deckAfterSecondDeal initialPlayerHand
                      let finalPlayerScore = adjustForAces finalPlayerHand
                      putStrLn $ "Your final score: " ++ show finalPlayerScore
                      if finalPlayerScore > 21 then
                        putStrLn "Bust! Dealer wins."
                      else do
                        putStrLn "Dealer's turn..."
                        let (dealerCard1, deckAfterDealerFirstCard) = dealCard deckAfterPlayerTurn
                        let (dealerCard2, finalDeck) = dealCard deckAfterDealerFirstCard
                        let initialDealerHand = [dealerCard1, dealerCard2] >>= maybeToList
                        printHand initialDealerHand
                        (finalDealerHand, _) <- dealerTurn finalDeck initialDealerHand
                        determineWinner finalPlayerHand finalDealerHand
                Nothing -> putStrLn "The deck was unexpectedly empty."
        Nothing -> putStrLn "The deck was unexpectedly empty."