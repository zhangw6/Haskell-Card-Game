
--  File     : Proj1.hs
--  Author   : Wen Zhang
--  Purpose  : Program for guessing the right N-card group 

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List
import Data.Array


-- |allCards are defined as the list of all possible card in this game
--  from minimal bound to maximal bound
allCards = [minBound..maxBound]::[Card]


-- |GameState are defined as the list of lists of Cards
type GameState = [[Card]]

-- |One of three main functions of this prgram
--  initialGuess 1 will return error as not enough card number for this game
--  initialGuess 2,3,4 will return the first guess as [2C,6C], [2C,4C,6C],
--  and[2C,4C,6C,8C] respectively
--  GameStates are used to stored all possible candidate answers for the guess initially
initialGuess :: Int -> ([Card],GameState)
initialGuess n 
            |n<=1 =error"not enough card number"
            |n==2 = ([Card Club R2, Card Club R6], filter (doubleCardFilter) (sequence (initialPool 2)))
            |n==3 = ([Card Club R2, Card Club R4,Card Club R6], filter (doubleCardFilter) (sequence (initialPool 3)))      
            |n==4 = ([Card Club R2, Card Club R4,Card Club R6,Card Club R8], filter (doubleCardFilter) (sequence (initialPool 4)))      

-- |One of three main functions of this prgram
--  cc is the correct cards of guess with the answer
--    with both suits and ranks;
--  lt is the number of cards in the answer having rank 
--    lower than the lowest rank in the guess;
--  eg counts how many of the cards in the answer have 
--    the same rank as a card in the guess;
--  gt counts how many cards in the answer have rank higher 
--    than the highest rank in the guess;
--  correctSuit counts how many of the cards in the answer 
--    have the same suit as a card in the guess, only 
--    counting a card in the guess once.

feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback cardsA cardsG = (cc,lt,eq,gt,correctSuit) where 
          cc = cg cardsA cardsG
          lt =lowerThan cardsA cardsG
          eq = correctRankCard cardsA cardsG
          gt = higherThan cardsA cardsG
          correctSuit = correctSuitCard cardsA cardsG 

-- |Used in Function initailGuess;
--  To filter out card groups with any duplicate cards
--  Retuen true if there is no duplicate in the card group
--  Retuen false if there is duplicate in the card group
doubleCardFilter :: [Card]->Bool
doubleCardFilter [] =True
doubleCardFilter (c:cards) 
              |cards == []   = True
              |c == (head cards)  = False
              |otherwise    = not (c `elem` cards) && doubleCardFilter cards

 
-- |Used in Function initailGuess;
--  To genereate the card in format like 
--    [[2C,3C,4C..AS][2C,3C,4C..AS]] for 2
--    [[2C,3C,4C..AS][2C,3C,4C..AS][2C,3C,4C..AS]] for 3
--    [[2C,3C,4C..AS][2C,3C,4C..AS][2C,3C,4C..AS]] for 4 
--   Used for function:sequence later            
initialPool :: Int->[[Card]]
initialPool 1  = [allCards]
initialPool n = allCards:initialPool (n-1)

-- |Generate the list of Ranks for a list of Cards
--  The list of Ranks are unique and sorted in ascending order
eR :: [Card] -> [Rank]
eR [] = []
eR (x:xs) = sort (nub ([(rank x)] ++ (eR xs)))

-- |Generate the list of Ranks for a list of Cards
--  The list of Ranks are sorted in ascending order
--  but not unique
eR' :: [Card] -> [Rank]
eR' [] = []
eR' (x:xs) = sort ([(rank x)] ++ (eR' xs))

-- |Generate the list of Suits for a list of Cards
--  The list of Suits are unique and sorted in ascending order
eS :: [Card] -> [Suit]
eS [] = []
eS (x:xs) = sort (nub([(suit x)] ++ (eS xs)))

-- |Generate the list of Suits for a list of Cards
--  The list of Suits are sorted in ascending order
--  but not unique
eS' :: [Card] -> [Suit]
eS' [] = []
eS' (x:xs) = sort ([(suit x)] ++ (eS' xs))


-- |Find the lowsest Rank in a list of cards
lowest :: [Card] -> Rank
lowest cards = minimum (eR cards) 

-- |Find the highest Rank in a list of cards
highest :: [Card] -> Rank
highest cards = maximum (eR cards)

-- |Find the number of cards in the answer having rank 
--    lower than the lowest rank in the guess;
lowerThan :: [Card]->[Card]->Int
lowerThan [] cardsG = 0
lowerThan (c:cardsA) cardsG 
       |(rank c) < (lowest cardsG) = 1 + lowerThan cardsA cardsG
       |otherwise  =  lowerThan cardsA cardsG


--counts how many cards in the answer have rank higher 
--    than the highest rank in the guess;
higherThan :: [Card]->[Card]->Int
higherThan [] cardsG = 0
higherThan (c:cardsA) cardsG 
       |(rank c) > (highest cardsG) = 1 + higherThan cardsA cardsG
       |otherwise  =  higherThan cardsA cardsG


-- | counts how many of the cards in the answer have 
--    the same rank as a card in the gues
correctRankCard :: [Card]->[Card]->Int
correctRankCard [] cardG = 0
correctRankCard cardA [] = 0
correctRankCard cardsA cardsG = correctRank rank1 rank2 where 
                                      rank1 = eR cardsA
                                      rank2 = eR' cardsG

-- | counts how many of the cards in the answer 
--    have the same suit as a card in the guess, only 
--    counting a card in the guess once.
correctSuitCard [] cardG = 0
correctSuitCard cardA [] = 0
correctSuitCard cardsA cardsG = correctSuit suit1 suit2 where 
                                      suit1 = eS cardsA 
                                      suit2 = eS' cardsG  


-- | counts how many same ranks between the two lists of Ranks
correctRank :: [Rank] -> [Rank]->Int 
correctRank [] rankG = 0
correctRank rankA [] = 0
correctRank (ra:rankA) (rg:rankG) 
            |ra == rg  = 1 + correctRank rankA (rg:rankG)
            |ra < rg = correctRank rankA (rg:rankG) 
            |ra > rg = correctRank (ra:rankA) rankG 

-- | counts how many same suits between the two lists of Ranks        
correctSuit :: [Suit] -> [Suit]->Int
correctSuit [] suitG = 0
correctSuit suitA [] = 0
correctSuit (sa:suitA) (sg:suitG) 
            |sa == sg  = 1 + correctSuit suitA suitG 
            |sa < sg = correctSuit suitA (sg:suitG) 
            |sa > sg = correctSuit (sa:suitA) suitG 



-- Returns x ranks higher than rank r
greaterRank :: Rank -> Int -> Rank
greaterRank r x = toEnum (((fromEnum r) + x) `mod` 13)::Rank

-- Returns x suits greater than suit s
greaterSuit :: Suit -> Int -> Suit
greaterSuit s x = toEnum (((fromEnum s) + x) `mod` 4)::Suit

-- Returns x ranks smallerr than rank r
smallerRank :: Rank -> Int -> Rank
smallerRank r x = toEnum (((fromEnum r) - x) `mod` 13)::Rank

-- Returns x ranks smallerr than suit s
smallerSuit :: Suit -> Int -> Suit
smallerSuit s x = toEnum (((fromEnum s) - x) `mod` 4)::Suit

-- |Return the number of same cards betwenn two lists of cards 
--   |both the rank and suit have to be the same
cg :: [Card]->[Card]->Int
cg [] c = 0
cg (ca:cardsA) cardsG = (correctCard ca cardsG) + (cg cardsA cardsG)

-- |Return the number of same cards the list of cards has compared 
--  to the assigned Card c
correctCard :: Card -> [Card]->Int
correctCard c [] = 0
correctCard c (cg:cardsG)
          | c == cg =1+correctCard c cardsG
          | otherwise = correctCard c cardsG

-- |Filter out the candidate answers without n correct cards feedback
--  If [Card] has exactly n correct card with the guess
--  return true
--  otherwise return false
cgFilter :: Int->[Card]->[Card]->Bool
cgFilter n cardsG cardsA  = (cg cardsA cardsG == n)

-- |Filter out the candidate answers without n correct suit cards feedback
--  If [Card] has at least n correct suit card with the guess
--  return true
--  otherwise return false
suitFilter1 :: Int->[Card]->[Card]->Bool
suitFilter1 n cardsG cardsA = (correctSuitCard cardsA cardsG >= n)

-- |Filter out the candidate answers without n correct rank cards feedback
--  If [Card] has exactly n correct rank card with the guess
--  return true
--  otherwise return false
rankFilter :: Int->[Card]->[Card]->Bool
rankFilter n cardsG cardsA  = (correctRankCard cardsA cardsG == n)


-- |Filter out the candidate answers without n higher rank cards feedback
--  If [Card] has exactly n higher rank cards with the guess
--  return true
--  otherwise return false
higherFilter :: Int->[Card]->[Card]->Bool
higherFilter n cardsG cardsA = (higherThan cardsA cardsG == n)

-- |Filter out the candidate answers without n lower rank cards feedback
--  If [Card] has exactly n lower rank cards with the guess
--  return true
--  otherwise return false
lowerFilter :: Int->[Card]->[Card]->Bool
lowerFilter n cardsG cardsA  = (lowerThan cardsA cardsG == n)

-- |One of three main functions of this prgram
-- |the function is to select arbitray next guess from the filterd
-- |possible anserws pool
-- |Any card group which has inconsistent feedback as the actual answers with
-- |the last guess is considered as not possible
-- |Therefore they are filtered out from candidate answer
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (guess,gameState)(cc,lt,eq,gt,correctSuit) =
    (head finalGameState,  tail finalGameState)
       where 
-- |Filter out the candidate answers without n correct cards feedback
--  make sure left [Card] has exactly n correct card with the guess                             
            gameState1 =  filter (cgFilter cc guess) gameState
-- |Filter out the candidate answers without n correct rank cards feedback
--  make sure left [Card] has exactly n correct rank card with the guess     
            gameState3 =  filter (rankFilter eq guess) gameState1
-- |Filter out the candidate answers without n higher rank cards feedback
--  make sure left [Card] has exactly n higher rank cards with the guess           
            gameState4 =  filter (higherFilter gt guess) gameState3
-- |Filter out the candidate answers without n lower rank cards feedback
--  make sure left [Card] has exactly n lower rank cards with the guess          
            gameState5 =  filter (lowerFilter lt guess) gameState4
-- |Filter out the candidate answers without n correct suit cards feedback
--  make sure left [Card] has at least n correct suit card with the guess           
            gameState6 =  filter (suitFilter1 correctSuit guess) gameState5
            finalGameState = gameState6









     


       


