


-- module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List



type GameState = [Card]

                              
allCards = [minBound..maxBound]::[Card]

initialGuess :: Int -> ([Card],GameState)
initialGuess n 
            |n<=1 =error"not enough card number"
            |n==2 = ([Card Club R2, Card Club R6], (ug [Card Club R2, Card Club R6] (allCards)))
            |n==3 = ([Card Club R2,Card Club R5,Card Club R8],(ug [Card Club R2,Card Club R5,Card Club R8] (allCards)))
            |n==4 = ([Card Club R2, Card Club R4,Card Club R6, Card Club R8],(ug [Card Club R2, Card Club R4,Card Club R6, Card Club R8] (allCards)))


feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback cardsA cardsG = (cc,lt,eq,gt,correctSuit) where 
          cc = cg cardsA cardsG
          lt =lowerThan cardsA cardsG
          eq = correctRankCard cardsA cardsG
          gt = higherThan cardsA cardsG
          correctSuit = correctSuitCard cardsA cardsG



ug :: [Card]->[Card]->[Card]
ug [] c  = c
ug (c:cardsG) cardsA = ug cardsG (filterGussed c cardsA)


filterGussed :: Card ->[Card]->[Card]
filterGussed c [] = []
filterGussed c (ca:cardsA)
              |c == ca =filterGussed c cardsA
              |otherwise =ca:(filterGussed c cardsA)
              

eR :: [Card] -> [Rank]
eR [] = []
eR (x:xs) = sort (nub ([(rank x)] ++ (eR xs)))


eR' :: [Card] -> [Rank]
eR' [] = []
eR' (x:xs) = sort ([(rank x)] ++ (eR' xs))

eS :: [Card] -> [Suit]
eS [] = []
eS (x:xs) = sort (nub([(suit x)] ++ (eS xs)))

eS' :: [Card] -> [Suit]
eS' [] = []
eS' (x:xs) = sort ([(suit x)] ++ (eS' xs))

lowest :: [Card] -> Rank
lowest cards = minimum (eR cards) 

highest :: [Card] -> Rank
highest cards = maximum (eR cards)

lowerThan :: [Card]->[Card]->Int
lowerThan [] cardsG = 0
lowerThan (c:cardsA) cardsG 
       |(rank c) < (lowest cardsG) = 1 + lowerThan cardsA cardsG
       |otherwise  =  lowerThan cardsA cardsG


higherThan :: [Card]->[Card]->Int
higherThan [] cardsG = 0
higherThan (c:cardsA) cardsG 
       |(rank c) > (highest cardsG) = 1 + higherThan cardsA cardsG
       |otherwise  =  higherThan cardsA cardsG


correctRankCard :: [Card]->[Card]->Int
correctRankCard [] cardG = 0
correctRankCard cardA [] = 0
correctRankCard cardsA cardsG = correctRank rank1 rank2 where 
                                      rank1 = eR' cardsA
                                      rank2 = eR cardsG

correctSuitCard :: [Card]->[Card]->Int
correctSuitCard [] cardG = 0
correctSuitCard cardA [] = 0
correctSuitCard cardsA cardsG = correctSuit suit1 suit2 where 
                                      suit1 = eS' cardsA
                                      suit2 = eS cardsG

correctRank :: [Rank] -> [Rank]->Int
correctRank [] rankG = 0
correctRank rankA [] = 0
correctRank (ra:rankA) (rg:rankG) 
            |ra == rg  = 1 + correctRank rankA (rg:rankG)
            |ra < rg = correctRank rankA (rg:rankG) 
            |ra > rg = correctRank (ra:rankA) rankG 

        
correctSuit :: [Suit] -> [Suit]->Int
correctSuit [] suitG = 0
correctSuit suitA [] = 0
correctSuit (sa:suitA) (sg:suitG) 
            |sa == sg  = 1 + correctSuit suitA suitG 
            |sa < sg = correctSuit suitA (sg:suitG) 
            |sa > sg = correctSuit (sa:suitA) suitG 


-- Returns x ranks higher than r
greaterRank :: Rank -> Int -> Rank
greaterRank r x = toEnum (((fromEnum r) + x) `mod` 13)::Rank

-- Returns x suits higher than s
greaterSuit :: Suit -> Int -> Suit
greaterSuit s x = toEnum (((fromEnum s) + x) `mod` 4)::Suit

smallerRank :: Rank -> Int -> Rank
smallerRank r x = toEnum (((fromEnum r) - x) `mod` 13)::Rank

smallerSuit :: Suit -> Int -> Suit
smallerSuit s x = toEnum (((fromEnum s) - x) `mod` 4)::Suit

cg :: [Card]->[Card]->Int
cg [] c = 0
cg (ca:cardsA) cardsG = (correctCard ca cardsG) + (cg cardsA cardsG)
     

correctCard :: Card -> [Card]->Int
correctCard c [] = 0
correctCard c (cg:cardsG)
          | c == cg =1+correctCard c cardsG
          | otherwise = correctCard c cardsG




      
-- nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
-- nextGuess (cardsG,gs) (fb_correct, fb_lower, fb_equal, fb_higher, fb_suits) = 
--   ([Card s1 r1, Card s2 r2], GameState r)
--    where 
--      r1 
--         |fb_lower ==2  = rank(cardG !! 0)
--         |fb_lower == 0 && fb_equal == 0 && fb_higher == 2  = smallerRank (highest pGuess) 1
--         |fb_lower == 2 && fb_equal==0

     


       


