module Main (main) where
import Lib
import System.Random

data Suit = 
    Clubs |
    Spades | 
    Hearts |
    Diamonds
    deriving Show
    
data Card = Card Int Suit
    deriving Show
type Deck = [Card]
type Pile = [Card]
type Board = [Pile]


main :: IO ()
main = do
    let suits = [Clubs, Spades, Hearts, Diamonds]
    let deck = foldr ((++) . initiateSuit) [] suits
    shuffled <- shuffleDeck deck
    print $ take 5 shuffled
    print $ length shuffled


shuffleDeck :: Deck -> IO Deck 
shuffleDeck xs = randNum >>= (\r -> shuffler 100 r xs)where 
                    shuffler :: Int -> Int -> Deck -> IO Deck 
                    shuffler 0 _ xs = return xs 
                    shuffler times n (x:xs) = do 
                        r <- randNum
                        shuffleTail <- shuffler (times - 1) r $ swap n (x:xs)
                        return shuffleTail
                                            
--getRandom :: Int -> Int 
--getRandom n = getHead(randomRs (0, 51) getStdGen)

swap :: Int -> [a] -> [a] 
swap n (x:xs) = do 
              let end = drop n (x:xs)
              let area = take n (x:xs) 
              let front = last area 
              let middle = drop 1 $ take (n - 1) (x:xs)
              front : middle ++ [x] ++ end

--get n ys :(take (n - 1) ys) ++[x] ++ drop n ys


randNum :: IO Int 
randNum =  getStdRandom (randomR (2, 51))

initiateSuit = helper [] 13 where 
    helper :: Deck -> Int -> Suit -> Deck
    helper xs 0 _ = xs
    helper xs n s = helper (Card n s :xs) (n-1) s

