module Main (main) where
import Lib
import System.Random
import Text.Read (Lexeme(String))
data Suit = 
    Clubs |
    Spades | 
    Hearts |
    Diamonds
    deriving Show
   
data Card = Card Int Suit Bool
    deriving Show
type Deck = [Card]
type Pile = [Card]
type Board = [Pile]
type Hand = Pile 
type VisualHand = Pile
data Line = Line Int String 
    deriving Show


main :: IO ()
main = do
    let suits = [Clubs, Spades, Hearts, Diamonds]
    let deck = concatMap initiateSuit suits
    shuffled <- shuffleDeck deck
    print $ take 5 shuffled
    print $ length shuffled
    let noard = initiatePiles shuffled
    let board =  fst noard
    let hand = snd noard
    print "-----------------------"
    draw board

-- drawing -- 
draw :: Board -> IO ()
draw board = do
    allCards <- readFile "cards.txt"
    let cards = lines allCards
    putStrLn "first test"
    print $ lineifySprite 0 $ take 2 cards
    putStrLn "second test"
    --print $ lineifyPile (last board) cards
    print $ lineifyBoard (findLimit board) board cards
    putStrLn $ stringifyLines $ lineifyBoard (findLimit board) board cards


stringifyLines :: [[Line]] -> String 
stringifyLines lines = helper (map head lines) ++ stringifyLines (map (drop 1) lines)
    where 
        helper :: [Line] -> String
        helper [] = "\n"
        helper ((Line n string):xs) = string ++ helper xs  

findLimit :: Board -> Int 
findLimit board = do
    let long = findLongest board
    if long == 0 
        then 0
        else 2*(long - 1) + 5 
    

findLongest :: Board -> Int 
findLongest = helper 0 
    where 
        helper :: Int -> Board -> Int 
        helper n [] = n 
        helper n (x:xs) 
            | n < length x = helper (length x) xs 
            | otherwise = helper n xs

lineifyBoard :: Int -> Board -> [String] -> [[Line]]
lineifyBoard _ [] _ = []
lineifyBoard limit (x:xs) sprites = lineifyPile limit x sprites : lineifyBoard limit xs sprites

lineifyPile :: Int -> Pile -> [String] -> [Line]
lineifyPile limit pile sprites = helper 0 (reverse pile) sprites 
    where 
        helper :: Int -> Pile -> [String] -> [Line]
        helper n [x] sprites = lineifySprite n (findSprite True x sprites) ++ fillToLimit (n + 1) 
            where 
                fillToLimit :: Int -> [Line]
                fillToLimit n 
                    | n < limit = (Line n "       ") : fillToLimit (n + 1)
                    | otherwise = []
        helper n (x:xs) sprites = 
            do 
                let firstLine = lineifySprite n (findSprite False x sprites)
                let rest = helper (n + length firstLine) xs sprites
                firstLine ++ rest
        helper _ _ _ = []


findSprite :: Bool -> Card -> [String] -> [String]
findSprite True (Card num suit True) sprites = take 5 $ drop 6 sprites 
findSprite False (Card num suit True) sprites = 
    case suit of 
        Clubs -> take 2 $ drop 3 sprites
        Hearts -> take 2 $ drop 3 sprites
        Spades -> take 2 $ drop 3 sprites
        Diamonds -> take 2 $ drop 3 sprites
findSprite _ (Card _ _ False) sprites = take 2 sprites 


lineifySprite :: Int -> [String] -> [Line]
lineifySprite _ [] = []
-- ++ " " so there is space between piles
lineifySprite n xs = Line n (head xs ++ " " ) : lineifySprite (n+1) (drop 1 xs)




-- we gaming --
showFront :: Pile -> Pile 
showFront ((Card x s _):rest) = Card x s True : rest
showFront [] = []

-- still not done!!!!
-- move int amount of cards, from pile int, to pile int 
moveCardsBetweenPiles :: Board -> Int -> Int -> Int -> Board
moveCardsBetweenPiles xs amount from to 
    | from < to = do
        let area = drop (from - 1) (take to xs)
        let front = take (from - 1) xs
        let end = drop to xs
        front ++ helper amount area True ++ end

    | otherwise = do 
        let area = drop (to - 1) (take from xs)
        let front = take (to - 1) xs
        let end = drop from xs
        front ++ helper amount area False ++ end

    where
        helper :: Int -> [Pile] -> Bool -> [Pile] 
        helper n (x:xs) True = [popAmoumt n x] ++ take (length xs - 1) xs ++ [multiPopAndMove n x $ last xs]
        helper n (x:xs) False = [multiPopAndMove n (last xs) x] ++ take (length xs - 1) xs ++ [popAmoumt n $ last xs]
        helper _ [] _ = []



popAmoumt :: Int -> Pile -> Pile
popAmoumt 0 xs = xs
popAmoumt n (x:xs) = popAmoumt (n - 1) xs
popAmoumt _ xs = xs

-- move amount, from pile, to pile
multiPopAndMove :: Int -> Pile -> Pile -> Pile
multiPopAndMove = helper [] where 
    helper :: Pile -> Int -> Pile -> Pile -> Pile
    helper acc _ [] ys = acc ++ ys
    helper acc 0 xs ys = acc ++ ys
    helper acc n (x:xs) ys = helper (acc ++ [x]) (n-1) xs ys

-- initiation --
shuffleDeck :: Deck -> IO Deck 
shuffleDeck xs = randNum >>= (\r -> shuffler 100 r xs) where 
                    shuffler :: Int -> Int -> Deck -> IO Deck 
                    shuffler 0 _ xs = return xs 
                    shuffler times n (x:xs) = do 
                        r <- randNum
                        shuffleTail <- shuffler (times - 1) r $ swap n (x:xs)
                        return shuffleTail
                                            

initiatePiles :: Deck -> (Board, Hand)
initiatePiles xs =
    do 
        let board = helper 1 $ take 28 xs 
        let hand = drop 28 xs
        let resBoard = map showFront board
        (resBoard, hand)
            where 
                helper :: Int -> Deck -> Board
                helper 7 xs = [take 7 xs] 
                helper n xs = take n xs : helper (n+1) (drop n xs)

swap :: Int -> [a] -> [a] 
swap n (x:xs) = do 
              let end = drop n (x:xs)
              let area = take n (x:xs) 
              let front = last area 
              let middle = drop 1 $ take (n - 1) (x:xs)
              front : middle ++ [x] ++ end
swap _ [] = error "something went wrong in swap function"


randNum :: IO Int 
randNum =  getStdRandom (randomR (2, 51))

initiateSuit :: Suit -> Deck
initiateSuit = helper [] 13 where
    helper :: Deck -> Int -> Suit -> Deck
    helper xs 0 _ = xs
    helper xs n s = helper (Card n s False :xs) (n-1) s

