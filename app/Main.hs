module Main (main) where

import System.Random

import System.Cmd

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
type HiddenHand = Pile
type TopPiles = [Pile]
data Line = Line Int String 
    deriving Show


clubsstart :: Int
clubsstart = 3

heartsstart :: Int
heartsstart = 120

spadesstart :: Int
spadesstart = 237

diamondsstart :: Int
diamondsstart = 354

emptyStart :: Int 
emptyStart = 471

commandArray :: [String] 
commandArray = ["move X",
                "move from pile X to top pile",
                "move X from pile Y to pile Z",
                "move from hand to top pile ",
                "exit",
                "restart"]


main :: IO ()
main = do
    let suits = [Clubs, Spades, Hearts, Diamonds]
    let deck = concatMap initiateSuit suits
    shuffled <- shuffleDeck deck
    let noard = initiatePiles shuffled
    let board =  fst noard
    let hand = snd noard
    
    let topPile = initTopPile
    gaming (initHand hand, board, topPile)

exit :: IO () 
exit = putStr ""

displayHelp :: ((Hand, HiddenHand), Board, TopPiles) -> IO () 
displayHelp stuff = do 
    putStrLn "----- commands -----"
    mapM_ putStrLn commandArray
    putStrLn "--------------------"
    putStrLn "press enter to continue"
    _r <- getLine
    gaming stuff



--------------- Game loop -------------------
gaming :: ((Hand, HiddenHand), Board, TopPiles) -> IO () 
gaming ((hand, hidden), board, topPile) = do
    _ <- system "clear"
    draw board hand topPile
    command <- getLine
    let commands = words command
    if length commands == 1 
        then handleIO ((hand, hidden), board, topPile) commands 
        else gaming (handleCommand hidden hand board topPile commands)

handleIO :: ((Hand, HiddenHand), Board, TopPiles) -> [String] -> IO () 
handleIO stuff (command:_) = case command of 
    "restart" -> main 
    "exit" -> exit
    "help" -> displayHelp stuff
    _ -> print "command not found"
handleIO _ _ = error "something went worng in handleIO"

handleCommand :: HiddenHand -> Hand -> Board -> TopPiles -> [String] -> ((Hand, HiddenHand), Board, TopPiles) 
handleCommand hidden hand board topPiles xs = 
    case length xs of
        2 -> do 
            let bruh = moveFromHandToBoard hand (getCommandInt 1 xs) board 
            let b = fst bruh 
            let bing = snd bruh 
            ((b, hidden), bing, topPiles)
        3 -> (drawToHand hand hidden, board, topPiles)
        6 -> do 
            let (resHand, resTop) = moveFromHandToTopPile hand topPiles
            ((resHand, hidden), board, resTop)
        7 -> do 
            let (resB, resT) = moveFromBoardToTopPile (getCommandInt 3 xs) board topPiles
            ((hand, hidden) , resB, resT)
        8 -> ((hand, hidden), map showFront $ moveCardsBetweenPiles board (getCommandInt 1 xs) (getCommandInt 4 xs) (getCommandInt 7 xs), topPiles)
        _ -> ((hand, hidden), board, topPiles) 

getCommandInt :: Int -> [String] -> Int
getCommandInt _ [] = error "int not found"
getCommandInt 0 (x:_) = stringToInt x 
getCommandInt n (_:xs) = getCommandInt (n-1) xs 

initHand :: Hand -> (Hand, HiddenHand)
initHand hand = splitAt 3 (helper hand) where
    helper :: Hand -> Hand
    helper [] = []
    helper ((Card n s _):xs) = Card n s True : helper xs


-- drawing -- 
draw :: Board -> Hand -> TopPiles -> IO ()
draw board hand topPiles = do
    allCards <- readFile "cards.txt"
    let cards = lines allCards
    putStrLn $ stringifyLines $ lineifyTopPile 0 topPiles cards 
    putStrLn $ stringifyLines $ lineifyBoard (findLimit board) board cards
    putStrLn $ stringifyLines $ lineifyHand (findLimit board + 5) (reverse (take 3 hand)) cards 


stringToInt :: String -> Int 
stringToInt = read


stringifyLines :: [[Line]] -> String
stringifyLines [] = ""
stringifyLines cardLines =
    if foldr ((+) . length) 0 cardLines == 0
        then ""
        else 
            helper (map head cardLines) ++ stringifyLines (map (drop 1) cardLines)
                where 
                    helper :: [Line] -> String
                    helper [] = "\n"
                    helper ((Line _ string):xs) = string ++ helper xs  



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

---------------------- top pile specific drawing -----------------------
-- There is a bug somewhere here
lineifyTopPile :: Int -> TopPiles -> [String] -> [[Line]]
lineifyTopPile n (x:xs) sprites 
    | null x = lineifySprite n (emptySprite sprites) : lineifyTopPile n xs sprites
    | otherwise = lineifySprite n (findSprite True (head x) sprites) : lineifyTopPile n xs sprites
lineifyTopPile _ [] _ = []

---------------------- Hand specific drawing -----------------------
-- length of board, and hand
lineifyHand :: Int -> Hand -> [String] -> [[Line]]
lineifyHand n [x] sprites = [lineifySprite n (findSprite True x sprites)]
lineifyHand n (x:xs) sprites = sideifyCards n (findSprite True x sprites) : lineifyHand n xs sprites 
lineifyHand _ _ _ = []

    
sideifyCards :: Int -> [String] -> [Line]
sideifyCards _ [] = []
sideifyCards n (x:xs) = Line n (take 3 x ++ " ") : sideifyCards (n+1) xs 
---------------------- Board specific drawing -----------------------

lineifyBoard :: Int -> Board -> [String] -> [[Line]]
lineifyBoard _ [] _ = []
lineifyBoard limit (x:xs) sprites 
    | null x = lineifyEmptyPile limit sprites : lineifyBoard limit xs sprites  
    | otherwise = lineifyPile limit x sprites : lineifyBoard limit xs sprites

lineifyPile :: Int -> Pile -> [String] -> [Line]
lineifyPile limit pile sprites = helper 0 (reverse pile) sprites 
    where 
        helper :: Int -> Pile -> [String] -> [Line]
        helper n [] _ = fillToLimit n (limit + 4)
        helper n [x] sprites = lineifySprite n (findSprite True x sprites) ++ fillToLimit (n + 1) limit
        helper n (x:xs) sprites = 
            do 
                let firstLine = lineifySprite n (findSprite False x sprites)
                let rest = helper (n + length firstLine) xs sprites
                firstLine ++ rest

fillToLimit :: Int -> Int -> [Line]
fillToLimit n limit
    | n < limit = Line n "       " : fillToLimit (n + 1) limit
    | otherwise = []

lineifyEmptyPile :: Int -> [String] -> [Line]
lineifyEmptyPile limit sprites = lineifySprite 0 (emptySprite sprites) ++ fillToLimit 5 (limit + 4)

---------------------- finding and using sprites -----------------------
findSprite :: Bool -> Card -> [String] -> [String]
findSprite True (Card num suit True) sprites =
    case suit of 
        Clubs -> take 5 $ drop (clubsstart + 39 + 6 *(num - 1)) sprites 
        Hearts -> take 5 $ drop (heartsstart + 39 + 6 * (num- 1)) sprites 
        Spades -> take 5 $ drop (spadesstart + 39 + 6 * (num - 1)) sprites 
        Diamonds -> take 5 $ drop (diamondsstart + 39 + 6 * (num - 1)) sprites 
findSprite False (Card num suit True) sprites = 
    case suit of 
        Clubs -> take 2 $ drop (clubsstart + 3 * (num - 1)) sprites
        Hearts -> take 2 $ drop (heartsstart + 3 * (num - 1) ) sprites
        Spades -> take 2 $ drop (spadesstart + 3 * (num - 1) ) sprites
        Diamonds -> take 2 $ drop (diamondsstart + 3 * (num - 1) ) sprites
findSprite _ (Card _ _ False) sprites = take 2 sprites 


lineifySprite :: Int -> [String] -> [Line]
lineifySprite _ [] = []
-- ++ " " so there is space between piles
lineifySprite n (x:xs) = Line n (x ++ " " ) : lineifySprite (n+1) xs



showFront :: Pile -> Pile 
showFront ((Card x s _):rest) = Card x s True : rest
showFront [] = []




---------------------- Game behaviour -----------------------
drawToHand ::  Hand ->HiddenHand -> (Hand, HiddenHand)
drawToHand [] xs = splitAt 3 xs
drawToHand ys xs = (take 3 xs, drop 3 xs ++ ys)


moveFromBoardToTopPile :: Int -> Board -> TopPiles -> (Board, TopPiles)
moveFromBoardToTopPile n board topPiles = 
    do 
        let front = take (n - 1) board
        let end = drop n board
        let middle = popAmoumt 1 . head $ drop (n - 1) board
        let resBoard = front ++ [showFront middle] ++ end
        let card = head . head $ drop (n - 1) board
        if topChecker card topPiles 
            then (resBoard, handleAddToTopPile card topPiles) 
            else (board, topPiles)
        

handleAddToTopPile :: Card -> TopPiles -> TopPiles
handleAddToTopPile card@(Card _ suit _) topPiles = case suit of 
    Clubs ->  addToTopPile 1 card topPiles    
    Hearts ->  addToTopPile 2 card topPiles    
    Spades ->  addToTopPile 3 card topPiles   
    Diamonds ->  addToTopPile 4 card topPiles

-- This looks ugly, valve pls fix!!!
topChecker :: Card -> TopPiles -> Bool
topChecker card@(Card _ suit _) topPiles = case suit of 
    Clubs ->  checkIfMoveToTopPileIsLegal 1 card topPiles    
    Hearts ->  checkIfMoveToTopPileIsLegal 2 card topPiles    
    Spades ->  checkIfMoveToTopPileIsLegal 3 card topPiles   
    Diamonds -> checkIfMoveToTopPileIsLegal 4 card topPiles


checkIfMoveToTopPileIsLegal :: Int -> Card -> TopPiles -> Bool  
checkIfMoveToTopPileIsLegal 1 (Card x _ _) (y:_) = do 
    let (Card target _ _) = head y
    if null y 
        then x == 1 
        else x == target + 1 
checkIfMoveToTopPileIsLegal n card (x:xs) = checkIfMoveToTopPileIsLegal (n-1) card xs


addToTopPile :: Int -> Card -> TopPiles -> TopPiles
addToTopPile 1 card (x:xs) = (card:x) : xs
addToTopPile n card (x:xs) = x : addToTopPile (n - 1) card xs
addToTopPile _ _ _ = error "something went wrong in addToTopPile"


moveFromHandToBoard :: Hand -> Int -> Board -> (Hand, Board)  
moveFromHandToBoard (x:xs) n board  
    | checkHelper 1 (x:xs) $ head $ drop (n - 1) board = (xs, helper (x:xs) n board)
    | otherwise = (x:xs, board)
    where 
        helper :: Hand -> Int -> Board -> Board
        helper (x:_) n board = do 
            let area = drop (n-1) board
            let targetPile = head area
            let front = take (n-1) board
            let end = drop n board
            front ++ [x : targetPile] ++ end 

moveFromHandToTopPile :: Hand -> TopPiles -> (Hand, TopPiles)
moveFromHandToTopPile (x:xs) topPiles = (xs, handleAddToTopPile x topPiles)
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
        helper n (x:xs) True = do 
            if checkHelper n x (last xs) 
                then [popAmoumt n x] ++ take (length xs - 1) xs ++ [multiPopAndMove n x $ last xs]
                else x:xs
        helper n (x:xs) False = do 
            if checkHelper n (last xs) x 
                then [multiPopAndMove n (last xs) x] ++ take (length xs - 1) xs ++ [popAmoumt n $ last xs] 
                else x:xs
        helper _ [] _ = []

checkHelper :: Int -> Pile -> Pile -> Bool
checkHelper _ [] _ = False
checkHelper _ ((Card n _ _):_) [] = n == 13 
checkHelper 1 (x:_) (y:_) = checkIfLegal x y 
checkHelper n (_:xs) ys = checkHelper (n - 1) xs ys

checkIfLegal :: Card -> Card -> Bool 
checkIfLegal (Card _ _ False) _ = False
checkIfLegal (Card n suit _) (Card targetN targetSuit _) = case suit of
    Clubs -> case targetSuit of 
        Clubs  -> False 
        Spades -> False 
        _ -> n == (targetN - 1)
    Spades -> case targetSuit of 
        Clubs  -> False 
        Spades -> False 
        _ -> n == (targetN - 1)
    Hearts -> case targetSuit of 
        Hearts -> False 
        Diamonds -> False 
        _ -> n == (targetN - 1)
    Diamonds -> case targetSuit of 
        Hearts -> False 
        Diamonds -> False 
        _ -> n == (targetN - 1)


popAmoumt :: Int -> Pile -> Pile
popAmoumt 0 xs = xs
popAmoumt n (_:xs) = popAmoumt (n - 1) xs
popAmoumt _ [] = []

-- move amount, from pile, to pile
multiPopAndMove :: Int -> Pile -> Pile -> Pile
multiPopAndMove = helper [] where 
    helper :: Pile -> Int -> Pile -> Pile -> Pile
    helper acc _ [] ys = acc ++ ys
    helper acc 0 _ ys = acc ++ ys
    helper acc n (x:xs) ys = helper (acc ++ [x]) (n-1) xs ys


---------------------- initiating game -----------------------
initTopPile :: TopPiles
initTopPile = [[],[],[],[]]

emptySprite :: [String] -> [String]
emptySprite = take 5 . drop emptyStart

shuffleDeck :: Deck -> IO Deck 
shuffleDeck xs = randNum >>= (\r -> shuffler 100 r xs) where 
                    shuffler :: Int -> Int -> Deck -> IO Deck 
                    shuffler 0 _ xs = return xs 
                    shuffler times n (x:xs) = do 
                        r <- randNum
                        shuffler (times - 1) r $ swap n (x:xs)
                        
                                            

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

