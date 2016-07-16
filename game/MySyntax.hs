module MySyntax where

--data Ball = Red | Blue | Yellow deriving (Eq)
type Ball = Color

instance Show Ball where
  show Red = "o"
  show Blue = "x"
  show Yellow = "+"
  
type Board = [[Ball]]
type Pt = (Int, Int)
        
printBoard :: Board -> IO ()
printBoard board =
  do putStr "----<board>----\n"
     putStr "   a b c d ...\n"
     mapM_ (\(col,x) -> putStr $  show x ++ " " ++ show col ++ "\n") $ zip board [0..]
  

pickable :: Board -> Pt -> Bool
pickable board pt            
  | isValidPos board pt = not $ length marks == 1
  | otherwise           = False
  where marks = markBoard board pt

-- pt is valid
pickBall :: Board -> Pt -> Board
pickBall board pt =
  filter (not.null) $ zipWith (\col x -> fst $ unzip $ filter (\(elem, y) -> notElem (x,y) marks) $ zip col [0..]) board [0..]
    where marks = markBoard board pt
        
-- pt is valid
markBoard :: Board -> Pt -> [Pt]
markBoard board pt@(x,y) =
  submark pt []
  where c = choice board pt
        submark :: Pt -> [Pt] -> [Pt]
        submark pt@(x,y) marks
          | elem pt marks               = marks
          | not $ isValidPos board pt   = marks
          | c == choice board pt        = mark'
          | otherwise                   = marks
            where mark' = foldl (flip submark) (pt:marks) [(x+1,y),(x,y+1),(x-1,y),(x,y-1)] 
        
choice :: Board -> Pt -> Ball
choice board pt@(x,y) =          
  board !! x !! y
        
isValidPos :: Board -> Pt -> Bool        
isValidPos board pt =
  foldl (||) False $ map (elem pt) numBoard
  where numBoard :: [[Pt]]
        numBoard = zipWith (\col x -> zipWith (\elem y -> (x,y)) col [0..]) board [0..]

endJudge :: Board -> Bool
endJudge board =
  and $ map (and . map (\e -> not $ pickable board e)) numb
  where numb = zipWith (\col x -> zipWith (\_ y -> (x,y)) col [0..]) board [0..]
        
calcScore :: [Pt] -> Int
calcScore marks = (length marks - 2) * (length marks - 2)