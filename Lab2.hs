-- CptS 355 - Lab 2 (Haskell) - Spring 2023
-- Name: Josh Abbott
-- Collaborated with: 

module Lab2
     where


-- 1
{- (a) merge2 -}
merge2 :: [a] -> [a] -> [a]
merge2 [] l2 = l2
merge2 l1 [] = l1
merge2 (x:xs) (y:ys) = x : y : merge2 xs ys
                         

{- (b) merge2Tail -}
merge2Tail :: [a] -> [a] -> [a]
merge2Tail xs ys = tailHelper xs ys []
     where
          tailHelper [] l2 acc = acc ++ l2
          tailHelper l1 [] acc = acc ++ l1
          tailHelper (x:xs) (y:ys) acc = tailHelper xs ys (acc ++ [x,y])


{- (c) mergeN -}
mergeN :: [[a]] -> [a]
mergeN = foldl merge2 []

-- 2
{- (a) count -}
count :: Eq a => a -> [a] -> Int
count v xs = length $ filter (\x -> x == v) xs

{- (b) histogram  -}

eliminateDuplicates xs = foldr helper [] xs
     where helper x base | (x `elem` base) = base
                         | otherwise = x:base

histogram :: Eq a => [a] -> [(a, Int)]
histogram xs = map (\x -> (x, count x xs)) helper
     where
          helper = eliminateDuplicates xs
          count x ys = length (filter (== x) ys)

-- 3                
{- (a) concatAll -}
concatAll :: [[String]] -> String
concatAll = foldr (++) "" . map concat

{- (b) concat2Either -}               
data AnEither  = AString String | AnInt Int
                deriving (Show, Read, Eq)

concat2Either :: [[AnEither]] -> AnEither
concat2Either = foldr concatEither (AString "") . map (foldr concatEither (AString ""))
     where concatEither (AString s1) (AString s2) = AString (s1 ++ s2)
           concatEither (AString s) (AnInt i) = AString (s ++ show i)
           concatEither (AnInt i) (AString s) = AString (show i ++ s)
           concatEither (AnInt i1) (AnInt i2) = AString (show i1 ++ show i2)

-- 4      
{-  concat2Str -}               
concat2Str :: [[AnEither]] -> String




data Op = Add | Sub | Mul | Pow
          deriving (Show, Read, Eq)

evaluate:: Op -> Int -> Int -> Int
evaluate Add x y =  x+y
evaluate Sub   x y =  x-y
evaluate Mul x y =  x*y
evaluate Pow x y = x^y

data ExprTree a = ELEAF a | ENODE Op (ExprTree a) (ExprTree a)
                  deriving (Show, Read, Eq)

-- 5 
{- evaluateTree -}
evaluateTree :: ExprTree Int -> Int
evaluateTree (ELEAF n) = n
evaluateTree (ENODE op l r) = evaluate op (evaluateTree l) (evaluateTree r)

-- 6
{- printInfix -}
printInfix :: Show a => ExprTree a -> String
printInfix (ELEAF v) = show v
printInfix (ENODE op t1 t2) = "(" ++ (printInfix t1) ++ " `" ++ show op ++ "` " ++ (printInfix t2) ++ ")"

--7
{- createRTree -}
data ResultTree a  = RLEAF a | RNODE a (ResultTree a) (ResultTree a)
                     deriving (Show, Read, Eq)








