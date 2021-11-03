module Main where
import Data.List ( (\\), sort, sortBy, intercalate)
import Data.Maybe (fromJust)
import Text.Printf ( printf, PrintfArg )

data DivisionResult = DivisionResult { quotient :: Int, remainder ::  [Int] } deriving Show

newtype DivisionResults = DivisionResults { results:: [DivisionResult] }

instance Show DivisionResults where
  show (DivisionResults xs) =
    printf "quotient: %s remainder: %s" (polynomPowersToString  $ filter (>0) $ map quotient xs) (polynomPowersToString $ remainder $ last xs)
    where polynomPowersToString xs = intercalate " + " $ map (printf "x^%d") xs

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

substract :: (Ord a, Num a) => [a] -> [a] -> [a]
substract a b =  filter (> 0)  $ (a \\ b) ++ (b \\ a)

multiply :: Int -> [Int] -> [Int]
multiply x = map (+x)

findK :: [Int] -> [Int] -> Maybe Int
findK (x0:_) (x1:_)
    | difference >= 0 = Just difference
    | otherwise = Nothing
    where
        difference = x0 - x1


divide :: [Int] -> [Int] -> [DivisionResult]
divide xs0 xs1 = case k of
    Just multiplier -> DivisionResult multiplier remainder : divide remainder xs1
    Nothing -> []
    where
        remainder = substract xs0s $ multiply (fromJust k) xs1
        k = findK xs0s xs1s
        xs0s = sortDesc xs0
        xs1s = sortDesc xs1

main :: IO()
main = print $ DivisionResults $ divide [9, 4, 3] [3, 2, 0]