{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad            ((>=>), guard)
import Control.Concurrent.Async (async, wait)

import Data.Ix                  (inRange)
import Data.List                (permutations, subsequences)
import Data.Maybe               (mapMaybe)
import Data.Ratio

import Options.Applicative

import System.Console.ANSI
import System.IO
import System.Random            (randomRIO)
import System.Random.Shuffle    (shuffleM)

data Options = Play | Solve Int [Int]

options :: ParserInfo Options
options = info (helper <*> parseOptions) fullDesc
  where
    parseOptions :: Parser Options
    parseOptions = subparser $ command "solve" solveCmd <> command "play" playCmd

    playCmd :: ParserInfo Options
    playCmd = info (helper <*> pure Play) $ progDesc "Play a game interactively"

    solveCmd :: ParserInfo Options
    solveCmd = info (helper <*> parseSolve) $ progDesc "Solve with predefined input"

    parseSolve :: Parser Options
    parseSolve = Solve <$> option auto ( short 't'
                                      <> long "target"
                                      <> metavar "NUMBER"
                                      <> help "Target number" )
                       <*> some (argument auto ( metavar "NUMBER..."
                                              <> help "Number tiles" ))

newtype Fix f = Fx (f (Fix f))

type Algebra f a = f a -> a

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

data ExprF a = Lit Int
             | Add a a
             | Sub a a
             | Mul a a
             | Div a a

type Expr = Fix ExprF

instance Functor ExprF where
    fmap _ (Lit i)   = Lit i
    fmap f (Add l r) = Add (f l) (f r)
    fmap f (Sub l r) = Sub (f l) (f r)
    fmap f (Mul l r) = Mul (f l) (f r)
    fmap f (Div l r) = Div (f l) (f r)

showExpr :: Expr -> String
showExpr = alg . fmap showExpr . unFix
  where
    alg :: Algebra ExprF String
    alg (Lit i)   = show i
    alg (Add l r) = showNode [l, "+", r]
    alg (Sub l r) = showNode [l, "-", r]
    alg (Mul l r) = showNode [l, "×", r]
    alg (Div l r) = showNode [l, "÷", r]

    showNode xs = "(" ++ unwords xs ++ ")"

eval :: Bool
     -- ^ Whether to allow negative values after subtraction
     -> Expr -> Maybe Int
eval allowNeg = eval'
  where
    eval' = alg . fmap eval' . unFix

    alg :: Algebra ExprF (Maybe Int)
    alg (Lit i)   = Just i
    alg (Add l r) = liftA2 (+) l r
    alg (Sub l r) = do x <- liftA2 (-) l r
                       guard $ x >= 0 || allowNeg
                       return x
    alg (Mul l r) = liftA2 (*) l r
    alg (Div l r) = do denom <- r
                       guard $ denom /= 0
                       numer <- l
                       let x = numer % denom
                       if denominator x == 1 then Just (numerator x) else Nothing

genExprs :: [Int] -> [Expr]
genExprs = subsequences >=> filter (not . null) . permutations >=> go
  where
    go :: [Int] -> [Expr]
    go [n]    = [Fx $ Lit n]
    go (n:ns) = do l <- go ns
                   (op, commutes) <- ops
                   f <- if commutes then [id] else [id, flip]
                   return $ Fx $ f op l $ Fx $ Lit n

    ops = [(Add, True), (Sub, False), (Mul, True), (Div, False)]

solve :: Int -> [Int] -> (Expr, Int)
solve target =
    minByAbs ((target -) . snd) . mapMaybe (\a -> (a,) <$> eval False a) . genExprs

hPutStrLnColor :: Handle -> [SGR] -> String -> IO ()
hPutStrLnColor h sgrs t =
    hSetSGR h sgrs >> hPutStrLn h t >> hSetSGR h []

putStrLnColor :: [SGR] -> String -> IO ()
putStrLnColor = hPutStrLnColor stdout

minByAbs :: (a -> Int) -> [a] -> a
minByAbs f (x:xs) = go x xs where
    go m [] = m
    go m (x:xs) = let fm = abs (f m)
                  in if fm == 0 then m
                     else go (if fm < abs (f x) then m else x) xs
minByAbs _ [] = error "minByAbs on empty list"

printSolution :: Int -> (Expr, Int) -> IO ()
printSolution target (expr, i) =
    putStrLnColor [color] $ unwords [showExpr expr, "=", show i]

  where
    color = if i == target then green else yellow
    green  = SetColor Foreground Dull Green
    yellow = SetColor Foreground Dull Yellow

play :: IO ()
play =
  do
    nLarge <- askNLarge
    let nSmall = 6 - nLarge
    ns <- (++) <$> (take nLarge <$> shuffleM [25, 50, 75, 100])
               <*> (take nSmall <$> shuffleM (dup [1..10]))
    target   <- randomRIO (100, 999)
    solution <- async $ return $! solve target ns

    putStrLn $ "Your numbers are: " ++ unwords (map show ns)
    putStrLn $ "Your target is: " ++ show target
    putStrLn "Press Enter to show solution." <* getLine

    wait solution >>= printSolution target

  where
    askNLarge :: IO Int
    askNLarge = do putStrLn "How many large numbers?"
                   nLarge <- read <$> getLine
                   if not (inRange (0, 4) nLarge)
                     then fail "Choose between 0 and 4 large numbers."
                     else return nLarge

    dup a = a <> a

main :: IO ()
main = do
    opts <- execParser options
    case opts of
        Play -> play
        Solve t ns -> printSolution t $ solve t ns
