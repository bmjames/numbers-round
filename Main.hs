{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad            (guard)
import Control.Concurrent.Async (async, wait)

import Data.Ix                  (inRange)
import Data.List
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

data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

instance Show Expr where
    showsPrec p expr =
      case expr of
        Lit i   -> showString (show i)
        Add l r -> showExpr l r "+" (0, 0, 1)
        Sub l r -> showExpr l r "-" (0, 0, 1)
        Mul l r -> showExpr l r "×" (1, 1, 2)
        Div l r -> showExpr l r "÷" (1, 1, 2)
      where
        showExpr l r op (ql, q, qr) =
            showParen (p > q) (showsPrec ql l . pad ' ' op . showsPrec qr r)
        pad delim s = showChar delim . showString s . showChar delim

eval :: Bool
     -- ^ Whether to allow negative values after subtraction
     -> Expr -> Maybe Int
eval allowNeg = go
  where
    go (Lit i)   = Just i
    go (Add l r) = liftA2 (+) (go l) (go r)
    go (Sub l r) = do x <- liftA2 (-) (go l) (go r)
                      guard $ x >= 0 || allowNeg
                      return x
    go (Mul l r) = liftA2 (*) (go l) (go r)
    go (Div l r) = do denom <- go r
                      guard $ denom /= 0
                      numer <- go l
                      let x = numer % denom
                      if denominator x == 1 then Just (numerator x) else Nothing

genExprs :: [Int] -> [Expr]
genExprs ns = subsequences ns >>= permutations >>= go
  where
    go :: [Int] -> [Expr]
    go []  = []
    go [n] = [Lit n]
    go ns' = do (ls, rs) <- spans ns'
                l <- go ls
                r <- go rs
                op <- ops
                return $ op l r

    ops = [Add, Sub, Mul, Div]

spans :: [a] -> [([a], [a])]
spans xs = tail $ init $ zip (inits xs) (tails xs)

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
    putStrLnColor [color] $ unwords [show expr, "=", show i]

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
