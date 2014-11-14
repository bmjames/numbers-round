{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad ((>=>), guard)
import Data.List (permutations, subsequences)
import Data.Maybe (mapMaybe)
import Data.Ratio
import Options.Applicative
import System.Console.ANSI
import System.IO

data Options = Options Int [Int]

parseOptions :: Parser Options
parseOptions = Options
    <$> option auto (short 't' <> long "target" <> metavar "NUMBER")
    <*> some (argument auto (metavar "NUMBER..."))

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> parseOptions) (progDesc "Numbers round solver")

data Expr = Value Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

instance Show Expr where
    show (Value i) = show i
    show expr = "(" ++ unwords xs ++ ")" where
      xs = case expr of Add a1 a2 -> [show a1, "+", show a2]
                        Sub a1 a2 -> [show a1, "-", show a2]
                        Mul a1 a2 -> [show a1, "×", show a2]
                        Div a1 a2 -> [show a1, "÷", show a2]

eval :: Expr -> Maybe Int
eval (Value i)   = Just i
eval (Add a1 a2) = (+) <$> eval a1 <*> eval a2
eval (Sub a1 a2) = (-) <$> eval a1 <*> eval a2
eval (Mul a1 a2) = (*) <$> eval a1 <*> eval a2
eval (Div a1 a2) = do denom <- eval a2
                      guard $ denom /= 0
                      numer <- eval a1
                      let r = numer % denom
                      if denominator r == 1 then Just (numerator r) else Nothing

genExprs :: [Int] -> [Expr]
genExprs = subsequences >=> filter (not . null) . permutations >=> go
  where
    go :: [Int] -> [Expr]
    go [n]    = [Value n]
    go (n:ns) = do (op, commutes) <- [(Add, True), (Sub, False), (Mul, True), (Div, False)]
                   lhs <- go ns
                   f   <- if commutes then [id] else [id, flip]
                   return $ f op lhs (Value n)

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

main :: IO ()
main =
  do
    Options t ns <- execParser parserInfo
    let (nearest, i) = minByAbs ((t -) . snd) $ mapMaybe (\a -> (a,) <$> eval a) $ genExprs ns
    let color = if i == t then green else yellow
    putStrLnColor [color] $ unwords [show nearest, "=", show i]

  where
    green  = SetColor Foreground Dull Green
    yellow = SetColor Foreground Dull Yellow
