module REPL
  (repl, rep) where

import System.IO (hSetBuffering, stdin, BufferMode(..))

import AST (Tm(..))
import Parser (parseTm, runParser)
import Print (showTm, showTy)
import Type (deriveType)
import Eval (eval)

readTm :: String -> Either String Tm
readTm input = either (Left . show) Right $ runParser parseTm "TopLevel" [] input

rep :: String -> String
rep input = either id id $ do
  tm <- readTm input
  ty <- deriveType [] tm
  let tm' = eval tm
  ty' <- deriveType [] tm'
  return $ showTm [] tm ++ " : " ++ showTy ty ++ " => " ++ showTm [] tm'  ++ " : " ++ showTy ty'

repl :: IO ()
repl = do
  hSetBuffering stdin LineBuffering
  input <- getLine
  putStrLn $ rep input
  repl

