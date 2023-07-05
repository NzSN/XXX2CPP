{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib (displayTSAst, walkTSAst)

main :: IO ()
main = do
  -- Display byte
  displayTSAst "1 + 1; 1 + 1"
  walkTSAst "1+1\n1+1"
