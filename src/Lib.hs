{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Lib (someFunc) where

import Source.Span
import Source.Range
import AST.Unmarshal as TS

import Language.TypeScript.Grammar
import Language.TypeScript.AST

someFunc :: IO ()
someFunc = TS.parseByteString
           @Language.TypeScript.AST.Program
           @(Source.Span.Span, Source.Range.Range)
           Language.TypeScript.Grammar.tree_sitter_typescript "let a: int = 1"

           >>= \ast -> print ast
