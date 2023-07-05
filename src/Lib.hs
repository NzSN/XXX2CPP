{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Lib (
  displayTSAst,
  walkTSAst,
  ) where

import Control.Monad ( when, unless )
import Prelude as P
import Data.ByteString ( ByteString )
import Source.Span
import Source.Range
import AST.Unmarshal as AU

import Foreign.Storable (peek, sizeOf)
import Foreign.Ptr as FP
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.String (peekCString)

import Language.TypeScript.Grammar ( tree_sitter_typescript )
import Language.TypeScript.AST

import TreeSitter.Parser as TS
import TreeSitter.Cursor as TS
import TreeSitter.Tree as TS
import TreeSitter.Node as TS
import TreeSitter.Symbol as TS

displayTSAst :: ByteString -> IO ()
displayTSAst program = do
  ast <- AU.parseByteString
         @Language.TypeScript.AST.Program
         @(Source.Span.Span, Source.Range.Range)
         Language.TypeScript.Grammar.tree_sitter_typescript program

  print ast


walkTSAst :: ByteString -> IO ()
walkTSAst bytestring = do
  withParser tree_sitter_typescript $
    \parser -> withParseTree parser bytestring $
    \treePtr ->
      if treePtr == nullPtr then
        print "error: failed to get ast"
      else
        withRootNode treePtr $
        \rootPtr ->
          withCursor (castPtr rootPtr) $
          \cursor -> traverse cursor
  where

    traverse :: Ptr Cursor -> IO ()
    traverse cursor = do

      -- Your process
      alloca $ \nodePtr -> do
        hasCurrentNode <- ts_tree_cursor_current_node_p cursor nodePtr
        currentNode <- peek nodePtr
        peekCString (nodeType currentNode) >>= \v -> print v
        print $ nodeChildCount currentNode

      -- to next node
      hasChild <- ts_tree_cursor_goto_first_child cursor
      when hasChild $
        traverse cursor >> ts_tree_cursor_goto_parent cursor >> return ()

      -- to next layer
      hasSibling <- ts_tree_cursor_goto_next_sibling cursor
      when hasSibling $ traverse cursor
