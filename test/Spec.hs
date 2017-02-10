{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Lib (annotations)
import Test.HUnit

main :: IO ()
main = putStrLn "Things seem to compile. No runnable tests for now!"

[annotations|
data Foo = Foo Int

data Recursive = Recursive Int Recursive
               | Stop
|]

f :: Foo
f = Foo 3

fAnn :: FooAnn String
fAnn = FooAnn "This is all annotated and stuff" 4

r :: Recursive
r = Recursive 4 Stop

rAnn :: RecursiveAnn String
rAnn = RecursiveAnn "annotation!" 4
          (RecursiveAnn "another!" 10 (StopAnn "a third!"))


