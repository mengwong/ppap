{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib

main :: IO ()
main = do
  putStrLn "https://www.youtube.com/watch?v=Ct6BUPvE2sM\n"

  method1 <- act1; sing method1; putStrLn "\n(dance time)\n"
  method2 <- act2; sing method2; putStrLn ""

  if method1 == method2
    then putStrLn "Pikotaro!"
    else putStrLn "Different!"

  where
    sing x = print (prt x <> ".")

act1, act2 :: IO Multi
act1 = do
  let pen1  = iHave Pen
      apple = iHave Apple
  scene1 <- pen1 `unh` apple
  
  let pen2      = iHave Pen
      pineapple = iHave Pineapple
  scene2 <- pen2 `unh` pineapple

  scene1 `unh` scene2

act2 = do
  scene3 <- iHave Pen   `unh` iHave Pen
  scene4 <- iHave Apple `unh` iHave Pineapple
  scene3 `unh` scene4
