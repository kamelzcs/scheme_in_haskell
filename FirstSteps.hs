#! /usr/bin/env runhugs +l
--
-- helloworld.hs
-- Copyright (C) 2015 zhao <zhao@kamel-ThinkPad-X201>
--
-- Distributed under terms of the MIT license.
--


 module Main where
 import System.Environment

 main :: IO ()
 main = do
     args <- getArgs
     let number1 = read(args !! 0)
         number2 = read(args !! 1)
         in putStrLn ("Hello, " ++ show(number1 + number2))
