module Main where

import           Lib.Main                       ( getArgs
                                                , run
                                                )

main :: IO ()
main = getArgs >>= run
