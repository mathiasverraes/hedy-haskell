module Main where

import qualified Level1

import Text.Megaparsec.Error
import System.Environment
import System.Exit

main = getArgs >>= cmd

cmd ["1", filename]  = go filename Level1.parse Level1.run                
cmd ["-h"] = usage   >> exitSuccess
cmd ["-v"] = version >> exitSuccess
cmd _      = usage   >> die "Wrong arguments"      

usage   = putStrLn "Usage: hedy [-vh] [LEVEL FILE]"
version = putStrLn "Hedy 0.1"


go filename parse run = do
    body <- readFile filename
    case (parse filename body) of 
        Right program -> run program >> exitSuccess
        Left errorBundle -> die $ errorBundlePretty errorBundle

