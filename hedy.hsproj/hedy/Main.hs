module Main where

import Level1Lang
import Level1Parser

import Text.Megaparsec.Error
import System.Environment
import System.Exit

main = getArgs >>= cmd

cmd ["-h"] = usage   >> exitSuccess
cmd ["-v"] = version >> exitSuccess
cmd []     = usage   >> die "Missing file"
cmd [filename] = do
    body <- readFile filename
    case (parse filename body) of 
        Right program -> run program
        Left errorBundle -> putStrLn $ errorBundlePretty errorBundle
    

usage   = putStrLn "Usage: hedy [-vh] [file]"
version = putStrLn "Hedy 0.1"

