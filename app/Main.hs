module Main where

import           Flow
import qualified Level1                as L1
import           System.Environment
import           System.Exit
import           Text.Megaparsec.Error as M

version = putStrLn "Hedy 0.1"

usage = putStrLn "Usage: hedy [-vh] [LEVEL FILE]"

main = getArgs >>= cmd

cmd ["-h"] = usage >> exitSuccess
cmd ["-v"] = version >> exitSuccess
cmd ["1", filename] = do
    body <- readFile filename
    let parsed = L1.parse L1.pAll filename body
    case parsed of
        Right program -> L1.run program >> exitSuccess
        Left error    -> M.errorBundlePretty error |> die
cmd _ = usage >> die "Wrong arguments"
