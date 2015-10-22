module Main where

import qualified Data.ByteString.Lazy as B

import qualified Data.Attoparsec.ByteString.Lazy as A

import qualified Data.Vector as V

import System.Environment

import SNXi.Types
import SNXi.Op
import SNXi.Parser
import SNXi.Exec

parseArgs :: [String] -> ((V.Vector Op -> IO ()), FilePath)
parseArgs ("-m":"-f":fp:[]) = (runProgF showCPUmem, fp)
parseArgs ("-m":fp:[])     = (runProg showCPUmem, fp)
parseArgs ("-f":fp:[])     = (runProgF showCPU, fp)
parseArgs (fp:[])          = (runProg showCPU, fp)
parseArgs _                = error "usage: [-m] [-f] file_path"

runProg :: (CPU -> String) -> V.Vector Op -> IO ()
runProg p o = (putStrLn (p (bootCPU o))) >> putStr "\n" >> stepProg p (bootCPU o)
    where stepProg p c = case cycleCPU c
                         of Nothing   -> return ()
                            (Just c') -> putStrLn (p c')
                                         >> putStr "\n" >> stepProg p c'

runProgF :: (CPU -> String) -> V.Vector Op -> IO ()
runProgF p o = stepProg p (bootCPU o)
    where stepProg p c = case cycleCPU c
                         of Nothing   -> putStrLn (p c)
                            (Just c') -> stepProg p c'

main :: IO ()
main = do
    (p, fp) <- parseArgs <$> getArgs
    r <- B.readFile fp
    case (A.eitherResult $ A.parse objs r)
            of Left e -> putStr "Parse Error: " >> putStrLn e
               Right o -> p o
