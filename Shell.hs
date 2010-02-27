module Main where

import qualified System.Console.SimpleLineEditor as Line
import ProofCheck
import Term
import Parser
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Map as Map
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Arrow
import Data.Maybe (listToMaybe)

main = Line.initialise >> evalStateT go Map.empty
    where
    go = do
        line <- lift $ Line.getLineEdited ">>> "
        if line == Just "" then go else do
        env <- get
        case fmap (parseLine env) line of
            Nothing -> return ()
            Just Nothing -> lift (putStrLn "Parse Error") >> go
            Just (Just (Left (name, term))) -> do
                lift . putStrLn $ head name ++ " = " ++ showTerm term
                modify (Map.insert name term) >> go
            Just (Just (Right term)) -> do
                res <- return $ runProve ["P"] (prove term)
                case res of
                    Left err -> lift $ putStrLn err
                    Right () -> lift $ putStrLn "OK"
                go
    parseLine env line = listToMaybe [ x | (x,[]) <- P.readP_to_S (parser env) (line ++ "\n") ]
    parser env = defn P.+++ check
        where
        defn = (Left . second (substs env)) `fmap` definition []
        check = (Right . substs env) `fmap` expr []

substs :: Map.Map Name Term -> Term -> Term
substs mp t = Map.foldWithKey (\k v -> substitute v k) t mp
