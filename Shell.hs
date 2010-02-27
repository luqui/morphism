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
import Debug.Trace
import Data.List (intercalate)

main = Line.initialise >> evalStateT go Map.empty
    where
    go = do
        line <- lift $ fmap concat getParagraph
        env <- get
        case parseLine env line of
            Nothing -> lift (putStrLn "Parse Error") >> go
            Just (Left (name, term)) -> do
                lift . putStrLn $ head name ++ " = " ++ showTerm term
                modify (Map.insert name term) >> go
            Just (Right term) -> do
                res <- return $ runProve ["P"] (prove term)
                case res of
                    Left err -> lift $ putStrLn err
                    Right () -> lift $ putStrLn "OK"
                go
    parseLine env line = 
        case results of
            [x] -> Just x
            [] -> Nothing
            ps -> trace ("Multiple Parses:\n" ++ intercalate "\n" (map show ps)) $ Nothing
        where
        results = map fst . filter (null . snd) $ P.readP_to_S (parser env) (line ++ "\n")
    
    parser env = defn P.+++ check
        where
        defn = (Left . second (substs env)) `fmap` definition []
        check = (Right . substs env) `fmap` expr []

    getParagraph = do
        Just line <- Line.getLineEdited ">>> "
        if null line then getParagraph else do
        let rest = do
                Just line' <- Line.getLineEdited "... "
                if null line' then return [] else fmap (line':) rest
        fmap (line:) rest

substs :: Map.Map Name Term -> Term -> Term
substs mp t = Map.foldWithKey (\k v -> substitute v k) t mp
