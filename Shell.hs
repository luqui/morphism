module Main where

import qualified System.Console.SimpleLineEditor as Line
import ProofCheck
import Term
import Parser
import qualified Text.Parsec as P
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Applicative
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
            Just (Left (name, term))
                | Set.null (freeVars term) -> modify (Map.insert name term) >> go
                | otherwise -> do
                    lift . putStrLn $ "Error: term is not closed.  Free variables: " ++ intercalate ", " (map (showTerm . Free) (Set.toList (freeVars term)))
                    go
            Just (Right term) -> do
                res <- return $ runProve ["P"] (prove term)
                case res of
                    Left err -> lift $ putStrLn err
                    Right () -> lift $ putStrLn "OK"
                go
    parseLine env line = 
        case results of
            Left e  -> trace (show e) Nothing
            Right x -> Just x
        where
        results = P.parse (parser env) "<input>" $ line ++ "\n"
    
    parser env = defn <|> check
        where
        defn = (Left . second (substs env)) `fmap` definition []
        check = (Right . substs env) `fmap` Parser.check []

    getParagraph = do
        Just line <- Line.getLineEdited ">>> "
        if null line then getParagraph else do
        let rest = do
                Just line' <- Line.getLineEdited "... "
                if null line' then return [] else fmap (line':) rest
        fmap (line:) rest

substs :: Map.Map Name Term -> Term -> Term
substs mp t = Map.foldrWithKey (\k v -> substitute v k) t mp
