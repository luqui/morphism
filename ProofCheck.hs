{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards #-}

module ProofCheck where

import qualified Data.Map as Map
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error
import Term
import Parser
import Debug.Trace
import Data.List (intercalate)

newtype Prove a = Prove (ReaderT (Map.Map Name Term) (ReaderT Name (ErrorT String (State Int))) a)
    deriving (Functor, Monad)

fresh :: Prove Name
fresh = Prove $ do
    root <- lift ask
    count <- (lift.lift.lift) get
    (lift.lift.lift) (put $! count+1)
    return $ ('x':show count) : root

withBinding :: Name -> Term -> Prove a -> Prove a
withBinding var term (Prove pf) = Prove $ local (Map.insert var term) pf

getEnvironment = Prove ask

showEnv :: Map.Map Name Term -> String
showEnv = intercalate "\n" . map (\(k,v) -> showTerm (v `Apply` Free k)) . Map.toList

prove :: Term -> Prove ()
prove goal = do
    env <- getEnvironment
    case whnf goal of
        t | trace (showEnv env ++ "\n|- " ++ showTerm t ++ "\n") False -> undefined
        (x `Apply` y) | Free y' <- whnf y
                      , Just x' <- Map.lookup y' env
                      , x === x' -> return ()
        x -> go x
    where
    go (L `Apply` L) = return ()
    go (L `Apply` (G `Apply` a `Apply` b)) = do
        prove (L `Apply` a)
        var <- fresh
        withBinding var a . prove . whnf $ L `Apply` (b `Apply` Free var)
    go (G `Apply` a `Apply` b `Apply` c) = do
        prove (L `Apply` a)
        var <- fresh
        withBinding var a . prove . whnf $ b `Apply` Free var `Apply` (c `Apply` Free var)
    go goal = fail $ "Unable to prove subgoal: " ++ showTerm goal

runProve :: Name -> Prove a -> Either String a
runProve name (Prove pf) = evalState (runErrorT (runReaderT (runReaderT pf Map.empty) name)) 0

