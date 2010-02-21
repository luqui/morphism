module Parser (expr) where

import Term
import Text.ParserCombinators.ReadP as P
import qualified Data.Char as Char
import Control.Monad (guard, ap)
import Control.Applicative

instance Applicative P.ReadP where
    pure = return
    (<*>) = ap

tok :: P.ReadP a -> P.ReadP a
tok p = p <* P.skipSpaces

atom :: Name -> P.ReadP Term
atom cx = var P.+++ lambda P.+++ parens P.+++ g P.+++ l
    where
    var = Free . (:cx) <$> tok identifier
    lambda = do
        tok (P.char '\\')
        name <- tok identifier
        tok (P.char '.')
        body <- expr cx
        return $ Lambda (abstract (name:cx) body)
    parens = tok (P.char '(') *> expr cx <* tok (P.char ')')
    g = tok (P.char 'G') *> pure G
    l = tok (P.char 'L') *> pure L

appExpr :: Name -> P.ReadP Term
appExpr cx = foldl1 Apply <$> many1 (atom cx)

opExpr :: Name -> P.ReadP Term
opExpr cx = do
    l <- atom cx
    op <- tok operator
    r <- atom cx
    return (Apply (Apply (Free (op:cx)) l) r)

expr :: Name -> P.ReadP Term
expr cx = appExpr cx P.+++ opExpr cx
    

identifier = do
    n <- P.munch Char.isAlphaNum 
    guard (n /= "G" && n /= "L")
    return n

operator = P.munch (\c -> not (Char.isSpace c) && not (Char.isAlphaNum c))
