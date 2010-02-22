module Parser (expr, definition, parse) where

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

var cx = (:cx) <$> tok identifier

atom :: Name -> P.ReadP Term
atom cx = (Free <$> var cx) P.+++ parens P.+++ g P.+++ l
    where
    parens = tok (P.char '(') *> expr cx <* tok (P.char ')')
    g = tok (symbol "G") *> pure G
    l = tok (symbol "L") *> pure L

appExpr :: Name -> P.ReadP Term
appExpr cx = foldl1 Apply <$> many1 (atom cx)

opExpr :: Name -> P.ReadP Term
opExpr cx = do
    l <- appExpr cx
    op <- tok operator
    r <- appExpr cx
    return (Apply (Apply (Free (op:cx)) l) r)

expr :: Name -> P.ReadP Term
expr cx = appExpr cx P.+++ opExpr cx P.+++ lambda
    where
    lambda = do
        tok (P.char '\\')
        name <- tok identifier
        tok (P.char '.')
        body <- expr cx
        return $ Lambda (abstract (name:cx) body)
    

identifier = do
    n <- P.munch1 Char.isAlphaNum 
    guard (n /= "G" && n /= "L")
    return n

symbol p = do
    n <- P.munch1 Char.isAlphaNum
    guard (n == p)
    return n

definition cx = do
    n <- var cx P.+++ (fmap (:cx) $ tok (P.char '(') *> operator <* tok (P.char ')'))
    tok (string "=")
    def <- expr cx
    return (n,def)

operator = P.munch1 (\c -> not (Char.isSpace c) && not (Char.isAlphaNum c) && not (c `elem` "()\\.="))

parse :: Name -> String -> Term
parse name input = head [ x | (x,[]) <- P.readP_to_S (expr name) input ]
