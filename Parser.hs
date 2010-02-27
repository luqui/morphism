module Parser (expr, definition, parse) where

import Term
import Text.ParserCombinators.ReadP as P
import qualified Data.Char as Char
import Control.Monad (guard, ap)
import Control.Applicative

instance Applicative P.ReadP where
    pure = return
    (<*>) = ap

skipSpace = P.munch Char.isSpace >> P.option () comment
    where
    comment = lineComment >> return ()
    lineComment = constrain (== "--") operatorLike >> P.munch (/= '\n') >> tok (P.char '\n')

tok :: P.ReadP a -> P.ReadP a
tok p = p <* skipSpace

atom :: Name -> P.ReadP Term
atom cx = (Free <$> name cx) P.+++ parens P.+++ g P.+++ l
    where
    parens = tok (P.char '(') *> expr cx <* tok (P.char ')')
    g = constrain (== "G") identifier *> pure G
    l = constrain (== "L") identifier *> pure L

appExpr :: Name -> P.ReadP Term
appExpr cx = foldl1 Apply <$> many1 (atom cx)

opExpr :: Name -> P.ReadP Term
opExpr cx = do
    l <- appExpr cx
    op <- operator
    r <- appExpr cx
    return $ (Free (op:cx) `Apply` l) `Apply` r

expr :: Name -> P.ReadP Term
expr cx = appExpr cx P.+++ opExpr cx P.+++ lambda
    where
    lambda = do
        tok (P.char '\\')
        names <- many1 (name cx)
        tok (P.char '.')
        body <- expr cx
        return $ foldr (\n -> Lambda . abstract n) body names
    
reservedWords = ["G", "L"]

identifier = tok $ P.munch1 Char.isAlphaNum

name cx = fmap (:cx) $ constrain (not . (`elem` reservedWords)) identifier 
                          P.+++ 
                       (tok (P.char '(') *> operator <* tok (P.char ')'))

constrain f p = do
    n <- p
    guard (f n)
    return n

definition cx = do
    n <- name cx
    constrain (== "=") operatorLike
    def <- expr cx
    return (n,def)

operatorLike = tok (P.munch1 (\c -> (Char.isPunctuation c || Char.isSymbol c) && not (c `elem` "()")))

operator = constrain (not . (`elem` ["=", "--"])) operatorLike

parse :: Name -> String -> Term
parse name input = head [ x | (x,[]) <- P.readP_to_S (expr name) input ]
