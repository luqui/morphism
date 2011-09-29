module Parser (expr, definition, check, whitespace, parse) where

import Term
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Data.Char as Char
import Control.Monad (guard, ap)
import Control.Applicative
import Control.Arrow

type Parser = P.Parsec String ()

tok = P.makeTokenParser $ P.LanguageDef {
        P.commentStart = "{-",
        P.commentEnd = "-}",
        P.commentLine = "--",
        P.nestedComments = True,
        P.identStart = P.letter <|> P.oneOf "_",
        P.identLetter = P.alphaNum <|> P.oneOf "_-'",
        P.opStart = opChars,
        P.opLetter = opChars,
        P.reservedNames = ["L","G"],
        P.reservedOpNames = ["=","\\","--"],
        P.caseSensitive = True
    }
    where
    opChars = P.oneOf "~!@#$%^&*-=+\\|;:<>,./?"

atom :: Name -> Parser Term
atom cx = P.choice [
        Free <$> name cx, 
        parens,
        P.reserved tok "G" *> pure G,
        P.reserved tok "L" *> pure L
    ]
    where
    parens = P.symbol tok "(" *> expr cx <* P.symbol tok ")"

appExpr :: Name -> Parser Term
appExpr cx = P.choice [
        foldl1 Apply <$> P.many1 (atom cx),
        lambdaExpr cx
    ]

opExpr :: Name -> Parser Term
opExpr cx = P.chainl1 (appExpr cx) op
    where
    op = (\o a b -> (o `Apply` a) `Apply` b) <$> infixExpr cx 

expr :: Name -> Parser Term
expr = opExpr

lambdaExpr cx = do
    P.reservedOp tok "\\"
    names <- P.many1 (name cx)
    P.symbol tok "."
    body <- expr cx
    return $ foldr (\n -> Lambda . abstractArg n) body names
    where

-- don't abstract over underscores
abstractArg ("_":_) = Scope
abstractArg n = abstract n
    
name cx = (:cx) <$> P.choice [
        P.identifier tok,
        P.try $ P.symbol tok "(" *> P.operator tok <* P.symbol tok ")"
    ]

definition cx = (makeDefn =<< expr cx) <* P.reservedOp tok "=" <*> expr cx
    where
    makeDefn x = case nameArgs x of
        Just (n, vs) -> return $ \body ->
            (n, foldr (\v -> Lambda . abstractArg v) body (reverse vs))
        Nothing -> fail "Malformed left-hand side of definition"
    
    nameArgs (Free n)           = Just (n, [])
    nameArgs (Apply t (Free v)) = (id *** (v:)) <$> nameArgs t
    nameArgs _                  = Nothing

check cx = P.symbol tok "!" *> expr cx

whitespace = P.whiteSpace tok

infixExpr cx = P.choice [
        (Free . (:cx) <$> P.operator tok),
        (P.symbol tok "`" *> atom cx <* P.symbol tok "`")
    ]

parse :: Name -> String -> Term
parse name = either (error.show) id . P.parse (expr name) "<input>"
