module Term (Name, Term(..), Scope(..), (===), showTerm, abstract, instantiate, substitute, whnf, normalForm) where

import Data.List (intercalate)

type Name = [String]

data Term 
    = Free Name
    | Bound Int
    | Lambda Scope
    | Apply Term Term
    | G | L
    deriving Eq

newtype Scope = Scope Term deriving Eq

(===) :: Term -> Term -> Bool
t === u = normalForm ["=="] t == normalForm ["=="] u

instance Show Term where show = showTerm

showTerm :: Term -> String
showTerm = go False False
    where
    go ap lp (Free n) = intercalate ":" (reverse n)
    go ap lp (Bound z) = show z
    go ap lp (Lambda (Scope body)) = parens lp $ "\\." ++ go False False body
    go ap lp (Apply t u) = parens ap $ go False True t ++ " " ++ go True True u
    go ap lp G = "G"
    go ap lp L = "L"

    parens False x = x
    parens True x = "(" ++ x ++ ")" 

abstract :: Name -> Term -> Scope
abstract name exp = Scope $ go 0 exp
    where
    go varnum (Free name') | name == name' = Bound varnum
                           | otherwise     = Free name'
    go varnum (f `Apply` x) = go varnum f `Apply` go varnum x
    go varnum (Lambda (Scope body)) = Lambda (Scope ((go $! varnum + 1) body))
    go varnum x = x

instantiate :: Term -> Scope -> Term
instantiate new (Scope body) = go 0 body where
    go varnum (Bound z) | z == varnum = new
                        | otherwise   = Bound z
    go varnum (f `Apply` x) = go varnum f `Apply` go varnum x
    go varnum (Lambda (Scope body)) = Lambda (Scope ((go $! varnum + 1) body))
    go varnum x = x

substitute :: Term -> Name -> Term -> Term
substitute image name = instantiate image . abstract name

whnf :: Term -> Term
whnf (Apply f x) = 
    case whnf f of
        Lambda body -> whnf (instantiate x body)
        -- L and G are strict in all their arguments
        L -> Apply L (whnf x)
        G -> Apply G (whnf x)
        Apply G a -> Apply (Apply G a) (whnf x)
        Apply (Apply G a) b -> Apply (Apply (Apply G a) b) (whnf x)
        y -> Apply y x
whnf x = x

normalForm :: Name -> Term -> Term
normalForm name t =
    case whnf t of
        Lambda scope -> Lambda . abstract ("0":name) . normalForm ("0":name) . instantiate (Free ("0":name)) $ scope
        Apply x y -> Apply (normalForm ("0":name) x) (normalForm ("1":name) y)
        x -> x
