module Term (Name, Term(..), Scope, abstract, instantiate, substitute)
where

type Name = [String]

data Term 
    = Free Name
    | Bound Int
    | Lambda Scope
    | Apply Term Term
    | G | L

newtype Scope = Scope Term

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
