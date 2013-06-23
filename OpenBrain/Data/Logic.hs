module OpenBrain.Data.Logic where

data Exp = Var String
         | And Exp Exp
         | Or  Exp Exp
         | Not Exp
         deriving (Eq, Read)

and' = foldr1 And
or'  = foldr1 Or

-- | FIXME check if the syntax really looks like this
instance Show Exp where
  show (Var s)   = s
  show (And x y) = "and(" ++ show x ++ "," ++ show y ++ ")"
  show (Or  x y) =  "or(" ++ show x ++ "," ++ show y ++ ")"
  show (Not x)   = "not(" ++ show x ++ ")"

-- | FIXME build an instance for read/some kind of parser
-- | PostgreSQL Backend uses read to parse atm
