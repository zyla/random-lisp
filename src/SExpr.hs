module SExpr where

import Data.Text (Text)

data SExpr = Symbol Text | String Text | Num Integer | List [SExpr] deriving (Eq, Show)
