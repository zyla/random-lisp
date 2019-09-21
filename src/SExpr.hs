module SExpr where

import Data.Text (Text)

data SExpr = Symbol Text | String Text | Num Integer | List [SExpr] | Vector [SExpr] | Map [SExpr] deriving (Eq, Show)
