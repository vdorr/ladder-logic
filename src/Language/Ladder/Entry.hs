
module Language.Ladder.Entry where

data Entry s
    = Label   s
    | Device [s]
    | Node    s
    deriving (Show, Eq)


