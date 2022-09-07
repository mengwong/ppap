{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Prettyprinter
    ( Pretty(pretty), Doc, viaShow, (<+>), encloseSep, hsep, semi )
import Data.List (sort)

data PPAP = Apple
          | Pineapple
          | Pen
          deriving (Eq, Ord, Show)

newtype Multi = S [PPAP]
  deriving (Eq)

iHave :: PPAP -> Multi
iHave p = S [p]

instance Semigroup Multi where
  (<>) (S p1) (S p2) = S (sort (p1 ++ p2))

unh :: (Semigroup a, Pretty a) => a -> a -> IO a
unh x y = do
  let xy = x <> y
  print $ pretty x <> semi <+> pretty y <+> "..." <+> "unh:" <+> pretty xy <> "!"
  return xy
infixl 7 `unh`

instance Pretty Multi where
  pretty (S [x])                 = "I have" <+> pretty x
  pretty (S [Pen, Pen])          = "Long Pen"
  pretty (S p)
    | length (filter (== Pen) p) == 2 = dash [ viaShow Pen
                                             , pretty (S [ notp
                                                         | notp <- reverse p
                                                         , notp /= Pen])
                                             , viaShow Pen ]
    | otherwise                       = dash (viaShow <$> p)

instance Pretty PPAP where
  pretty p = hsep $ determiner p ++ [ viaShow p ]
    where
      determiner Apple     = ["an"]
      determiner Pen       = ["a" ]
      determiner Pineapple = [    ]


dash :: [Doc ann] -> Doc ann
dash = encloseSep "" "" "-"

prt :: (Pretty a) => a -> Doc ann
prt = pretty

