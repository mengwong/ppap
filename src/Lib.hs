{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Prettyprinter
    ( Pretty(pretty), Doc, viaShow, (<+>), encloseSep, hsep, semi )
import Data.List (sort)

data PPAP = Apple
          | Pineapple
          | Pen
          deriving (Eq, Ord, Show)

newtype Multi = M [PPAP]
  deriving (Eq)

iHave :: PPAP -> Multi
iHave p = M [p]

instance Semigroup Multi where
  (<>) (M p1) (M p2) = M (sort (p1 ++ p2))

unh :: (Semigroup a, Pretty a) => a -> a -> IO a
unh x y = do
  let xy = x <> y
  print $ pretty x <> semi <+> pretty y <+> "..." <+> "unh:" <+> pretty xy <> "!"
  return xy
infixl 7 `unh`

instance Pretty Multi where
  pretty (M [x])                 = "I have" <+> pretty x
  pretty (M [Pen, Pen])          = "Long Pen"
  pretty (M p)
    | length (filter (== Pen) p) == 2 = dash [ viaShow Pen
                                             , pretty (M [ notp
                                                         | notp <- reverse p
                                                         , notp /= Pen])
                                             , viaShow Pen ]
    | otherwise                       = dash (viaShow <$> p)
    where
      dash :: [Doc ann] -> Doc ann
      dash = encloseSep "" "" "-"

instance Pretty PPAP where
  pretty p = hsep $ determiner p ++ [ viaShow p ]
    where
      determiner Apple     = ["an"]
      determiner Pen       = ["a" ]
      determiner Pineapple = [    ]

prt :: (Pretty a) => a -> Doc ann
prt = pretty

