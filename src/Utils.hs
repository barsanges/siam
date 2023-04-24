{- |
   Module      : Utils
   Copyright   : Copyright (C) 2023 barsanges

Utilitaires divers.
-}

module Utils
  ( toLowerStr
  , trimStr
  , sanitizeStr
  ) where

import Data.Char ( isSpace, toLower )
import Data.List ( dropWhileEnd )

-- | Met la chaîne de caractères en minuscules.
toLowerStr :: String -> String
toLowerStr = fmap toLower

-- | Supprime les espaces au début et à la fin de la chaîne de caractères.
trimStr :: String -> String
trimStr = dropWhileEnd isSpace . dropWhile isSpace

-- | Met la chaîne de caractères en minuscules et supprime les espaces au début
-- et à la fin.
sanitizeStr :: String -> String
sanitizeStr = toLowerStr . trimStr
