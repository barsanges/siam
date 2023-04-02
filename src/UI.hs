{- |
   Module      : UI
   Copyright   : Copyright (C) 2023 barsanges

Interface d'une partie de Siam.
-}

module UI
  ( app
  ) where

import Brick ( App(..), BrickEvent(..), BrickEvent(..), EventM, Widget,
               attrMap, halt, str, neverShowCursor )
import Brick.Widgets.Border ( borderWithLabel, border )
import Brick.Widgets.Core ( hBox, vBox )
import qualified Graphics.Vty as V
import Board
import Rules

type Name = ()

app :: App Game e Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const $ attrMap V.defAttr []
          }

-- | Affiche l'interface.
drawUI :: Game -> [Widget Name]
drawUI (Ongoing _ b) = [drawBoard b]
drawUI (Ended _ b) = [drawBoard b]

-- | Affiche le plateau.
drawBoard :: Board -> Widget Name
drawBoard b = borderWithLabel (str "Plateau")
  $ vBox [ hBox [drawCell (lookupCell i b)
                | i <- ix
                ]
         | ix <- idxes
         ]
  where
    drawCell :: Maybe Pawn -> Widget Name
    drawCell mp = border (str marker) -- FIXME : il manque l'orientation
      where
        marker = case mp of
          Nothing -> " "
          Just Rock -> "■"
          Just (Animal Rhino _) -> "R"
          Just (Animal Elephant _) -> "E"

-- | Fait évoluer l'interface en fonction d'un événement.
handleEvent :: BrickEvent Name e -> EventM Name Game ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent _ = return () -- FIXME
