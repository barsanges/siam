{-# LANGUAGE TemplateHaskell #-}
{- |
   Module      : UI
   Copyright   : Copyright (C) 2023 barsanges

Interface d'une partie de Siam.
-}

module UI
  ( app
  , mkState
  ) where

import Brick ( App(..), BrickEvent(..), BrickEvent(..), EventM, Widget,
               attrMap, halt, str, strWrap, showFirstCursor )
import Brick.Types ( zoom )
import Brick.Widgets.Border ( borderWithLabel, border )
import Brick.Widgets.Center ( hCenter, vCenter )
import Brick.Widgets.Core ( hBox, vBox, (<=>), (<+>), hLimit, vLimit,
                            Padding(..), padRight )
import Brick.Widgets.Edit ( Editor, editor, handleEditorEvent, renderEditor,
                            getEditContents )
import Data.Maybe ( catMaybes )
import Data.List ( intercalate )
import qualified Graphics.Vty as V
import Lens.Micro.Mtl ( use, (.=) )
import Lens.Micro.TH ( makeLenses )

import Board
import Rules
import Utils

-- | Les noms utilisés pour identifier les ressources de l'interface.
type Name = ()

-- | L'état de l'interface.
data State = State { _currentGame :: Game
                   , _prevGame :: Maybe Game
                   , _minibuffer :: Editor String Name
                   , _echo :: String
                   }
makeLenses ''State

-- | L'application Brick.
app :: App State e Name
app = App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const $ attrMap V.defAttr []
          }

-- | Un minibuffer vide.
emptyMinibuffer :: Editor String Name
emptyMinibuffer = editor () (Just 1) ""

-- | Construit l'état de l'interface à partir d'une partie.
mkState :: Game -> State
mkState g = State { _currentGame = g
                  , _prevGame = Nothing
                  , _minibuffer = emptyMinibuffer
                  , _echo = ""
                  }

-- | Affiche l'interface.
drawUI :: State -> [Widget Name]
drawUI x = [window <=> e <=> mb]
  where
    window = case _currentGame x of
      Ongoing f b -> drawBoard b <+> (drawDescrOngoing b f <=> help)
      Ended f b -> drawBoard b <+> drawDescrEnded f
    e = if null (_echo x)
        then strWrap " "
        else strWrap (_echo x)
    mb = (str "> ") <+> (renderEditor drawMinibuffer True (_minibuffer x))

-- | Affiche le minibuffer.
drawMinibuffer :: [String] -> Widget Name
drawMinibuffer xs = str (intercalate " " xs)

-- | Affiche le plateau.
drawBoard :: Board -> Widget Name
drawBoard b = borderWithLabel (str "Plateau")
  $ (hLimit 51 $ hCenter $ str "Nord")
  <=> str "       a        b        c        d        e      "
  -- FIXME : se doter d'une fonction pour calculer la taille des éléments,
  -- plutôt que de renseigner des chiffres en dur.
  <=> ( (vLimit 25 $ vCenter $ vBox $ fmap (\ x -> str [x, ' ']) "Ouest")
        <+> vBox (fmap (\ x -> str [x]) "  5    4    3    2    1  ")
        <+> vBox [ hBox $ catMaybes
                   [fmap (\ i -> drawCell (lookupCell i b)) (parseIdx (c ++ r))
                   | c <- ["a", "b", "c", "d", "e"]
                   ]
                 | r <- ["1", "2", "3", "4", "5"]
                 ]
        <+> vBox (fmap (\ x -> str [x]) "  5    4    3    2    1  ")
        <+> (vLimit 25 $ vCenter $ vBox $ fmap (\ x -> str [' ', x]) "Est")
      )
  <=> str "       a        b        c        d        e      "
  <=> (hLimit 51 $ hCenter $ str "Sud")
  where
    drawCell :: Maybe Pawn -> Widget Name
    drawCell mp = border (vBox [str top, str middle, str bottom])
      where
        dir = case mp of
          Nothing -> Nothing
          Just (Animal _ x) -> Just x
          Just _ -> Nothing
        c = case mp of
          Nothing -> " "
          Just Rock -> "■"
          Just (Animal Rhino _) -> "R"
          Just (Animal Elephant _) -> "E"
        (top, middle, bottom) = case dir of
          Nothing ->    ("       ", "   " ++ c ++ "   ", "       ")
          Just North -> ("   ·   ", "   " ++ c ++ "   ", "       ")
          Just West ->  ("       ", "  ·" ++ c ++ "   ", "       ")
          Just South -> ("       ", "   " ++ c ++ "   ", "   ·   ")
          Just East ->  ("       ", "   " ++ c ++ "·  ", "       ")

-- | Affiche une brève description de l'état de la partie lorsque celle-ci est
-- en cours.
drawDescrOngoing :: Board -> Faction -> Widget Name
drawDescrOngoing b f = borderWithLabel (str "Etat de la partie")
  $ str " "
  <+> padRight Max (vBox [ str " "
                         , elephantsStatus
                         , rhinosStatus
                         , str " "
                         , elephantsOut
                         , rhinosOut
                         , str " "
                         ])
  <+> str " "
  where
    (elephantsStatus, rhinosStatus) = case f of
      Elephant -> (str "Les éléphants jouent.", str "Les rhinocéros attendent.")
      Rhino -> (str "Les éléphants attendent.", str "Les rhinocéros jouent.")
    elephantsOut = str ("Eléphants en attente : " ++ (show $ numberOut b Elephant))
    rhinosOut = str ("Rhinocéros en attente : " ++ (show $ numberOut b Rhino))

-- | Affiche une brève description de l'état de la partie lorsque celle-ci est
-- terminée.
drawDescrEnded :: Faction -> Widget Name
drawDescrEnded f = borderWithLabel (str "Etat de la partie")
  $ str " "
  <+> padRight Max (descr f)
  <+> str " "
  where
    descr :: Faction -> Widget Name
    descr Elephant = vBox [ str " "
                          , str "Les éléphants ont gagné"
                          , str " "
                          ]
    descr Rhino = vBox [ str " "
                       , str "Les rhinocéros ont gagné"
                       , str " "
                       ]

-- | Affiche une aide succincte.
help :: Widget Name
help = borderWithLabel (str "Commandes")
  $ str " "
  <+> padRight Max (vBox [ strWrap " "
                         , strWrap "Les commandes doivent être renseignées via l'invite de commande débutant par '>'."
                         , strWrap " "
                         , strWrap "Un coup se déclare avec la syntaxe 'x y o', où x est la case de départ, y la case d'arrivée, et o l'orientation finale (N/W/S/E)."
                         , strWrap " "
                         , strWrap "Exemples :"
                         , strWrap "  a1 a2 W   La pièce en a1 se déplace en a2 et termine orientée vers l'ouest."
                         , strWrap "  b5 E      La pièce en b5 s'oriente vers l'est."
                         , strWrap "  r e3 N    Une pièce entre sur le plateau en e3 et est orientée vers le nord."
                         , strWrap "  c4 r      La pièce en c4 sort du plateau."
                         , strWrap " "
                         , strWrap "La commande 'undo' permet d'annuler coup qui vient d'être joué."
                         , strWrap " "
                         , strWrap "La commande 'end' permet de terminer le tour (i.e. : valider le coup qui vient d'être joué)."
                         , strWrap " "
                         , strWrap "La commande 'quit' permet de quitter le jeu."
                         , strWrap " "
                         ])
  <+> str " "

-- | Fait évoluer l'interface en fonction d'un événement.
handleEvent :: BrickEvent Name e -> EventM Name State ()
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  mb <- use minibuffer
  let content = getEditContents mb
  minibuffer .= emptyMinibuffer
  case content of
    [] -> do
      echo .= ""
    (cmd:_) -> case sanitizeStr cmd of
      "quit" -> halt
      "undo" -> do
        mpg <- use prevGame
        case mpg of
          Just pg -> do
            currentGame .= pg
            prevGame .= Nothing
            echo .= "Annulation du dernier coup joué."
          Nothing -> do
            echo .= "Impossible de revenir en arrière, vous n'avez pas encore joué ce tour-ci."
      "" -> do
        echo .= ""
      gibberish -> do
        echo .= "'" ++ gibberish ++ "' n'est pas une commande valide."
handleEvent e = do
    zoom minibuffer $ handleEditorEvent e
