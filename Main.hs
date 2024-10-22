module Main where

import Brick.Main (App(..), neverShowCursor, resizeOrQuit, defaultMain)
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( vBox
  , hBox
  , str
  , padAll
  , padLeft
  , padRight
  , padTop
  , padBottom
  , padTopBottom
  , padLeftRight
  , Padding(..)
  , hLimit
  , vLimit
  , (<+>)
  , (<=>)
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.AttrMap (attrMap)
import qualified Graphics.Vty as V


ui :: Widget () 
ui = 
    B.border $
    vBox [
        hBox [ 
            -- Left thin box
            hLimit 10 $
            vLimit 200 $
            B.borderWithLabel (str "Notebooks") $
            vBox [ str "Left"
                 , str "Panel"
                 , str "TESTESTTESTTESTTEST"
            ]
            
            -- Right thick box
            , B.borderWithLabel (str "Notes") $
              C.hCenter$
              vBox [
                    str "More content here"
              ]
        ]
        -- Bottom box spanning full width
        ,
          C.hCenter $
          padTop Max $
          B.borderWithLabel (str "LittleLazyNotes") $
          vBox [ str "Functions"
               , str "n - New note, b - Browse notes, l - Load different notebook, p - Create new notebook, d - Delete selected note or notebook"
               ]
    ]
app :: App () e ()
app =
    App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = defaultMain app ()

