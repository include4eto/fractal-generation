module FractalWindow (
        display,
        ClickEvent
    )
    where

import Graphics.Gloss.Interface.Pure.Game

type ClickEvent = (String -> String)

menuOptions = ["Mandelbrots"]
display :: ClickEvent -> IO ()
display = undefined
