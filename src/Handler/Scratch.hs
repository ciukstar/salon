{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Scratch (getScratchR) where


import Foundation (Handler)
import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout))
import Settings (widgetFile)

getScratchR :: Handler Html
getScratchR = do
    defaultLayout $(widgetFile "scratch/scratch")
