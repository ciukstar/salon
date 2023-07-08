{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Common where

import Data.FileEmbed (embedFile)
import Foundation ( Handler )
import Yesod.Core
    ( TypedContent (TypedContent), ToContent (toContent)
    , typePlain, cacheSeconds, typeSvg
    )


getPhotoPlaceholderR :: Handler TypedContent
getPhotoPlaceholderR = do
    cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
    return $ TypedContent typeSvg
           $ toContent $(embedFile "static/img/person_FILL0_wght400_GRAD0_opsz48.svg")


getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")


getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
