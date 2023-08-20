{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Admin.Brand
  ( getBrandR
  , postBrandR
  , getBrandEditR
  , postBrandEditR
  , postBrandDeleteR
  , getBrandMarkR
  , getBrandCreateR
  ) where

import Control.Monad (void)
import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, TypedContent
    , preEscapedToMarkup, getMessages, FileInfo, redirect
    , SomeMessage (SomeMessage), whamlet, setUltDestCurrent
    )
import Yesod.Form.Fields (unTextarea, fileField, textareaField)
import Yesod.Form.Functions (generateFormPost, mopt)
import Yesod.Form.Types
    ( MForm, FormResult
    , FieldView (fvLabel, fvInput)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Settings (widgetFile)

import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist (Entity (Entity, entityVal))
import Database.Esqueleto.Experimental (selectOne, from, table, delete)

import Foundation
    ( Handler, Widget
    , Route (AdminR, AuthR, PhotoPlaceholderR, AccountPhotoR, StaticR)
    , AdminR (BrandR, BrandCreateR, BrandEditR, BrandDeleteR, BrandMarkR)
    , AppMessage
      ( MsgBrand, MsgYesDelete, MsgPleaseConfirm, MsgPhoto, MsgLogout
      , MsgDeleteAreYouSure, MsgLogout, MsgSave, MsgCancel, MsgBrandMark
      , MsgNoBrandYet, MsgBrandName, MsgBrandStrapline, MsgFavicon, MsgMore
      )
    )
    
import Model (BrandId, Brand (Brand, brandName, brandStrapline, brandMore))
import Settings.StaticFiles (img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg)


getBrandMarkR :: BrandId -> Handler TypedContent
getBrandMarkR bid = undefined


postBrandDeleteR :: Handler Html
postBrandDeleteR = do
    runDB $ delete $ void $ from (table @Brand)
    redirect $ AdminR BrandR


postBrandEditR :: BrandId -> Handler Html
postBrandEditR bid = undefined


getBrandEditR :: BrandId -> Handler Html
getBrandEditR bid = undefined


postBrandR :: Handler Html
postBrandR = undefined


getBrandCreateR :: Handler Html
getBrandCreateR = do
    (fw,et) <- generateFormPost $ formBrand Nothing
    defaultLayout $ do
        setTitleI MsgBrand
        $(widgetFile "admin/brand/create")


formBrand :: Maybe (Entity Brand) -> Html -> MForm Handler (FormResult (Brand,Maybe FileInfo, Maybe FileInfo),Widget)
formBrand brand extra = do
    (markR,markV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgBrandMark
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing
    (nameR,nameV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgBrandName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (brandName . entityVal <$> brand)
    (strapR,strapV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgBrandStrapline
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (brandStrapline . entityVal <$> brand)
    (icoR,icoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgFavicon
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } Nothing
    (moreR,moreV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgMore
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (brandMore . entityVal <$> brand)
    let r = (,,) <$> (Brand Nothing <$> nameR <*> strapR <*> pure Nothing <*> moreR) <*> markR <*> icoR
    let w = [whamlet|
#{extra}
<div.form-field>
  <label>
    <figure>
      $maybe Entity bid _ <- brand
        <img src=@{AdminR $ BrandMarkR bid} alt=_{MsgBrandMark}>
      $nothing
        <img src=@{StaticR img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg} alt=_{MsgBrandMark}>
      <figcaption>
        #{fvLabel markV}
    ^{fvInput markV}
$forall v <- [nameV,strapV,icoV,moreV]
  <div.form-field>
    <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField>
      <span.mdc-text-field__ripple>
      <span.mdc-floating-label>#{fvLabel v}
      ^{fvInput v}
      <span.mdc-line-ripple>
|]
    return (r,w)


getBrandR :: Handler Html
getBrandR = do
    muid <- maybeAuth
    brand <- runDB $ selectOne $ from $ table @Brand
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgBrand
        $(widgetFile "admin/brand/brand")
