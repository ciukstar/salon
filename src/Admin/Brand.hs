{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Admin.Brand
  ( getBrandR
  , postBrandR
  , getBrandEditR
  , postBrandEditR
  , postBrandDeleteR
  , getBrandMarkR
  , getBrandIcoR
  , getBrandCreateR
  ) where

import Control.Monad (void)
import Data.Maybe (isNothing)
import Text.Hamlet (Html)
import Data.Text.Encoding (encodeUtf8)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, TypedContent (TypedContent)
    , preEscapedToMarkup, getMessages, FileInfo (fileContentType), redirect
    , SomeMessage (SomeMessage), setUltDestCurrent, fileSourceByteString
    , typeSvg, ToContent (toContent), emptyContent, addMessageI
    )
import Yesod.Form.Fields (unTextarea, fileField, textareaField)
import Yesod.Form.Functions (generateFormPost, mopt, runFormPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldView (fvLabel, fvInput, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Settings (widgetFile)

import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist (Entity (Entity, entityVal), PersistStoreWrite (insert_))
import Database.Esqueleto.Experimental
    (selectOne, from, table, delete, val, where_
    , (^.), (==.), (=.), set, update
    )

import Foundation
    ( Handler, Widget
    , Route (ProfileR, AdminR, AuthR, PhotoPlaceholderR, AccountPhotoR, StaticR)
    , AdminR (BrandR, BrandCreateR, BrandEditR, BrandDeleteR, BrandMarkR, BrandIcoR)
    , AppMessage
      ( MsgBrand, MsgYesDelete, MsgPleaseConfirm, MsgPhoto, MsgBack
      , MsgDeleteAreYouSure, MsgSave, MsgCancel, MsgBrandMark
      , MsgNoBrandYet, MsgBrandName, MsgBrandStrapline, MsgFavicon, MsgMore
      , MsgRecordAdded, MsgRecordEdited, MsgRecordDeleted, MsgNavigationMenu
      , MsgLogin, MsgUserProfile, MsgEdit, MsgDel
      )
    )
    
import Model
    ( BrandId
    , Brand
      (Brand, brandName, brandStrapline, brandMore, brandMark, brandIco
      , brandMarkMime, brandIcoMime
      )
    , EntityField
      ( BrandId, BrandMark, BrandName, BrandStrapline, BrandMore, BrandMarkMime
      , BrandIco, BrandIcoMime
      )
    )
    
import Settings.StaticFiles (img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg)

import Menu (menu)


getBrandIcoR :: BrandId -> Handler TypedContent
getBrandIcoR bid = do
    brand <- runDB $ selectOne $ do
        x <- from $ table @Brand
        where_ $ x ^. BrandId ==. val bid
        return x
    return $ case brand of
      Just (Entity _ (Brand _ _ _ _ (Just bs) (Just mime) _)) -> TypedContent (encodeUtf8 mime) (toContent bs)
      _ -> TypedContent typeSvg emptyContent


getBrandMarkR :: BrandId -> Handler TypedContent
getBrandMarkR bid = do
    brand <- runDB $ selectOne $ do
        x <- from $ table @Brand
        where_ $ x ^. BrandId ==. val bid
        return x
    return $ case brand of
      Just (Entity _ (Brand (Just bs) (Just mime) _ _ _ _ _)) -> TypedContent (encodeUtf8 mime) (toContent bs)
      _ -> TypedContent typeSvg emptyContent


postBrandDeleteR :: Handler Html
postBrandDeleteR = do
    runDB $ delete $ void $ from (table @Brand)
    addMessageI "info" MsgRecordDeleted
    redirect $ AdminR BrandR


postBrandEditR :: BrandId -> Handler Html
postBrandEditR bid = do
    ((fr,fw),et) <- runFormPost $ formBrand Nothing
    case fr of
      FormSuccess (r,mmark,mico) -> do
          (mark,markMime) <- (,fileContentType <$> mmark) <$> mapM fileSourceByteString mmark
          (ico,icoMime) <- (,fileContentType <$> mico) <$> mapM fileSourceByteString mico
          runDB $ update $ \x -> do
              set x [ BrandName =. val (brandName r)
                    , BrandStrapline =. val (brandStrapline r)
                    , BrandMore =. val (brandMore r)
                    ]
              where_ $ x ^. BrandId ==. val bid
          case mark of
            Just x -> runDB $ update $ \y -> do
              set y [ BrandMark =. val (Just x), BrandMarkMime =. val markMime ]
              where_ $ y ^. BrandId ==. val bid
            Nothing -> return ()
          case ico of
            Just x -> runDB $ update $ \y -> do
              set y [ BrandIco =. val (Just x), BrandIcoMime =. val icoMime ]
              where_ $ y ^. BrandId ==. val bid
            Nothing -> return ()
          addMessageI "info" MsgRecordEdited
          redirect $ AdminR BrandR
      _ -> defaultLayout $ do
          setTitleI MsgBrand
          $(widgetFile "admin/brand/edit")


getBrandEditR :: BrandId -> Handler Html
getBrandEditR bid = do
    brand <- runDB $ selectOne $ do
        x <- from $ table @Brand
        where_ $ x ^. BrandId ==. val bid
        return x
    (fw,et) <- generateFormPost $ formBrand brand
    defaultLayout $ do
        setTitleI MsgBrand
        $(widgetFile "admin/brand/edit")


postBrandR :: Handler Html
postBrandR = do
    ((fr,fw),et) <- runFormPost $ formBrand Nothing
    case fr of
      FormSuccess (r,mmark,mico) -> do
          (mark,markMime) <- (,fileContentType <$> mmark) <$> mapM fileSourceByteString mmark
          (ico,icoMime) <- (,fileContentType <$> mico) <$> mapM fileSourceByteString mico
          runDB $ insert_ $ Brand { brandMark = mark
                                  , brandMarkMime = markMime
                                  , brandName = brandName r
                                  , brandStrapline = brandStrapline r
                                  , brandIco = ico
                                  , brandIcoMime = icoMime
                                  , brandMore = brandMore r
                                  }
          addMessageI "info" MsgRecordAdded
          redirect $ AdminR BrandR
      _ -> defaultLayout $ do
          setTitleI MsgBrand
          $(widgetFile "admin/brand/create")


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
        , fsAttrs = [("style","display:none"),("accept","image/*")]
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
        , fsAttrs = [("style","display:none"),("accept","image/ico,.ico")]
        } Nothing
    (moreR,moreV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgMore
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (brandMore . entityVal <$> brand)
    let r = (,,) <$>
            (Brand Nothing Nothing <$> nameR <*> strapR <*> pure Nothing <*> pure Nothing <*> moreR)
            <*> markR <*> icoR
    let w = $(widgetFile "admin/brand/form")
    return (r,w)


getBrandR :: Handler Html
getBrandR = do
    user <- maybeAuth
    brand <- runDB $ selectOne $ from $ table @Brand
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgBrand
        $(widgetFile "admin/brand/brand")
