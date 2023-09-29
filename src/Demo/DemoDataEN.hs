{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Demo.DemoDataEN (populateEN) where

import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (st)
import qualified Data.ByteString.Base64 as B64 (decode)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay,utctDayTime), DiffTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.LocalTime (timeToTimeOfDay, utc)
import Control.Monad.IO.Class (MonadIO (liftIO))
import ClassyPrelude.Yesod (ReaderT)
import Yesod.Form.Fields (Textarea (Textarea))
import Yesod.Auth.Util.PasswordStore (makePassword)
import Database.Persist.Sql (SqlBackend)
import Database.Persist ( PersistStoreWrite(insert_, insert) )

import Model
    ( User (User, userName, userPassword, userAdmin, userEmail, userFullName)
    , UserPhoto (UserPhoto, userPhotoUser, userPhotoPhoto, userPhotoMime)
    , Service
      ( Service, serviceName, serviceDescr, serviceGroup, serviceOverview
      , servicePublished, serviceDuration
      )
    , Thumbnail (Thumbnail, thumbnailService, thumbnailPhoto, thumbnailMime, thumbnailAttribution)
    , Offer
      ( Offer, offerName, offerPrice, offerPrefix
      , offerSuffix, offerDescr, offerService
      )
    , EmplStatus (EmplStatusEmployed, EmplStatusDismissed)
    , Staff (Staff, staffName, staffStatus, staffPhone, staffMobile, staffEmail, staffUser)
    , StaffPhoto (StaffPhoto, staffPhotoPhoto, staffPhotoMime, staffPhotoStaff)
    , Role (Role, roleStaff, roleService, roleName, roleRating)
    , Contents (Contents, contentsSection, contentsContent)
    , BookStatus (BookStatusRequest)
    , Book (Book, bookUser, bookOffer, bookRole, bookDay, bookTime, bookTz, bookStatus)
    )
import Data.FileEmbed (embedFile)
import Demo.DemoPhotos
    ( man01, man02, man03, man04, man05, man06
    , woman01, woman02, woman03, woman04, woman05
    )

populateEN :: MonadIO m => ReaderT SqlBackend m ()
populateEN = do

    (today,time) <- liftIO $ getCurrentTime >>= \x -> return (utctDay x,timeToTimeOfDay (utctDayTime x))

    insert_ $ Contents { contentsSection = "CONTACTS"
                       , contentsContent = Textarea [st|
<section style="margin:0 1rem">
  <h3 style="color:gray">Call Us</h3>
  <dl>
    <dt><i>Telephone</i></dt>
    <dd>937-810-6140</dd>
    <dt><i>Mobile</i></dt>
    <dd>567-274-7469</dd>
  </dl>
</section>
<section style="margin:0 1rem">
  <h3 style="color:gray">Email Us</h3>
  <dl>
    <dt><i>Email</i></dt>
    <dd>salon@mail.org</dd>
  </dl>
</section>
<section style="margin:0 1rem">
  <h3 style="color:gray">Come see us</h3>
  <dl>
    <dt><i>Address</i><dt>
    <dd>5331 Rexford Court, Montgomery AL 36116</dd>
  </dl>
</section>
<p>
  <iframe width="100%" height="400px" loding="lazy" title="Salon" style="border:none" src='https://api.mapbox.com/styles/v1/mapbox/streets-v12.html?title=false&zoomwheel=false&access_token=pk.eyJ1IjoiY2l1a3N0YXIiLCJhIjoiY2o1enNibDNsMGNrNDJ3dDhxeTJuc3luMiJ9.Jgc5GdYUMbYwGq-zRWtzfw'></iframe>
</p>
|]
                       }

    insert_ $ Contents { contentsSection = "ABOUT_US"
                       , contentsContent = Textarea [st|
<h2 style="color:gray">Our Mission</h2>
<p>
The mission of <i>Salon</i> is simple: to offer a special environment to every individual who walks through their doors, where they can indulge, groom and pamper themselves, while enhancing their personal image and bringing a feeling of well-being into their lives.
</p>
<h2 style="color:gray">Our Ethos</h2>
<p>
Any individual who comes to our salons is unique. We treat each unique client to their personal needs. We pride ourselves in offering the service that the customer expects and the treatments he/she requires, and we will continuously reassess their demands based on their lifestyle and bodies.
We will never offer treatments that are unnecessary and make every client a priority. This is exactly the reason why we can proudly say that, over the years, we have built up a loyal client base. We learn from them as we look after them, and endeavour to keep up with the latest trends & treatments available to ensure we always meet the needs of our valued clients and any future visitors.
</p>
<h2 style="color:gray">Our Goals</h2>
<p>
We will continue to offer the latest treatments, the most innovative techniques while using the best products on the market place. All this in elegant, clean and welcoming environments with trained, professional and friendly therapists. We will endeavour to divulge our message that is everyone’s right to feel good!
</p>
|]
                       }

    pass0 <- liftIO $ makePassword "root" 17
    insert_ $ User { userName = "root"
                   , userPassword = decodeUtf8 pass0
                   , userAdmin = True
                   , userFullName = Just "The Root"
                   , userEmail = Just "theroot@mail.en"
                   }

    pass1 <- liftIO $ makePassword "johnnysmith" 17
    u1 <- insert $ User { userName = "johnnysmith"
                        , userPassword = decodeUtf8 pass1
                        , userAdmin = False
                        , userFullName = Just "Johnny Smith"
                        , userEmail = Just "jsmith@mail.en"
                        }

    e1 <- insert $ Staff { staffName = "Johnny Smith"
                         , staffStatus = EmplStatusEmployed
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "jsmith@mail.en"
                         , staffUser = Just u1
                         }

    case B64.decode man01 of
      Left _ -> return ()
      Right x -> do
          insert_ $ StaffPhoto { staffPhotoStaff = e1
                               , staffPhotoPhoto = x
                               , staffPhotoMime = "image/avif"
                               }
          insert_ $ UserPhoto { userPhotoUser = u1
                              , userPhotoPhoto = x
                              , userPhotoMime = "image/avif"
                              }

    pass2 <- liftIO $ makePassword "marylopez" 17
    u2 <- insert $ User { userName = "marylopez"
                        , userPassword = decodeUtf8 pass2
                        , userAdmin = False
                        , userFullName = Just "Mary Lopez"
                        , userEmail = Just "mlopez@mail.en"
                        }

    e2 <- insert $ Staff { staffName = "Mary Lopez"
                         , staffStatus = EmplStatusEmployed
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "mlopez@mail.en"
                         , staffUser = Just u2
                         }

    case B64.decode woman01 of
      Left _ -> return ()
      Right x -> do
          insert_ $ StaffPhoto { staffPhotoStaff = e2
                               , staffPhotoPhoto = x
                               , staffPhotoMime = "image/avif"
                               }
          insert_ $ UserPhoto { userPhotoUser = u2
                              , userPhotoPhoto = x
                              , userPhotoMime = "image/avif"
                              }

    pass3 <- liftIO $ makePassword "johnjohnson" 17
    u3 <- insert $ User { userName = "johnjohnson"
                        , userPassword = decodeUtf8 pass3
                        , userAdmin = False
                        , userFullName = Just "John Johnson"
                        , userEmail = Just "jjohnson@mail.en"
                        }

    e3 <- insert $ Staff { staffName = "John Johnson"
                         , staffStatus = EmplStatusEmployed
                        , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "jjohnson@mail.en"
                         , staffUser = Just u3
                         }

    case B64.decode man02 of
      Left _ -> return ()
      Right x -> do
          insert_ $ StaffPhoto { staffPhotoStaff = e3
                               , staffPhotoPhoto = x
                               , staffPhotoMime = "image/avif"
                               }
          insert_ $ UserPhoto { userPhotoUser = u3
                              , userPhotoPhoto = x
                              , userPhotoMime = "image/avif"
                              }

    pass4 <- liftIO $ makePassword "patriciabrown" 17
    u4 <- insert $ User { userName = "patriciabrown"
                        , userPassword = decodeUtf8 pass4
                        , userAdmin = False
                        , userFullName = Just "Patricia Brown"
                        , userEmail = Just "pbrown@mail.en"
                        }

    e4 <- insert $ Staff { staffName = "Patricia Brown"
                         , staffStatus = EmplStatusEmployed
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "pbrown@mail.en"
                         , staffUser = Just u4
                         }

    case B64.decode woman02 of
      Left _ -> return ()
      Right x -> do
          insert_ $ StaffPhoto { staffPhotoStaff = e4
                               , staffPhotoPhoto = x
                               , staffPhotoMime = "image/avif"
                               }
          insert_ $ UserPhoto { userPhotoUser = u4
                              , userPhotoPhoto = x
                              , userPhotoMime = "image/avif"
                              }

    pass5 <- liftIO $ makePassword "chriswilson" 17
    u5 <- insert $ User { userName = "chriswilson"
                        , userPassword = decodeUtf8 pass5
                        , userAdmin = False
                        , userFullName = Just "Chris Wilson"
                        , userEmail = Just "cwilson@mail.en"
                        }

    e5 <- insert $ Staff { staffName = "Chris Wilson"
                         , staffStatus = EmplStatusEmployed
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "cwilson@mail.en"
                         , staffUser = Just u5
                         }

    case B64.decode man03 of
      Left _ -> return ()
      Right x -> do
          insert_ $ StaffPhoto { staffPhotoStaff = e5
                               , staffPhotoPhoto = x
                               , staffPhotoMime = "image/avif"
                               }
          insert_ $ UserPhoto { userPhotoUser = u5
                              , userPhotoPhoto = x
                              , userPhotoMime = "image/avif"
                              }

    e6 <- insert $ Staff { staffName = "Philip Davis"
                         , staffStatus = EmplStatusEmployed
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "phdavis@mail.en"
                         , staffUser = Nothing
                         }

    case B64.decode man04 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e6
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e7 <- insert $ Staff { staffName = "Helen Taylor"
                         , staffStatus = EmplStatusEmployed
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "htaylor@mail.en"
                         , staffUser = Nothing
                         }

    case B64.decode woman03 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e7
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e8 <- insert $ Staff { staffName = "Barbara Young"
                         , staffStatus = EmplStatusEmployed
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "byoung@mail.en"
                         , staffUser = Nothing
                         }

    case B64.decode woman04 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e8
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e9 <- insert $ Staff { staffName = "Jorge Walker"
                         , staffStatus = EmplStatusEmployed
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "jwalker@mail.en"
                         , staffUser = Nothing
                         }

    case B64.decode man05 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e9
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e10 <- insert $ Staff { staffName = "Robert William Evans"
                         , staffStatus = EmplStatusEmployed
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "revans@mail.en"
                         , staffUser = Nothing
                         }

    case B64.decode man06 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e10
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e11 <- insert $ Staff { staffName = "Isabel Hughes"
                          , staffStatus = EmplStatusDismissed
                          , staffPhone = Just "0491 570 006"
                          , staffMobile = Just "0491 570 156"
                          , staffEmail = Just "ihughes@mail.en"
                          , staffUser = Nothing
                          }

    case B64.decode woman05 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e11
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }



    s1 <- insert $ Service { serviceName = "Hair care"
                           , serviceOverview = Just "Hair Care Services"
                           , servicePublished = True
                           , serviceDescr = Just "<p>Always distinctive and never run of the mill, our hair care experts have trained extensively to provide designer cuts and styling services that are customized to each client’s needs. As a salon we support our team in their efforts to perfect their individual techniques, and we give our stylists the freedom to fully express and explore their creativity. This in turn gives our guests the opportunity to enjoy personalized service each and every time. Whether you love modern looks or a classic cut speaks to your signature style – the options are endless with us.</p>"
                           , serviceDuration = duration "01:30"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s1
                        , thumbnailPhoto = $(embedFile "static/img/hair-care.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s11 <- insert $ Service { serviceName = "Men hair cuts"
                            , servicePublished = True
                            , serviceOverview = Just "Hair cuts for men"
                            , serviceDescr = Just "Hair cuts for men"
                            , serviceDuration = duration "01:00"
                            , serviceGroup = Just s1
                            }

    insert_ $ Role { roleStaff = e1
                          , roleService = s11
                          , roleName = "Makeup artist"
                          , roleRating = Just 5
                          }

    insert_ $ Offer { offerService = s11
                        , offerName = "Price"
                        , offerPrice = 26
                        , offerPrefix = Just "$"
                        , offerSuffix = Nothing
                        , offerDescr = Nothing
                        }

    insert_ $ Thumbnail { thumbnailService = s11
                        , thumbnailPhoto = $(embedFile "static/img/men-haircuts.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s12 <- insert $ Service { serviceName = "Women hair cuts (above shoulders)"
                            , servicePublished = True
                            , serviceOverview = Just "Hair cuts above shoulders for women"
                            , serviceDescr = Just "Hair cuts above shoulders for women"
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s1
                            }

    o121 <- insert $ Offer { offerService = s12
                           , offerName = "Price"
                           , offerPrice = 28
                           , offerPrefix = Just "$"
                           , offerSuffix = Nothing
                           , offerDescr = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s12
                        , thumbnailPhoto = $(embedFile "static/img/women-hair-cuts-above-shoulders.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e2
                   , roleService = s12
                   , roleName = "Barbers"
                   , roleRating = Just 3
                   }

    s13 <- insert $ Service { serviceName = "Women hair cuts (below shoulders)"
                            , servicePublished = True
                            , serviceOverview = Just "Hair cuts below shoulders for women"
                            , serviceDescr = Just "Hair cuts below shoulders for women"
                            , serviceDuration = duration "01:35"
                            , serviceGroup = Just s1
                            }

    insert_ $ Offer { offerService = s13
                    , offerName = "Price"
                    , offerPrice = 35
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s13
                        , thumbnailPhoto = $(embedFile "static/img/women-hair-cuts-below-shoulders.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e11
                   , roleService = s13
                   , roleName = "Stylist"
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s13
                   , roleName = "Assistant stylist"
                   , roleRating = Just 4
                   }

    s14 <- insert $ Service { serviceName = "Children hair cuts"
                            , servicePublished = True
                            , serviceOverview = Just "Hair cuts for children"
                            , serviceDescr = Just "Hair cuts for children"
                            , serviceDuration = duration "01:20"
                            , serviceGroup = Just s1
                            }

    insert_ $ Offer { offerService = s14
                        , offerName = "Price"
                        , offerPrice = 16
                        , offerPrefix = Just "$"
                        , offerSuffix = Just "-$20 (depending on the length of their hair)"
                        , offerDescr = Nothing
                        }

    insert_ $ Thumbnail { thumbnailService = s14
                        , thumbnailPhoto = $(embedFile "static/img/children-hair-cuts.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e4
                   , roleService = s14
                   , roleName = "Junior stylist"
                   , roleRating = Just 5
                   }

    s15 <- insert $ Service { serviceName = "Chemical services"
                            , servicePublished = True
                            , serviceOverview = Just "Chemical services"
                            , serviceDescr = Just "<p>Our chemical services address a wide range of hair care needs. Our smoothing treatments combat frizz, increase manageability and provide the long-lasting results you’ve always desired. You can enjoy smooth and silky locks with our keratin service, as well as repair damage to your hair by replenishing lost protein. If added waves, curl and volume are what you’re looking for, our professional perm services will allow you to achieve your desired texture. We utilize innovative products and techniques to establish defined curls you are sure to love.</p>"
                            , serviceDuration = duration "02:00"
                            , serviceGroup = Just s1
                            }

    insert_ $ Thumbnail { thumbnailService = s15
                        , thumbnailPhoto = $(embedFile "static/img/chemical-services.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s151 <- insert $ Service { serviceName = "Conditioning"
                             , servicePublished = True
                             , serviceOverview = Just "Conditioning services"
                             , serviceDescr = Just "Conditioning"
                             , serviceDuration = duration "01:35"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s151
                        , thumbnailPhoto = $(embedFile "static/img/conditioning.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s1511 <- insert $ Service { serviceName = "After Perm Conditioner"
                              , servicePublished = True
                              , serviceOverview = Just "After Perm Conditioner"
                              , serviceDescr = Just "After Perm Conditioner"
                              , serviceDuration = duration "01:25"
                              , serviceGroup = Just s151
                              }

    insert_ $ Offer { offerService = s1511
                    , offerName = "Price"
                    , offerPrice = 99
                    , offerPrefix = Just "$"
                    , offerSuffix = Just " & up"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s1511
                        , thumbnailPhoto = $(embedFile "static/img/after-perm-conditioner.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    r51511 <- insert $ Role { roleStaff = e5
                            , roleService = s1511
                            , roleName = "Hairdresser"
                            , roleRating = Just 2
                            }

    s1512 <- insert $ Service { serviceName = "Before Perm Conditioner"
                              , servicePublished = True
                              , serviceOverview = Just "Before Perm Conditioner"
                              , serviceDescr = Just "Before Perm Conditioner"
                              , serviceDuration = duration "01:15"
                              , serviceGroup = Just s151
                              }

    insert_ $ Offer { offerService = s1512
                    , offerName = "Price"
                    , offerPrice = 110
                    , offerPrefix = Just "$"
                    , offerSuffix = Just " & up"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s1512
                        , thumbnailPhoto = $(embedFile "static/img/before-perm-conditioner.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s152 <- insert $ Service { serviceName = "Highlights & Color"
                             , servicePublished = True
                             , serviceOverview = Just "Highlights & Color"
                             , serviceDescr = Just "Highlights & Color"
                             , serviceDuration = duration "01:10"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s152
                        , thumbnailPhoto = $(embedFile "static/img/highlights-and-color.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s1521 <- insert $ Service { serviceName = "Full"
                              , servicePublished = True
                              , serviceOverview = Just "Full"
                              , serviceDescr = Just "Full"
                              , serviceDuration = duration "01:00"
                              , serviceGroup = Just s152
                              }

    insert_ $ Offer { offerService = s1521
                    , offerName = "Price"
                    , offerPrice = 130
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s1521
                        , thumbnailPhoto = $(embedFile "static/img/highlights-and-color-full.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e8
                   , roleService = s1521
                   , roleName = "Colourist"
                   , roleRating = Just 5
                   }

    s1522 <- insert $ Service { serviceName = "Partial"
                              , servicePublished = True
                              , serviceOverview = Just "Partial"
                              , serviceDescr = Just "Partial"
                              , serviceDuration = duration "01:15"
                              , serviceGroup = Just s152
                              }

    insert_ $ Offer { offerService = s1522
                    , offerName = "Price"
                    , offerPrice = 68
                    , offerPrefix = Just "$"
                    , offerSuffix = Just " & up"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s1522
                        , thumbnailPhoto = $(embedFile "static/img/highlights-and-color-partial.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s1523 <- insert $ Service { serviceName = "Permanent Color"
                              , servicePublished = True
                              , serviceOverview = Just "Permanent Color"
                              , serviceDescr = Just "Permanent Color"
                              , serviceDuration = duration "01:45"
                              , serviceGroup = Just s152
                              }

    insert_ $ Offer { offerService = s1523
                    , offerName = "Price"
                    , offerPrice = 68
                    , offerPrefix = Just "$"
                    , offerSuffix = Just " & up"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s1523
                        , thumbnailPhoto = $(embedFile "static/img/highlights-and-color-permanent.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s153 <- insert $ Service { serviceName = "Perm"
                             , servicePublished = True
                             , serviceOverview = Just "Perm"
                             , serviceDescr = Just "Perm"
                             , serviceDuration = duration "00:45"
                             , serviceGroup = Just s15
                             }

    s1531 <- insert $ Service { serviceName = "Full Perm"
                              , servicePublished = True
                              , serviceOverview = Just "Full Perm"
                              , serviceDescr = Just "Full Perm"
                              , serviceDuration = duration "00:35"
                              , serviceGroup = Just s153
                              }

    insert_ $ Offer { offerService = s1531
                        , offerName = "Price"
                        , offerPrice = 79
                        , offerPrefix = Just "$"
                        , offerSuffix = Just " & up"
                        , offerDescr = Nothing
                        }

    s1532 <- insert $ Service { serviceName = "Acid Repair Perm"
                              , servicePublished = True
                              , serviceOverview = Just "Acid Repair Perm"
                              , serviceDescr = Just "Acid Repair Perm"
                              , serviceDuration = duration "01:25"
                              , serviceGroup = Just s153
                              }

    insert_ $ Offer { offerService = s1532
                        , offerName = "Price"
                        , offerPrice = 89
                        , offerPrefix = Just "$"
                        , offerSuffix = Just " & up"
                        , offerDescr = Nothing
                        }

    s1533 <- insert $ Service { serviceName = "Japanese Straightening Perm"
                              , servicePublished = True
                              , serviceOverview = Just "Japanese Straightening Perm"
                              , serviceDescr = Just "Japanese Straightening Perm"
                              , serviceDuration = duration "01:25"
                              , serviceGroup = Just s153
                              }

    insert_ $ Offer { offerService = s1533
                        , offerName = "Price"
                        , offerPrice = 250
                        , offerPrefix = Just "$"
                        , offerSuffix = Just " & up"
                        , offerDescr = Nothing
                        }

    s2 <- insert $ Service { serviceName = "Facial Treatments"
                           , servicePublished = True
                           , serviceOverview = Just "Facial Treatments"
                           , serviceDescr = Just "<p>Your face is an expressive canvass that shows experience and emotion. At one of the best salons around, our palette holds nourishing treatments, which enhances, emphasizes beauty, youth and color for your body. Before any facial, our professional esthetician will give you a consolidation and work from there You won’t believe the difference!</p><p>All facial treatments include eyebrow shaping.</p>"
                           , serviceDuration = duration "01:45"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s2
                        , thumbnailPhoto = $(embedFile "static/img/facial-treatments.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s21 <- insert $ Service { serviceName = "Basic Facial (60 min)"
                            , servicePublished = True
                            , serviceOverview = Just "Basic Facial (60 min)"
                            , serviceDescr = Just "Free Deep cleansing, exfoliation with steam treatment, followed by extractions, then eyebrow shaping; a de-stressing massage for the face, neck & shoulders. A custom mask, plus regular eye treatment, followed by moisturizer/sunscreen application. This relaxing but serious cleansing treatment will leave you with a clean, fresh, & glowing complexion."
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s21
                    , offerName = "Price"
                    , offerPrice = 55
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s21
                    , offerName = "Package"
                    , offerPrice = 250
                    , offerPrefix = Just "$"
                    , offerSuffix = Just "/5 sessions"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s21
                        , thumbnailPhoto = $(embedFile "static/img/facial-treatments-basic-facial.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e10
                   , roleService = s21
                   , roleName = "Esthetician"
                   , roleRating = Just 5
                   }

    s22 <- insert $ Service { serviceName = "Deluxe Facial"
                            , servicePublished = True
                            , serviceOverview = Just "Deluxe Facial"
                            , serviceDescr = Just "This special facial can be customized to the client’s skin situation (ie. dry, oily, sensitive, etc.) It is created to smooth and soften your complexion while it de-stresses your entire body. Our deluxe facial will make you feel and look healthier."
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s22
                    , offerName = "Price"
                    , offerPrice = 75
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s22
                    , offerName = "Package"
                    , offerPrice = 350
                    , offerPrefix = Just "$"
                    , offerSuffix = Just "/5 sessions"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s22
                        , thumbnailPhoto = $(embedFile "static/img/facial-treatments-deluxe-facial.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s23 <- insert $ Service { serviceName = "Pampering Facial (90 min)"
                            , servicePublished = True
                            , serviceOverview = Just "Pampering Facial (90 min)"
                            , serviceDescr = Just "A hydrating clinical treatment, creating a cooling effect on the skin to revitalize, moisturize, and soothe. Its thermo-cooling effect on the skin makes it a remarkable revitalizing treatment particularly for reducing redness. ALGOMASK+ offers instant radiance and long-lasting hydration."
                            , serviceDuration = duration "00:90"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s23
                    , offerName = "Price"
                    , offerPrice = 90
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s23
                    , offerName = "Package"
                    , offerPrice = 400
                    , offerPrefix = Just "$"
                    , offerSuffix = Just "/5 sessions"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s23
                        , thumbnailPhoto = $(embedFile "static/img/facial-treatments-pampering-facial.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s24 <- insert $ Service { serviceName = "Acne Treatment (120 min)"
                            , servicePublished = True
                            , serviceOverview = Just "Acne Treatment (120 min)"
                            , serviceDescr = Just "It is a very innovative and effective way of treating acne conditions that have not responded to other treatments and has produced many remarkable results. Urea peroxide, alpha-hydroxyl acids, and a special anti-androgen element are incorporated in Rejuvi Normalizing Formula."
                            , serviceDuration = duration "02:00"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s24
                    , offerName = "Price"
                    , offerPrice = 95
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s24
                    , offerName = "Package"
                    , offerPrice = 600
                    , offerPrefix = Just "$"
                    , offerSuffix = Just "/7 sessions"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s24
                        , thumbnailPhoto = $(embedFile "static/img/facial-treatments-acne-treatment.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s25 <- insert $ Service { serviceName = "24k Gold Whitening Treatment"
                            , servicePublished = True
                            , serviceOverview = Just "24k Gold Whitening Treatment"
                            , serviceDescr = Just "<p>A luxurious age defying facial treatment. This hydrating active age defying formula combines the power of pure vitamins, plant extracts and 24k gold. These ingredients effectively assist in the stimulation of collagen manufacturing. They form a continuous protective barrier to mimic the effects of surgery.</p><p>The mask fits like a “second skin” and perfectly adapts to the contours of the face. It delivers maximum hydration, fortifies the skin’s natural protective barrier, and rejuvenate your sensitive skin to reduce signs of aging.</p>"
                            , serviceDuration = duration "01:15"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s25
                    , offerName = "Price"
                    , offerPrice = 100
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s25
                    , offerName = "Package"
                    , offerPrice = 460
                    , offerPrefix = Just "$"
                    , offerSuffix = Just "/5 sessions"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s25
                        , thumbnailPhoto = $(embedFile "static/img/facial-treatments-24k-gold-whitening-treatment.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s3 <- insert $ Service { serviceName = "Advanced Facial Treatments"
                           , servicePublished = True
                           , serviceOverview = Just "Advanced Facial Treatments"
                           , serviceDescr = Just "Advanced Facial Treatments"
                           , serviceDuration = duration "01:10"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s3
                        , thumbnailPhoto = $(embedFile "static/img/advanced-facial-treatments.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s4 <- insert $ Service { serviceName = "Anti-Aging Treatments"
                           , servicePublished = True
                           , serviceOverview = Just "Anti-Aging Treatments"
                           , serviceDescr = Just "Anti-Aging Treatments"
                           , serviceDuration = duration "01:30"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s4
                        , thumbnailPhoto = $(embedFile "static/img/anti-aging-treatments.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s5 <- insert $ Service { serviceName = "Eye Treatment Center"
                           , servicePublished = True
                           , serviceOverview = Just "Eye Treatment Center"
                           , serviceDescr = Just "Eye Treatment Center"
                           , serviceDuration = duration "02:00"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s5
                        , thumbnailPhoto = $(embedFile "static/img/eye-treatment-center.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s6 <- insert $ Service { serviceName = "Body Massage"
                           , servicePublished = True
                           , serviceOverview = Just "Body Massage"
                           , serviceDescr = Just "Body Massage"
                           , serviceDuration = duration "02:00"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s6
                        , thumbnailPhoto = $(embedFile "static/img/body-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e9
                   , roleService = s6
                   , roleName = "Massage Therapist"
                   , roleRating = Just 5
                   }

    s7 <- insert $ Service { serviceName = "Makeup Services"
                           , servicePublished = True
                           , serviceOverview = Just "Makeup Services"
                           , serviceDescr = Just "Makeup Services"
                           , serviceDuration = duration "01:30"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s7
                        , thumbnailPhoto = $(embedFile "static/img/makeup-services.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s8 <- insert $ Service { serviceName = "Waxing"
                           , servicePublished = True
                           , serviceOverview = Just "Waxing"
                           , serviceDescr = Just "Waxing"
                           , serviceDuration = duration "01:25"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s8
                        , thumbnailPhoto = $(embedFile "static/img/waxing.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e6
                   , roleService = s8
                   , roleName = "Waxing specialist"
                   , roleRating = Just 5
                   }

    s9 <- insert $ Service { serviceName = "Nail Care"
                           , servicePublished = True
                           , serviceOverview = Just "Nail Care"
                           , serviceDescr = Just "Nail Care"
                           , serviceDuration = duration "01:00"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s9
                        , thumbnailPhoto = $(embedFile "static/img/nail-care.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e7
                   , roleService = s9
                   , roleName = "Nail technician"
                   , roleRating = Just 5
                   }

    s10 <- insert $ Service { serviceName = "Body Shaping & Fitness"
                            , servicePublished = True
                            , serviceOverview = Just "Body Shaping & Fitness"
                            , serviceDescr = Just "Body Shaping & Fitness"
                            , serviceDuration = duration "01:15" 
                            , serviceGroup = Nothing
                            }

    insert_ $ Thumbnail { thumbnailService = s10
                        , thumbnailPhoto = $(embedFile "static/img/body-shaping-and-fitness.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Book { bookOffer = o121
                   , bookRole = Just r51511 
                   , bookUser = u2
                   , bookDay = addDays 1 today
                   , bookTime = time
                   , bookTz = utc
                   , bookStatus = BookStatusRequest
                   }

    insert_ $ Book { bookOffer = o121
                   , bookRole = Just r51511 
                   , bookUser = u2
                   , bookDay = addDays 2 today
                   , bookTime = time
                   , bookTz = utc
                   , bookStatus = BookStatusRequest
                   }

    return ()
  where
      duration :: String -> Maybe DiffTime
      duration = parseTimeM True defaultTimeLocale "%H:%M"
