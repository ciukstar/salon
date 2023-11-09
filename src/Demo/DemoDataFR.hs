{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}

module Demo.DemoDataFR (populateFR) where

import ClassyPrelude.Yesod (ReaderT, toGregorian)
import Control.Monad (forM_)
import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (st)
import qualified Data.ByteString.Base64 as B64 (decode)
import Data.Time.Calendar.Month (pattern YearMonth)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar
    ( DayOfWeek (Saturday, Sunday), DayPeriod (periodFirstDay, periodLastDay)
    , addDays, dayOfWeek
    )
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay,utctDayTime), DiffTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay), timeToTimeOfDay, TimeZone (TimeZone))
import Control.Monad.IO.Class (MonadIO (liftIO))
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
    , Thumbnail
      ( Thumbnail, thumbnailService, thumbnailPhoto, thumbnailMime
      , thumbnailAttribution
      )
    , Offer
      ( Offer, offerName, offerPrice, offerPrefix
      , offerSuffix, offerDescr, offerService
      )
    , EmplStatus (EmplStatusAvailable, EmplStatusUnavailable)
    , Staff
      ( Staff, staffName, staffStatus, staffPhone, staffMobile, staffEmail
      , staffUser
      )
    , StaffPhoto (StaffPhoto, staffPhotoPhoto, staffPhotoMime, staffPhotoStaff)
    , Role (Role, roleStaff, roleService, roleName, roleDuration, roleRating)
    , BookStatus (BookStatusRequest)
    , Book
      ( Book, bookCustomer, bookOffer, bookRole, bookDay, bookTime, bookTzo
      , bookTz, bookStatus, bookAddr
      )
    , Business
      ( Business, businessName, businessFullName, businessAddr, businessTzo, businessTz
      , businessPhone, businessMobile, businessEmail, businessCurrency
      )
    , Hist
      ( Hist, histBook, histLogtime, histDay, histTime, histAddr, histTzo
      , histStatus, histUser, histTz, histRoleName, histStaffName
      )
    , Schedule
      ( Schedule, scheduleStaff, scheduleWorkDay, scheduleWorkStart, scheduleWorkEnd)
    , BusinessHours
      ( BusinessHours, businessHoursBusiness, businessHoursDay, businessHoursOpen
      , businessHoursClose, businessHoursDayType
      )
    , AboutUs (AboutUs, aboutUsBusiness, aboutUsHtml)
    , ContactUs
      ( ContactUs, contactUsBusiness, contactUsShowSchedule, contactUsHtml
      , contactUsShowMap, contactUsLongitude, contactUsLatitude, contactUsShowAddress
      )
    , DayType (Weekday)
    )
import Data.FileEmbed (embedFile)
import Demo.DemoPhotos
    ( man01, man02, man03, man04, man05, man06
    , woman01, woman02, woman03, woman04, woman05
    )

populateFR :: MonadIO m => ReaderT SqlBackend m ()
populateFR = do

    (now,today,time,month) <- liftIO $ do
        t <- getCurrentTime
        return ( t
               , utctDay t
               , timeToTimeOfDay (utctDayTime t)
               , let (y,m,_) = toGregorian (utctDay t) in YearMonth y m
               )

    let business = Business { businessName = "Salon"
                            , businessFullName = Just "SARL Salon"
                            , businessCurrency = "EUR"
                            , businessAddr = "Champ de Mars, 5 Av. Anatole France, 75007 Paris"
                            , businessTzo = TimeZone 60 False "FR"
                            , businessTz = "Europe/Paris"
                            , businessPhone = Just "+33-775-552-652"
                            , businessMobile = Just "+33-655-537-079"
                            , businessEmail = Just "salon@mail.fr"
                            }

    b <- insert business

    forM_ [periodFirstDay month .. periodLastDay month] $ \d -> do
        let (start,end) = case dayOfWeek d of
              Saturday -> (TimeOfDay 9 0 0, TimeOfDay 19 0 0)
              Sunday -> (TimeOfDay 11 0 0,TimeOfDay 17 0 0)
              _ -> (TimeOfDay 10 0 0,TimeOfDay 20 0 0)
        insert_ BusinessHours
            { businessHoursBusiness = b
            , businessHoursDay = d
            , businessHoursOpen = start
            , businessHoursClose = end
            , businessHoursDayType = Weekday
            }

    insert_ $ AboutUs { aboutUsBusiness = b
                      , aboutUsHtml = [shamlet|
<h2 style="color:gray">Notre mission
<p>La mission de <i>Salon</i> est simple : offrir à chaque personne qui franchit ses portes un environnement spécial, où elle peut se faire plaisir, se toiletter et se faire dorloter, tout en valorisant son image personnelle et en apportant un sentiment de bien-être dans sa vie.
<h2 style="color:gray">Notre éthique
<p>Chaque personne qui vient dans nos salons est unique. Nous traitons chaque client unique en fonction de ses besoins personnels. Nous sommes fiers d'offrir le service que le client attend et les traitements dont il a besoin, et nous réévaluerons continuellement ses demandes en fonction de son style de vie et de son corps.
<p>Nous n’offrirons jamais de traitements inutiles et ferons de chaque client une priorité. C’est exactement la raison pour laquelle nous pouvons affirmer avec fierté qu’au fil des années, nous avons bâti une clientèle fidèle. Nous apprenons d'eux tout en prenant soin d'eux et nous nous efforçons de suivre les dernières tendances et traitements disponibles pour garantir que nous répondons toujours aux besoins de nos précieux clients et de tous les futurs visiteurs.
<h2 style="color:gray">Nos objectifs
<p>Nous continuerons d'offrir les derniers traitements, les techniques les plus innovantes tout en utilisant les meilleurs produits sur le marché. Tout cela dans des environnements élégants, propres et accueillants avec des thérapeutes formés, professionnels et sympathiques. Nous nous efforcerons de divulguer notre message selon lequel chacun a le droit de se sentir bien !
|]
                      }

    insert_ $ ContactUs { contactUsBusiness = b
                        , contactUsHtml = [shamlet|
<section>
  <h3 style="color:gray">Appelez-nous
  <dl>
    <dt>
      <i>Téléphone
    <dd>
      $maybe phone <- businessPhone business
        #{phone}
    <dt>
      <i>Portable
    <dd>
      $maybe mobile <- businessMobile business
        #{mobile}
<section>
  <h3 style="color:gray">Envoyez-nous un email
  <dl>
    <dt>
      <i>E-mail
    <dd>
      $maybe email <- businessEmail business
        #{email}
<section>
  <h3 style="color:gray">Venez nous voir
|]
                        , contactUsShowAddress = True
                        , contactUsShowSchedule = True
                        , contactUsShowMap = True
                        , contactUsLongitude = Just 2.2945
                        , contactUsLatitude = Just 48.858222
                        }
    
    pass <- liftIO $ makePassword "root" 17
    insert_ $ User { userName = "root"
                   , userPassword = decodeUtf8 pass
                   , userAdmin = True
                   , userFullName = Just "Arnaud Antoine"
                   , userEmail = Just "arnauda@mail.fr"
                   }

    pass1 <- liftIO $ makePassword "martinl" 17
    let user1 = User { userName = "martinl"
                     , userPassword = decodeUtf8 pass1
                     , userAdmin = False
                     , userFullName = Just "Martin Léo"
                     , userEmail = Just "martinl@mail.fr"
                     }
                
    u1 <- insert user1

    let empl1 = Staff { staffName = case userFullName user1 of Just name -> name; Nothing -> userName user1
                      , staffStatus = EmplStatusAvailable
                      , staffPhone = businessPhone business
                      , staffMobile = businessMobile business
                      , staffEmail = userEmail user1
                      , staffUser = Just u1
                      }

    e1 <- insert empl1

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

    insert_ $ Schedule { scheduleStaff = e1
                       , scheduleWorkDay = addDays (-1) today
                       , scheduleWorkStart = TimeOfDay 9 0 0
                       , scheduleWorkEnd = TimeOfDay 18 0 0
                       }

    insert_ $ Schedule { scheduleStaff = e1
                       , scheduleWorkDay = today
                       , scheduleWorkStart = TimeOfDay 9 0 0
                       , scheduleWorkEnd = TimeOfDay 18 0 0
                       }

    insert_ $ Schedule { scheduleStaff = e1
                       , scheduleWorkDay = addDays 1 today
                       , scheduleWorkStart = TimeOfDay 9 0 0
                       , scheduleWorkEnd = TimeOfDay 18 0 0
                       }

    insert_ $ Schedule { scheduleStaff = e1
                       , scheduleWorkDay = addDays 2 today
                       , scheduleWorkStart = TimeOfDay 9 0 0
                       , scheduleWorkEnd = TimeOfDay 18 0 0
                       }

    pass2 <- liftIO $ makePassword "bernardj" 17
    let user2 = User { userName = "bernardj"
                     , userPassword = decodeUtf8 pass2
                     , userAdmin = False
                     , userFullName = Just "Bernard Jade"
                     , userEmail = Just "bernardj@mail.fr"
                     }
    u2 <- insert user2

    e2 <- insert $ Staff { staffName = case userFullName user2 of Just name -> name; Nothing -> userName user2
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = userEmail user2
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

    pass3 <- liftIO $ makePassword "thomasgr" 17
    let user3 = User { userName = "thomasgr"
                     , userPassword = decodeUtf8 pass3
                     , userAdmin = False
                     , userFullName = Just "Thomas Gabriel Raphaël"
                     , userEmail = Just "thomasgr@mail.fr"
                     }
                
    u3 <- insert user3

    e3 <- insert $ Staff { staffName = case userFullName user3 of Just name -> name; Nothing -> userName user3
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = userEmail user3
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

    pass4 <- liftIO $ makePassword "robertle" 17
    let user4 = User { userName = "robertle"
                     , userPassword = decodeUtf8 pass4
                     , userAdmin = False
                     , userFullName = Just "Robert Louise Emma"
                     , userEmail = Just "robertle@mail.fr"
                     }
                
    u4 <- insert user4

    e4 <- insert $ Staff { staffName = case userFullName user4 of Just name -> name; Nothing -> userName user4
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = userEmail user4
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

    pass5 <- liftIO $ makePassword "richardal" 17
    let user5 = User { userName = "richardal"
                     , userPassword = decodeUtf8 pass5
                     , userAdmin = False
                     , userFullName = Just "Richard Arthur Louis"
                     , userEmail = Just "richardal@mail.fr"
                     }
    u5 <- insert user5

    e5 <- insert $ Staff { staffName = case userFullName user5 of Just name -> name; Nothing -> userName user5
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = userEmail user5
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

    e6 <- insert $ Staff { staffName = "Durand Jules"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "durandj@mail.fr"
                         , staffUser = Nothing
                         }

    case B64.decode man04 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e6
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e7 <- insert $ Staff { staffName = "Dubois Alice Ambre"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "duboisaa@mail.fr"
                         , staffUser = Nothing
                         }

    case B64.decode woman03 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e7
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e8 <- insert $ Staff { staffName = "Moreau Lina"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "moreaul@mail.fr"
                         , staffUser = Nothing
                         }

    case B64.decode woman04 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e8
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    let empl9 = Staff { staffName = "Laurent Adam"
                      , staffStatus = EmplStatusAvailable
                      , staffPhone = businessPhone business
                      , staffMobile = businessMobile business
                      , staffEmail = Just "laurenta@mail.fr"
                      , staffUser = Nothing
                      }

    e9 <- insert empl9

    case B64.decode man05 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e9
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    insert_ $ Schedule { scheduleStaff = e9
                       , scheduleWorkDay = addDays (-2) today
                       , scheduleWorkStart = TimeOfDay 9 0 0
                       , scheduleWorkEnd = TimeOfDay 18 0 0
                       }

    insert_ $ Schedule { scheduleStaff = e9
                       , scheduleWorkDay = addDays (-1) today
                       , scheduleWorkStart = TimeOfDay 9 0 0
                       , scheduleWorkEnd = TimeOfDay 18 0 0
                       }

    insert_ $ Schedule { scheduleStaff = e9
                       , scheduleWorkDay = addDays 0 today
                       , scheduleWorkStart = TimeOfDay 9 0 0
                       , scheduleWorkEnd = TimeOfDay 18 0 0
                       }

    insert_ $ Schedule { scheduleStaff = e9
                       , scheduleWorkDay = addDays 1 today
                       , scheduleWorkStart = TimeOfDay 9 0 0
                       , scheduleWorkEnd = TimeOfDay 18 0 0
                       }

    insert_ $ Schedule { scheduleStaff = e9
                       , scheduleWorkDay = addDays 2 today
                       , scheduleWorkStart = TimeOfDay 9 0 0
                       , scheduleWorkEnd = TimeOfDay 18 0 0
                       }

    insert_ $ Schedule { scheduleStaff = e9
                       , scheduleWorkDay = addDays 3 today
                       , scheduleWorkStart = TimeOfDay 9 0 0
                       , scheduleWorkEnd = TimeOfDay 18 0 0
                       }

    e10 <- insert $ Staff { staffName = "Simon Maël Lucas"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "simonml@mail.fr"
                         , staffUser = Nothing
                         }

    case B64.decode man06 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e10
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    pass11 <- liftIO $ makePassword "michelrc" 17
    let user11 = User { userName = "michelrc"
                      , userPassword = decodeUtf8 pass11
                      , userAdmin = False
                      , userFullName = Just "Michel Rose Chloé"
                      , userEmail = Just "michelrc@mail.fr"
                      }
                 
    u11 <- insert user11

    let empl11 = Staff { staffName = case userFullName user11 of Just name -> name; Nothing -> userName user11
                       , staffStatus = EmplStatusUnavailable
                       , staffPhone = businessPhone business
                       , staffMobile = businessMobile business
                       , staffEmail = userEmail user11
                       , staffUser = Just u11
                       }

    e11 <- insert empl11

    case B64.decode woman05 of
      Left _ -> return ()
      Right x -> do
          insert_ $ StaffPhoto { staffPhotoStaff = e11
                               , staffPhotoPhoto = x
                               , staffPhotoMime = "image/avif"
                               }
          insert_ $ UserPhoto { userPhotoUser = u11
                              , userPhotoPhoto = x
                              , userPhotoMime = "image/avif"
                              }
              
    s1 <- insert $ Service { serviceName = "Soin des cheveux"
                           , serviceOverview = Just "Services de soins capillaires"
                           , servicePublished = True
                           , serviceDescr = Just "<p>Toujours distinctifs et jamais banals, nos experts en soins capillaires ont suivi une formation approfondie pour fournir des coupes de créateurs et des services de coiffage adaptés aux besoins de chaque client. En tant que salon, nous soutenons notre équipe dans ses efforts pour perfectionner ses techniques individuelles et nous donnons à nos stylistes la liberté d'exprimer et d'explorer pleinement leur créativité. Cela donne à nos clients la possibilité de bénéficier d'un service personnalisé à chaque fois. Que vous aimiez les looks modernes ou qu'une coupe classique reflète votre style distinctif, les options sont infinies chez nous.</p>"
                           , serviceDuration = duration "01:30"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s1
                        , thumbnailPhoto = $(embedFile "static/img/hair-care.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s11 <- insert $ Service { serviceName = "Coupes de cheveux hommes"
                            , servicePublished = True
                            , serviceOverview = Just "Coupes de cheveux pour hommes"
                            , serviceDescr = Just "Coupes de cheveux pour hommes"
                            , serviceDuration = duration "01:00"
                            , serviceGroup = Just s1
                            }

    let role111 =  Role { roleStaff = e1
                        , roleService = s11
                        , roleName = "Coiffeur"
                        , roleDuration = 60 * (1 * 60 + 0)
                        , roleRating = Just 5
                        } 

    r111 <- insert role111

    insert_ $ Role { roleStaff = e2
                   , roleService = s11
                   , roleName = "Coiffeuse"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    o111 <- insert $ Offer { offerService = s11
                           , offerName = "Prix"
                           , offerPrice = 26
                           , offerPrefix = Nothing
                           , offerSuffix = Nothing
                           , offerDescr = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s11
                        , thumbnailPhoto = $(embedFile "static/img/men-haircuts.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s12 <- insert $ Service { serviceName = "Coupes de cheveux femmes (au dessus des épaules)"
                            , servicePublished = True
                            , serviceOverview = Just "Coupes de cheveux au-dessus des épaules pour les femmes"
                            , serviceDescr = Just "Coupes de cheveux au-dessus des épaules pour les femmes"
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s1
                            }

    insert_ $ Offer { offerService = s12
                    , offerName = "Prix"
                    , offerPrice = 28
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s12
                        , thumbnailPhoto = $(embedFile "static/img/women-hair-cuts-above-shoulders.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e2
                   , roleService = s12
                   , roleName = "Coiffeuse"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 3
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s12
                   , roleName = "Coiffeur"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s12
                   , roleName = "Coiffeuse"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    s13 <- insert $ Service { serviceName = "Coupes de cheveux femmes (sous les épaules)"
                            , servicePublished = True
                            , serviceOverview = Just "Coupes de cheveux sous les épaules pour les femmes"
                            , serviceDescr = Just "Coupes de cheveux sous les épaules pour les femmes"
                            , serviceDuration = duration "01:35"
                            , serviceGroup = Just s1
                            }

    o131 <- insert $ Offer { offerService = s13
                           , offerName = "Prix"
                           , offerPrice = 35
                           , offerPrefix = Nothing
                           , offerSuffix = Nothing
                           , offerDescr = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s13
                        , thumbnailPhoto = $(embedFile "static/img/women-hair-cuts-below-shoulders.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    let role1311 = Role { roleStaff = e11
                        , roleService = s13
                        , roleName = "Styliste"
                        , roleDuration = 60 * (1 * 60 + 35)
                        , roleRating = Just 5
                        }

    r1311 <- insert role1311

    insert_ $ Role { roleStaff = e3
                   , roleService = s13
                   , roleName = "Assistante styliste"
                   , roleDuration = 60 * (1 * 60 + 35)
                   , roleRating = Just 4
                   }

    s14 <- insert $ Service { serviceName = "Coupes de cheveux pour enfants"
                            , servicePublished = True
                            , serviceOverview = Just "Coupes de cheveux pour enfants"
                            , serviceDescr = Just "Coupes de cheveux pour enfants"
                            , serviceDuration = duration "01:20"
                            , serviceGroup = Just s1
                            }

    o141 <- insert $ Offer { offerService = s14
                           , offerName = "Prix"
                           , offerPrice = 16
                           , offerPrefix = Nothing
                           , offerSuffix = Just "-20 € (en fonction de la longueur de leurs cheveux)"
                           , offerDescr = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s14
                        , thumbnailPhoto = $(embedFile "static/img/children-hair-cuts.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e4
                   , roleService = s14
                   , roleName = "Styliste junior"
                   , roleDuration = 60 * (1 * 60 + 20)
                   , roleRating = Just 5
                   }

    s15 <- insert $ Service { serviceName = "Services chimiques"
                            , servicePublished = True
                            , serviceOverview = Just "Services chimiques"
                            , serviceDescr = Just "<p>Nos services chimiques répondent à un large éventail de besoins en matière de soins capillaires. Nos traitements lissants combattent les frisottis, augmentent la maniabilité et fournissent les résultats durables que vous avez toujours souhaités. Vous pouvez profiter de cheveux lisses et soyeux grâce à notre service de kératine, ainsi que réparer les dommages causés à vos cheveux en reconstituant les protéines perdues. Si vous recherchez des vagues, des boucles et du volume supplémentaires, nos services de permanente professionnels vous permettront d’obtenir la texture souhaitée. Nous utilisons des produits et des techniques innovants pour établir des boucles définies que vous adorerez à coup sûr.</p>"
                            , serviceDuration = duration "02:00"
                            , serviceGroup = Just s1
                            }

    insert_ $ Thumbnail { thumbnailService = s15
                        , thumbnailPhoto = $(embedFile "static/img/chemical-services.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s151 <- insert $ Service { serviceName = "Conditionnement"
                             , servicePublished = True
                             , serviceOverview = Just "Prestations de conditionnement"
                             , serviceDescr = Just "Conditionner"
                             , serviceDuration = duration "01:35"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s151
                        , thumbnailPhoto = $(embedFile "static/img/conditioning.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s1511 <- insert $ Service { serviceName = "Après-shampooing permanente"
                              , servicePublished = True
                              , serviceOverview = Just "Après-shampooing permanente"
                              , serviceDescr = Just "Après-shampooing permanente"
                              , serviceDuration = duration "01:25"
                              , serviceGroup = Just s151
                              }

    insert_ $ Offer { offerService = s1511
                    , offerName = "Prix"
                    , offerPrice = 99
                    , offerPrefix = Nothing
                    , offerSuffix = Just " et plus"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s1511
                        , thumbnailPhoto = $(embedFile "static/img/after-perm-conditioner.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e3
                   , roleService = s1511
                   , roleName = "Styliste"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 4
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s1511
                   , roleName = "Styliste"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 4
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1511
                   , roleName = "Styliste"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 4
                   }

    s1512 <- insert $ Service { serviceName = "Avant le revitalisant permanent"
                              , servicePublished = True
                              , serviceOverview = Just "Avant le revitalisant permanent"
                              , serviceDescr = Just "Avant le revitalisant permanent"
                              , serviceDuration = duration "01:15"
                              , serviceGroup = Just s151
                              }

    insert_ $ Offer { offerService = s1512
                    , offerName = "Prix"
                    , offerPrice = 110
                    , offerPrefix = Nothing
                    , offerSuffix = Just " et plus"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e4
                   , roleService = s1512
                   , roleName = "Styliste"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1512
                   , roleName = "Styliste"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s1512
                   , roleName = "Styliste"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s1512
                        , thumbnailPhoto = $(embedFile "static/img/before-perm-conditioner.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s152 <- insert $ Service { serviceName = "Mèches et couleur"
                             , servicePublished = True
                             , serviceOverview = Just "Mèches et couleur"
                             , serviceDescr = Just "Faits saillants et couleur"
                             , serviceDuration = duration "01:10"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s152
                        , thumbnailPhoto = $(embedFile "static/img/highlights-and-color.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s1521 <- insert $ Service { serviceName = "Complet"
                              , servicePublished = True
                              , serviceOverview = Just "Mèches et couleur - Complet"
                              , serviceDescr = Just "Faits saillants et couleur - Complet"
                              , serviceDuration = duration "01:00"
                              , serviceGroup = Just s152
                              }

    insert_ $ Offer { offerService = s1521
                    , offerName = "Prix"
                    , offerPrice = 130
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s1521
                        , thumbnailPhoto = $(embedFile "static/img/highlights-and-color-full.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e8
                   , roleService = s1521
                   , roleName = "Coloriste"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s1521
                   , roleName = "Coloriste"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e10
                   , roleService = s1521
                   , roleName = "Coloriste"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    s1522 <- insert $ Service { serviceName = "Partiel"
                              , servicePublished = True
                              , serviceOverview = Just "Mèches et couleur - Partiel"
                              , serviceDescr = Just "Faits saillants et couleur - Partiel"
                              , serviceDuration = duration "01:15"
                              , serviceGroup = Just s152
                              }

    insert_ $ Offer { offerService = s1522
                    , offerName = "Prix"
                    , offerPrice = 68
                    , offerPrefix = Nothing
                    , offerSuffix = Just " et plus"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s1522
                        , thumbnailPhoto = $(embedFile "static/img/highlights-and-color-partial.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e10
                   , roleService = s1522
                   , roleName = "Coloriste"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s1522
                   , roleName = "Coloriste"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    s1523 <- insert $ Service { serviceName = "Couleur permanente"
                              , servicePublished = True
                              , serviceOverview = Just "Couleur permanente"
                              , serviceDescr = Just "Couleur permanente"
                              , serviceDuration = duration "01:45"
                              , serviceGroup = Just s152
                              }

    insert_ $ Offer { offerService = s1523
                    , offerName = "Prix"
                    , offerPrice = 68
                    , offerPrefix = Nothing
                    , offerSuffix = Just " et plus"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s1523
                        , thumbnailPhoto = $(embedFile "static/img/highlights-and-color-permanent.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e9
                   , roleService = s1523
                   , roleName = "Coloriste"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e10
                   , roleService = s1523
                   , roleName = "Coloriste"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s1523
                   , roleName = "Coloriste"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    s153 <- insert $ Service { serviceName = "Perm"
                             , servicePublished = True
                             , serviceOverview = Just "Une vague permanente"
                             , serviceDescr = Just "Perm est un excellent moyen de vous donner un nouveau look"
                             , serviceDuration = duration "00:45"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s153
                        , thumbnailPhoto = $(embedFile "static/img/perm.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s1531 <- insert $ Service { serviceName = "Perm complète"
                              , servicePublished = True
                              , serviceOverview = Just "Perm complète"
                              , serviceDescr = Just "Perm complète"
                              , serviceDuration = duration "00:35"
                              , serviceGroup = Just s153
                              }

    insert_ $ Offer { offerService = s1531
                    , offerName = "Prix"
                    , offerPrice = 79
                    , offerPrefix = Nothing
                    , offerSuffix = Just " et plus"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s1531
                        , thumbnailPhoto = $(embedFile "static/img/full-perm.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e1
                   , roleService = s1531
                   , roleName = "Styliste"
                   , roleDuration = 60 * (0 * 60 + 35)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s1531
                   , roleName = "Styliste"
                   , roleDuration = 60 * (0 * 60 + 35)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s1531
                   , roleName = "Styliste"
                   , roleDuration = 60 * (0 * 60 + 35)
                   , roleRating = Just 5
                   }

    s1532 <- insert $ Service { serviceName = "Perm Réparatrice Acide"
                              , servicePublished = True
                              , serviceOverview = Just "Permanente Réparatrice Acide"
                              , serviceDescr = Just "Permanente Réparatrice Acide"
                              , serviceDuration = duration "01:25"
                              , serviceGroup = Just s153
                              }

    insert_ $ Offer { offerService = s1532
                    , offerName = "Prix"
                    , offerPrice = 89
                    , offerPrefix = Nothing
                    , offerSuffix = Just " et plus"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s1532
                        , thumbnailPhoto = $(embedFile "static/img/acid-repair-perm.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e3
                   , roleService = s1532
                   , roleName = "Styliste"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s1532
                   , roleName = "Styliste"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1532
                   , roleName = "Styliste"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    s1533 <- insert $ Service { serviceName = "Perm lissante japonaise"
                              , servicePublished = True
                              , serviceOverview = Just "Permanente Lissante Japonaise"
                              , serviceDescr = Just "Permanente Lissante Japonaise"
                              , serviceDuration = duration "01:25"
                              , serviceGroup = Just s153
                              }

    insert_ $ Offer { offerService = s1533
                    , offerName = "Prix"
                    , offerPrice = 250
                    , offerPrefix = Nothing
                    , offerSuffix = Just " et plus"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s1533
                        , thumbnailPhoto = $(embedFile "static/img/japanese-straightening-perm.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1533
                   , roleName = "Styliste"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s1533
                   , roleName = "Styliste"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s1533
                   , roleName = "Styliste"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    s2 <- insert $ Service { serviceName = "Soins du visage"
                           , servicePublished = True
                           , serviceOverview = Just "Soins du visage"
                           , serviceDescr = Just $ Textarea [st|
<p>
Votre visage est une toile expressive qui montre l'expérience et l'émotion. Dans l'un des meilleurs salons du monde, notre palette propose des soins nourrissants qui rehaussent, soulignent la beauté, la jeunesse et la couleur de votre corps. Avant tout soin du visage, notre esthéticienne professionnelle vous fera une consolidation et travaillera à partir de là. Vous n’en croirez pas la différence !
</p>
<p>Tous les soins du visage incluent la mise en forme des sourcils.</p>
|]
                           , serviceDuration = duration "01:45"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s2
                        , thumbnailPhoto = $(embedFile "static/img/facial-treatments.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s21 <- insert $ Service { serviceName = "Soin du visage de base"
                            , servicePublished = True
                            , serviceOverview = Just "Soin du visage de base (60 min)"
                            , serviceDescr = Just "Nettoyage en profondeur, exfoliation avec traitement à la vapeur, suivi d'extractions, puis mise en forme des sourcils ; un massage déstressant du visage, du cou et des épaules. Un masque personnalisé, ainsi qu'un traitement oculaire régulier, suivi d'une application de crème hydratante/solaire. Ce traitement nettoyant relaxant mais sérieux vous laissera un teint propre, frais et éclatant."
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s21
                    , offerName = "Prix"
                    , offerPrice = 55
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s21
                    , offerName = "Forfait"
                    , offerPrice = 250
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 séances"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s21
                        , thumbnailPhoto = $(embedFile "static/img/facial-treatments-basic-facial.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e8
                   , roleService = s21
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s21
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e10
                   , roleService = s21
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    s22 <- insert $ Service { serviceName = "Soin du visage de luxe"
                            , servicePublished = True
                            , serviceOverview = Just "Soin du visage de luxe"
                            , serviceDescr = Just "Ce soin du visage spécial peut être personnalisé en fonction de la situation cutanée du client (c'est-à-dire sèche, grasse, sensible, etc.). Il est créé pour lisser et adoucir votre teint tout en déstressant tout votre corps. Notre soin du visage de luxe vous fera sentir et paraître en meilleure santé."
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s22
                    , offerName = "Prix"
                    , offerPrice = 75
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s22
                    , offerName = "Forfait"
                    , offerPrice = 350
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 séances"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s22
                        , thumbnailPhoto = $(embedFile "static/img/facial-treatments-deluxe-facial.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e6
                   , roleService = s22
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s22
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s22
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    s23 <- insert $ Service { serviceName = "Soin du visage cocooning"
                            , servicePublished = True
                            , serviceOverview = Just "Soin du visage cocooning (90 min)"
                            , serviceDescr = Just "Un traitement clinique hydratant, créant un effet rafraîchissant sur la peau pour revitaliser, hydrater et apaiser. Son effet thermo-rafraîchissant sur la peau en fait un soin revitalisant remarquable notamment pour atténuer les rougeurs. ALGOMASK+ offre un éclat instantané et une hydratation longue durée."
                            , serviceDuration = duration "00:90"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s23
                    , offerName = "Prix"
                    , offerPrice = 90
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s23
                    , offerName = "Forfait"
                    , offerPrice = 400
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 séances"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s23
                        , thumbnailPhoto = $(embedFile "static/img/facial-treatments-pampering-facial.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e3
                   , roleService = s23
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s23
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s23
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s23
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    s24 <- insert $ Service { serviceName = "Traitement de l'acné"
                            , servicePublished = True
                            , serviceOverview = Just "Traitement de l'acné (120 min)"
                            , serviceDescr = Just "Il s’agit d’une manière très innovante et efficace de traiter les affections acnéiques qui n’ont pas répondu aux autres traitements et qui a produit de nombreux résultats remarquables. Le peroxyde d'urée, les acides alpha-hydroxyles et un élément anti-androgène spécial sont incorporés dans la formule normalisante Rejuvi."
                            , serviceDuration = duration "02:00"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s24
                    , offerName = "Prix"
                    , offerPrice = 95
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s24
                    , offerName = "Forfait"
                    , offerPrice = 600
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/7 séances"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s24
                        , thumbnailPhoto = $(embedFile "static/img/facial-treatments-acne-treatment.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e1
                   , roleService = s24
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s24
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s24
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    s25 <- insert $ Service { serviceName = "Traitement de blanchiment à l'or 24 carats"
                            , servicePublished = True
                            , serviceOverview = Just "Traitement de blanchiment à l'or 24 carats"
                            , serviceDescr = Just "<p>Un soin du visage luxueux anti-âge. Cette formule active hydratante anti-âge combine le pouvoir des vitamines pures, des extraits de plantes et de l'or 24 carats. Ces ingrédients contribuent efficacement à la stimulation de la production de collagène. Ils forment une barrière protectrice continue pour imiter les effets de la chirurgie.</p><p>Le masque s'adapte comme une « seconde peau » et s'adapte parfaitement aux contours du visage. Il offre une hydratation maximale, fortifie la barrière protectrice naturelle de la peau et rajeunit votre peau sensible pour réduire les signes du vieillissement.</p>"
                            , serviceDuration = duration "01:15"
                            , serviceGroup = Just s2
                            }

    o251 <- insert $ Offer { offerService = s25
                           , offerName = "Prix"
                           , offerPrice = 100
                           , offerPrefix = Nothing
                           , offerSuffix = Nothing
                           , offerDescr = Nothing
                           }

    insert_ $ Offer { offerService = s25
                    , offerName = "Forfait"
                    , offerPrice = 460
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 séances"
                    , offerDescr = Nothing
                    }

    insert_ $ Thumbnail { thumbnailService = s25
                        , thumbnailPhoto = $(embedFile "static/img/facial-treatments-24k-gold-whitening-treatment.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    let role925 = Role { roleStaff = e9
                       , roleService = s25
                       , roleName = "Esthéticien"
                       , roleDuration = 60 * (1 * 60 + 15)
                       , roleRating = Just 5
                       }

    r925 <- insert role925

    insert_ $ Role { roleStaff = e10
                   , roleService = s25
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s25
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s25
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    s3 <- insert $ Service { serviceName = "Soins du visage avancés"
                           , servicePublished = True
                           , serviceOverview = Just "Soins du visage avancés"
                           , serviceDescr = Just "Soins du visage avancés"
                           , serviceDuration = duration "01:10"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s3
                        , thumbnailPhoto = $(embedFile "static/img/advanced-facial-treatments.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s31 <- insert $ Service { serviceName = "Peeling visage au lait"
                            , servicePublished = True
                            , serviceOverview = Just "Peeling visage au lait (90 min)"
                            , serviceDescr = Just $ Textarea [st|
<p>
Peeling au lait comprend un extrait naturel de lait aigre (acide lactique), de papaïne (enzyme de papaye), d'acide salicylique (issu de plantes naturelles) et un contrôleur de pénétration spécial. Peeling au lait offre un processus d’exfoliation cutanée sûr, sans les effets secondaires que les peelings au phénol et au TCA peuvent provoquer.
</p>
<p>
La formule Peeling au lait remplit les fonctions suivantes :
</p>
<ul>
  <li>Génère une exfoliation cutanée maximale mais douce.</li>
  <li>Stimuler la prolifération des fibroblastes pour augmenter le collagène et l'élastine dermiques.</li>
  <li>Normaliser les cellules et les tissus de la peau.</li>
</ul>
<p>
En conséquence, le peeling au lait est un puissant processus de resurfaçage de la peau avec des effets cutanés importants. Il présente plusieurs bienfaits pour la peau tels que :
</p>
<ol>
  <li>Éliminer les rides et ridules visibles sur la peau.</li>
  <li>Diminution des marques de boutons ou d’acné.</li>
  <li>Réduire les cicatrices.</li>
  <li>Lissage des creux de la peau.</li>
  <li>Diminution des vergetures et des taches de naissance.</li>
  <li>Polir la peau en douceur pour le renouvellement du visage.</li>
</ol>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s3
                            }

    insert_ $ Offer { offerService = s31
                    , offerName = "Prix"
                    , offerPrice = 330
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e8
                   , roleService = s31
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s31
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s31
                        , thumbnailPhoto = $(embedFile "static/img/milk-peel.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s32 <- insert $ Service { serviceName = "Rouleau derma"
                            , servicePublished = True
                            , serviceOverview = Just "Aidez votre peau à se renouveler"
                            , serviceDescr = Just $ Textarea [st|
<h4>Avantages :</h4>
<ul>
  <li>Élimine et traite les vergetures, les cicatrices d'acné et les rides.</li>
  <li>Anti-âge.</li>
  <li>Traitement de perte de cheveux ou restauration de cheveux.</li>
  <li>Traitement de la cellulite et réduction de la cellulite.</li>
  <li>Remplacez le collagène, vous pouvez aider votre peau à se renouveler et à se réparer à un niveau cosmétique.</li>
</ul>
<h4>Alors, qu’est-ce qu’un Derma Roller ?</h4>
<p>
Le Scientia Derma Roller est un appareil incroyable qui augmente naturellement les niveaux de collagène et d'élastine de votre peau. Les vergetures, les rides, les cicatrices et la peau inégale et piquée sont toutes dues à un manque de collagène. Ainsi, en utilisant un DermaRoller pour remplacer le collagène, vous pouvez aider votre peau à se renouveler et à se réparer à un niveau cosmétique.
</p>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s3
                            }

    o321 <- insert $ Offer { offerService = s32
                           , offerName = "Prix"
                           , offerPrice = 330
                           , offerPrefix = Nothing
                           , offerSuffix = Nothing
                           , offerDescr = Nothing
                           }

    insert_ $ Role { roleStaff = e7
                   , roleService = s32
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s32
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s32
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s32
                        , thumbnailPhoto = $(embedFile "static/img/derma-roller.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s33 <- insert $ Service { serviceName = "Microdermabrasion"
                            , servicePublished = True
                            , serviceOverview = Just "Peeling par microdermabrasion"
                            , serviceDescr = Just $ Textarea [st|
<p>
Procédure indolore, elle aide à éliminer les cicatrices d'acné, les pores dilatés, les rides du visage, les rides, les points noirs ainsi que les dommages causés par le soleil, etc. Cette technique aide également à épaissir votre collagène, ce qui donne un teint d'apparence plus jeune. Le collagène est une protéine présente dans votre peau qui est abondante lorsque vous êtes enfant et qui donne à la peau une apparence tendue et lisse. La production de collagène diminue avec l’âge, ce qui entraîne une peau plus lâche et inégale.
</p>
|]
                            , serviceDuration = duration "00:45"
                            , serviceGroup = Just s3
                            }

    insert_ $ Offer { offerService = s33
                    , offerName = "Prix"
                    , offerPrice = 70
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e5
                   , roleService = s33
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s33
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s33
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s33
                        , thumbnailPhoto = $(embedFile "static/img/micro-dermabrasion.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s34 <- insert $ Service { serviceName = "Blanchiment des taches de rousseur"
                            , servicePublished = True
                            , serviceOverview = Just "Blanchiment des taches de rousseur (120 min)"
                            , serviceDescr = Just $ Textarea [st|
<p>Ce traitement obtient un effet synergique pour améliorer le teint, les problèmes de pigmentation ainsi que les cernes.</p>
|]
                            , serviceDuration = duration "02:00"
                            , serviceGroup = Just s3
                            }

    insert_ $ Offer { offerService = s34
                    , offerName = "Prix"
                    , offerPrice = 95
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s34
                    , offerName = "Forfait"
                    , offerPrice = 600
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/7 séances"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s34
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s34
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s34
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s34
                        , thumbnailPhoto = $(embedFile "static/img/freckle-bleaching.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s4 <- insert $ Service { serviceName = "Traitements Anti-Âge"
                           , servicePublished = True
                           , serviceOverview = Just "Traitements Anti-Âge"
                           , serviceDescr = Just "Traitements Anti-Âge"
                           , serviceDuration = duration "01:30"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s4
                        , thumbnailPhoto = $(embedFile "static/img/anti-aging-treatments.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s41 <- insert $ Service { serviceName = "Sea C Spa"
                            , servicePublished = True
                            , serviceOverview = Just "Sea C Spa (100 % vitamine C)"
                            , serviceDescr = Just $ Textarea [st|
<p>
Un puissant soin anti-âge conçu pour retarder les signes visibles du vieillissement et dynamiser la peau.
</p>
<p>
Formulé avec des ingrédients marins et végétaux (Concentré de Vitamine C, Patchs d'Algues Biomatrix et Boue Thermale Organique).
</p>
<p>
Ce soin est exceptionnel aussi bien en pré qu'en post-exposition solaire. Idéal pour les personnes vivant dans des zones urbaines avec des niveaux de pollution élevés.
</p>
<h4>Avantages :</h4>
<p>
Ce soin réduit l'apparence des rides et ridules. Unifie le teint et illumine la peau, lui redonnant ainsi son aspect de jeunesse.
</p>
|]
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s41
                    , offerName = "Prix"
                    , offerPrice = 95
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s41
                    , offerName = "Forfait"
                    , offerPrice = 430
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 séances"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e2
                   , roleService = s41
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s41
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s41
                        , thumbnailPhoto = $(embedFile "static/img/sea-C-spa.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s42 <- insert $ Service { serviceName = "Soin du visage au Botinol"
                            , servicePublished = True
                            , serviceOverview = Just "Soin du visage au Botinol (150 min)"
                            , serviceDescr = Just $ Textarea [st|
<p>
Un traitement électif très avancé formulé pour hydrater, régénérer et réduire l’apparence des rides et ridules.
</p>
<p>
Cette séance de soin relaxante offre des textures agréables et des essences exquises procurant une sensation globale de bien-être. Un soin idéal pour les personnes déterminées à masquer les signes de l’âge.
</p>
<h4>Avantages :</h4>
<p>
Après un seul traitement, les rides d'expression semblent détendues. Les rides et ridules sont visiblement réduites. La peau paraît visiblement plus jeune.
</p>
|]
                            , serviceDuration = duration "02:30"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s42
                    , offerName = "Prix"
                    , offerPrice = 170
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s42
                    , offerName = "Forfait"
                    , offerPrice = 780
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 séances"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e1
                   , roleService = s42
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s42
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s42
                        , thumbnailPhoto = $(embedFile "static/img/botinol-facial.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s43 <- insert $ Service { serviceName = "Collagène 90-II"
                            , servicePublished = True
                            , serviceOverview = Just "Collagène 90-II (150 min)"
                            , serviceDescr = Just $ Textarea [st|
<p>
Ce soin hydratant anti-âge procure une hydratation intensive, une régénération cellulaire et un renouvellement épidermique. Avec l’âge, le processus de renouvellement cellulaire est retardé. Les contours du visage perdent leur définition et la peau semble s'affaisser.
</p>
<p>
Collagen 90-II est un traitement anti-âge exclusif de G.M. Collin Skin Care qui associe une feuille de collagène natif pur à des ingrédients sélectionnés, pour apporter une hydratation intense, raffermir la peau et réduire l'apparence du vieillissement.
</p>
<p>
Le Collagène 90-II est un traitement de renouvellement cutané anti-âge très respecté et recherché qui hydrate, lisse et tonifie les rides pour combattre les signes visibles du vieillissement.
</p>
<h4>Avantages :</h4>
<p>
Ce traitement améliore le teint global en minimisant l'apparence des rides et ridules. Il restaure l'hydratation, laissant la peau bien hydratée et éclatante. Recommandé pour tous les types de peau
</p>
|]
                            , serviceDuration = duration "02:30"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s43
                    , offerName = "Prix"
                    , offerPrice = 160
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s43
                    , offerName = "Forfait"
                    , offerPrice = 730
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 séances"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e10
                   , roleService = s43
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s43
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s43
                        , thumbnailPhoto = $(embedFile "static/img/collagen-90-II.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s44 <- insert $ Service { serviceName = "Électro-ionisation"
                            , servicePublished = True
                            , serviceOverview = Just "Électrostimulation du visage"
                            , serviceDescr = Just $ Textarea [st|
<p>
Il favorise la production de collagène, resserre et raffermit les tissus cutanés, hydrate et rajeunit l'épiderme, traite l'acné et les ridules. Moins de rides en seulement 15 minutes !
</p>
|]
                            , serviceDuration = duration "00:15"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s44
                    , offerName = "Prix"
                    , offerPrice = 180
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s44
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (0 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s44
                        , thumbnailPhoto = $(embedFile "static/img/electro-ionization.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s5 <- insert $ Service { serviceName = "Traitements des yeux"
                           , servicePublished = True
                           , serviceOverview = Just "Centre de traitement des yeux"
                           , serviceDescr = Just "Centre de traitement des yeux"
                           , serviceDuration = duration "02:00"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s5
                        , thumbnailPhoto = $(embedFile "static/img/eye-treatment-center.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s51 <- insert $ Service { serviceName = "Permanente des cils"
                            , servicePublished = True
                            , serviceOverview = Just "Permanente des cils"
                            , serviceDescr = Just $ Textarea [st|
<p>
Notre technique et notre solution de permanente de cils ont été affinées au cours d'une décennie pour agir de manière cohérente et douce sur vos cils.
</p>
<p>
Malgré leur nom, les permanentes de cils ne sont pas permanentes. Les résultats d’une permanente de cils durent généralement de 2 à 3 mois, soit le cycle de croissance naturel des cils. Lorsque les cils permanentés tombent naturellement, les nouveaux poils des cils pousseront droit (comme c'était le cas avant votre permanente).
</p>
<p>
Avant votre permanente des cils, il vous sera demandé de retirer vos lentilles de contact et votre maquillage pour les yeux (un démaquillant, une solution pour lentilles et des gobelets temporaires gratuits sont à votre disposition.) Pendant votre permanente des cils, qui dure environ 1 heure, vous pouvez vous allonger. et détendez-vous en écoutant les sons de notre musique apaisante de spa. Veuillez garder les yeux fermés et détendus pendant toute la durée du traitement pour obtenir les meilleurs résultats de permanente des cils.
</p>
<p>
Semblable à une permanente, essayez d’éviter de mouiller vos cils pendant 4 heures après votre permanente. Nous vous recommandons d'appliquer quotidiennement le revitalisant pour cils sur les cils. Du gel et du mascara revitalisants pour cils sont disponibles au spa, par téléphone et via notre boutique en ligne. L’utilisation régulière du revitalisant pour cils garantira que vos cils restent en bonne santé.
</p>
|]
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s51
                    , offerName = "Prix"
                    , offerPrice = 45
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e7
                   , roleService = s51
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s51
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s51
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s51
                        , thumbnailPhoto = $(embedFile "static/img/eyelash-perm.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s52 <- insert $ Service { serviceName = "Extension de cils"
                            , servicePublished = True
                            , serviceOverview = Just "Extension de cils"
                            , serviceDescr = Just $ Textarea [st|
<h4>Soins à domicile pour extensions de cils</h4>
<ol>
  <li>Dans les 2 premières heures, ne laissez pas l'eau entrer en contact avec les cils.</li>
  <li>Dans les 2 premières heures, ne vaporisez pas le visage, n'utilisez pas de bain de vapeur, ne nagez pas et ne lavez pas le visage à l'eau chaude.</li>
  <li>Utilisez uniquement du mascara lavable et brossez légèrement la pointe des cils.</li>
  <li>N'utilisez pas de mascara sur la zone adhésive des cils avec du dissolvant pour mascara.</li>
  <li>Ne permanentez pas les cils.</li>
  <li>N'utilisez pas de recourbe-cils car cela briserait à la fois les cils et les cils naturels.</li>
  <li>Ne vous frottez pas les yeux ni les cils.</li>
  <li>Lorsque vous vous lavez le visage, séchez toujours les cils après les avoir nettoyés.</li>
</ol>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s52
                    , offerName = "Prix"
                    , offerPrice = 130
                    , offerPrefix = Nothing
                    , offerSuffix = Just " et plus"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e6
                   , roleService = s52
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s52
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s52
                        , thumbnailPhoto = $(embedFile "static/img/eyelash-extension.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s53 <- insert $ Service { serviceName = "Teinture des sourcils et des cils"
                            , servicePublished = True
                            , serviceOverview = Just "Teinture des sourcils et des cils"
                            , serviceDescr = Just $ Textarea [st|Teinture des sourcils et des cils|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s53
                    , offerName = "Prix"
                    , offerPrice = 25
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e4
                   , roleService = s53
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s53
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s53
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s53
                        , thumbnailPhoto = $(embedFile "static/img/eyebrow-or-eyelash-tint.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s54 <- insert $ Service { serviceName = "Traitement de rajeunissement des yeux"
                            , servicePublished = True
                            , serviceOverview = Just "Traitement de rajeunissement des yeux"
                            , serviceDescr = Just $ Textarea [st|
<p>
Ceci est élégamment conçu pour réduire en douceur les poches et les rides ainsi que les cernes autour des yeux. Il s'agit d'un traitement oculaire puissant qui comprend un certain nombre de produits Rejuvi, tels que : le gel réparateur pour les yeux « i », le complexe de fruits, le complexe de vitamine A, le complexe de vitamine C, la formule contour et le masque facial, etc., pour conférer un effet synergique sur les yeux. zone. Des résultats étonnants peuvent être observés immédiatement après la procédure.
</p>
<p>
Les traitements peuvent être effectués toutes les semaines ou toutes les deux semaines, selon l'état de la peau. Un traitement oculaire complet doit comprendre 4 à 6 applications de traitements individuels, plus un programme de soins à domicile.
</p>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s54
                    , offerName = "Prix"
                    , offerPrice = 40
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s54
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s54
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s54
                        , thumbnailPhoto = $(embedFile "static/img/eye-rejuvenating-treatment.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s55 <- insert $ Service { serviceName = "Traitement Anti-Âge des Yeux"
                            , servicePublished = True
                            , serviceOverview = Just "Traitement Anti-Âge des Yeux"
                            , serviceDescr = Just $ Textarea [st|Traitement Anti-Âge des Yeux|]
                            , serviceDuration = duration "01:45"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s55
                    , offerName = "Prix"
                    , offerPrice = 40
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e2
                   , roleService = s55
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s55
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s55
                        , thumbnailPhoto = $(embedFile "static/img/anti-aging-eye-treatment.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s6 <- insert $ Service { serviceName = "Massage corporel"
                           , servicePublished = True
                           , serviceOverview = Just "Massage corporel"
                           , serviceDescr = Just "Massage du corps"
                           , serviceDuration = duration "02:00"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s6
                        , thumbnailPhoto = $(embedFile "static/img/body-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s61 <- insert $ Service { serviceName = "Massage suédois"
                            , servicePublished = True
                            , serviceOverview = Just "Massothérapie suédoise"
                            , serviceDescr = Just $ Textarea [st|
<p>
La massothérapie suédoise est la modalité qui vient à l’esprit lorsque la plupart des gens pensent au massage. En tant que type de travail corporel le plus connu aujourd’hui, l’un des principaux objectifs de la technique du massage suédois est de détendre l’ensemble du corps. Ceci est accompli en frottant les muscles avec de longs mouvements glissants dans le sens du retour du sang vers le cœur. Mais la massothérapie suédoise va au-delà de la relaxation. Le massage suédois est exceptionnellement bénéfique pour augmenter le niveau d'oxygène dans le sang, diminuer les toxines musculaires, améliorer la circulation et la flexibilité tout en relâchant les tensions.
</p>
|]
                            , serviceDuration = duration "01:00"
                            , serviceGroup = Just s6
                            }

    insert_ $ Offer { offerService = s61
                    , offerName = "Prix"
                    , offerPrice = 60
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s61
                   , roleName = "Massothérapeute"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s61
                   , roleName = "Massothérapeute"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s61
                   , roleName = "Massothérapeute"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s61
                        , thumbnailPhoto = $(embedFile "static/img/swedish-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s62 <- insert $ Service { serviceName = "Massage sur chaise"
                            , servicePublished = True
                            , serviceOverview = Just "Massage sur chaise"
                            , serviceDescr = Just $ Textarea [st|
<p>
Le massage sur chaise est un massage de 15 à 20 minutes axé sur le dos, les épaules, le cou, les bras et la tête. Il est conçu pour détendre les muscles et améliorer la flexibilité et le mouvement.
</p>
|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s6
                            }

    insert_ $ Offer { offerService = s62
                    , offerName = "Prix"
                    , offerPrice = 60
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e10
                   , roleService = s62
                   , roleName = "Massothérapeute"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s62
                        , thumbnailPhoto = $(embedFile "static/img/chair-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s63 <- insert $ Service { serviceName = "Massage des pieds"
                            , servicePublished = True
                            , serviceOverview = Just "Massage des pieds"
                            , serviceDescr = Just $ Textarea [st|
<p>
Un massage des pieds est une pratique thérapeutique qui consiste à manipuler les pieds et à appliquer une pression sur des zones spécifiques pour favoriser la relaxation, soulager les tensions et améliorer le bien-être général. Contrairement à la réflexologie, qui est une pratique spécialisée qui se concentre sur des points réflexes spécifiques des pieds qui correspondent à différents organes et systèmes du corps, un massage des pieds implique généralement une approche plus générale.
</p>
<p>
Lors d'un massage des pieds, diverses techniques peuvent être utilisées, notamment le pétrissage, les caresses, le frottement et l'application d'une pression avec les mains, les doigts ou des outils spécialisés. Le massage peut cibler des zones spécifiques des pieds, telles que la voûte plantaire, les talons, les orteils et la plante des pieds, ainsi que les muscles et les articulations environnants.
</p>
<h4>Avantages :</h4>
<ol>
  <li>Détente : Les massages des pieds peuvent aider à réduire le stress et induire un état de relaxation.</li>
  <li>Circulation améliorée : Les techniques de massage utilisées peuvent améliorer le flux sanguin vers les pieds et les membres inférieurs, favorisant ainsi une meilleure circulation.</li>
  <li>Soulagement de la douleur : les massages des pieds peuvent aider à soulager la douleur aux pieds, y compris des affections telles que la fasciite plantaire ou l'inconfort général du pied.</li>
  <li>Reduced muscle tension: By targeting specific muscles in the feet, a massage can help release tension and improve flexibility.</li>
  <li>Bien-être général amélioré : les massages des pieds auraient un impact positif sur le flux d’énergie du corps, contribuant ainsi à une sensation de bien-être.</li>
</ol>
|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s6
                            }

    insert_ $ Offer { offerService = s63
                    , offerName = "Prix"
                    , offerPrice = 30
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e1
                   , roleService = s63
                   , roleName = "Massothérapeute"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s63
                   , roleName = "Massothérapeute"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s63
                        , thumbnailPhoto = $(embedFile "static/img/foot-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s7 <- insert $ Service { serviceName = "Services de maquillage"
                           , servicePublished = True
                           , serviceOverview = Just "Services de maquillage"
                           , serviceDescr = Just "Services de maquillage"
                           , serviceDuration = duration "03:30"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s7
                        , thumbnailPhoto = $(embedFile "static/img/makeup-services.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s71 <- insert $ Service { serviceName = "Spécial mariage"
                            , servicePublished = True
                            , serviceOverview = Just "Spécial mariage"
                            , serviceDescr = Just $ Textarea [st|
<p>Le forfait comprend : maquillage de mariée, chignon, soin du visage et manucure</p>
|]
                            , serviceDuration = duration "03:30"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s71
                    , offerName = "Prix"
                    , offerPrice = 200
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s71
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (3 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s71
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (3 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s71
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (3 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s71
                        , thumbnailPhoto = $(embedFile "static/img/wedding-special.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s72 <- insert $ Service { serviceName = "Maquillage de mariée"
                            , servicePublished = True
                            , serviceOverview = Just "Maquillage de mariée"
                            , serviceDescr = Just $ Textarea [st|Maquillage de mariée (45 min)|]
                            , serviceDuration = duration "00:45"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s72
                    , offerName = "Prix"
                    , offerPrice = 85
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e4
                   , roleService = s72
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s72
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s72
                        , thumbnailPhoto = $(embedFile "static/img/bridal-make-up.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s73 <- insert $ Service { serviceName = "Maquillage de soirée"
                            , servicePublished = True
                            , serviceOverview = Just "Maquillage de soirée"
                            , serviceDescr = Just $ Textarea [st|Maquillage de soirée (30 min)|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s73
                    , offerName = "Prix"
                    , offerPrice = 60
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e5
                   , roleService = s73
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s73
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s73
                        , thumbnailPhoto = $(embedFile "static/img/evening-make-up.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s74 <- insert $ Service { serviceName = "Cours de maquillage"
                            , servicePublished = True
                            , serviceOverview = Just "Cours de maquillage"
                            , serviceDescr = Just $ Textarea [st|Cours de maquillage (60 min)|]
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s74
                    , offerName = "Prix"
                    , offerPrice = 100
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e6
                   , roleService = s74
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s74
                   , roleName = "Maquilleur"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s74
                        , thumbnailPhoto = $(embedFile "static/img/make-up-lesson.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s8 <- insert $ Service { serviceName = "Epilation à la cire"
                           , servicePublished = True
                           , serviceOverview = Just "Epilation à la cire"
                           , serviceDescr = Just "Epilation à la cire"
                           , serviceDuration = duration "01:25"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s8
                        , thumbnailPhoto = $(embedFile "static/img/waxing.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s81 <- insert $ Service { serviceName = "Épilation à la cire corporelle"
                            , servicePublished = True
                            , serviceOverview = Just "Épilation à la cire corporelle"
                            , serviceDescr = Just $ Textarea [st|Épilation à la cire corporelle|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s8
                            }

    insert_ $ Offer { offerService = s81
                    , offerName = "Prix"
                    , offerPrice = 50
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e8
                   , roleService = s81
                   , roleName = "Spécialiste de l'épilation"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s81
                   , roleName = "Spécialiste de l'épilation"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s81
                   , roleName = "Spécialiste de l'épilation"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s81
                        , thumbnailPhoto = $(embedFile "static/img/body-waxing.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s82 <- insert $ Service { serviceName = "Épilation du visage"
                            , servicePublished = True
                            , serviceOverview = Just "Épilation du visage"
                            , serviceDescr = Just $ Textarea [st|Épilation du visage|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s8
                            }

    insert_ $ Offer { offerService = s82
                    , offerName = "Prix"
                    , offerPrice = 45
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s82
                   , roleName = "Spécialiste de l'épilation"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s82
                   , roleName = "Spécialiste de l'épilation"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s82
                        , thumbnailPhoto = $(embedFile "static/img/face-waxing.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s9 <- insert $ Service { serviceName = "Soins des ongles"
                           , servicePublished = True
                           , serviceOverview = Just "Services de soins des ongles"
                           , serviceDescr = Just "Services de soins des ongles"
                           , serviceDuration = duration "00:45"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s9
                        , thumbnailPhoto = $(embedFile "static/img/nail-care.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s91 <- insert $ Service { serviceName = "Manucure"
                            , servicePublished = True
                            , serviceOverview = Just "Manucure"
                            , serviceDescr = Just $ Textarea [st|Manucure|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s91
                    , offerName = "Prix"
                    , offerPrice = 15
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s91
                   , roleName = "Technicien en manucure"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s91
                   , roleName = "Technicienne en manucure"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s91
                   , roleName = "Technicienne en manucure"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s91
                        , thumbnailPhoto = $(embedFile "static/img/manicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s92 <- insert $ Service { serviceName = "Manucure sans puce"
                            , servicePublished = True
                            , serviceOverview = Just "Manucure sans puce"
                            , serviceDescr = Just $ Textarea [st|Manucure sans puce|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s92
                    , offerName = "Prix"
                    , offerPrice = 32
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e10
                   , roleService = s92
                   , roleName = "Technicien en ongles"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s92
                   , roleName = "Technicien en ongles"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s92
                        , thumbnailPhoto = $(embedFile "static/img/no-chip-manicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s93 <- insert $ Service { serviceName = "Pédicure"
                            , servicePublished = True
                            , serviceOverview = Just "Pédicure"
                            , serviceDescr = Just $ Textarea [st|Pédicure|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s93
                    , offerName = "Prix"
                    , offerPrice = 35
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e1
                   , roleService = s93
                   , roleName = "Technicien en ongles"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s93
                   , roleName = "Technicienne en ongles"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s93
                        , thumbnailPhoto = $(embedFile "static/img/pedicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s94 <- insert $ Service { serviceName = "Pédicure sans puce"
                            , servicePublished = True
                            , serviceOverview = Just "Pédicure sans puce"
                            , serviceDescr = Just $ Textarea [st|Pédicure sans puce|]
                            , serviceDuration = duration "00:40"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s94
                    , offerName = "Prix"
                    , offerPrice = 55
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e2
                   , roleService = s94
                   , roleName = "Technicienne en ongles"
                   , roleDuration = 60 * (0 * 60 + 40)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s94
                   , roleName = "Technicien en ongles"
                   , roleDuration = 60 * (0 * 60 + 40)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s94
                        , thumbnailPhoto = $(embedFile "static/img/no-chip-pedicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s95 <- insert $ Service { serviceName = "Ongle Acrylique"
                            , servicePublished = True
                            , serviceOverview = Just "Ensemble complet d'ongles en acrylique"
                            , serviceDescr = Just $ Textarea [st|Ensemble complet d'ongles en acrylique|]
                            , serviceDuration = duration "00:25"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s95
                    , offerName = "Prix"
                    , offerPrice = 38
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s95
                   , roleName = "Technicien en ongles"
                   , roleDuration = 60 * (0 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s95
                   , roleName = "Technicienne en ongles"
                   , roleDuration = 60 * (0 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s95
                        , thumbnailPhoto = $(embedFile "static/img/acrylic-nail-full-set.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s96 <- insert $ Service { serviceName = "Trempette à la paraffine pour les mains"
                            , servicePublished = True
                            , serviceOverview = Just "Trempette à la paraffine pour les mains"
                            , serviceDescr = Just $ Textarea [st|Trempette à la paraffine pour les mains|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s96
                    , offerName = "Prix"
                    , offerPrice = 10
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e4
                   , roleService = s96
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s96
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s96
                        , thumbnailPhoto = $(embedFile "static/img/hand-paraffin-dip.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s97 <- insert $ Service { serviceName = "Changement de vernis"
                            , servicePublished = True
                            , serviceOverview = Just "Changement de vernis"
                            , serviceDescr = Just $ Textarea [st|Changement de vernis|]
                            , serviceDuration = duration "00:15"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s97
                    , offerName = "Prix"
                    , offerPrice = 5
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e6
                   , roleService = s97
                   , roleName = "Technicien en ongles"
                   , roleDuration = 60 * (0 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s97
                   , roleName = "Technicien en ongles"
                   , roleDuration = 60 * (0 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s97
                        , thumbnailPhoto = $(embedFile "static/img/polish-change.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s10 <- insert $ Service { serviceName = "Mise en forme et remise en forme"
                            , servicePublished = True
                            , serviceOverview = Just "Mise en forme et fitness"
                            , serviceDescr = Just "Mise en forme et fitness"
                            , serviceDuration = duration "01:15"
                            , serviceGroup = Nothing
                            }

    insert_ $ Thumbnail { thumbnailService = s10
                        , thumbnailPhoto = $(embedFile "static/img/body-shaping-and-fitness.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s101 <- insert $ Service { serviceName = "Mise en forme du corps"
                             , servicePublished = True
                             , serviceOverview = Just "Abdomen et taille, hanches et cuisses, jambes et bras"
                             , serviceDescr = Just $ Textarea [st|
<p>
Mise en forme du corps : Abdomen et taille, hanches et cuisses, jambes et bras
</p>
|]
                             , serviceDuration = duration "01:15"
                             , serviceGroup = Just s10
                             }

    insert_ $ Offer { offerService = s101
                    , offerName = "Prix"
                    , offerPrice = 350
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e7
                   , roleService = s101
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s101
                   , roleName = "Esthéticien"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s101
                        , thumbnailPhoto = $(embedFile "static/img/body-shaping.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s102 <- insert $ Service { serviceName = "Mise en forme du visage"
                             , servicePublished = True
                             , serviceOverview = Just "Mise en forme du visage"
                             , serviceDescr = Just $ Textarea [st|Mise en forme du visage|]
                             , serviceDuration = duration "00:45"
                             , serviceGroup = Just s10
                             }

    insert_ $ Offer { offerService = s102
                    , offerName = "Prix"
                    , offerPrice = 300
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e8
                   , roleService = s102
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s102
                   , roleName = "Esthéticienne"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s102
                        , thumbnailPhoto = $(embedFile "static/img/face-shaping.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }


    pass6 <- liftIO $ makePassword "bernardl" 17
    c1 <- insert $ User { userName = "bernardl"
                        , userPassword = decodeUtf8 pass6
                        , userAdmin = False
                        , userFullName = Just "Bernard Louise"
                        , userEmail = Just "bernardl@mail.fr"
                        }

    insert_ $ UserPhoto { userPhotoUser = c1
                        , userPhotoPhoto = $(embedFile "static/img/customer-women-1.avif")
                        , userPhotoMime = "image/avif"
                        }

    let book1 = Book { bookOffer = o131
                     , bookRole = Just r1311
                     , bookCustomer = c1
                     , bookDay = addDays 1 today
                     , bookTime = time
                     , bookAddr = businessAddr business
                     , bookTzo = businessTzo business
                     , bookTz = businessTz business
                     , bookStatus = BookStatusRequest
                     }

    b1 <- insert book1

    insert_ $ Hist { histBook = b1
                   , histUser = c1
                   , histLogtime = now
                   , histDay = bookDay book1
                   , histTime = bookTime book1
                   , histAddr = bookAddr book1
                   , histTzo = bookTzo book1
                   , histTz = bookTz book1
                   , histStatus = BookStatusRequest
                   , histRoleName = Just $ roleName role1311
                   , histStaffName = Just $ staffName empl11
                   }


    pass7 <- liftIO $ makePassword "bardota" 17
    c2 <- insert $ User { userName = "bardota"
                        , userPassword = decodeUtf8 pass7
                        , userAdmin = False
                        , userFullName = Just "Bardot Alain"
                        , userEmail = Just "bardota@mail.fr"
                        }

    insert_ $ UserPhoto { userPhotoUser = c2
                        , userPhotoPhoto = $(embedFile "static/img/customer-men-1.avif")
                        , userPhotoMime = "image/avif"
                        }

    let book2 = Book { bookOffer = o111
                     , bookRole = Just r111
                     , bookCustomer = c2
                     , bookDay = addDays 2 today
                     , bookTime = time
                     , bookAddr = businessAddr business
                     , bookTzo = businessTzo business
                     , bookTz = businessTz business
                     , bookStatus = BookStatusRequest
                     }

    b2 <- insert book2

    insert_ $ Hist { histBook = b2
                   , histUser = c2
                   , histLogtime = now
                   , histDay = bookDay book2
                   , histTime = bookTime book2
                   , histAddr = bookAddr book2
                   , histTzo = bookTzo book2
                   , histTz = bookTz book2
                   , histStatus = BookStatusRequest
                   , histRoleName = Just $ roleName role111
                   , histStaffName = Just $ staffName empl1
                   }

    let book3 = Book { bookOffer = o141
                     , bookRole = Nothing
                     , bookCustomer = c1
                     , bookDay = addDays 3 today
                     , bookTime = time
                     , bookAddr = businessAddr business
                     , bookTzo = businessTzo business
                     , bookTz = businessTz business
                     , bookStatus = BookStatusRequest
                     }

    b3 <- insert book3

    insert_ $ Hist { histBook = b3
                   , histUser = c1
                   , histLogtime = now
                   , histDay = bookDay book3
                   , histTime = bookTime book3
                   , histAddr = bookAddr book3
                   , histTzo = bookTzo book3
                   , histTz = bookTz book3
                   , histStatus = BookStatusRequest
                   , histRoleName = Nothing
                   , histStaffName = Nothing
                   }

    let book4 = Book { bookOffer = o251
                     , bookRole = Just r925
                     , bookCustomer = c1
                     , bookDay = addDays 1 today
                     , bookTime = time
                     , bookAddr = businessAddr business
                     , bookTzo = businessTzo business
                     , bookTz = businessTz business
                     , bookStatus = BookStatusRequest
                     }

    b4 <- insert book4

    insert_ $ Hist { histBook = b4
                   , histUser = c1
                   , histLogtime = now
                   , histDay = bookDay book4
                   , histTime = bookTime book4
                   , histAddr = bookAddr book4
                   , histTzo = bookTzo book4
                   , histTz = bookTz book4
                   , histStatus = BookStatusRequest
                   , histRoleName = Just $ roleName role925
                   , histStaffName = Just $ staffName empl9
                   }

    let book5 = Book { bookOffer = o321
                     , bookRole = Just r925
                     , bookCustomer = c1
                     , bookDay = addDays 1 today
                     , bookTime = time
                     , bookAddr = businessAddr business
                     , bookTzo = businessTzo business
                     , bookTz = businessTz business
                     , bookStatus = BookStatusRequest
                     }

    b5 <- insert book5

    insert_ $ Hist { histBook = b5
                   , histUser = c1
                   , histLogtime = now
                   , histDay = bookDay book5
                   , histTime = bookTime book5
                   , histAddr = bookAddr book5
                   , histTzo = bookTzo book5
                   , histTz = bookTz book5
                   , histStatus = BookStatusRequest
                   , histRoleName = Just $ roleName role925
                   , histStaffName = Just $ staffName empl9
                   }

    let book6 = Book { bookOffer = o321
                     , bookRole = Just r925
                     , bookCustomer = c1
                     , bookDay = addDays 2 today
                     , bookTime = time
                     , bookAddr = businessAddr business
                     , bookTzo = businessTzo business
                     , bookTz = businessTz business
                     , bookStatus = BookStatusRequest
                     }

    b6 <- insert book6

    insert_ $ Hist { histBook = b6
                   , histUser = c1
                   , histLogtime = now
                   , histDay = bookDay book6
                   , histTime = bookTime book6
                   , histAddr = bookAddr book6
                   , histTzo = bookTzo book6
                   , histTz = bookTz book6
                   , histStatus = BookStatusRequest
                   , histRoleName = Just $ roleName role925
                   , histStaffName = Just $ staffName empl9
                   }

    let book7 = Book { bookOffer = o321
                     , bookRole = Just r925
                     , bookCustomer = c1
                     , bookDay = addDays 3 today
                     , bookTime = time
                     , bookAddr = businessAddr business
                     , bookTzo = businessTzo business
                     , bookTz = businessTz business
                     , bookStatus = BookStatusRequest
                     }

    b7 <- insert book7

    insert_ $ Hist { histBook = b7
                   , histUser = c1
                   , histLogtime = now
                   , histDay = bookDay book7
                   , histTime = bookTime book7
                   , histAddr = bookAddr book7
                   , histTzo = bookTzo book7
                   , histTz = bookTz book7
                   , histStatus = BookStatusRequest
                   , histRoleName = Just $ roleName role925
                   , histStaffName = Just $ staffName empl9
                   }
    return ()
  where
      duration :: String -> Maybe DiffTime
      duration = parseTimeM True defaultTimeLocale "%H:%M"
