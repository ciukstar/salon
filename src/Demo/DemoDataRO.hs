{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}

module Demo.DemoDataRO (populateRO) where

import ClassyPrelude.Yesod (ReaderT)
import Control.Monad (forM_)
import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (st)
import qualified Data.ByteString.Base64 as B64 (decode)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar
    ( DayPeriod (periodFirstDay, periodLastDay), DayOfWeek (Saturday, Sunday)
    , dayOfWeek, addDays, toGregorian
    )
import Data.Time.Calendar.Month (pattern YearMonth)
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
      , contactUsShowMap, contactUsLongitude, contactUsLatitude
      )
    , DayType (Weekday)
    )
import Data.FileEmbed (embedFile)
import Demo.DemoPhotos
    ( man01, man02, man03, man04, man05, man06
    , woman01, woman02, woman03, woman04, woman05
    )

populateRO :: MonadIO m => ReaderT SqlBackend m ()
populateRO = do

    (now,today,time,month) <- liftIO $ do
        t <- getCurrentTime
        return ( t
               , utctDay t
               , timeToTimeOfDay (utctDayTime t)
               , let (y,m,_) = toGregorian (utctDay t) in YearMonth y m
               )

    let business = Business { businessName = "Salon"
                            , businessFullName = Just "SRL Salon"
                            , businessCurrency = "RON"
                            , businessAddr = "Bulevardul Ion C. Brătianu, București, Romania"
                            , businessTzo = TimeZone 120 False "RO"
                            , businessTz = "Europe/Bucharest"
                            , businessPhone = Just "+40768469474"
                            , businessMobile = Just "+40769859190"
                            , businessEmail = Just "salon@mail.ro"
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
<h2 style="color:gray">Misiunea noastră
<p>Misiunea noastră este simplă: să oferim un mediu special fiecărei persoane care își intră pe uși, unde se pot răsfăța, se pot îngriji și se pot răsfăța, sporindu-și în același timp imaginea personală și aducând un sentiment de bunăstare în viața lor.
<h2 style="color:gray">Etosul nostru
<p>Orice persoană care vine la saloanele noastre este unică. Tratăm fiecare client unic în funcție de nevoile sale personale. Ne mândrim să oferim serviciul pe care clientul se așteaptă și tratamentele de care el/ea are nevoie și le vom reevalua continuu cerințele în funcție de stilul de viață și de corp.
<p>Nu vom oferi niciodată tratamente care nu sunt necesare și facem din fiecare client o prioritate. Acesta este exact motivul pentru care putem spune cu mândrie că, de-a lungul anilor, ne-am construit o bază de clienți loiali. Învățăm de la ei pe măsură ce avem grijă de ei și ne străduim să ținem pasul cu cele mai recente tendințe și tratamente disponibile pentru a ne asigura că îndeplinim întotdeauna nevoile clienților noștri valoroși și ale oricăror viitori vizitatori.
<h2 style="color:gray">Obiectivele noastre
<p>Vom continua să oferim cele mai noi tratamente, cele mai inovatoare tehnici în timp ce folosim cele mai bune produse de pe piață. Toate acestea în medii elegante, curate și primitoare cu terapeuți pregătiți, profesioniști și prietenoși. Ne vom strădui să divulgăm mesajul nostru că este dreptul fiecăruia de a se simți bine!
|]
                      }

    insert_ $ ContactUs { contactUsBusiness = b
                        , contactUsHtml = [shamlet|
<section style="margin:0 1rem">
  <h3 style="color:gray">Sunați-ne
  <dl>
    <dt>
      <i>Telefon
    <dd>
      $maybe phone <- businessPhone business
        #{phone}
    <dt>
      <i>Mobil
    <dd>
      $maybe mobile <- businessMobile business
        #{mobile}
<section style="margin:0 1rem">
  <h3 style="color:gray">Trimiteți-ne un e-mail
  <dl>
    <dt>
      <i>E-mail
    <dd>
      $maybe email <- businessEmail business
        #{email}
<section style="margin:0 1rem">
  <h3 style="color:gray">Vino să ne vezi
  <dl>
    <dt>
      <i>Adresa
    <dd>
      #{businessAddr business}
|]
                        , contactUsShowSchedule = True
                        , contactUsShowMap = True
                        , contactUsLongitude = Just 26.1039028
                        , contactUsLatitude = Just 44.4327417
                        }
    
    pass <- liftIO $ makePassword "root" 17
    insert_ $ User { userName = "root"
                   , userPassword = decodeUtf8 pass
                   , userAdmin = True
                   , userFullName = Just "Popescu Ion Andrei"
                   , userEmail = Just "popescuia@mail.ro"
                   }

    pass1 <- liftIO $ makePassword "popaa" 17
    let user1 = User { userName = "popaa"
                     , userPassword = decodeUtf8 pass1
                     , userAdmin = False
                     , userFullName = Just "Popa Andrei"
                     , userEmail = Just "popaa@mail.ro"
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

    pass2 <- liftIO $ makePassword "raduam" 17
    let user2 = User { userName = "raduam"
                     , userPassword = decodeUtf8 pass2
                     , userAdmin = False
                     , userFullName = Just "Radu Ana-Maria"
                     , userEmail = Just "raduam@mail.ro"
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

    pass3 <- liftIO $ makePassword "johnjohnson" 17
    let user3 = User { userName = "johnjohnson"
                     , userPassword = decodeUtf8 pass3
                     , userAdmin = False
                     , userFullName = Just "Ionescu Alexandru Victor"
                     , userEmail = Just "ionescuav@mail.ro"
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

    pass4 <- liftIO $ makePassword "stoicama" 17
    let user4 = User { userName = "stoicama"
                     , userPassword = decodeUtf8 pass4
                     , userAdmin = False
                     , userFullName = Just "Stoica Maria Alexandra"
                     , userEmail = Just "stoicama@mail.ro"
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

    pass5 <- liftIO $ makePassword "rususa" 17
    let user5 = User { userName = "rususa"
                     , userPassword = decodeUtf8 pass5
                     , userAdmin = False
                     , userFullName = Just "Rusu Ştefan Alexandru"
                     , userEmail = Just "rususa@mail.ro"
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

    e6 <- insert $ Staff { staffName = "Munteanu David"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "munteanud@mail.ro"
                         , staffUser = Nothing
                         }

    case B64.decode man04 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e6
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e7 <- insert $ Staff { staffName = "Matei Andreea Alexandra"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "mateiaa@mail.ro"
                         , staffUser = Nothing
                         }

    case B64.decode woman03 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e7
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e8 <- insert $ Staff { staffName = "Marin Ioana"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "marini@mail.ro"
                         , staffUser = Nothing
                         }

    case B64.decode woman04 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e8
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    let empl9 = Staff { staffName = "Lazar Mihai"
                      , staffStatus = EmplStatusAvailable
                      , staffPhone = businessPhone business
                      , staffMobile = businessMobile business
                      , staffEmail = Just "lazarm@mail.ro"
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

    e10 <- insert $ Staff { staffName = "Ciobanu Ionuţ Ştefan"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "ciobanuis@mail.ro"
                         , staffUser = Nothing
                         }

    case B64.decode man06 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e10
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    pass11 <- liftIO $ makePassword "floreaim" 17
    let user11 = User { userName = "floreaim"
                      , userPassword = decodeUtf8 pass11
                      , userAdmin = False
                      , userFullName = Just "Florea Ioana Maria"
                      , userEmail = Just "floreaim@mail.ro"
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



    s1 <- insert $ Service { serviceName = "Îngrijirea părului"
                           , serviceOverview = Just "Servicii de îngrijire a părului"
                           , servicePublished = True
                           , serviceDescr = Just "<p>Întotdeauna distincți și niciodată neobișnuiți, experții noștri în îngrijirea părului s-au pregătit intens pentru a oferi servicii de tunsoare și coafare de designer care sunt personalizate pentru nevoile fiecărui client. Ca salon, ne susținem echipa în eforturile lor de a-și perfecționa tehnicile individuale și le oferim stiliștilor noștri libertatea de a-și exprima pe deplin și explora creativitatea. Acest lucru, la rândul său, oferă oaspeților noștri oportunitatea de a se bucura de servicii personalizate de fiecare dată. Indiferent dacă îți place aspectul modern sau o croială clasică vorbește despre stilul tău de semnătură – opțiunile sunt nesfârșite la noi.</p>"
                           , serviceDuration = duration "01:30"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s1
                        , thumbnailPhoto = $(embedFile "static/img/hair-care.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s11 <- insert $ Service { serviceName = "Tunsori pentru bărbați"
                            , servicePublished = True
                            , serviceOverview = Just "Tunsori pentru bărbați"
                            , serviceDescr = Just "Tunsori pentru bărbați"
                            , serviceDuration = duration "01:00"
                            , serviceGroup = Just s1
                            }

    let role111 =  Role { roleStaff = e1
                        , roleService = s11
                        , roleName = "Frizer"
                        , roleDuration = 60 * (1 * 60 + 0)
                        , roleRating = Just 5
                        } 

    r111 <- insert role111

    insert_ $ Role { roleStaff = e2
                   , roleService = s11
                   , roleName = "Frizer"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    o111 <- insert $ Offer { offerService = s11
                           , offerName = "Preț"
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

    s12 <- insert $ Service { serviceName = "Tunsori pentru femei (peste umeri)"
                            , servicePublished = True
                            , serviceOverview = Just "Tunsori deasupra umerilor pentru femei"
                            , serviceDescr = Just "Tunsori deasupra umerilor pentru femei"
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s1
                            }

    insert_ $ Offer { offerService = s12
                    , offerName = "Preț"
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
                   , roleName = "Coafeză"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 3
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s12
                   , roleName = "Coafor"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s12
                   , roleName = "Coafeză"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    s13 <- insert $ Service { serviceName = "Tunsori pentru femei (sub umeri)"
                            , servicePublished = True
                            , serviceOverview = Just "Tunsori sub umeri pentru femei"
                            , serviceDescr = Just "Tunsori sub umeri pentru femei"
                            , serviceDuration = duration "01:35"
                            , serviceGroup = Just s1
                            }

    o131 <- insert $ Offer { offerService = s13
                           , offerName = "Preț"
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
                        , roleName = "Stilist"
                        , roleDuration = 60 * (1 * 60 + 35)
                        , roleRating = Just 5
                        }

    r1311 <- insert role1311

    insert_ $ Role { roleStaff = e3
                   , roleService = s13
                   , roleName = "Stilist asistent"
                   , roleDuration = 60 * (1 * 60 + 35)
                   , roleRating = Just 4
                   }

    s14 <- insert $ Service { serviceName = "Tunsori pentru copii"
                            , servicePublished = True
                            , serviceOverview = Just "Tunsori pentru copii"
                            , serviceDescr = Just "Tunsori pentru copii"
                            , serviceDuration = duration "01:20"
                            , serviceGroup = Just s1
                            }

    o141 <- insert $ Offer { offerService = s14
                           , offerName = "Preț"
                           , offerPrice = 16
                           , offerPrefix = Nothing
                           , offerSuffix = Just "-20 RON (în funcție de lungimea părului)"
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
                   , roleName = "Stilist junior"
                   , roleDuration = 60 * (1 * 60 + 20)
                   , roleRating = Just 5
                   }

    s15 <- insert $ Service { serviceName = "Servicii chimice"
                            , servicePublished = True
                            , serviceOverview = Just "Servicii chimice"
                            , serviceDescr = Just "<p>Serviciile noastre chimice se adresează unei game largi de nevoi de îngrijire a părului. Tratamentele noastre de netezire combate încrețirea, cresc manevrabilitate și oferă rezultatele de lungă durată pe care le-ai dorit întotdeauna. Vă puteți bucura de șuvițe netede și mătăsoase cu serviciul nostru de keratina, precum și de a repara deteriorarea părului prin completarea proteinelor pierdute. Dacă valuri adăugate, bucle și volum sunt ceea ce cauți, serviciile noastre profesionale de permanentă vă vor permite să obțineți textura dorită. Folosim produse și tehnici inovatoare pentru a stabili bucle definite pe care sigur le veți iubi.</p>"
                            , serviceDuration = duration "02:00"
                            , serviceGroup = Just s1
                            }

    insert_ $ Thumbnail { thumbnailService = s15
                        , thumbnailPhoto = $(embedFile "static/img/chemical-services.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s151 <- insert $ Service { serviceName = "Condiţionare"
                             , servicePublished = True
                             , serviceOverview = Just "Servicii de condiționare"
                             , serviceDescr = Just "Servicii de condiționare"
                             , serviceDuration = duration "01:35"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s151
                        , thumbnailPhoto = $(embedFile "static/img/conditioning.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s1511 <- insert $ Service { serviceName = "După Balsam Perm"
                              , servicePublished = True
                              , serviceOverview = Just "După Balsam Perm"
                              , serviceDescr = Just "După Balsam Perm"
                              , serviceDuration = duration "01:25"
                              , serviceGroup = Just s151
                              }

    insert_ $ Offer { offerService = s1511
                    , offerName = "Preț"
                    , offerPrice = 99
                    , offerPrefix = Nothing
                    , offerSuffix = Just " și mai mult"
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
                   , roleName = "Stilist"
                   , roleDuration = 60 * (1 * 60 + 20)
                   , roleRating = Just 4
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s1511
                   , roleName = "Stilist"
                   , roleDuration = 60 * (1 * 60 + 20)
                   , roleRating = Just 4
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1511
                   , roleName = "Stilist"
                   , roleDuration = 60 * (1 * 60 + 20)
                   , roleRating = Just 4
                   }

    s1512 <- insert $ Service { serviceName = "Înainte de Balsam Perm"
                              , servicePublished = True
                              , serviceOverview = Just "Înainte de Balsam Perm"
                              , serviceDescr = Just "Înainte de Balsam Perm"
                              , serviceDuration = duration "01:15"
                              , serviceGroup = Just s151
                              }

    insert_ $ Offer { offerService = s1512
                    , offerName = "Preț"
                    , offerPrice = 110
                    , offerPrefix = Nothing
                    , offerSuffix = Just " și mai mult"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e4
                   , roleService = s1512
                   , roleName = "Stilist"
                   , roleDuration = 60 * (1 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1512
                   , roleName = "Stilist"
                   , roleDuration = 60 * (1 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s1512
                   , roleName = "Stilist"
                   , roleDuration = 60 * (1 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s1512
                        , thumbnailPhoto = $(embedFile "static/img/before-perm-conditioner.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s152 <- insert $ Service { serviceName = "Repere și culoare"
                             , servicePublished = True
                             , serviceOverview = Just "Repere și culoare"
                             , serviceDescr = Just "Repere și culoare"
                             , serviceDuration = duration "01:10"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s152
                        , thumbnailPhoto = $(embedFile "static/img/highlights-and-color.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s1521 <- insert $ Service { serviceName = "Complete"
                              , servicePublished = True
                              , serviceOverview = Just "Repere și culoare - Complete"
                              , serviceDescr = Just "Repere și culoare - Complete"
                              , serviceDuration = duration "01:00"
                              , serviceGroup = Just s152
                              }

    insert_ $ Offer { offerService = s1521
                    , offerName = "Preț"
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
                   , roleName = "Colorist"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s1521
                   , roleName = "Colorist"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e10
                   , roleService = s1521
                   , roleName = "Colorist"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    s1522 <- insert $ Service { serviceName = "Parțial"
                              , servicePublished = True
                              , serviceOverview = Just "Relete și culoare - Parțial"
                              , serviceDescr = Just "Relete și culoare - Parțial"
                              , serviceDuration = duration "01:15"
                              , serviceGroup = Just s152
                              }

    insert_ $ Offer { offerService = s1522
                    , offerName = "Preț"
                    , offerPrice = 68
                    , offerPrefix = Nothing
                    , offerSuffix = Just " și mai mult"
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
                   , roleName = "Colorist"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s1522
                   , roleName = "Colorist"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    s1523 <- insert $ Service { serviceName = "Culoare permanentă"
                              , servicePublished = True
                              , serviceOverview = Just "Culoare permanentă"
                              , serviceDescr = Just "Culoare permanentă"
                              , serviceDuration = duration "01:45"
                              , serviceGroup = Just s152
                              }

    insert_ $ Offer { offerService = s1523
                    , offerName = "Preț"
                    , offerPrice = 68
                    , offerPrefix = Nothing
                    , offerSuffix = Just " și mai mult"
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
                   , roleName = "Colorist"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e10
                   , roleService = s1523
                   , roleName = "Colorist"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s1523
                   , roleName = "Colorist"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    s153 <- insert $ Service { serviceName = "Perm"
                             , servicePublished = True
                             , serviceOverview = Just "Un val permanent"
                             , serviceDescr = Just "Perm este o modalitate excelentă de a vă oferi un nou aspect"
                             , serviceDuration = duration "00:45"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s153
                        , thumbnailPhoto = $(embedFile "static/img/perm.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s1531 <- insert $ Service { serviceName = "Perm complet"
                              , servicePublished = True
                              , serviceOverview = Just "Perm complet"
                              , serviceDescr = Just "Perm complet"
                              , serviceDuration = duration "00:35"
                              , serviceGroup = Just s153
                              }

    insert_ $ Offer { offerService = s1531
                    , offerName = "Preț"
                    , offerPrice = 79
                    , offerPrefix = Nothing
                    , offerSuffix = Just " și mai mult"
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
                   , roleName = "Stilist"
                   , roleDuration = 60 * (0 * 60 + 35)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s1531
                   , roleName = "Stilist"
                   , roleDuration = 60 * (0 * 60 + 35)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s1531
                   , roleName = "Stilist"
                   , roleDuration = 60 * (0 * 60 + 35)
                   , roleRating = Just 5
                   }

    s1532 <- insert $ Service { serviceName = "Reparare acidă Perm"
                              , servicePublished = True
                              , serviceOverview = Just "Reparare acidă Perm"
                              , serviceDescr = Just "Reparare acidă Perm"
                              , serviceDuration = duration "01:25"
                              , serviceGroup = Just s153
                              }

    insert_ $ Offer { offerService = s1532
                    , offerName = "Preț"
                    , offerPrice = 89
                    , offerPrefix = Nothing
                    , offerSuffix = Just " și mai mult"
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
                   , roleName = "Stilist"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s1532
                   , roleName = "Stilist"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1532
                   , roleName = "Stilist"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    s1533 <- insert $ Service { serviceName = "Îndreptare Perm japoneză"
                              , servicePublished = True
                              , serviceOverview = Just "Îndreptare Perm japoneză"
                              , serviceDescr = Just "Îndreptare Perm japoneză"
                              , serviceDuration = duration "01:25"
                              , serviceGroup = Just s153
                              }

    insert_ $ Offer { offerService = s1533
                    , offerName = "Preț"
                    , offerPrice = 250
                    , offerPrefix = Nothing
                    , offerSuffix = Just " și mai mult"
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
                   , roleName = "Stilist"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s1533
                   , roleName = "Stilist"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s1533
                   , roleName = "Stilist"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    s2 <- insert $ Service { serviceName = "Îngrijirea feței"
                           , servicePublished = True
                           , serviceOverview = Just "Îngrijirea feței"
                           , serviceDescr = Just $ Textarea [st|
<p>
Fața ta este o pânză expresivă care arată experiența și emoția. Într-unul dintre cele mai bune saloane din lume, paleta noastră oferă tratamente hrănitoare care sporesc și subliniază frumusețea, tinerețea și culoarea corpului tău. Înainte de orice tratament facial, cosmeticianul nostru profesionist vă va oferi o consolidare și lucrați de acolo. Nu o sa crezi diferenta!
</p>
<p>Toate tratamentele faciale includ modelarea sprâncenelor.</p>
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

    s21 <- insert $ Service { serviceName = "Îngrijire de bază a feței"
                            , servicePublished = True
                            , serviceOverview = Just "Îngrijire de bază a feței (60 min)"
                            , serviceDescr = Just "Curățare profundă, exfoliere cu tratament cu abur, urmată de extracții, apoi modelarea sprâncenelor; un masaj de stres al feței, gâtului și umerilor. O mască personalizată, precum și un tratament regulat pentru ochi, urmat de aplicarea unei creme hidratante/de soare. Acest tratament de curățare relaxant, dar serios, vă va lăsa cu un ten curat, proaspăt și strălucitor."
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s21
                    , offerName = "Preț"
                    , offerPrice = 55
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s21
                    , offerName = "Pachet"
                    , offerPrice = 250
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 sesiuni"
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
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s21
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e10
                   , roleService = s21
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    s22 <- insert $ Service { serviceName = "Tratament facial Deluxe"
                            , servicePublished = True
                            , serviceOverview = Just "Tratament facial Deluxe"
                            , serviceDescr = Just "Acest tratament facial special poate fi personalizat în funcție de situația pielii clientului (adică uscată, grasă, sensibilă etc.). Este creat pentru a netezi și a înmuia tenul în timp ce vă detensionează întregul corp. Fața noastră de lux te va face să te simți și să arăți mai sănătos."
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s22
                    , offerName = "Preț"
                    , offerPrice = 75
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s22
                    , offerName = "Pachet"
                    , offerPrice = 350
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 sesiuni"
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
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s22
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s22
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    s23 <- insert $ Service { serviceName = "Tratament facial COCOON"
                            , servicePublished = True
                            , serviceOverview = Just "Tratament facial COCOON (90 min)"
                            , serviceDescr = Just "Un tratament clinic de hidratare, care creează un efect de răcire asupra pielii pentru a revitaliza, hidrata și calma. Efectul său termo-înviorător asupra pielii îl face un tratament remarcabil de revitalizare, în special pentru reducerea roșeață. ALGOMASK+ oferă strălucire instantanee și hidratare de lungă durată."
                            , serviceDuration = duration "00:90"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s23
                    , offerName = "Preț"
                    , offerPrice = 90
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s23
                    , offerName = "Pachet"
                    , offerPrice = 400
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 sesiuni"
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
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s23
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s23
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s23
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    s24 <- insert $ Service { serviceName = "Tratamentul acneei"
                            , servicePublished = True
                            , serviceOverview = Just "Tratamentul acneei (120 min)"
                            , serviceDescr = Just "Aceasta este o modalitate foarte inovatoare și eficientă de a trata afecțiunile acneice care nu au răspuns la alte tratamente și a produs multe rezultate remarcabile. Peroxidul de uree, alfa hidroxiacizii și un element special anti-androgenic sunt încorporate în formula de normalizare Rejuvi."
                            , serviceDuration = duration "02:00"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s24
                    , offerName = "Preț"
                    , offerPrice = 95
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s24
                    , offerName = "Pachet"
                    , offerPrice = 600
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/7 sesiuni"
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
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s24
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s24
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    s25 <- insert $ Service { serviceName = "Tratament mască cu aur de 24k"
                            , servicePublished = True
                            , serviceOverview = Just "Tratament de albire cu aur de 24k"
                            , serviceDescr = Just "<p>Un tratament facial anti-îmbătrânire de lux. Această formulă activă de hidratare anti-îmbătrânire combină puterea vitaminelor pure, extractelor de plante și aurului de 24 de karate. Aceste ingrediente contribuie eficient la stimularea producției de colagen. Ele formează o barieră de protecție continuă pentru a imita efectele intervenției chirurgicale.</p><p>Masca se potrivește ca o „a doua piele” și se adaptează perfect contururilor feței. Oferă hidratare maximă, întărește bariera naturală de protecție a pielii și întinerește pielea sensibilă pentru a reduce semnele îmbătrânirii.</p>"
                            , serviceDuration = duration "01:15"
                            , serviceGroup = Just s2
                            }

    o251 <- insert $ Offer { offerService = s25
                           , offerName = "Preț"
                           , offerPrice = 100
                           , offerPrefix = Nothing
                           , offerSuffix = Nothing
                           , offerDescr = Nothing
                           }

    insert_ $ Offer { offerService = s25
                    , offerName = "Pachet"
                    , offerPrice = 460
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 sesiuni"
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
                       , roleName = "Cosmetician"
                       , roleDuration = 60 * (1 * 60 + 15)
                       , roleRating = Just 5
                       }

    r925 <- insert role925

    insert_ $ Role { roleStaff = e10
                   , roleService = s25
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s25
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s25
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    s3 <- insert $ Service { serviceName = "Tratamente faciale avansate"
                           , servicePublished = True
                           , serviceOverview = Just "Tratamente faciale avansate"
                           , serviceDescr = Just "Tratamente faciale avansate"
                           , serviceDuration = duration "01:10"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s3
                        , thumbnailPhoto = $(embedFile "static/img/advanced-facial-treatments.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s31 <- insert $ Service { serviceName = "Peeling facial cu lapte"
                            , servicePublished = True
                            , serviceOverview = Just "Peeling facial cu lapte (90 min)"
                            , serviceDescr = Just $ Textarea [st|
<p>
Milk Peel include extract natural de lapte acru (acid lactic), papaina (enzima papaya), acid salicilic (din plante naturale) si un controlor special de penetrare. Milk Peel oferă un proces sigur de exfoliere a pielii, fără efectele secundare pe care le pot provoca peelingurile cu fenol și TCA.
</p>
<p>
Formula Milk Peeling îndeplinește următoarele funcții:
</p>
<ul>
  <li>Generează o exfoliere maximă, dar blândă a pielii.</li>
  <li>Stimulează proliferarea fibroblastelor pentru a crește colagenul și elastina dermică.</li>
  <li>Normalizează celulele și țesuturile pielii.</li>
</ul>
<p>
Ca rezultat, peelingul cu lapte este un proces puternic de refacere a pielii cu efecte semnificative asupra pielii. Are mai multe beneficii pentru piele, cum ar fi:
</p>
<ol>
  <li>Elimina ridurile si liniile fine vizibile pe piele.</li>
  <li>Reducerea urmelor de cosuri sau acnee.</li>
  <li>Reduce cicatricile.</li>
  <li>Netezirea golurilor din piele.</li>
  <li>Reducerea vergeturilor și a vergeturilor din naștere.</li>
  <li>Lustruiește delicat pielea pentru reînnoirea feței.</li>
</ol>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s3
                            }

    insert_ $ Offer { offerService = s31
                    , offerName = "Preț"
                    , offerPrice = 330
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e8
                   , roleService = s31
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s31
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s31
                        , thumbnailPhoto = $(embedFile "static/img/milk-peel.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s32 <- insert $ Service { serviceName = "Derma roller"
                            , servicePublished = True
                            , serviceOverview = Just "Ajută-ți pielea să se reînnoiască"
                            , serviceDescr = Just $ Textarea [st|
<h4>Beneficii:</h4>
<ul>
  <li>Elimină și tratează vergeturile, cicatricile de acnee și ridurile.</li>
  <li>Anti îmbătrânire.</li>
  <li>Tratament pentru căderea părului sau refacerea părului.</li>
  <li>Tratamentul celulitei și reducerea celulitei.</li>
  <li>Înlocuiți colagenul, vă puteți ajuta pielea să se reînnoiască și să se repare la nivel cosmetic.</li>
</ul>
<h4>Deci, ce este un Derma Roller?</h4>
<p>
Scientia Derma Roller este un dispozitiv incredibil care crește în mod natural nivelul de colagen și elastină din piele. Vergeturile, ridurile, cicatricile și pielea neuniformă, cu sâmburi, toate sunt cauzate de lipsa de colagen. Deci, folosind un DermaRoller pentru a înlocui colagenul, vă puteți ajuta pielea să se reînnoiască și să se repare la nivel cosmetic.
</p>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s3
                            }

    o321 <- insert $ Offer { offerService = s32
                           , offerName = "Preț"
                           , offerPrice = 330
                           , offerPrefix = Nothing
                           , offerSuffix = Nothing
                           , offerDescr = Nothing
                           }

    insert_ $ Role { roleStaff = e7
                   , roleService = s32
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s32
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s32
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s32
                        , thumbnailPhoto = $(embedFile "static/img/derma-roller.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s33 <- insert $ Service { serviceName = "Microdermabraziune"
                            , servicePublished = True
                            , serviceOverview = Just "Peeling prin microdermabraziune"
                            , serviceDescr = Just $ Textarea [st|
<p>
O procedură nedureroasă, ajută la îndepărtarea cicatricilor de acnee, a porilor dilatați, a liniilor faciale, a ridurilor, a punctelor negre, precum și a daunelor solare etc. Această tehnică ajută, de asemenea, la îngroșarea colagenului, rezultând un ten cu aspect mai tânăr. Colagenul este o proteină care se găsește în pielea ta, care este din abundență atunci când ești copil și oferă pielii un aspect strâns, neted. Producția de colagen scade odată cu vârsta, ceea ce duce la o piele mai laxă și neuniformă.
</p>
|]
                            , serviceDuration = duration "00:45"
                            , serviceGroup = Just s3
                            }

    insert_ $ Offer { offerService = s33
                    , offerName = "Preț"
                    , offerPrice = 70
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e5
                   , roleService = s33
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s33
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s33
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s33
                        , thumbnailPhoto = $(embedFile "static/img/micro-dermabrasion.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s34 <- insert $ Service { serviceName = "Albirea pistruilor"
                            , servicePublished = True
                            , serviceOverview = Just "Albirea pistruilor (120 min)"
                            , serviceDescr = Just $ Textarea [st|
<p>Acest tratament realizează un efect sinergic pentru a îmbunătăți tenul, problemele de pigmentare precum și cearcănele.</p>
|]
                            , serviceDuration = duration "02:00"
                            , serviceGroup = Just s3
                            }

    insert_ $ Offer { offerService = s34
                    , offerName = "Preț"
                    , offerPrice = 95
                    , offerPrefix = Nothing
                    , offerSuffix = Just "ROL"
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s34
                    , offerName = "Pachet"
                    , offerPrice = 600
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/7 sesiuni"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s34
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s34
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s34
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s34
                        , thumbnailPhoto = $(embedFile "static/img/freckle-bleaching.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s4 <- insert $ Service { serviceName = "Tratamente anti-îmbătrânire"
                           , servicePublished = True
                           , serviceOverview = Just "Tratamente anti-îmbătrânire"
                           , serviceDescr = Just "Tratamente anti-îmbătrânire"
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
                            , serviceOverview = Just "Sea C Spa (100% vitamina C)"
                            , serviceDescr = Just $ Textarea [st|
<p>
Un tratament anti-îmbătrânire puternic conceput pentru a întârzia semnele vizibile ale îmbătrânirii și pentru a energiza pielea.
</p>
<p>
Formulat cu ingrediente marine și vegetale (concentrat de vitamina C, plasturi de alge Biomatrix și nămol termic organic).
</p>
<p>
Acest tratament este excepțional atât înainte, cât și după expunerea la soare. Ideal pentru persoanele care locuiesc în zonele urbane cu niveluri ridicate de poluare.
</p>
<h4>Beneficii:</h4>
<p>
Acest tratament reduce aspectul liniilor fine și al ridurilor. Uniformizează tenul și luminează pielea, redându-i astfel aspectul tineresc.
</p>
|]
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s41
                    , offerName = "Preț"
                    , offerPrice = 95
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s41
                    , offerName = "Pachet"
                    , offerPrice = 430
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 sesiuni"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e2
                   , roleService = s41
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s41
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s41
                        , thumbnailPhoto = $(embedFile "static/img/sea-C-spa.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s42 <- insert $ Service { serviceName = "Tratament facial cu Botinol"
                            , servicePublished = True
                            , serviceOverview = Just "Tratament facial cu Botinol (150 min)"
                            , serviceDescr = Just $ Textarea [st|
<p>
Un tratament electiv foarte avansat, formulat pentru a hidrata, regenera și reduce aspectul liniilor fine și ridurilor.
</p>
<p>
Această sesiune de tratament relaxantă oferă texturi plăcute și esențe rafinate, oferind o senzație generală de bine. Un tratament ideal pentru persoanele hotărâte să ascundă semnele îmbătrânirii.
</p>
<h4>Beneficii:</h4>
<p>
După doar un tratament, liniile de expresie par relaxate. Ridurile fine și ridurile sunt vizibil reduse. Pielea pare vizibil mai tânără.
</p>
|]
                            , serviceDuration = duration "02:30"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s42
                    , offerName = "Preț"
                    , offerPrice = 170
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s42
                    , offerName = "Pachet"
                    , offerPrice = 780
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 sesiuni"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e1
                   , roleService = s42
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s42
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s42
                        , thumbnailPhoto = $(embedFile "static/img/botinol-facial.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s43 <- insert $ Service { serviceName = "Colagen 90-II"
                            , servicePublished = True
                            , serviceOverview = Just "Colagen 90-II (150 min)"
                            , serviceDescr = Just $ Textarea [st|
<p>
Această cremă hidratantă anti-îmbătrânire asigură hidratare intensivă, regenerare celulară și reînnoire epidermică. Odată cu vârsta, procesul de reînnoire a celulelor este întârziat. Contururile feței își pierd definiția și pielea pare să se lase.
</p>
<p>
Collagen 90-II este un tratament anti-îmbătrânire exclusiv de la G.M. Collin Skin Care care combină o foaie de colagen nativ pur cu ingrediente selectate, pentru a oferi o hidratare intensă, fermitate pielii și reducerea aspectului de îmbătrânire.
</p>
<p>
Collagen 90-II este un tratament anti-îmbătrânire de reînnoire a pielii foarte respectat și căutat, care hidratează, netezește și tonifică ridurile pentru a combate semnele vizibile ale îmbătrânirii.
</p>
<h4>Beneficii:</h4>
<p>
Acest tratament îmbunătățește tenul general reducând la minimum aspectul liniilor fine și al ridurilor. Reface hidratarea, lasand pielea bine hidratata si stralucitoare. Recomandat pentru toate tipurile de piele
</p>
|]
                            , serviceDuration = duration "02:30"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s43
                    , offerName = "Preț"
                    , offerPrice = 160
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s43
                    , offerName = "Pachet"
                    , offerPrice = 730
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 sesiuni"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e10
                   , roleService = s43
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s43
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s43
                        , thumbnailPhoto = $(embedFile "static/img/collagen-90-II.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s44 <- insert $ Service { serviceName = "Ionizare faciala"
                            , servicePublished = True
                            , serviceOverview = Just "Electrostimulare facială"
                            , serviceDescr = Just $ Textarea [st|
<p>
Promovează producerea de colagen, strânge și fermește țesuturile pielii, hidratează și întinerește epiderma, tratează acneea și liniile fine. Mai puține riduri în doar 15 minute!
</p>
|]
                            , serviceDuration = duration "00:15"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s44
                    , offerName = "Preț"
                    , offerPrice = 180
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s44
                   , roleName = "Cosmetician"
                   , roleDuration = 60 * (0 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s44
                        , thumbnailPhoto = $(embedFile "static/img/electro-ionization.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s5 <- insert $ Service { serviceName = "Tratamente pentru ochi"
                           , servicePublished = True
                           , serviceOverview = Just "Centru de tratament pentru ochi"
                           , serviceDescr = Just "Centru de tratament pentru ochi"
                           , serviceDuration = duration "02:00"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s5
                        , thumbnailPhoto = $(embedFile "static/img/eye-treatment-center.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s51 <- insert $ Service { serviceName = "Perm. genelor"
                            , servicePublished = True
                            , serviceOverview = Just "Perming genelor"
                            , serviceDescr = Just $ Textarea [st|
<p>
Tehnica și soluția noastră de permanentizare a genelor au fost rafinate de peste un deceniu pentru a lucra în mod constant și delicat asupra genelor tale.
</p>
<p>
În ciuda numelui lor, permanentele genelor nu sunt permanente. Rezultatele unei permanente de gene durează de obicei 2 până la 3 luni, care este ciclul natural de creștere a genelor. Când genele permanente cad în mod natural, noile fire de păr ale genelor vor crește drept (la fel cum făceau înainte de permanentă).
</p>
<p>
Înainte de permarea genelor, vi se va cere să vă îndepărtați lentilele de contact și machiajul ochilor (demachiant gratuit, soluție pentru lentile și cupe temporare sunt disponibile pentru utilizarea dvs.). În timpul permanentei genelor, care durează aproximativ 1 oră, puteți să vă întindeți. și relaxați-vă la sunetele muzicii noastre liniștitoare de spa. Vă rugăm să vă păstrați ochii închiși și relaxați pe tot parcursul tratamentului pentru a obține cele mai bune rezultate de permanentă a genelor.
</p>
<p>
Similar cu o permanentă, încercați să evitați să vă umeziți genele timp de 4 ore după permanentă. Vă recomandăm să aplicați zilnic balsam de gene pe gene. Gelul de condiționare a genelor și rimelul sunt disponibile la spa, la telefon și prin magazinul nostru online. Utilizarea regulată a balsamului pentru gene va asigura că genele dumneavoastră rămân sănătoase.
</p>
|]
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s51
                    , offerName = "Preț"
                    , offerPrice = 45
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e7
                   , roleService = s51
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s51
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s51
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s51
                        , thumbnailPhoto = $(embedFile "static/img/eyelash-perm.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s52 <- insert $ Service { serviceName = "Extensii de gene"
                            , servicePublished = True
                            , serviceOverview = Just "Extensii de gene"
                            , serviceDescr = Just $ Textarea [st|
<h4>Îngrijire la domiciliu pentru extensiile de gene</h4>
<ol>
  <li>In primele 2 ore, nu lasa apa sa intre in contact cu genele.</li>
  <li>În primele 2 ore, nu aburiți fața, nu folosiți o baie de aburi, înotați sau spălați fața cu apă fierbinte.</li>
  <li>Folosiți doar rimel lavabil și periați ușor vârfurile genelor.</li>
  <li>Nu utilizați rimel pe zona adezivă a genelor cu dispozitiv de îndepărtare a rimelului.</li>
  <li>Nu permați genele.</li>
  <li>Nu folosiți un ondulator de gene deoarece acesta va rupe atât genele, cât și genele naturale.</li>
  <li>Nu vă frecați ochii sau genele.</li>
  <li>Când vă spălați fața, uscați întotdeauna genele după ce le curățați.</li>
</ol>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s52
                    , offerName = "Preț"
                    , offerPrice = 130
                    , offerPrefix = Nothing
                    , offerSuffix = Just " și mai mult"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e6
                   , roleService = s52
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s52
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s52
                        , thumbnailPhoto = $(embedFile "static/img/eyelash-extension.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s53 <- insert $ Service { serviceName = "Tentă de sprâncene și gene"
                            , servicePublished = True
                            , serviceOverview = Just "Tentă de sprâncene și gene"
                            , serviceDescr = Just $ Textarea [st|Tentă de sprâncene și gene|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s53
                    , offerName = "Preț"
                    , offerPrice = 25
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e4
                   , roleService = s53
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s53
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s53
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s53
                        , thumbnailPhoto = $(embedFile "static/img/eyebrow-or-eyelash-tint.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s54 <- insert $ Service { serviceName = "Tratament de întinerirea ochilor"
                            , servicePublished = True
                            , serviceOverview = Just "Tratament pentru întinerirea ochilor"
                            , serviceDescr = Just $ Textarea [st|
<p>
Acesta este proiectat elegant pentru a reduce ușor umflarea și ridurile, precum și cercurile întunecate din jurul ochilor. Este un tratament puternic pentru ochi, care cuprinde o serie de produse Rejuvi, cum ar fi: Gel de reparare a ochilor „i”, Complex de fructe, Complex de vitamina A, Complex de vitamina C, Formula de contur și mască facială etc., pentru a conferi un efect sinergic asupra ochilor. zonă. Rezultate uimitoare pot fi observate imediat după procedură.
</p>
<p>
Tratamentele pot fi efectuate în fiecare săptămână sau la două săptămâni, în funcție de starea pielii. Un tratament complet pentru ochi ar trebui să conțină 4 – 6 aplicații ale tratamentelor individuale, plus un program de îngrijire la domiciliu.
</p>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s54
                    , offerName = "Preț"
                    , offerPrice = 40
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s54
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s54
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s54
                        , thumbnailPhoto = $(embedFile "static/img/eye-rejuvenating-treatment.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s55 <- insert $ Service { serviceName = "Tratament anti-îmbătrânire pentru ochi"
                            , servicePublished = True
                            , serviceOverview = Just "Tratament anti-îmbătrânire pentru ochi"
                            , serviceDescr = Just $ Textarea [st|Tratament pentru ochi anti-îmbătrânire|]
                            , serviceDuration = duration "01:45"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s55
                    , offerName = "Preț"
                    , offerPrice = 40
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e2
                   , roleService = s55
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s55
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s55
                        , thumbnailPhoto = $(embedFile "static/img/anti-aging-eye-treatment.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s6 <- insert $ Service { serviceName = "Masaj corporal"
                           , servicePublished = True
                           , serviceOverview = Just "Masaj pentru corp"
                           , serviceDescr = Just "Masaj de corp"
                           , serviceDuration = duration "02:00"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s6
                        , thumbnailPhoto = $(embedFile "static/img/body-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s61 <- insert $ Service { serviceName = "Masaj suedez"
                            , servicePublished = True
                            , serviceOverview = Just "Masaj terapie suedeză"
                            , serviceDescr = Just $ Textarea [st|
<p>
Terapia de masaj suedez este modalitatea care vine în minte atunci când majoritatea oamenilor se gândesc la masaj. Fiind cel mai cunoscut tip de caroserie efectuat astăzi, unul dintre obiectivele principale ale tehnicii de masaj suedez este relaxarea întregului corp. Acest lucru se realizează prin frecarea mușchilor cu mișcări lungi de alunecare în direcția întoarcerii sângelui la inimă. Dar masajul suedez merge dincolo de relaxare. Masajul suedez este excepțional de benefic pentru creșterea nivelului de oxigen din sânge, scăderea toxinelor musculare, îmbunătățirea circulației și flexibilității în timp ce ușurează tensiunea.
</p>
|]
                            , serviceDuration = duration "01:00"
                            , serviceGroup = Just s6
                            }

    insert_ $ Offer { offerService = s61
                    , offerName = "Preț"
                    , offerPrice = 60
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s61
                   , roleName = "Maseur"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s61
                   , roleName = "Maseur"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s61
                   , roleName = "Maseur"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s61
                        , thumbnailPhoto = $(embedFile "static/img/swedish-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s62 <- insert $ Service { serviceName = "Masaj pe scaun"
                            , servicePublished = True
                            , serviceOverview = Just "Masaj pe scaun"
                            , serviceDescr = Just $ Textarea [st|
<p>
Masajul pe scaun este un masaj de 15-20 de minute care se concentrează pe spate, umeri, gât, brațe și cap. Este conceput pentru a relaxa mușchii și pentru a îmbunătăți flexibilitatea și mișcarea.
</p>
|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s6
                            }

    insert_ $ Offer { offerService = s62
                    , offerName = "Preț"
                    , offerPrice = 60
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e10
                   , roleService = s62
                   , roleName = "Maseur"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s62
                        , thumbnailPhoto = $(embedFile "static/img/chair-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s63 <- insert $ Service { serviceName = "Masaj la picioare"
                            , servicePublished = True
                            , serviceOverview = Just "Masajul picioarelor"
                            , serviceDescr = Just $ Textarea [st|
<p>
Masajul picioarelor este o practică terapeutică care implică manipularea picioarelor și aplicarea unei presiuni asupra unor zone specifice pentru a promova relaxarea, a elibera tensiunea și a îmbunătăți starea generală de bine. Spre deosebire de reflexoterapie, care este o practică specializată care se concentrează asupra punctelor reflexe specifice ale picioarelor care corespund diferitelor organe și sisteme din corp, un masaj al picioarelor implică de obicei o abordare mai generală.
</p>
<p>
În timpul unui masaj al picioarelor, pot fi utilizate diverse tehnici, inclusiv frământare, mângâiere, frecare și aplicarea unei presiuni cu mâinile, degetele sau unelte specializate. Masajul poate viza anumite zone ale picioarelor, cum ar fi arcurile, călcâiele, degetele de la picioare și mingele picioarelor, precum și mușchii și articulațiile din jur.
</p>
<h4>Beneficii:</h4>
<ol>
  <li>Relaxare: Masajul picioarelor poate ajuta la reducerea stresului si induce o stare de relaxare.</li>
  <li>Îmbunătățirea circulației: Tehnicile de masaj utilizate pot îmbunătăți fluxul de sânge către picioare și extremitățile inferioare, promovând o mai bună circulație.</li>
  <li>Ameliorarea durerii: masajele picioarelor pot ajuta la ameliorarea durerilor de picioare, inclusiv afecțiuni precum fasciita plantară sau disconfortul general al picioarelor.</li>
  <li>Reducerea tensiunii musculare: prin țintirea anumitor mușchi ai picioarelor, un masaj poate ajuta la eliberarea tensiunii și la îmbunătățirea flexibilității.</li>
  <li>Bunăstare generală îmbunătățită: se crede că masajele picioarelor au un impact pozitiv asupra fluxului de energie al corpului, contribuind la un sentiment de bunăstare.</li>
</ol>
|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s6
                            }

    insert_ $ Offer { offerService = s63
                    , offerName = "Preț"
                    , offerPrice = 30
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e1
                   , roleService = s63
                   , roleName = "Maseur"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s63
                   , roleName = "Maseur"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s63
                        , thumbnailPhoto = $(embedFile "static/img/foot-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s7 <- insert $ Service { serviceName = "Servicii de machiaj"
                           , servicePublished = True
                           , serviceOverview = Just "Servicii de machiaj"
                           , serviceDescr = Just "Servicii de machiaj"
                           , serviceDuration = duration "03:30"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s7
                        , thumbnailPhoto = $(embedFile "static/img/makeup-services.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s71 <- insert $ Service { serviceName = "Special de nuntă"
                            , servicePublished = True
                            , serviceOverview = Just "Servicii speciale de nuntă"
                            , serviceDescr = Just $ Textarea [st|
<p>
Pachetul include: machiaj de mireasă, up-do, tratament facial și manichiură
</p>
|]
                            , serviceDuration = duration "03:30"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s71
                    , offerName = "Preț"
                    , offerPrice = 200
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s71
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (3 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s71
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (3 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s71
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (3 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s71
                        , thumbnailPhoto = $(embedFile "static/img/wedding-special.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s72 <- insert $ Service { serviceName = "Machiaj de mireasă"
                            , servicePublished = True
                            , serviceOverview = Just "Machiaj de mireasă (45 min)"
                            , serviceDescr = Just $ Textarea [st|Machiaj de mireasă (45 min)|]
                            , serviceDuration = duration "00:45"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s72
                    , offerName = "Preț"
                    , offerPrice = 85
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e4
                   , roleService = s72
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s72
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s72
                        , thumbnailPhoto = $(embedFile "static/img/bridal-make-up.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s73 <- insert $ Service { serviceName = "Machiajul de seară"
                            , servicePublished = True
                            , serviceOverview = Just "Machiajul de seară (30 min)"
                            , serviceDescr = Just $ Textarea [st|Machiajul de seară (30 min)|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s73
                    , offerName = "Preț"
                    , offerPrice = 60
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e5
                   , roleService = s73
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s73
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s73
                        , thumbnailPhoto = $(embedFile "static/img/evening-make-up.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s74 <- insert $ Service { serviceName = "Lecție de machiaj"
                            , servicePublished = True
                            , serviceOverview = Just "Lecție de machiaj (60 min)"
                            , serviceDescr = Just $ Textarea [st|Lecție de machiaj (60 min)|]
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s74
                    , offerName = "Preț"
                    , offerPrice = 100
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e6
                   , roleService = s74
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s74
                   , roleName = "Makeup artist"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s74
                        , thumbnailPhoto = $(embedFile "static/img/make-up-lesson.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s8 <- insert $ Service { serviceName = "Epilare cu ceară"
                           , servicePublished = True
                           , serviceOverview = Just "Epilare cu ceară"
                           , serviceDescr = Just "Epilare cu ceară"
                           , serviceDuration = duration "01:25"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s8
                        , thumbnailPhoto = $(embedFile "static/img/waxing.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s81 <- insert $ Service { serviceName = "Epilarea cu ceară a corpului"
                            , servicePublished = True
                            , serviceOverview = Just "Epilarea cu ceară a corpului"
                            , serviceDescr = Just $ Textarea [st|Epilarea cu ceară a corpului|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s8
                            }

    insert_ $ Offer { offerService = s81
                    , offerName = "Preț"
                    , offerPrice = 50
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e8
                   , roleService = s81
                   , roleName = "Specialist în epilare"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s81
                   , roleName = "Specialist în epilare"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s81
                   , roleName = "Specialist în epilare"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s81
                        , thumbnailPhoto = $(embedFile "static/img/body-waxing.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s82 <- insert $ Service { serviceName = "Epilarea cu ceară a feței"
                            , servicePublished = True
                            , serviceOverview = Just "Epilarea cu ceară a feței"
                            , serviceDescr = Just $ Textarea [st|Epilarea cu ceară a feței|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s8
                            }

    insert_ $ Offer { offerService = s82
                    , offerName = "Preț"
                    , offerPrice = 45
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s82
                   , roleName = "Specialist în epilare"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s82
                   , roleName = "Specialist în epilare"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s82
                        , thumbnailPhoto = $(embedFile "static/img/face-waxing.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s9 <- insert $ Service { serviceName = "Îngrijirea unghiilor"
                           , servicePublished = True
                           , serviceOverview = Just "Servicii de îngrijire a unghiilor"
                           , serviceDescr = Just "Servicii de îngrijire a unghiilor"
                           , serviceDuration = duration "00:45"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s9
                        , thumbnailPhoto = $(embedFile "static/img/nail-care.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s91 <- insert $ Service { serviceName = "Manichiură"
                            , servicePublished = True
                            , serviceOverview = Just "Manichiură"
                            , serviceDescr = Just $ Textarea [st|Manichiură|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s91
                    , offerName = "Preț"
                    , offerPrice = 15
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s91
                   , roleName = "Manichiurista"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s91
                   , roleName = "Manichiurista"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s91
                   , roleName = "Manichiurista"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s91
                        , thumbnailPhoto = $(embedFile "static/img/manicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s92 <- insert $ Service { serviceName = "Manichiură fără așchii"
                            , servicePublished = True
                            , serviceOverview = Just "Manichiură fără cioburi"
                            , serviceDescr = Just $ Textarea [st|Manichiură fără cioburi|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s92
                    , offerName = "Preț"
                    , offerPrice = 32
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e10
                   , roleService = s92
                   , roleName = "Manichiurista"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s92
                   , roleName = "Manichiurista"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s92
                        , thumbnailPhoto = $(embedFile "static/img/no-chip-manicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s93 <- insert $ Service { serviceName = "Pedichiură"
                            , servicePublished = True
                            , serviceOverview = Just "Pedichiură"
                            , serviceDescr = Just $ Textarea [st|Pedichiură|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s93
                    , offerName = "Preț"
                    , offerPrice = 35
                    , offerPrefix = Nothing
                    , offerSuffix = Just "$"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e1
                   , roleService = s93
                   , roleName = "Manichiurista"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s93
                   , roleName = "Manichiurista"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s93
                        , thumbnailPhoto = $(embedFile "static/img/pedicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s94 <- insert $ Service { serviceName = "Pedichiură fără așchii"
                            , servicePublished = True
                            , serviceOverview = Just "Pedichiură fără cioburi"
                            , serviceDescr = Just $ Textarea [st|Pedichiură fără cioburi|]
                            , serviceDuration = duration "00:40"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s94
                    , offerName = "Preț"
                    , offerPrice = 55
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e2
                   , roleService = s94
                   , roleName = "Manichiurista"
                   , roleDuration = 60 * (0 * 60 + 40)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s94
                   , roleName = "Manichiurista"
                   , roleDuration = 60 * (0 * 60 + 40)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s94
                        , thumbnailPhoto = $(embedFile "static/img/no-chip-pedicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s95 <- insert $ Service { serviceName = "Unghii acrilice"
                            , servicePublished = True
                            , serviceOverview = Just "Set complet de unghii acrilice"
                            , serviceDescr = Just $ Textarea [st|Set complet de unghii acrilice|]
                            , serviceDuration = duration "00:25"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s95
                    , offerName = "Preț"
                    , offerPrice = 38
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s95
                   , roleName = "Manichiurista"
                   , roleDuration = 60 * (0 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s95
                   , roleName = "Manichiurista"
                   , roleDuration = 60 * (0 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s95
                        , thumbnailPhoto = $(embedFile "static/img/acrylic-nail-full-set.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s96 <- insert $ Service { serviceName = "Tratamente cu parafină pentru mâini"
                            , servicePublished = True
                            , serviceOverview = Just "Tratamente cu parafină pentru mâini"
                            , serviceDescr = Just $ Textarea [st|Tratamente cu parafină pentru mâini|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s96
                    , offerName = "Preț"
                    , offerPrice = 10
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e4
                   , roleService = s96
                   , roleName = "Estetician"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s96
                   , roleName = "Estetician"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s96
                        , thumbnailPhoto = $(embedFile "static/img/hand-paraffin-dip.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s97 <- insert $ Service { serviceName = "Schimbarea ojei"
                            , servicePublished = True
                            , serviceOverview = Just "Schimbarea lacului de unghii"
                            , serviceDescr = Just $ Textarea [st|Schimbarea lacului de unghii|]
                            , serviceDuration = duration "00:15"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s97
                    , offerName = "Preț"
                    , offerPrice = 5
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e6
                   , roleService = s97
                   , roleName = "Manichiurista"
                   , roleDuration = 60 * (0 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s97
                   , roleName = "Manichiurista"
                   , roleDuration = 60 * (0 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s97
                        , thumbnailPhoto = $(embedFile "static/img/polish-change.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s10 <- insert $ Service { serviceName = "Modelarea corpului și fitness"
                            , servicePublished = True
                            , serviceOverview = Just "Modelarea corpului și fitness"
                            , serviceDescr = Just "Modelarea corpului și fitness"
                            , serviceDuration = duration "01:15"
                            , serviceGroup = Nothing
                            }

    insert_ $ Thumbnail { thumbnailService = s10
                        , thumbnailPhoto = $(embedFile "static/img/body-shaping-and-fitness.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s101 <- insert $ Service { serviceName = "Modelarea corpului"
                             , servicePublished = True
                             , serviceOverview = Just "Modelarea corpului"
                             , serviceDescr = Just $ Textarea [st|
<p>Modelarea corpului: Abdomen și talie, șolduri și coapse, picioare și brațe</p>
|]
                             , serviceDuration = duration "01:15"
                             , serviceGroup = Just s10
                             }

    insert_ $ Offer { offerService = s101
                    , offerName = "Preț"
                    , offerPrice = 350
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e7
                   , roleService = s101
                   , roleName = "Estetician"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s101
                   , roleName = "Estetician"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s101
                        , thumbnailPhoto = $(embedFile "static/img/body-shaping.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s102 <- insert $ Service { serviceName = "Modelarea feței"
                             , servicePublished = True
                             , serviceOverview = Just "Modelarea feței"
                             , serviceDescr = Just $ Textarea [st|Modelarea feței|]
                             , serviceDuration = duration "00:45"
                             , serviceGroup = Just s10
                             }

    insert_ $ Offer { offerService = s102
                    , offerName = "Preț"
                    , offerPrice = 300
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e8
                   , roleService = s102
                   , roleName = "Estetician"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s102
                   , roleName = "Estetician"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s102
                        , thumbnailPhoto = $(embedFile "static/img/face-shaping.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }


    pass6 <- liftIO $ makePassword "grigorescudg" 17
    c1 <- insert $ User { userName = "grigorescudg"
                        , userPassword = decodeUtf8 pass6
                        , userAdmin = False
                        , userFullName = Just "Grigorescu Daniela Ghiorghe"
                        , userEmail = Just "grigorescudg@mail.org"
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


    pass7 <- liftIO $ makePassword "vasilescuam" 17
    c2 <- insert $ User { userName = "vasilescuam"
                        , userPassword = decodeUtf8 pass7
                        , userAdmin = False
                        , userFullName = Just "Vasilescu Anton Mihai"
                        , userEmail = Just "vasilescuam@mail.org"
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
