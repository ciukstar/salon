{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Demo.DemoDataRU (populateRU) where

import ClassyPrelude.Yesod (ReaderT)
import Control.Monad (forM_)
import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (st)
import qualified Data.ByteString.Base64 as B64 (decode)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar
    ( DayOfWeek (Saturday, Sunday), DayPeriod (periodFirstDay, periodLastDay)
    , addDays, dayOfWeek, toGregorian
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
    , Thumbnail (Thumbnail, thumbnailService, thumbnailPhoto, thumbnailMime, thumbnailAttribution)
    , Offer
      ( Offer, offerName, offerPrice, offerPrefix
      , offerSuffix, offerDescr, offerService
      )
    , EmplStatus (EmplStatusAvailable, EmplStatusUnavailable)
    , Staff (Staff, staffName, staffStatus, staffPhone, staffMobile, staffEmail, staffUser)
    , StaffPhoto (StaffPhoto, staffPhotoPhoto, staffPhotoMime, staffPhotoStaff)
    , Role (Role, roleStaff, roleService, roleName, roleDuration, roleRating)
    , BookStatus (BookStatusRequest, BookStatusCancelled, BookStatusApproved)
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


populateRU :: MonadIO m => ReaderT SqlBackend m ()
populateRU = do

    (now,today,time,month) <- liftIO $ do
        t <- getCurrentTime
        return ( t
               , utctDay t
               , timeToTimeOfDay (utctDayTime t)
               , let (y,m,_) = toGregorian (utctDay t) in YearMonth y m
               )

    let business = Business { businessName = "Салон"
                            , businessFullName = Just "ООО Салон"
                            , businessCurrency = "RUB"
                            , businessAddr = "Россия, г. Москва, проезд Воскресенские Ворота, 1А, Москва, 109012"
                            , businessTzo = TimeZone 180 False "MSK"
                            , businessTz = "Europe/Moscow"
                            , businessPhone = Just "+7 (958) 759-52-25"
                            , businessMobile = Just "940(8537)418-74-67"
                            , businessEmail = Just "salon@mail.ru"
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
<h2 style="color:gray">Наша миссия
<p>Миссия <i>Салона</i> проста: предложить каждому человеку, входящему в его двери, особую среду, где он может ухаживать и баловать себя, одновременно улучшая свой личный имидж и привнося ощущение благополучия в свою жизнь.
<h2 style="color:gray">Наш этос
<p>Любой человек, пришедший в наш салон, уникален. Мы относимся к каждому уникальному клиенту с учетом его личных потребностей. Мы гордимся тем, что предлагаем услуги, которые ожидает клиент, и лечение, которое ему требуется, и мы будем постоянно переоценивать их требования в зависимости от их образа жизни и тела.
<p>Мы никогда не будем предлагать ненужные процедуры и ставим каждого клиента в приоритет. Именно поэтому мы можем с гордостью сказать, что за эти годы у нас накопилась лояльная клиентская база. Мы учимся у них, заботясь о них, и стараемся идти в ногу с последними тенденциями и доступными методами лечения, чтобы всегда удовлетворять потребности наших уважаемых клиентов и любых будущих посетителей.
<h2 style="color:gray">Наши цели
<p>Мы продолжим предлагать новейшие методы лечения, самые инновационные методы, используя лучшие продукты на рынке. И все это в элегантной, чистой и гостеприимной обстановке под руководством обученных, профессиональных и дружелюбных терапевтов. Мы постараемся донести нашу идею о том, что каждый имеет право чувствовать себя хорошо!
|]
                      }

    insert_ $ ContactUs { contactUsBusiness = b
                        , contactUsHtml = [shamlet|
<section>
  <h3 style="color:gray">Звоните нам
  <dl>
    <dt>
      <i>телефон
    <dd>
      $maybe phone <- businessPhone business
        #{phone}
    <dt>
      <i>мобильный
    <dd>
      $maybe mobile <- businessMobile business
        #{mobile}
<section>
  <h3 style="color:gray">Напишите нам
  <dl>
    <dt>
      <i>Электронная почта
    <dd>
      $maybe email <- businessEmail business
        #{email}
<section>
  <h3 style="color:gray">Приходите к нам
|]
                        , contactUsShowAddress = True
                        , contactUsShowSchedule = True
                        , contactUsShowMap = True
                        , contactUsLongitude = Just 37.618423
                        , contactUsLatitude = Just 55.751244
                        }
                             
    pass <- liftIO $ makePassword "root" 17
    insert_ $ User { userName = "root"
                   , userPassword = decodeUtf8 pass
                   , userAdmin = True
                   , userFullName = Just "Админов Админ Админовичь"
                   , userEmail = Just "adminovaa@mail.ru"
                   }

    pass1 <- liftIO $ makePassword "ivanoviv" 17
    let user1 = User { userName = "ivanoviv"
                     , userPassword = decodeUtf8 pass1
                     , userAdmin = False
                     , userFullName = Just "Иванов Игорь Васильевич"
                     , userEmail = Just "ivanoviv@mail.ru"
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

    pass2 <- liftIO $ makePassword "bulanovalm" 17
    let user2 = User { userName = "bulanovalm"
                     , userPassword = decodeUtf8 pass2
                     , userAdmin = False
                     , userFullName = Just "Буланова Любовь Михайловна"
                     , userEmail = Just "bulanovalm@mail.ru"
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

    pass3 <- liftIO $ makePassword "petrovia" 17
    let user3 = User { userName = "petrovia"
                     , userPassword = decodeUtf8 pass3
                     , userAdmin = False
                     , userFullName = Just "Петров Иван Александрович"
                     , userEmail = Just "petrovia@mail.ru"
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

    pass4 <- liftIO $ makePassword "lebedevamv" 17
    let user4 = User { userName = "lebedevamv"
                     , userPassword = decodeUtf8 pass4
                     , userAdmin = False
                     , userFullName = Just "Лебедева Марина Викторовна"
                     , userEmail = Just "lebedevamv@mail.ru"
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

    pass5 <- liftIO $ makePassword "smirnovav" 17
    let user5 = User { userName = "smirnovav"
                     , userPassword = decodeUtf8 pass5
                     , userAdmin = False
                     , userFullName = Just "Смирнов Андрей Васильевич"
                     , userEmail = Just "smirnovav@mail.ru"
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

    e6 <- insert $ Staff { staffName = "Иванов Алексей Васильевич"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "ivanovav@mail.ru"
                         , staffUser = Nothing
                         }

    case B64.decode man04 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e6
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e7 <- insert $ Staff { staffName = "Сергеева Александра Владимировна"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "sergeevaav@mail.ru"
                         , staffUser = Nothing
                         }

    case B64.decode woman03 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e7
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e8 <- insert $ Staff { staffName = "Степанова Татьяна Николаевна"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "stepanovatn@mail.ru"
                         , staffUser = Nothing
                         }

    case B64.decode woman04 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e8
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }
    let empl9 = Staff { staffName = "Кузнецов Артем Сергеевич"
                      , staffStatus = EmplStatusAvailable
                      , staffPhone = businessPhone business
                      , staffMobile = businessMobile business
                      , staffEmail = Just "kuznetsovas@mail.ru"
                      , staffUser = Nothing
                      }

    e9 <- insert empl9

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

    case B64.decode man05 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e9
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e10 <- insert $ Staff { staffName = "Попов Дмитрий Александрович"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "popovda@mail.ru"
                         , staffUser = Nothing
                         }

    case B64.decode man06 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e10
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    pass11 <- liftIO $ makePassword "baranovaag" 17
    let user11 = User { userName = "baranovaag"
                      , userPassword = decodeUtf8 pass11
                      , userAdmin = False
                      , userFullName = Just "Баранова Алиса Григорьевна"
                      , userEmail = Just "baranovaag@mail.ru"
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

    s1 <- insert $ Service { serviceName = "Уход за волосами"
                           , serviceOverview = Just "Услуги по уходу за волосами"
                           , servicePublished = True
                           , serviceDescr = Just $ Textarea [st|
<p>
Всегда уникальные и нестандартные, наши специалисты по уходу за волосами прошли тщательную подготовку, чтобы предоставлять услуги дизайнерской стрижки и укладки, адаптированные к потребностям каждого клиента. Как салон, мы поддерживаем нашу команду в их усилиях по совершенствованию индивидуальных техник и даем нашим стилистам свободу полностью выражать и исследовать свой творческий потенциал. Это, в свою очередь, дает нашим гостям возможность каждый раз наслаждаться индивидуальным обслуживанием. Любите ли вы современный образ или классический крой, подчеркивающий ваш фирменный стиль – с нами варианты безграничны.
</p>
|]
                           , serviceDuration = duration "01:30"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s1
                        , thumbnailPhoto = $(embedFile "static/img/hair-care.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s11 <- insert $ Service { serviceName = "Мужские стрижки"
                            , servicePublished = True
                            , serviceOverview = Just "Стрижки мужские"
                            , serviceDescr = Just "Стрижки мужские"
                            , serviceDuration = duration "01:00"
                            , serviceGroup = Just s1
                            }

    let role111 =  Role { roleStaff = e1
                        , roleService = s11
                        , roleName = "Парикмахер"
                        , roleDuration = 60 * (1 * 60 + 0)
                        , roleRating = Just 5
                        }

    r111 <- insert role111

    insert_ $ Role { roleStaff = e2
                   , roleService = s11
                   , roleName = "Парикмахер"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    o111 <- insert $ Offer { offerService = s11
                           , offerName = "Цена"
                           , offerPrice = 800
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

    s12 <- insert $ Service { serviceName = "Женские стрижки (выше плеч)"
                            , servicePublished = True
                            , serviceOverview = Just "Стрижки выше плеч женские"
                            , serviceDescr = Just "Стрижки выше плеч женские"
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s1
                            }

    insert_ $ Offer { offerService = s12
                    , offerName = "Цена"
                    , offerPrice = 2800
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
                   , roleName = "Парикмахер"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 3
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s12
                   , roleName = "Парикмахер"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s12
                   , roleName = "Парикмахер"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    s13 <- insert $ Service { serviceName = "Женские стрижки (ниже плеч)"
                            , servicePublished = True
                            , serviceOverview = Just "Стрижки ниже плеч женские"
                            , serviceDescr = Just "Стрижки ниже плеч женские"
                            , serviceDuration = duration "01:35"
                            , serviceGroup = Just s1
                            }

    o131 <- insert $ Offer { offerService = s13
                           , offerName = "Цена"
                           , offerPrice = 3500
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
                        , roleName = "Стилист"
                        , roleDuration = 60 * (1 * 60 + 35)
                        , roleRating = Just 5
                        }

    r1311 <- insert role1311

    insert_ $ Role { roleStaff = e3
                   , roleService = s13
                   , roleName = "Ассистент стилиста"
                   , roleDuration = 60 * (1 * 60 + 35)
                   , roleRating = Just 4
                   }

    s14 <- insert $ Service { serviceName = "Детские стрижки"
                            , servicePublished = True
                            , serviceOverview = Just "Стрижки для детей"
                            , serviceDescr = Just "Стрижки для детей"
                            , serviceDuration = duration "01:20"
                            , serviceGroup = Just s1
                            }

    o141 <- insert $ Offer { offerService = s14
                           , offerName = "Цена"
                           , offerPrice = 1600
                           , offerPrefix = Nothing
                           , offerSuffix = Just "-2 000 ₽ (в зависимости от длины волос)"
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
                   , roleName = "Младший стилист"
                   , roleDuration = 60 * (1 * 60 + 20)
                   , roleRating = Just 5
                   }

    s15 <- insert $ Service { serviceName = "Химические услуги"
                            , servicePublished = True
                            , serviceOverview = Just "Химические услуги"
                            , serviceDescr = Just "<p>Наши химические услуги удовлетворяют широкий спектр потребностей по уходу за волосами. Наши разглаживающие процедуры борются с вьющимися волосами, улучшают управляемость и обеспечивают долгосрочные результаты, о которых вы всегда мечтали. Вы можете наслаждаться гладкими и шелковистыми локонами с помощью нашей услуги кератина, а также восстанавливать поврежденные волосы, восполняя потерянный белок. Если вам нужны дополнительные волны, завитки и объем, наши профессиональные услуги по химической завивке позволят вам добиться желаемой текстуры. Мы используем инновационные продукты и методы для создания четких локонов, которые вам обязательно понравятся.</p>"
                            , serviceDuration = duration "02:00"
                            , serviceGroup = Just s1
                            }

    insert_ $ Thumbnail { thumbnailService = s15
                        , thumbnailPhoto = $(embedFile "static/img/chemical-services.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s151 <- insert $ Service { serviceName = "Кондиционирование"
                             , servicePublished = True
                             , serviceOverview = Just "Услуги кондиционирования"
                             , serviceDescr = Just "Кондиционирование"
                             , serviceDuration = duration "01:35"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s151
                        , thumbnailPhoto = $(embedFile "static/img/conditioning.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s1511 <- insert $ Service { serviceName = "Кондиционер после химической завивки"
                              , servicePublished = True
                              , serviceOverview = Just "Кондиционер после химической завивки"
                              , serviceDescr = Just "Кондиционер после химической завивки"
                              , serviceDuration = duration "01:25"
                              , serviceGroup = Just s151
                              }

    insert_ $ Offer { offerService = s1511
                    , offerName = "Цена"
                    , offerPrice = 9900
                    , offerPrefix = Nothing
                    , offerSuffix = Just " и выше"
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
                   , roleName = "Стилист"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 4
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s1511
                   , roleName = "Стилист"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 4
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1511
                   , roleName = "Стилист"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 4
                   }

    s1512 <- insert $ Service { serviceName = "Кондиционер перед химической завивкой"
                              , servicePublished = True
                              , serviceOverview = Just "Кондиционер перед химической завивкой"
                              , serviceDescr = Just "Кондиционер перед химической завивкой"
                              , serviceDuration = duration "01:15"
                              , serviceGroup = Just s151
                              }

    insert_ $ Offer { offerService = s1512
                    , offerName = "Цена"
                    , offerPrice = 11000
                    , offerPrefix = Nothing
                    , offerSuffix = Just " и выше"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e4
                   , roleService = s1512
                   , roleName = "Стилист"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1512
                   , roleName = "Стилист"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s1512
                   , roleName = "Стилист"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s1512
                        , thumbnailPhoto = $(embedFile "static/img/before-perm-conditioner.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s152 <- insert $ Service { serviceName = "Мелирование и цвет"
                             , servicePublished = True
                             , serviceOverview = Just "Мелирование и цвет волос"
                             , serviceDescr = Just "Мелирование и цвет волос"
                             , serviceDuration = duration "01:10"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s152
                        , thumbnailPhoto = $(embedFile "static/img/highlights-and-color.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s1521 <- insert $ Service { serviceName = "Полное"
                              , servicePublished = True
                              , serviceOverview = Just "Полное окрашивание"
                              , serviceDescr = Just "Полное окрашивание волос"
                              , serviceDuration = duration "01:00"
                              , serviceGroup = Just s152
                              }

    insert_ $ Offer { offerService = s1521
                    , offerName = "Цена"
                    , offerPrice = 13000
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
                   , roleName = "Колорист"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s1521
                   , roleName = "Колорист"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e10
                   , roleService = s1521
                   , roleName = "Колорист"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    s1522 <- insert $ Service { serviceName = "Частичное"
                              , servicePublished = True
                              , serviceOverview = Just "Частичное окрашивание волос"
                              , serviceDescr = Just "Частичное окрашивание волос"
                              , serviceDuration = duration "01:15"
                              , serviceGroup = Just s152
                              }

    insert_ $ Offer { offerService = s1522
                    , offerName = "Цена"
                    , offerPrice = 6800
                    , offerPrefix = Nothing
                    , offerSuffix = Just " и выше"
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
                   , roleName = "Колорист"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s1522
                   , roleName = "Колорист"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    s1523 <- insert $ Service { serviceName = "Перманентный цвет"
                              , servicePublished = True
                              , serviceOverview = Just "Перманентная окраска"
                              , serviceDescr = Just "Перманентная окраска"
                              , serviceDuration = duration "01:45"
                              , serviceGroup = Just s152
                              }

    insert_ $ Offer { offerService = s1523
                    , offerName = "Цена"
                    , offerPrice = 6800
                    , offerPrefix = Nothing
                    , offerSuffix = Just " и выше"
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
                   , roleName = "Колорист"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e10
                   , roleService = s1523
                   , roleName = "Колорист"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s1523
                   , roleName = "Колорист"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    s153 <- insert $ Service { serviceName = "Завивка"
                             , servicePublished = True
                             , serviceOverview = Just "Перманентная завивка"
                             , serviceDescr = Just "Отличный способ придать себе новый облик"
                             , serviceDuration = duration "00:45"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s153
                        , thumbnailPhoto = $(embedFile "static/img/perm.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s1531 <- insert $ Service { serviceName = "Полная завивка"
                              , servicePublished = True
                              , serviceOverview = Just "Полная завивка"
                              , serviceDescr = Just "Полная завивка"
                              , serviceDuration = duration "00:35"
                              , serviceGroup = Just s153
                              }

    insert_ $ Offer { offerService = s1531
                    , offerName = "Цена"
                    , offerPrice = 7900
                    , offerPrefix = Nothing
                    , offerSuffix = Just " и выше"
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
                   , roleName = "Стилист"
                   , roleDuration = 60 * (0 * 60 + 35)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s1531
                   , roleName = "Стилист"
                   , roleDuration = 60 * (0 * 60 + 35)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s1531
                   , roleName = "Стилист"
                   , roleDuration = 60 * (0 * 60 + 35)
                   , roleRating = Just 5
                   }

    s1532 <- insert $ Service { serviceName = "Кислотная химическая завивка"
                              , servicePublished = True
                              , serviceOverview = Just "Кислотная химическая завивка"
                              , serviceDescr = Just "Кислотная химическая завивка"
                              , serviceDuration = duration "01:25"
                              , serviceGroup = Just s153
                              }

    insert_ $ Offer { offerService = s1532
                    , offerName = "Цена"
                    , offerPrice = 8900
                    , offerPrefix = Nothing
                    , offerSuffix = Just " и выше"
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
                   , roleName = "Стилист"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s1532
                   , roleName = "Стилист"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1532
                   , roleName = "Стилист"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    s1533 <- insert $ Service { serviceName = "Японское выпрямление волос"
                              , servicePublished = True
                              , serviceOverview = Just "Японское выпрямление волос"
                              , serviceDescr = Just "Японское выпрямление волос"
                              , serviceDuration = duration "01:25"
                              , serviceGroup = Just s153
                              }

    insert_ $ Offer { offerService = s1533
                    , offerName = "Цена"
                    , offerPrice = 25000
                    , offerPrefix = Nothing
                    , offerSuffix = Just " и выше"
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
                   , roleName = "Стилист"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s1533
                   , roleName = "Стилист"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s1533
                   , roleName = "Стилист"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    s2 <- insert $ Service { serviceName = "Процедуры для лица"
                           , servicePublished = True
                           , serviceOverview = Just "Процедуры по уходу за лицом"
                           , serviceDescr = Just $ Textarea [st|
<p>
Ваше лицо — это выразительное полотно, отражающее опыт и эмоции. В одном из лучших салонов в нашей палитре представлены питательные процедуры, которые подчеркивают красоту, молодость и цвет вашего тела. Перед любым уходом за лицом наш профессиональный косметолог проведет закрепление и начнет работу. Вы не поверите, разница!
</p>
<p>
Все процедуры по лицу включают в себя оформление бровей.
</p>
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

    s21 <- insert $ Service { serviceName = "Базовый уход за лицом"
                            , servicePublished = True
                            , serviceOverview = Just "Базовый уход за лицом (60 мин)"
                            , serviceDescr = Just "Глубокое очищение, отшелушивание паровой обработкой с последующей экстракцией, затем оформление бровей; антистрессовый массаж лица, шеи и плеч. Индивидуальная маска плюс регулярный уход за глазами с последующим нанесением увлажняющего/солнцезащитного крема. Эта расслабляющая, но серьезная очищающая процедура подарит вам чистый, свежий и сияющий цвет лица."
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s21
                    , offerName = "Цена"
                    , offerPrice = 5500
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s21
                    , offerName = "Пакет"
                    , offerPrice = 25000
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 сеансов"
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
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s21
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e10
                   , roleService = s21
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    s22 <- insert $ Service { serviceName = "Делюкс уход за лицом"
                            , servicePublished = True
                            , serviceOverview = Just "Делюкс уход за лицом"
                            , serviceDescr = Just "Этот специальный уход за лицом можно адаптировать к состоянию кожи клиента (например, сухая, жирная, чувствительная и т. д.). Он создан для того, чтобы разгладить и смягчить цвет лица, одновременно снимая напряжение со всего тела. Наш роскошный уход за лицом поможет вам чувствовать себя и выглядеть здоровее."
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s22
                    , offerName = "Цена"
                    , offerPrice = 7500
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s22
                    , offerName = "Пакет"
                    , offerPrice = 35000
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 сеансов"
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
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s22
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s22
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    s23 <- insert $ Service { serviceName = "Ухаживающие процедуры"
                            , servicePublished = True
                            , serviceOverview = Just "Ухаживающие процедуры по уходу за лицом (90 мин)"
                            , serviceDescr = Just "Увлажняющая клиническая процедура, оказывающая охлаждающий эффект на кожу, оживляющая, увлажняющая и успокаивающая. Его термоохлаждающее действие на кожу делает его замечательным восстанавливающим средством, особенно для уменьшения покраснений. ALGOMASK+ обеспечивает мгновенное сияние и длительное увлажнение."
                            , serviceDuration = duration "00:90"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s23
                    , offerName = "Цена"
                    , offerPrice = 9000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s23
                    , offerName = "Пакет"
                    , offerPrice = 40000
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 сеансов"
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
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s23
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s23
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s23
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    s24 <- insert $ Service { serviceName = "Лечение акне"
                            , servicePublished = True
                            , serviceOverview = Just "Лечение акне (120 мин)"
                            , serviceDescr = Just "Это очень инновационный и эффективный способ лечения угревой сыпи, который не поддается лечению другими методами лечения, и который дал множество замечательных результатов. В состав Rejuvi Normalizing Formula входят перекись мочевины, альфа-гидроксильные кислоты и специальный антиандрогенный элемент."
                            , serviceDuration = duration "02:00"
                            , serviceGroup = Just s2
                            }

    insert_ $ Offer { offerService = s24
                    , offerName = "Цена"
                    , offerPrice = 9500
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s24
                    , offerName = "Пакет"
                    , offerPrice = 60000
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/7 сеансов"
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
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s24
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s24
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    s25 <- insert $ Service { serviceName = "Отбеливающая процедура с 24-каратным золотом"
                            , servicePublished = True
                            , serviceOverview = Just "Отбеливающая процедура с 24-каратным золотом"
                            , serviceDescr = Just "<p>Роскошное антивозрастное средство для лица. Эта увлажняющая активная формула, бросающая вызов старению, сочетает в себе силу чистых витаминов, растительных экстрактов и 24-каратного золота. Эти ингредиенты эффективно помогают стимулировать выработку коллагена. Они образуют сплошной защитный барьер, имитирующий последствия хирургического вмешательства.</p><p>Маска ложится как «вторая кожа» и идеально адаптируется к контурам лица. Он обеспечивает максимальное увлажнение, укрепляет естественный защитный барьер кожи и омолаживает чувствительную кожу, уменьшая признаки старения.</p>"
                            , serviceDuration = duration "01:15"
                            , serviceGroup = Just s2
                            }

    o251 <- insert $ Offer { offerService = s25
                           , offerName = "Цена"
                           , offerPrice = 10000
                           , offerPrefix = Nothing
                           , offerSuffix = Nothing
                           , offerDescr = Nothing
                           }

    insert_ $ Offer { offerService = s25
                    , offerName = "Пакет"
                    , offerPrice = 46000
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 сеансов"
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
                       , roleName = "Косметолог"
                       , roleDuration = 60 * (1 * 60 + 15)
                       , roleRating = Just 5
                       }

    r925 <- insert role925

    insert_ $ Role { roleStaff = e10
                   , roleService = s25
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s25
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s25
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }
    s3 <- insert $ Service { serviceName = "Передовые процедуры для лица"
                           , servicePublished = True
                           , serviceOverview = Just "Продвинутые процедуры по уходу за лицом"
                           , serviceDescr = Just "Продвинутые процедуры по уходу за лицом"
                           , serviceDuration = duration "01:10"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s3
                        , thumbnailPhoto = $(embedFile "static/img/advanced-facial-treatments.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s31 <- insert $ Service { serviceName = "Молочный пилинг"
                            , servicePublished = True
                            , serviceOverview = Just "Молочный пилинг (90 мин)"
                            , serviceDescr = Just $ Textarea [st|
<p>
Milk Peel включает в себя натуральный экстракт кислого молока (молочная кислота), папаин (фермент папайи), салициловую кислоту (из натуральных растений) и специальный контроллер проникновения. Молочный пилинг обеспечивает безопасный процесс отшелушивания кожи без побочных эффектов, которые могут вызвать пилинги с фенолом и ТСА.
</p>
<p>
Молочный пилинг-формула выполняет следующие функции:
</p>
<ul>
  <li>Обеспечивает максимальное, но бережное отшелушивание кожи.</li>
  <li>Стимулирует пролиферацию фибробластов для увеличения дермального коллагена и эластина.</li>
  <li>Нормализация клеток и тканей кожи.</li>
</ul>
<p>
В результате молочный пилинг представляет собой мощный процесс обновления кожи с существенным кожным эффектом. Он имеет ряд преимуществ для кожи, таких как:
</p>
<ol>
  <li>Удаление морщин и видимых тонких линий на коже.</li>
  <li>Уменьшение следов прыщей или угрей.</li>
  <li>Уменьшение шрамов.</li>
  <li>Разглаживание вдавленных ямочек на коже.</li>
  <li>Уменьшение растяжек и родимых пятен.</li>
  <li>Мягкая полировка кожи для обновления лица.</li>
</ol>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s3
                            }

    insert_ $ Offer { offerService = s31
                    , offerName = "Цена"
                    , offerPrice = 33000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e8
                   , roleService = s31
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s31
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s31
                        , thumbnailPhoto = $(embedFile "static/img/milk-peel.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s32 <- insert $ Service { serviceName = "Дермароллер"
                            , servicePublished = True
                            , serviceOverview = Just "Дерма Роллер"
                            , serviceDescr = Just $ Textarea [st|
<h4>Преимущества:</h4>
<ul>
  <li>Удаление и лечение растяжек, шрамов от прыщей, морщин.</li>
  <li>Anti-aging.</li>
  <li>Лечение выпадения волос или восстановление волос.</li>
  <li>Лечение целлюлита и уменьшение целлюлита.</li>
  <li>Заменив коллаген, вы можете помочь своей коже обновиться и восстановиться до косметического уровня.</li>
</ul>
<h4>Так что же такое дерма-роллер?</h4>
<p>
Роллер Scientia Derma Roller — это невероятное устройство, которое естественным образом повышает уровень коллагена и эластина в вашей коже. Растяжки, морщины, шрамы и неровная кожа с ямками – все это связано с недостатком коллагена. Поэтому, используя DermaRoller для замены коллагена, вы можете помочь своей коже обновиться и восстановиться до косметического уровня.
</p>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s3
                            }

    o321 <- insert $ Offer { offerService = s32
                           , offerName = "Цена"
                           , offerPrice = 33000
                           , offerPrefix = Nothing
                           , offerSuffix = Nothing
                           , offerDescr = Nothing
                           }

    insert_ $ Role { roleStaff = e7
                   , roleService = s32
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s32
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s32
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s32
                        , thumbnailPhoto = $(embedFile "static/img/derma-roller.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s33 <- insert $ Service { serviceName = "Микродермабразия"
                            , servicePublished = True
                            , serviceOverview = Just "Микродермабразия"
                            , serviceDescr = Just $ Textarea [st|
<p>
Безболезненная процедура помогает удалить шрамы от прыщей, расширенные поры, морщины на лице, морщины, угри, а также повреждения от солнца и т. д. Этот метод также помогает утолщать коллаген, что приводит к более молодому цвету лица. Коллаген — это белок вашей кожи, которого много в детстве, и благодаря которому кожа выглядит подтянутой и гладкой. С возрастом выработка коллагена снижается, что приводит к дряблости и неровности кожи.
</p>
|]
                            , serviceDuration = duration "00:45"
                            , serviceGroup = Just s3
                            }

    insert_ $ Offer { offerService = s33
                    , offerName = "Цена"
                    , offerPrice = 7000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e5
                   , roleService = s33
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s33
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s33
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s33
                        , thumbnailPhoto = $(embedFile "static/img/micro-dermabrasion.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s34 <- insert $ Service { serviceName = "Отбеливание веснушек"
                            , servicePublished = True
                            , serviceOverview = Just "Отбеливание веснушек (120 мин)"
                            , serviceDescr = Just $ Textarea [st|
<p>
Эта процедура обеспечивает синергетический эффект, улучшая тон кожи, устраняя проблемы с пигментацией, а также темные круги под глазами.
</p>
|]
                            , serviceDuration = duration "02:00"
                            , serviceGroup = Just s3
                            }
           
    insert_ $ Offer { offerService = s34
                    , offerName = "Цена"
                    , offerPrice = 9500
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s34
                    , offerName = "Пакет"
                    , offerPrice = 60000
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/7 сеансов"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s34
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s34
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s34
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s34
                        , thumbnailPhoto = $(embedFile "static/img/freckle-bleaching.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s4 <- insert $ Service { serviceName = "Антивозрастные процедуры"
                           , servicePublished = True
                           , serviceOverview = Just "Антивозрастные процедуры"
                           , serviceDescr = Just "Антивозрастные процедуры"
                           , serviceDuration = duration "01:30"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s4
                        , thumbnailPhoto = $(embedFile "static/img/anti-aging-treatments.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s41 <- insert $ Service { serviceName = "Sea C Spa (100% витамин С)"
                            , servicePublished = True
                            , serviceOverview = Just "Восстановление молодости внешнего вида"
                            , serviceDescr = Just $ Textarea [st|
<p>
Мощный антивозрастной уход за кожей, предназначенный для замедления видимых признаков старения и придания коже энергии.
</p>
<p>
В состав входят морские и растительные ингредиенты (концентрат витамина С, биоматричные пластыри с водорослями и термальная органическая грязь).
</p>
<p>
Эта процедура является исключительной как до, так и после пребывания на солнце. Идеально подходит для людей, живущих в городских районах с высоким уровнем загрязнения.
</p>
<h4>Преимущества:</h4>
<p>
Эта процедура уменьшает появление линий и морщин. Выравнивает тон кожи. Осветляет кожу, возвращая ей молодой вид.
</p>
|]
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s41
                    , offerName = "Цена"
                    , offerPrice = 9500
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s41
                    , offerName = "Пакет"
                    , offerPrice = 43000
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 сеансов"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e2
                   , roleService = s41
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s41
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s41
                        , thumbnailPhoto = $(embedFile "static/img/sea-C-spa.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s42 <- insert $ Service { serviceName = "Ботинол для лица"
                            , servicePublished = True
                            , serviceOverview = Just "Ботинол для лица (150 мин)"
                            , serviceDescr = Just $ Textarea [st|
<p>
Высокотехнологичное плановое лечение, разработанное для увлажнения, регенерации и уменьшения морщин.
</p>
<p>
Этот сеанс расслабляющей процедуры предлагает приятные текстуры и изысканные эссенции, обеспечивающие общее ощущение благополучия. Идеальное средство для людей, стремящихся замаскировать признаки старения.
</p>
<h4>Преимущества:</h4>
<p>
Уже после одной процедуры мимические морщины становятся расслабленными. Линии и морщины заметно уменьшаются. Кожа выглядит заметно моложе.
</p>
|]
                            , serviceDuration = duration "02:30"
                            , serviceGroup = Just s4
                            }
           
    insert_ $ Offer { offerService = s42
                    , offerName = "Цена"
                    , offerPrice = 17000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s42
                    , offerName = "Пакет"
                    , offerPrice = 78000
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 сеансов"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e1
                   , roleService = s42
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s42
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s42
                        , thumbnailPhoto = $(embedFile "static/img/botinol-facial.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s43 <- insert $ Service { serviceName = "Коллаген 90-II"
                            , servicePublished = True
                            , serviceOverview = Just "Коллаген 90-II (150 мин)"
                            , serviceDescr = Just $ Textarea [st|
<p>
Это увлажняющее антивозрастное средство обеспечивает интенсивное увлажнение, регенерацию клеток и обновление эпидермиса. С возрастом процесс обновления клеток замедляется. Контуры лица теряют четкость, кожа обвисает.
</p>
<p>
Collagen 90-II — эксклюзивное антивозрастное средство от G.M. Collin Skin Care, сочетающий в себе чистый коллагеновый лист с выбранными ингредиентами, обеспечивает интенсивное увлажнение, укрепление кожи и замедление признаков старения.
</p>
<p>
Collagen 90-II — это уважаемое и востребованное антивозрастное средство для обновления кожи, которое увлажняет, разглаживает и тонизирует морщины для борьбы с видимыми признаками старения.
</p>
<h4>Преимущества:</h4>
<p>
Эта процедура улучшает общий цвет лица, сводя к минимуму появление тонких линий и морщин. Восстанавливает влагу, делая кожу хорошо увлажненной и сияющей. Рекомендуется для всех типов кожи
</p>
|]
                            , serviceDuration = duration "02:30"
                            , serviceGroup = Just s4
                            }
           
    insert_ $ Offer { offerService = s43
                    , offerName = "Цена"
                    , offerPrice = 16000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s43
                    , offerName = "Пакет"
                    , offerPrice = 73000
                    , offerPrefix = Nothing
                    , offerSuffix = Just "/5 sessions"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e10
                   , roleService = s43
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s43
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s43
                        , thumbnailPhoto = $(embedFile "static/img/collagen-90-II.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s44 <- insert $ Service { serviceName = "Электро ионизация"
                            , servicePublished = True
                            , serviceOverview = Just "Меньше морщин всего за 15 минут!"
                            , serviceDescr = Just $ Textarea [st|
<p>
Способствует выработке коллагена, подтягивает и укрепляет ткани кожи, увлажняет и омолаживает эпидермис, лечит прыщи и тонкие линии. Меньше морщин всего за 15 минут!
</p>
|]
                            , serviceDuration = duration "00:15"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s44
                    , offerName = "Цена"
                    , offerPrice = 18000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s44
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s44
                        , thumbnailPhoto = $(embedFile "static/img/electro-ionization.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s5 <- insert $ Service { serviceName = "Уход за глазами"
                           , servicePublished = True
                           , serviceOverview = Just "Центр ухода за глазами"
                           , serviceDescr = Just "Центр ухода за глазами"
                           , serviceDuration = duration "02:00"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s5
                        , thumbnailPhoto = $(embedFile "static/img/eye-treatment-center.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s51 <- insert $ Service { serviceName = "Завивка ресниц"
                            , servicePublished = True
                            , serviceOverview = Just "Химическая завивка ресниц"
                            , serviceDescr = Just $ Textarea [st|
<p>
Наша техника и раствор для химической завивки ресниц совершенствовались более десяти лет, чтобы последовательно и бережно воздействовать на ваши ресницы.
</p>
<p>
Несмотря на название, завивка ресниц не является постоянной. Результаты завивки ресниц обычно сохраняются в течение 2-3 месяцев или в течение естественного цикла роста ресниц. Когда завитые ресницы естественным образом выпадут, новые волоски ресниц будут расти прямо (как это было до химической завивки ресниц).
</p>
<p>
Перед завивкой ресниц вас попросят снять контактные линзы и макияж с глаз (вам легко доступны бесплатное средство для снятия макияжа, раствор для линз и временные чашки для линз). Во время завивки ресниц, которая занимает около 1 часа, вы можете лежать на спине. Расслабьтесь и насладитесь звуками нашей успокаивающей спа-музыки. Пожалуйста, держите глаза закрытыми и расслабленными во время процедуры, чтобы добиться наилучших результатов завивки ресниц.
</p>
<p>
Как и при обычной завивке волос, старайтесь не мочить ресницы в течение 4 часов после завивки ресниц. Мы рекомендуем ежедневно наносить на ресницы кондиционер для ресниц. Кондиционирующий гель для ресниц и тушь для ресниц можно приобрести в спа-центре, по телефону и в нашем интернет-магазине. Регулярное использование кондиционера для ресниц гарантирует, что ваши ресницы останутся здоровыми.
</p>
|]
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s51
                    , offerName = "Цена"
                    , offerPrice = 4500
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e7
                   , roleService = s51
                   , roleName = "Визажист"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s51
                   , roleName = "Визажист"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s51
                   , roleName = "Визажист"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s51
                        , thumbnailPhoto = $(embedFile "static/img/eyelash-perm.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s52 <- insert $ Service { serviceName = "Наращивание ресниц"
                            , servicePublished = True
                            , serviceOverview = Just "Наращивание ресниц"
                            , serviceDescr = Just $ Textarea [st|
<h4>Наращивание ресниц – уход в домашних условиях</h4>
<ol>
  <li>В течение первых 2 часов не допускайте попадания воды на ресницы.</li>
  <li>В течение первых 2 часов не распаривайте лицо, не пользуйтесь паровой баней, не купайтесь и не умывайтесь горячей водой.</li>
  <li>Используйте только смываемую тушь и слегка прокрашивайте кончики ресниц.</li>
  <li>Не наносите тушь на клеевую зону ресниц средством для снятия туши.</li>
  <li>Не делайте завивку ресниц.</li>
  <li>Не используйте щипцы для завивки ресниц, так как они сломают как ресницы, так и натуральные ресницы.</li>
  <li>Не трите глаза и ресницы.</li>
  <li>Умываясь, всегда промокайте ресницы насухо после умывания.</li>
</ol>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s5
                            }
           
    insert_ $ Offer { offerService = s52
                    , offerName = "Цена"
                    , offerPrice = 13000
                    , offerPrefix = Nothing
                    , offerSuffix = Just " и выше"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e6
                   , roleService = s52
                   , roleName = "Визажист"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s52
                   , roleName = "Визажист"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s52
                        , thumbnailPhoto = $(embedFile "static/img/eyelash-extension.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s53 <- insert $ Service { serviceName = "Покраска бровей и ресниц"
                            , servicePublished = True
                            , serviceOverview = Just "Покраска бровей и ресниц"
                            , serviceDescr = Just $ Textarea [st|Покраска бровей и ресниц|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s53
                    , offerName = "Цена"
                    , offerPrice = 2500
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e4
                   , roleService = s53
                   , roleName = "Визажист"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s53
                   , roleName = "Визажист"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s53
                   , roleName = "Визажист"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s53
                        , thumbnailPhoto = $(embedFile "static/img/eyebrow-or-eyelash-tint.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s54 <- insert $ Service { serviceName = "Омолаживающая процедура для глаз"
                            , servicePublished = True
                            , serviceOverview = Just "Процедуры омоложения глаз"
                            , serviceDescr = Just $ Textarea [st|
<p>
Он элегантно разработан, чтобы мягко уменьшить отечность и морщины, а также темные круги вокруг глаз. Это мощное средство для ухода за глазами, включающее ряд продуктов Rejuvi, таких как: гель для восстановления глаз «i», фруктовый комплекс, комплекс витамина А, комплекс витамина С, формула для контуринга, маска для лица и т. д., обеспечивающие синергетический эффект на глаза. область. Потрясающие результаты можно увидеть сразу после процедуры.
</p>
<p>
Процедуры можно проводить каждую неделю или раз в две недели, в зависимости от состояния кожи. Полный курс лечения глаз должен состоять из 4–6 применений отдельных процедур плюс программа домашнего ухода.
</p>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s54
                    , offerName = "Цена"
                    , offerPrice = 4000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s54
                   , roleName = "Визажист"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s54
                   , roleName = "Визажист"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s54
                        , thumbnailPhoto = $(embedFile "static/img/eye-rejuvenating-treatment.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s55 <- insert $ Service { serviceName = "Антивозрастной уход за глазами"
                            , servicePublished = True
                            , serviceOverview = Just "Антивозрастной уход за глазами"
                            , serviceDescr = Just $ Textarea [st|Антивозрастной уход за глазами|]
                            , serviceDuration = duration "01:45"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s55
                    , offerName = "Цена"
                    , offerPrice = 4000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e2
                   , roleService = s55
                   , roleName = "Визажист"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s55
                   , roleName = "Визажист"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s55
                        , thumbnailPhoto = $(embedFile "static/img/anti-aging-eye-treatment.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s6 <- insert $ Service { serviceName = "Массаж тела"
                           , servicePublished = True
                           , serviceOverview = Just "Массаж тела"
                           , serviceDescr = Just "Массаж тела"
                           , serviceDuration = duration "02:00"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s6
                        , thumbnailPhoto = $(embedFile "static/img/body-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s61 <- insert $ Service { serviceName = "Шведский массаж"
                            , servicePublished = True
                            , serviceOverview = Just "Шведский лечебный массаж"
                            , serviceDescr = Just $ Textarea [st|
<p>
Шведская массажная терапия – это метод, который приходит на ум большинству людей, когда они думают о массаже. Поскольку это самый известный вид работы с телом, выполняемый сегодня, одна из основных целей техники шведского массажа — расслабить все тело. Это достигается путем растирания мышц длинными скользящими движениями в направлении возврата крови к сердцу. Но шведский массаж выходит за рамки релаксации. Шведский массаж исключительно полезен для повышения уровня кислорода в крови, снижения токсинов в мышцах, улучшения кровообращения и гибкости, а также снятия напряжения.
</p>
|]
                            , serviceDuration = duration "01:00"
                            , serviceGroup = Just s6
                            }

    insert_ $ Offer { offerService = s61
                    , offerName = "Цена"
                    , offerPrice = 6000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s61
                   , roleName = "Массажист"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s61
                   , roleName = "Массажист"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s61
                   , roleName = "Массажист"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s61
                        , thumbnailPhoto = $(embedFile "static/img/swedish-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s62 <- insert $ Service { serviceName = "Массаж на стуле"
                            , servicePublished = True
                            , serviceOverview = Just "Офисный массаж"
                            , serviceDescr = Just $ Textarea [st|
<p>
Массаж на стуле — это 15-20-минутный массаж, ориентированный на спину, плечи, шею, руки и голову. Он предназначен для расслабления мышц и улучшения гибкости и движения.
</p>
|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s6
                            }

    insert_ $ Offer { offerService = s62
                    , offerName = "Цена"
                    , offerPrice = 6000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e10
                   , roleService = s62
                   , roleName = "Массажист"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s62
                        , thumbnailPhoto = $(embedFile "static/img/chair-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s63 <- insert $ Service { serviceName = "Массаж ног"
                            , servicePublished = True
                            , serviceOverview = Just "Массаж ног"
                            , serviceDescr = Just $ Textarea [st|
<p>
Массаж ног — это терапевтическая практика, которая включает в себя манипулирование ногами и надавливание на определенные области, чтобы способствовать расслаблению, снятию напряжения и улучшению общего самочувствия. В отличие от рефлексотерапии, которая представляет собой специализированную практику, фокусирующуюся на определенных рефлекторных точках на стопах, соответствующих различным органам и системам организма, массаж стоп обычно предполагает более общий подход.
</p>
<p>
Во время массажа стоп могут использоваться различные приемы, в том числе разминание, поглаживание, растирание, надавливание руками, пальцами или специальными инструментами. Массаж может воздействовать на определенные области стоп, такие как своды стоп, пятки, пальцы и подушечки стоп, а также на окружающие мышцы и суставы.
</p>
<h4>Преимущества:</h4>
<ol>
  <li>Релаксация: массаж ног может помочь уменьшить стресс и вызвать состояние релаксации.</li>
  <li>Улучшение кровообращения: используемые техники массажа могут улучшить приток крови к ступням и нижним конечностям, способствуя улучшению кровообращения.</li>
  <li>Облегчение боли. Массаж стоп может помочь облегчить боль в стопах, включая такие состояния, как подошвенный фасциит или общий дискомфорт в стопах.</li>
  <li>Снижение мышечного напряжения. Воздействуя на определенные мышцы стоп, массаж может помочь снять напряжение и улучшить гибкость.</li>
  <li>Улучшение общего самочувствия: считается, что массаж ног оказывает положительное влияние на поток энергии в организме, способствуя ощущению хорошего самочувствия.</li>
</ol>
|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s6
                            }

    insert_ $ Offer { offerService = s63
                    , offerName = "Цена"
                    , offerPrice = 3000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e1
                   , roleService = s63
                   , roleName = "Массажист"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s63
                   , roleName = "Массажист"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s63
                        , thumbnailPhoto = $(embedFile "static/img/foot-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s7 <- insert $ Service { serviceName = "Услуги макияжа"
                           , servicePublished = True
                           , serviceOverview = Just "Визаж"
                           , serviceDescr = Just "Услуги макияжа"
                           , serviceDuration = duration "03:30"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s7
                        , thumbnailPhoto = $(embedFile "static/img/makeup-services.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s71 <- insert $ Service { serviceName = "Свадебное специальное предложение"
                            , servicePublished = True
                            , serviceOverview = Just "Свадебное специальное предложение"
                            , serviceDescr = Just $ Textarea [st|
<p>В пакет входит: свадебный макияж, прическа, уход за лицом и маникюр.</p>
|]
                            , serviceDuration = duration "03:30"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s71
                    , offerName = "Цена"
                    , offerPrice = 20000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s71
                   , roleName = "Визажист"
                   , roleDuration = 60 * (3 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s71
                   , roleName = "Визажист"
                   , roleDuration = 60 * (3 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s71
                   , roleName = "Визажист"
                   , roleDuration = 60 * (3 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s71
                        , thumbnailPhoto = $(embedFile "static/img/wedding-special.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s72 <- insert $ Service { serviceName = "Свадебный макияж"
                            , servicePublished = True
                            , serviceOverview = Just "Свадебный макияж (45 min)"
                            , serviceDescr = Just $ Textarea [st|Свадебный макияж (45 мин)|]
                            , serviceDuration = duration "00:45"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s72
                    , offerName = "Цена"
                    , offerPrice = 8500
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e4
                   , roleService = s72
                   , roleName = "Визажист"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s72
                   , roleName = "Визажист"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s72
                        , thumbnailPhoto = $(embedFile "static/img/bridal-make-up.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s73 <- insert $ Service { serviceName = "Вечерний макияж"
                            , servicePublished = True
                            , serviceOverview = Just "Вечерний макияж (30 мин)"
                            , serviceDescr = Just $ Textarea [st|Вечерний макияж (30 мин)|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s73
                    , offerName = "Цена"
                    , offerPrice = 6000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e5
                   , roleService = s73
                   , roleName = "Визажист"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s73
                   , roleName = "Визажист"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s73
                        , thumbnailPhoto = $(embedFile "static/img/evening-make-up.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s74 <- insert $ Service { serviceName = "Урок макияжа"
                            , servicePublished = True
                            , serviceOverview = Just "Урок макияжа (60 мин)"
                            , serviceDescr = Just $ Textarea [st|Урок макияжа (60 мин)|]
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s74
                    , offerName = "Цена"
                    , offerPrice = 100
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e6
                   , roleService = s74
                   , roleName = "Визажист"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s74
                   , roleName = "Визажист"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s74
                        , thumbnailPhoto = $(embedFile "static/img/make-up-lesson.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s8 <- insert $ Service { serviceName = "Восковая эпиляция"
                           , servicePublished = True
                           , serviceOverview = Just "Депиляция воском"
                           , serviceDescr = Just "Депиляция воском"
                           , serviceDuration = duration "01:25"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s8
                        , thumbnailPhoto = $(embedFile "static/img/waxing.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s81 <- insert $ Service { serviceName = "Депиляция тела воском"
                            , servicePublished = True
                            , serviceOverview = Just "Восковая депиляция тела"
                            , serviceDescr = Just $ Textarea [st|Депиляция тела воском|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s8
                            }

    insert_ $ Offer { offerService = s81
                    , offerName = "Цена"
                    , offerPrice = 5000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e8
                   , roleService = s81
                   , roleName = "Специалист по депиляции"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s81
                   , roleName = "Специалист по депиляции"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s81
                   , roleName = "Специалист по депиляции"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s81
                        , thumbnailPhoto = $(embedFile "static/img/body-waxing.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s82 <- insert $ Service { serviceName = "Депиляция лица"
                            , servicePublished = True
                            , serviceOverview = Just "Депиляция лица воском"
                            , serviceDescr = Just $ Textarea [st|Депиляция лица воском|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s8
                            }

    insert_ $ Offer { offerService = s82
                    , offerName = "Цена"
                    , offerPrice = 4500
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s82
                   , roleName = "Специалист по депиляции"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s82
                   , roleName = "Специалист по депиляции"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s82
                        , thumbnailPhoto = $(embedFile "static/img/face-waxing.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s9 <- insert $ Service { serviceName = "Уход за ногтями"
                           , servicePublished = True
                           , serviceOverview = Just "Услуги по уходу за ногтями"
                           , serviceDescr = Just "Услуги по уходу за ногтями"
                           , serviceDuration = duration "00:45"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s9
                        , thumbnailPhoto = $(embedFile "static/img/nail-care.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s91 <- insert $ Service { serviceName = "Маникюр"
                            , servicePublished = True
                            , serviceOverview = Just "Маникюр"
                            , serviceDescr = Just $ Textarea [st|Маникюр|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s91
                    , offerName = "Цена"
                    , offerPrice = 1500
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s91
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s91
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s91
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s91
                        , thumbnailPhoto = $(embedFile "static/img/manicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s92 <- insert $ Service { serviceName = "Бесчиповый маникюр"
                            , servicePublished = True
                            , serviceOverview = Just "Бесчиповый маникюр"
                            , serviceDescr = Just $ Textarea [st|Бесчиповый маникюр|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s92
                    , offerName = "Цена"
                    , offerPrice = 3200
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e10
                   , roleService = s92
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s92
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s92
                        , thumbnailPhoto = $(embedFile "static/img/no-chip-manicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s93 <- insert $ Service { serviceName = "Педикюр"
                            , servicePublished = True
                            , serviceOverview = Just "Педикюр"
                            , serviceDescr = Just $ Textarea [st|Педикюр|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s93
                    , offerName = "Цена"
                    , offerPrice = 3500
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e1
                   , roleService = s93
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s93
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s93
                        , thumbnailPhoto = $(embedFile "static/img/pedicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s94 <- insert $ Service { serviceName = "Бесчиповый педикюр"
                            , servicePublished = True
                            , serviceOverview = Just "Бесчиповый педикюр"
                            , serviceDescr = Just $ Textarea [st|Бесчиповый педикюр|]
                            , serviceDuration = duration "00:40"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s94
                    , offerName = "Цена"
                    , offerPrice = 5500
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e2
                   , roleService = s94
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 40)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s94
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 40)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s94
                        , thumbnailPhoto = $(embedFile "static/img/no-chip-pedicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s95 <- insert $ Service { serviceName = "Акриловые ногти"
                            , servicePublished = True
                            , serviceOverview = Just "Полный набор акриловых ногтей"
                            , serviceDescr = Just $ Textarea [st|Полный набор акриловых ногтей|]
                            , serviceDuration = duration "00:25"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s95
                    , offerName = "Цена"
                    , offerPrice = 3800
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s95
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s95
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s95
                        , thumbnailPhoto = $(embedFile "static/img/acrylic-nail-full-set.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s96 <- insert $ Service { serviceName = "Парафин для рук"
                            , servicePublished = True
                            , serviceOverview = Just "Парафиновое погружение для рук"
                            , serviceDescr = Just $ Textarea [st|Парафиновое погружение для рук|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s96
                    , offerName = "Цена"
                    , offerPrice = 1000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e4
                   , roleService = s96
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s96
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s96
                        , thumbnailPhoto = $(embedFile "static/img/hand-paraffin-dip.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s97 <- insert $ Service { serviceName = "Смена лака"
                            , servicePublished = True
                            , serviceOverview = Just "Смена лака для ногтей"
                            , serviceDescr = Just $ Textarea [st|Смена лака для ногтей|]
                            , serviceDuration = duration "00:15"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s97
                    , offerName = "Цена"
                    , offerPrice = 500
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e6
                   , roleService = s97
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s97
                   , roleName = "Мастера маникюра"
                   , roleDuration = 60 * (0 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s97
                        , thumbnailPhoto = $(embedFile "static/img/polish-change.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s10 <- insert $ Service { serviceName = "Коррекция тела и фитнес"
                            , servicePublished = True
                            , serviceOverview = Just "Коррекция тела и фитнес"
                            , serviceDescr = Just "Коррекция тела и фитнес"
                            , serviceDuration = duration "01:15"
                            , serviceGroup = Nothing
                            }

    insert_ $ Thumbnail { thumbnailService = s10
                        , thumbnailPhoto = $(embedFile "static/img/body-shaping-and-fitness.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s101 <- insert $ Service { serviceName = "Коррекция тела"
                             , servicePublished = True
                             , serviceOverview = Just "Коррекция тела"
                             , serviceDescr = Just $ Textarea [st|
<p>Коррекция фигуры: живот и талия, бедра, ноги и руки.</p>
|]
                             , serviceDuration = duration "01:15"
                             , serviceGroup = Just s10
                             }

    insert_ $ Offer { offerService = s101
                    , offerName = "Цена"
                    , offerPrice = 35000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e7
                   , roleService = s101
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s101
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s101
                        , thumbnailPhoto = $(embedFile "static/img/body-shaping.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s102 <- insert $ Service { serviceName = "Коррекция лица"
                             , servicePublished = True
                             , serviceOverview = Just "Коррекция лица"
                             , serviceDescr = Just $ Textarea [st|Коррекция лица|]
                             , serviceDuration = duration "00:45"
                             , serviceGroup = Just s10
                             }

    insert_ $ Offer { offerService = s102
                    , offerName = "Цена"
                    , offerPrice = 30000
                    , offerPrefix = Nothing
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e8
                   , roleService = s102
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s102
                   , roleName = "Косметолог"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s102
                        , thumbnailPhoto = $(embedFile "static/img/face-shaping.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }


    pass6 <- liftIO $ makePassword "ivanovata" 17
    c1 <- insert $ User { userName = "ivanovata"
                        , userPassword = decodeUtf8 pass6
                        , userAdmin = False
                        , userFullName = Just "Иванова Тамара Александровна"
                        , userEmail = Just "ivanovata@mail.ru"
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


    pass7 <- liftIO $ makePassword "danilovip" 17
    c2 <- insert $ User { userName = "danilovip"
                        , userPassword = decodeUtf8 pass7
                        , userAdmin = False
                        , userFullName = Just "Данилов Илья Петрович"
                        , userEmail = Just "danilovip@mail.org"
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
                     , bookStatus = BookStatusCancelled
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
                   , histStatus = BookStatusCancelled
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
                     , bookStatus = BookStatusApproved
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
                   , histStatus = BookStatusApproved
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
                     , bookStatus = BookStatusCancelled
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
                   , histStatus = BookStatusCancelled
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
