{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Demo.DemoDataEN (populateEN) where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (st)
import qualified Data.ByteString.Base64 as B64 (decode)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay,utctDayTime), DiffTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay), timeToTimeOfDay, utc)
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
    , Contents (Contents, contentsSection, contentsContent)
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
    , DayType (Weekday, Holiday)
    )
    
import Data.FileEmbed (embedFile)
import Demo.DemoPhotos
    ( man01, man02, man03, man04, man05, man06
    , woman01, woman02, woman03, woman04, woman05
    )

populateEN :: MonadIO m => ReaderT SqlBackend m ()
populateEN = do

    (now,today,time) <- liftIO $ getCurrentTime >>= \x -> return (x ,utctDay x,timeToTimeOfDay (utctDayTime x))

    let business = Business { businessName = "Salon"
                            , businessFullName = Just "Salon Ltd"
                            , businessCurrency = "$"
                            , businessAddr = "Charing Cross, London WC2N 5DU, United Kingdom"
                            , businessTzo = utc
                            , businessTz = "Europe/London"
                            , businessPhone = Just "020-7736-6600"
                            , businessMobile = Just "567-274-7469"
                            , businessEmail = Just "salon@mail.uk"
                            }

    b <- insert business

    insert_ $ BusinessHours { businessHoursBusiness = b
                            , businessHoursDay = addDays (-1) today
                            , businessHoursOpen = TimeOfDay 9 0 0
                            , businessHoursClose = TimeOfDay 17 45 0
                            , businessHoursDayType = Holiday
                            }

    insert_ $ BusinessHours { businessHoursBusiness = b
                            , businessHoursDay = today
                            , businessHoursOpen = TimeOfDay 9 0 0
                            , businessHoursClose = TimeOfDay 18 0 0
                            , businessHoursDayType = Weekday
                            }

    insert_ $ BusinessHours { businessHoursBusiness = b
                            , businessHoursDay = addDays 1 today
                            , businessHoursOpen = TimeOfDay 9 0 0
                            , businessHoursClose = TimeOfDay 18 0 0
                            , businessHoursDayType = Weekday
                            }

    insert_ $ BusinessHours { businessHoursBusiness = b
                            , businessHoursDay = addDays 2 today
                            , businessHoursOpen = TimeOfDay 9 0 0
                            , businessHoursClose = TimeOfDay 18 0 0
                            , businessHoursDayType = Weekday
                            }

    insert_ $ Contents { contentsSection = "CONTACTS"
                       , contentsContent = Textarea $ toStrict $ renderHtml [shamlet|
<section style="margin:0 1rem">
  <h3 style="color:gray">Call Us
  <dl>
    <dt>
      <i>Telephone
    <dd>
      $maybe phone <- businessPhone business
        #{phone}
    <dt>
      <i>Mobile
    <dd>
      $maybe mobile <- businessMobile business
        #{mobile}
<section style="margin:0 1rem">
  <h3 style="color:gray">Email Us
  <dl>
    <dt>
      <i>Email
    <dd>
      $maybe email <- businessEmail business
        #{email}
<section style="margin:0 1rem">
  <h3 style="color:gray">Come see us
  <dl>
    <dt>
      <i>Address
    <dd>
      #{businessAddr business}
<iframe width="100%" height="400px" loding="lazy" title="Salon" style="border:none" src="https://api.mapbox.com/styles/v1/mapbox/streets-v12.html?title=false&zoomwheel=false&access_token=pk.eyJ1IjoiY2l1a3N0YXIiLCJhIjoiY2o1enNibDNsMGNrNDJ3dDhxeTJuc3luMiJ9.Jgc5GdYUMbYwGq-zRWtzfw#15/51.5073/-0.12755">
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
                   , userFullName = Just "Adam Smith"
                   , userEmail = Just "asmith@mail.uk"
                   }

    pass1 <- liftIO $ makePassword "johnnysmith" 17
    let user1 = User { userName = "johnnysmith"
                     , userPassword = decodeUtf8 pass1
                     , userAdmin = False
                     , userFullName = Just "Johnny Smith"
                     , userEmail = Just "jsmith@mail.uk"
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

    pass2 <- liftIO $ makePassword "marylopez" 17
    let user2 = User { userName = "marylopez"
                     , userPassword = decodeUtf8 pass2
                     , userAdmin = False
                     , userFullName = Just "Mary Lopez"
                     , userEmail = Just "mlopez@mail.uk"
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
                     , userFullName = Just "John Johnson"
                     , userEmail = Just "jjohnson@mail.uk"
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

    pass4 <- liftIO $ makePassword "patriciabrown" 17
    let user4 = User { userName = "patriciabrown"
                     , userPassword = decodeUtf8 pass4
                     , userAdmin = False
                     , userFullName = Just "Patricia Brown"
                     , userEmail = Just "pbrown@mail.uk"
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

    pass5 <- liftIO $ makePassword "chriswilson" 17
    let user5 = User { userName = "chriswilson"
                     , userPassword = decodeUtf8 pass5
                     , userAdmin = False
                     , userFullName = Just "Chris Wilson"
                     , userEmail = Just "cwilson@mail.uk"
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

    e6 <- insert $ Staff { staffName = "Philip Davis"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "phdavis@mail.uk"
                         , staffUser = Nothing
                         }

    case B64.decode man04 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e6
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e7 <- insert $ Staff { staffName = "Helen Taylor"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "htaylor@mail.uk"
                         , staffUser = Nothing
                         }

    case B64.decode woman03 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e7
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e8 <- insert $ Staff { staffName = "Barbara Young"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "byoung@mail.uk"
                         , staffUser = Nothing
                         }

    case B64.decode woman04 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e8
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e9 <- insert $ Staff { staffName = "Jorge Walker"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "jwalker@mail.uk"
                         , staffUser = Nothing
                         }

    case B64.decode man05 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e9
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e10 <- insert $ Staff { staffName = "Robert William Evans"
                         , staffStatus = EmplStatusAvailable
                         , staffPhone = businessPhone business
                         , staffMobile = businessMobile business
                         , staffEmail = Just "revans@mail.uk"
                         , staffUser = Nothing
                         }

    case B64.decode man06 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e10
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    pass11 <- liftIO $ makePassword "ihughes" 17
    let user11 = User { userName = "ihughes"
                      , userPassword = decodeUtf8 pass11
                      , userAdmin = False
                      , userFullName = Just "Isabel Hughes"
                      , userEmail = Just "ihughes@mail.uk"
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    let service11 = Service { serviceName = "Men haircuts"
                            , servicePublished = True
                            , serviceOverview = Just "Haircuts for men"
                            , serviceDescr = Just "Hair cuts for men"
                            , serviceDuration = duration "01:00"
                            , serviceGroup = Just s1
                            }

    s11 <- insert service11

    let role111 =  Role { roleStaff = e1
                        , roleService = s11
                        , roleName = "Barber"
                        , roleDuration = 3600
                        , roleRating = Just 5
                        } 

    r111 <- insert role111

    insert_ $ Role { roleStaff = e2
                   , roleService = s11
                   , roleName = "Barber"
                   , roleDuration = 3600
                   , roleRating = Just 5
                   }

    o111 <- insert $ Offer { offerService = s11
                           , offerName = "Price"
                           , offerPrice = 26
                           , offerPrefix = Just "$"
                           , offerSuffix = Nothing
                           , offerDescr = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s11
                        , thumbnailPhoto = $(embedFile "static/img/men-haircuts.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s12 <- insert $ Service { serviceName = "Women haircuts (above shoulders)"
                            , servicePublished = True
                            , serviceOverview = Just "Haircuts above shoulders for women"
                            , serviceDescr = Just "Haircuts above shoulders for women"
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s1
                            }

    insert_ $ Offer { offerService = s12
                    , offerName = "Price"
                    , offerPrice = 28
                    , offerPrefix = Just "$"
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
                   , roleName = "Hairdresser"
                   , roleDuration = 5400
                   , roleRating = Just 3
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s12
                   , roleName = "Hairdresser"
                   , roleDuration = 5400
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s12
                   , roleName = "Hairdresser"
                   , roleDuration = 5400
                   , roleRating = Just 5
                   }

    s13 <- insert $ Service { serviceName = "Women haircuts (below shoulders)"
                            , servicePublished = True
                            , serviceOverview = Just "Haircuts below shoulders for women"
                            , serviceDescr = Just "Haircuts below shoulders for women"
                            , serviceDuration = duration "01:35"
                            , serviceGroup = Just s1
                            }

    o131 <- insert $ Offer { offerService = s13
                           , offerName = "Price"
                           , offerPrice = 35
                           , offerPrefix = Just "$"
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
                        , roleName = "Stylist"
                        , roleDuration = 5580
                        , roleRating = Just 5
                        }

    r1311 <- insert role1311

    insert_ $ Role { roleStaff = e3
                   , roleService = s13
                   , roleName = "Assistant stylist"
                   , roleDuration = 5580
                   , roleRating = Just 4
                   }

    s14 <- insert $ Service { serviceName = "Children haircuts"
                            , servicePublished = True
                            , serviceOverview = Just "Haircuts for children"
                            , serviceDescr = Just "Haircuts for children"
                            , serviceDuration = duration "01:20"
                            , serviceGroup = Just s1
                            }

    o141 <- insert $ Offer { offerService = s14
                           , offerName = "Price"
                           , offerPrice = 16
                           , offerPrefix = Just "$"
                           , offerSuffix = Just "-$20 (depending on the length of their hair)"
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
                   , roleName = "Junior stylist"
                   , roleDuration = 4800
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e3
                   , roleService = s1511
                   , roleName = "Stylist"
                   , roleDuration = 5100
                   , roleRating = Just 4
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s1511
                   , roleName = "Stylist"
                   , roleDuration = 5100
                   , roleRating = Just 4
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1511
                   , roleName = "Stylist"
                   , roleDuration = 5100
                   , roleRating = Just 4
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

    insert_ $ Role { roleStaff = e4
                   , roleService = s1512
                   , roleName = "Stylist"
                   , roleDuration = 4500
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1512
                   , roleName = "Stylist"
                   , roleDuration = 4500
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s1512
                   , roleName = "Stylist"
                   , roleDuration = 4500
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s1512
                        , thumbnailPhoto = $(embedFile "static/img/before-perm-conditioner.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e8
                   , roleService = s1521
                   , roleName = "Colourist"
                   , roleDuration = 3600
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s1521
                   , roleName = "Colourist"
                   , roleDuration = 3600
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e10
                   , roleService = s1521
                   , roleName = "Colourist"
                   , roleDuration = 3600
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e10
                   , roleService = s1522
                   , roleName = "Colourist"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s1522
                   , roleName = "Colourist"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e9
                   , roleService = s1523
                   , roleName = "Colourist"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e10
                   , roleService = s1523
                   , roleName = "Colourist"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s1523
                   , roleName = "Colourist"
                   , roleDuration = 60 * (1 * 60 + 45)
                   , roleRating = Just 5
                   }

    s153 <- insert $ Service { serviceName = "Perm"
                             , servicePublished = True
                             , serviceOverview = Just "A permanent wave"
                             , serviceDescr = Just "Perm is a great way to give you a new look"
                             , serviceDuration = duration "00:45"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s153
                        , thumbnailPhoto = $(embedFile "static/img/perm.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
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

    insert_ $ Thumbnail { thumbnailService = s1531
                        , thumbnailPhoto = $(embedFile "static/img/full-perm.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e1
                   , roleService = s1531
                   , roleName = "Stylist"
                   , roleDuration = 60 * (0 * 60 + 35)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s1531
                   , roleName = "Stylist"
                   , roleDuration = 60 * (0 * 60 + 35) 
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s1531
                   , roleName = "Stylist"
                   , roleDuration = 60 * (0 * 60 + 35)
                   , roleRating = Just 5
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

    insert_ $ Thumbnail { thumbnailService = s1532
                        , thumbnailPhoto = $(embedFile "static/img/acid-repair-perm.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e3
                   , roleService = s1532
                   , roleName = "Stylist"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s1532
                   , roleName = "Stylist"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1532
                   , roleName = "Stylist"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
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

    insert_ $ Thumbnail { thumbnailService = s1533
                        , thumbnailPhoto = $(embedFile "static/img/japanese-straightening-perm.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1533
                   , roleName = "Stylist"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s1533
                   , roleName = "Stylist"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s1533
                   , roleName = "Stylist"
                   , roleDuration = 60 * (1 * 60 + 25)
                   , roleRating = Just 5
                   }

    s2 <- insert $ Service { serviceName = "Facial Treatments"
                           , servicePublished = True
                           , serviceOverview = Just "Facial Treatments"
                           , serviceDescr = Just $ Textarea [st|
<p>
Your face is an expressive canvass that shows experience and emotion. At one of the best salons around, our palette holds nourishing treatments, which enhances, emphasizes beauty, youth and color for your body. Before any facial, our professional esthetician will give you a consolidation and work from there You won’t believe the difference!
</p>
<p>All facial treatments include eyebrow shaping.</p>
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e8
                   , roleService = s21
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s21
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e10
                   , roleService = s21
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 60)
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e6
                   , roleService = s22
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s22
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s22
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e3
                   , roleService = s23
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s23
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s23
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s23
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 90)
                   , roleRating = Just 5
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e1
                   , roleService = s24
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s24
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s24
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    insert_ $ Role { roleStaff = e9
                   , roleService = s25
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e10
                   , roleService = s25
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s25
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s25
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s31 <- insert $ Service { serviceName = "Milk Peel (90 min)"
                            , servicePublished = True
                            , serviceOverview = Just "Safe skin exfoliation"
                            , serviceDescr = Just $ Textarea [st|
<p>
Milk Peel includes a natural extract from sour milk (lactic acid), papain (enzyme from papaya), salicylic acid (from natural plants), and a special penetration controller. Milk Peel provides a safe skin exfoliation process without the side effects phenol and TCA peels can cause.
</p>
<p>
Milk Peeling Formula performs the following functions:
</p>
<ul>
  <li>Generating maximum but gentle skin exfoliation.</li>
  <li>Stimulating proliferation of fibroblasts to increase dermal collagen and elastin.</li>
  <li>Normalizing skin cells and tissues.</li>
</ul>
<p>
As a result, the milk peel is a powerful skin resurfacing process with substantial dermal effects. It has several skin benefits such as:
</p>
<ol>
  <li>Removing wrinkles and visible fine lines on the skin.</li>
  <li>Diminishing pimple or acne marks.</li>
  <li>Reducing scars.</li>
  <li>Smoothing depressed pits on the skin.</li>
  <li>Decreasing stretch marks and birth marks.</li>
  <li>Polishing the skin gently for facial renewal.</li>
</ol>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s3
                            }

    insert_ $ Offer { offerService = s31
                    , offerName = "Price"
                    , offerPrice = 330
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e8
                   , roleService = s31
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s31
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s31
                        , thumbnailPhoto = $(embedFile "static/img/milk-peel.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s32 <- insert $ Service { serviceName = "Derma Roller"
                            , servicePublished = True
                            , serviceOverview = Just "Help your skin renew itself"
                            , serviceDescr = Just $ Textarea [st|
<h4>Benefits:</h4>
<ul>
  <li>Remove and treat stretch mark, acne scar, wrinkles.</li>
  <li>Anti-aging.</li>
  <li>Hair Loss Treatment or Hair Restoration.</li>
  <li>Cellulite Treatment and Cellulite Reduction.</li>
  <li>Replace the collagen you can help your skin renew itself and repair to a cosmetic level.</li>
</ul>
<h4>So what is a Derma Roller?</h4>
<p>
The Scientia Derma Roller is an incredible device which naturally increases the levels of collagen and elastin in your skin. Stretch marks, wrinkles, scars and uneven, pitted skin are all due to a lack of collagen – so by using a DermaRoller to replace the collagen you can help your skin renew itself and repair to a cosmetic level.
</p>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s3
                            }

    insert_ $ Offer { offerService = s32
                    , offerName = "Price"
                    , offerPrice = 330
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e7
                   , roleService = s32
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s32
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s32
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (1 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s32
                        , thumbnailPhoto = $(embedFile "static/img/derma-roller.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s33 <- insert $ Service { serviceName = "Micro-Dermabrasion"
                            , servicePublished = True
                            , serviceOverview = Just "Micro-Dermabrasion"
                            , serviceDescr = Just $ Textarea [st|
<p>
A painless procedure, it helps to remove acne scars, enlarged pores, facial lines, wrinkles, blackheads as well as sun damage etc. This technique also helps to thicken your collagen, which results in a younger looking complexion. Collagen is a protein in your skin that’s abundant when you’re a child and makes skin appear taut and smooth. Collagen production declines as we age, resulting in looser, uneven skin.
</p>
|]
                            , serviceDuration = duration "00:45"
                            , serviceGroup = Just s3
                            }

    insert_ $ Offer { offerService = s33
                    , offerName = "Price"
                    , offerPrice = 70
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e5
                   , roleService = s33
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s33
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s33
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s33
                        , thumbnailPhoto = $(embedFile "static/img/micro-dermabrasion.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s34 <- insert $ Service { serviceName = "Freckle Bleaching (120 min)"
                            , servicePublished = True
                            , serviceOverview = Just "A synergistic effect to improve skin tone"
                            , serviceDescr = Just $ Textarea [st|
<p>This treatment achieves a synergistic effect to improve skin tone, pigmentation problems as well as dark eye circles.</p>
|]
                            , serviceDuration = duration "02:00"
                            , serviceGroup = Just s3
                            }

    insert_ $ Offer { offerService = s34
                    , offerName = "Price"
                    , offerPrice = 95
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s34
                    , offerName = "Package"
                    , offerPrice = 600
                    , offerPrefix = Just "$"
                    , offerSuffix = Just "/7 sessions"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s34
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e4
                   , roleService = s34
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s34
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (2 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s34
                        , thumbnailPhoto = $(embedFile "static/img/freckle-bleaching.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s41 <- insert $ Service { serviceName = "Sea C Spa (100% Vitamin C)"
                            , servicePublished = True
                            , serviceOverview = Just "Restoring youthful appearance"
                            , serviceDescr = Just $ Textarea [st|
<p>
A powerful age-defying skin care designed to delay the visible signs of aging and energize the skin.
</p>
<p>
Formulated with marine and vegetal ingredients (Vitamin C Concentrate, Algae Biomatrix Patches, and Thermal Organic Mud).
</p>
<p>
This treatment is exceptional for both pre and post-sun exposure. Ideal for people who live in urban areas with high pollution levels.
</p>
<h4>Benefits:</h4>
<p>
This treatment reduces the appearance of lines and wrinkles Evens out skin tone And illuminates the skin, thus restoring its youthful appearance
</p>
|]
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s41
                    , offerName = "Price"
                    , offerPrice = 95
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s41
                    , offerName = "Package"
                    , offerPrice = 430
                    , offerPrefix = Just "$"
                    , offerSuffix = Just "/5 sessions"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e2
                   , roleService = s41
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s41
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 60)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s41
                        , thumbnailPhoto = $(embedFile "static/img/sea-C-spa.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s42 <- insert $ Service { serviceName = "Botinol Facial (150 min)"
                            , servicePublished = True
                            , serviceOverview = Just "Mask the signs of aging"
                            , serviceDescr = Just $ Textarea [st|
<p>
A highly advanced elective treatment formulated to hydrate, regenerate and reduce the appearance of lines & wrinkles.
</p>
<p>
This relaxing treatment session offers pleasant textures and exquisite essences delivering an overall sense of well-being. An ideal treatment for people determined to mask the signs of aging.
</p>
<h4>Benefits:</h4>
<p>
After just one treatment, expression lines appear relaxed Lines and wrinkles are visibly reduced The skin appears visibly younger.
</p>
|]
                            , serviceDuration = duration "02:30"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s42
                    , offerName = "Price"
                    , offerPrice = 170
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s42
                    , offerName = "Package"
                    , offerPrice = 780
                    , offerPrefix = Just "$"
                    , offerSuffix = Just "/5 sessions"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e1
                   , roleService = s42
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s42
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s42
                        , thumbnailPhoto = $(embedFile "static/img/botinol-facial.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s43 <- insert $ Service { serviceName = "Collagen 90-II (150 min)"
                            , servicePublished = True
                            , serviceOverview = Just "Hydrated and radiant"
                            , serviceDescr = Just $ Textarea [st|
<p>
This hydrating age-defying treatment provides intensive hydration, cellular regeneration, and epidermal renewal.  With age, the cellular renewal process is delayed. Facial contours lose their definition and the skin appears to sag.
</p>
<p>
Collagen 90-II is an exclusive age-defying treatment by G.M. Collin Skin Care that associates a pure native collagen sheet with selected ingredients, to provide intense hydration, firm the skin, and reduce the appearance of aging.
</p>
<p>
Collagen 90-II is a highly respected and sought-after age-defying skin renewing treatment that hydrates, smoothes and tones wrinkles to combat the visible signs of aging.
</p>
<h4>Benefits:</h4>
<p>
This treatment improves the overall complexion by minimizing the appearance of fine lines and wrinkles. It restores moisture, leaving the skin well hydrated and radiant. Recommended for all skin types
</p>
|]
                            , serviceDuration = duration "02:30"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s43
                    , offerName = "Price"
                    , offerPrice = 160
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Offer { offerService = s43
                    , offerName = "Package"
                    , offerPrice = 730
                    , offerPrefix = Just "$"
                    , offerSuffix = Just "/5 sessions"
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e10
                   , roleService = s43
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s43
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (2 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s43
                        , thumbnailPhoto = $(embedFile "static/img/collagen-90-II.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s44 <- insert $ Service { serviceName = "Electro Ionization"
                            , servicePublished = True
                            , serviceOverview = Just "Less wrinkles in just 15 min!"
                            , serviceDescr = Just $ Textarea [st|
<p>
It promotes collagen production, tightens and firms skin tissues, hydrates & rejuvenates the epidermis, treats acne and fine lines. Less wrinkles in just 15 min!
</p>
|]
                            , serviceDuration = duration "00:15"
                            , serviceGroup = Just s4
                            }

    insert_ $ Offer { offerService = s44
                    , offerName = "Price"
                    , offerPrice = 180
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s44
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s44
                        , thumbnailPhoto = $(embedFile "static/img/electro-ionization.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s5 <- insert $ Service { serviceName = "Eye Treatments"
                           , servicePublished = True
                           , serviceOverview = Just "Eye Treatment Center"
                           , serviceDescr = Just "Eye Treatment Center"
                           , serviceDuration = duration "02:00"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s5
                        , thumbnailPhoto = $(embedFile "static/img/eye-treatment-center.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s51 <- insert $ Service { serviceName = "Eyelash Perm"
                            , servicePublished = True
                            , serviceOverview = Just "Eyelash Perm"
                            , serviceDescr = Just $ Textarea [st|
<p>
Our technique and eyelash perm solution have been refined over a decade to work consistently and gently on your lashes.
</p>
<p>
Despite the name, eyelash perms are not permanent. The results of an eyelash perm typically last from 2-3 months, or the natural growth cycle of the eyelashes. When the permed eyelashes naturally fall out, the new eyelash hair will grow in straight (as it was before your eyelash perm).
</p>
<p>
Before your eyelash perm, you will be asked to remove your contact lenses and eye makeup (complimentary makeup remover, lens solution and temporary lens cups are readily available for you.) During your eyelash perm, which takes approximately 1 hour, you may lay back and relax, and take in the sounds of our soothing spa music. Please keep your eyes closed and relaxed for the duration of the treatment for the best eyelash perm results.
</p>
<p>
Similar to a regular hair perm, try to avoid wetting your eyelashes for 4 hours after your eyelash perm. We recommend applying Eyelash Conditioner to the eyelashes daily. Conditioning eyelash gel and mascara is available in the spa, by phone, and through our online store. Using Eyelash Conditioner regularly will ensure that your lashes stay healthy.
</p>
|]
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s51
                    , offerName = "Price"
                    , offerPrice = 45
                    , offerPrefix = Just "$"
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

    s52 <- insert $ Service { serviceName = "Eyelash Extension"
                            , servicePublished = True
                            , serviceOverview = Just "Eyelash Extension"
                            , serviceDescr = Just $ Textarea [st|
<h4>Eyelash Extension Home Care</h4>
<ol>
  <li>Within the first 2 hours, do not allow water to be in contact with the lashes.</li>
  <li>Within the first 2 hours, do not steam face, use steam bath, swim or wash face with hot water.</li>
  <li>Use only washable mascara, and lightly brush through the tip of lashes.</li>
  <li>Do not use mascara on lashes adhesive area with mascara remover.</li>
  <li>Do not perm lashes.</li>
  <li>Do not use a lash curler as it will break both lashes and natural lashes.</li>
  <li>Do not rub your eyes or lashes.</li>
  <li>When washing your face, always pat the lashes dry after cleansing.</li>
</ol>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s52
                    , offerName = "Price"
                    , offerPrice = 130
                    , offerPrefix = Just "$"
                    , offerSuffix = Just " & up"
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

    s53 <- insert $ Service { serviceName = "Eyebrow or Eyelash Tint"
                            , servicePublished = True
                            , serviceOverview = Just "Eyebrow or Eyelash Tint"
                            , serviceDescr = Just $ Textarea [st|Eyebrow or Eyelash Tint|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s53
                    , offerName = "Price"
                    , offerPrice = 25
                    , offerPrefix = Just "$"
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

    s54 <- insert $ Service { serviceName = "Eye Rejuvenating Treatment"
                            , servicePublished = True
                            , serviceOverview = Just "A synergistic effect on eye area"
                            , serviceDescr = Just $ Textarea [st|
<p>
This is elegantly designed to gently reduce puffiness and wrinkles as well as dark circles around the eyes. It’s a powerful eye treatment that encompasses a number of Rejuvi products, such as: “i” Eye Repair Gel, Fruit Complex, Vitamin A Complex, Vitamin C Complex, Contour Formula and Facial Mask, etc., to impart a synergistic effect on eye area. Amazing results can be seen immediately following the procedure.
</p>
<p>
Treatments can be performed every week or every other week, depending upon skin condition. A complete eye treatment should consist of 4 – 6 applications of the individual treatments, plus a home care program.
</p>
|]
                            , serviceDuration = duration "01:30"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s54
                    , offerName = "Price"
                    , offerPrice = 40
                    , offerPrefix = Just "$"
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

    s55 <- insert $ Service { serviceName = "Anti-Aging Eye Treatment"
                            , servicePublished = True
                            , serviceOverview = Just "Anti-Aging Eye Treatment"
                            , serviceDescr = Just $ Textarea [st||]
                            , serviceDuration = duration "01:45"
                            , serviceGroup = Just s5
                            }

    insert_ $ Offer { offerService = s55
                    , offerName = "Price"
                    , offerPrice = 40
                    , offerPrefix = Just "$"
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s61 <- insert $ Service { serviceName = "Swedish Massage"
                            , servicePublished = True
                            , serviceOverview = Just "Swedish massage therapy"
                            , serviceDescr = Just $ Textarea [st|
<p>
Swedish massage therapy is the modality that comes to mind when most people think about massage. As the best-known type of bodywork performed today, one of the primary goals of the Swedish massage technique is to relax the entire body. This is accomplished by rubbing the muscles with long gliding strokes in the direction of blood returning to the heart. But Swedish massage therapy goes beyond relaxation. Swedish massage is exceptionally beneficial for increasing the level of oxygen in the blood, decreasing muscle toxins, improving circulation and flexibility while easing tension.
</p>
|]
                            , serviceDuration = duration "01:00"
                            , serviceGroup = Just s6
                            }

    insert_ $ Offer { offerService = s61
                    , offerName = "Price"
                    , offerPrice = 60
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s61
                   , roleName = "Massage therapist"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s61
                   , roleName = "Massage therapist"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s61
                   , roleName = "Massage therapist"
                   , roleDuration = 60 * (1 * 60 + 0)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s61
                        , thumbnailPhoto = $(embedFile "static/img/swedish-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s62 <- insert $ Service { serviceName = "Chair Massage"
                            , servicePublished = True
                            , serviceOverview = Just "Chair Massage"
                            , serviceDescr = Just $ Textarea [st|
<p>
Chair massage is a 15-20 minute massage focused on the back, shoulders, neck, arms, and head. It is designed to relax the muscles and improve flexibility and movement.
</p>
|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s6
                            }

    insert_ $ Offer { offerService = s62
                    , offerName = "Price"
                    , offerPrice = 60
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e10
                   , roleService = s62
                   , roleName = "Massage therapist"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s62
                        , thumbnailPhoto = $(embedFile "static/img/chair-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s63 <- insert $ Service { serviceName = "Foot Massage"
                            , servicePublished = True
                            , serviceOverview = Just "Foot Massage"
                            , serviceDescr = Just $ Textarea [st|
<p>
A foot massage is a therapeutic practice that involves manipulating the feet and applying pressure to specific areas to promote relaxation, relieve tension, and improve overall well-being. Unlike reflexology, which is a specialized practice that focuses on specific reflex points on the feet that correspond to different organs and systems in the body, a foot massage typically involves a more general approach.
</p>
<p>
During a foot massage, various techniques may be used, including kneading, stroking, rubbing, and applying pressure with hands, fingers, or specialized tools. The massage may target specific areas of the feet, such as the arches, heels, toes, and the balls of the feet, as well as the surrounding muscles and joints.
</p>
<h4>Benefits:</h4>
<ol>
  <li>Relaxation: Foot massages can help reduce stress and induce a state of relaxation.</li>
  <li>Improved circulation: The massage techniques used can enhance blood flow to the feet and lower extremities, promoting better circulation.</li>
  <li>Pain relief: Foot massages may help alleviate foot pain, including conditions like plantar fasciitis or general foot discomfort.</li>
  <li>Reduced muscle tension: By targeting specific muscles in the feet, a massage can help release tension and improve flexibility.</li>
  <li>Enhanced overall well-being: Foot massages are believed to have a positive impact on the body’s energy flow, contributing to a sense of well-being.</li>
</ol>
|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s6
                            }

    insert_ $ Offer { offerService = s63
                    , offerName = "Price"
                    , offerPrice = 30
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e1
                   , roleService = s63
                   , roleName = "Massage therapist"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s63
                   , roleName = "Massage therapist"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s63
                        , thumbnailPhoto = $(embedFile "static/img/foot-massage.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s7 <- insert $ Service { serviceName = "Makeup Services"
                           , servicePublished = True
                           , serviceOverview = Just "Makeup Services"
                           , serviceDescr = Just "Makeup Services"
                           , serviceDuration = duration "03:30"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s7
                        , thumbnailPhoto = $(embedFile "static/img/makeup-services.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s71 <- insert $ Service { serviceName = "Wedding Special"
                            , servicePublished = True
                            , serviceOverview = Just "Wedding Special"
                            , serviceDescr = Just $ Textarea [st|
<p>
Package include: Bridal Make-up, Up-do, Facial Treatment and Manicure
</p>
|]
                            , serviceDuration = duration "03:30"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s71
                    , offerName = "Price"
                    , offerPrice = 200
                    , offerPrefix = Just "$"
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

    s72 <- insert $ Service { serviceName = "Bridal Makeup"
                            , servicePublished = True
                            , serviceOverview = Just "Bridal Makeup (45 min)"
                            , serviceDescr = Just $ Textarea [st|Bridal Makeup (45 min)|]
                            , serviceDuration = duration "00:45"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s72
                    , offerName = "Price"
                    , offerPrice = 85
                    , offerPrefix = Just "$"
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

    s73 <- insert $ Service { serviceName = "Evening Makeup"
                            , servicePublished = True
                            , serviceOverview = Just "Evening Makeup (30 min)"
                            , serviceDescr = Just $ Textarea [st|Evening Makeup (30 min)|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s73
                    , offerName = "Price"
                    , offerPrice = 60
                    , offerPrefix = Just "$"
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

    s74 <- insert $ Service { serviceName = "Makeup Lesson"
                            , servicePublished = True
                            , serviceOverview = Just "Makeup Lesson (60 min)"
                            , serviceDescr = Just $ Textarea [st|Makeup Lesson (60 min)|]
                            , serviceDuration = duration "00:60"
                            , serviceGroup = Just s7
                            }

    insert_ $ Offer { offerService = s74
                    , offerName = "Price"
                    , offerPrice = 100
                    , offerPrefix = Just "$"
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s81 <- insert $ Service { serviceName = "Body Waxing"
                            , servicePublished = True
                            , serviceOverview = Just "Body Waxing"
                            , serviceDescr = Just $ Textarea [st|Body Waxing|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s8
                            }

    insert_ $ Offer { offerService = s81
                    , offerName = "Price"
                    , offerPrice = 50
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e8
                   , roleService = s81
                   , roleName = "Waxing specialist"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s81
                   , roleName = "Waxing specialist"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s81
                   , roleName = "Waxing specialist"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s81
                        , thumbnailPhoto = $(embedFile "static/img/body-waxing.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s82 <- insert $ Service { serviceName = "Face Waxing"
                            , servicePublished = True
                            , serviceOverview = Just "Face Waxing"
                            , serviceDescr = Just $ Textarea [st|Face Waxing|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s8
                            }

    insert_ $ Offer { offerService = s82
                    , offerName = "Price"
                    , offerPrice = 45
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s82
                   , roleName = "Waxing specialist"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s82
                   , roleName = "Waxing specialist"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s82
                        , thumbnailPhoto = $(embedFile "static/img/face-waxing.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s9 <- insert $ Service { serviceName = "Nail Care"
                           , servicePublished = True
                           , serviceOverview = Just "Nail Services"
                           , serviceDescr = Just "Nail Care Services"
                           , serviceDuration = duration "00:45"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s9
                        , thumbnailPhoto = $(embedFile "static/img/nail-care.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s91 <- insert $ Service { serviceName = "Manicure"
                            , servicePublished = True
                            , serviceOverview = Just "Manicure"
                            , serviceDescr = Just $ Textarea [st|Manicure|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s91
                    , offerName = "Price"
                    , offerPrice = 15
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e9
                   , roleService = s91
                   , roleName = "Nail technician"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e8
                   , roleService = s91
                   , roleName = "Nail technician"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s91
                   , roleName = "Nail technician"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s91
                        , thumbnailPhoto = $(embedFile "static/img/manicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s92 <- insert $ Service { serviceName = "No-Chip Manicure"
                            , servicePublished = True
                            , serviceOverview = Just "No- Chip Manicure"
                            , serviceDescr = Just $ Textarea [st|No-Chip Manicure|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s92
                    , offerName = "Price"
                    , offerPrice = 32
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e10
                   , roleService = s92
                   , roleName = "Nail technician"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e9
                   , roleService = s92
                   , roleName = "Nail technician"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s92
                        , thumbnailPhoto = $(embedFile "static/img/no-chip-manicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s93 <- insert $ Service { serviceName = "Pedicure"
                            , servicePublished = True
                            , serviceOverview = Just "Pedicure"
                            , serviceDescr = Just $ Textarea [st|Pedicure|]
                            , serviceDuration = duration "00:30"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s93
                    , offerName = "Price"
                    , offerPrice = 35
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e1
                   , roleService = s93
                   , roleName = "Nail technician"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e11
                   , roleService = s93
                   , roleName = "Nail technician"
                   , roleDuration = 60 * (0 * 60 + 30)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s93
                        , thumbnailPhoto = $(embedFile "static/img/pedicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s94 <- insert $ Service { serviceName = "No-Chip Pedicure"
                            , servicePublished = True
                            , serviceOverview = Just "No-Chip Pedicure"
                            , serviceDescr = Just $ Textarea [st|No-Chip Pedicure|]
                            , serviceDuration = duration "00:40"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s94
                    , offerName = "Price"
                    , offerPrice = 55
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e2
                   , roleService = s94
                   , roleName = "Nail technician"
                   , roleDuration = 60 * (0 * 60 + 40)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e1
                   , roleService = s94
                   , roleName = "Nail technician"
                   , roleDuration = 60 * (0 * 60 + 40)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s94
                        , thumbnailPhoto = $(embedFile "static/img/no-chip-pedicure.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s95 <- insert $ Service { serviceName = "Acrylic Nail-Full Set"
                            , servicePublished = True
                            , serviceOverview = Just "Acrylic Nail-Full Set"
                            , serviceDescr = Just $ Textarea [st|Acrylic Nail-Full Set|]
                            , serviceDuration = duration "00:25"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s95
                    , offerName = "Price"
                    , offerPrice = 38
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e3
                   , roleService = s95
                   , roleName = "Nail technician"
                   , roleDuration = 60 * (0 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e2
                   , roleService = s95
                   , roleName = "Nail technician"
                   , roleDuration = 60 * (0 * 60 + 25)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s95
                        , thumbnailPhoto = $(embedFile "static/img/acrylic-nail-full-set.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s96 <- insert $ Service { serviceName = "Hand Paraffin Dip"
                            , servicePublished = True
                            , serviceOverview = Just "Hand Paraffin Dip"
                            , serviceDescr = Just $ Textarea [st|Hand Paraffin Dip|]
                            , serviceDuration = duration "00:20"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s96
                    , offerName = "Price"
                    , offerPrice = 10
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e4
                   , roleService = s96
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e3
                   , roleService = s96
                   , roleName = "Esthetician"
                   , roleDuration = 60 * (0 * 60 + 20)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s96
                        , thumbnailPhoto = $(embedFile "static/img/hand-paraffin-dip.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s97 <- insert $ Service { serviceName = "Polish Change"
                            , servicePublished = True
                            , serviceOverview = Just "Polish Change"
                            , serviceDescr = Just $ Textarea [st|Polish Change|]
                            , serviceDuration = duration "00:15"
                            , serviceGroup = Just s9
                            }

    insert_ $ Offer { offerService = s97
                    , offerName = "Price"
                    , offerPrice = 5
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e6
                   , roleService = s97
                   , roleName = "Nail technician"
                   , roleDuration = 60 * (0 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e5
                   , roleService = s97
                   , roleName = "Nail technician"
                   , roleDuration = 60 * (0 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s97
                        , thumbnailPhoto = $(embedFile "static/img/polish-change.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
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
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s101 <- insert $ Service { serviceName = "Body Shaping"
                             , servicePublished = True
                             , serviceOverview = Just "Abdomen & waist, hips & thighs, legs & arms"
                             , serviceDescr = Just $ Textarea [st|
<p>
Body Shaping: Abdomen & waist, hips & thighs, legs & arms
</p>
|]
                             , serviceDuration = duration "01:15"
                             , serviceGroup = Just s10
                             }

    insert_ $ Offer { offerService = s101
                    , offerName = "Price"
                    , offerPrice = 350
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e7
                   , roleService = s101
                   , roleName = "Aesthetician"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e6
                   , roleService = s101
                   , roleName = "Aesthetician"
                   , roleDuration = 60 * (1 * 60 + 15)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s101
                        , thumbnailPhoto = $(embedFile "static/img/body-shaping.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }

    s102 <- insert $ Service { serviceName = "Face Shaping"
                             , servicePublished = True
                             , serviceOverview = Just "Face Shaping"
                             , serviceDescr = Just $ Textarea [st|Face Shaping|]
                             , serviceDuration = duration "00:45"
                             , serviceGroup = Just s10
                             }

    insert_ $ Offer { offerService = s102
                    , offerName = "Price"
                    , offerPrice = 300
                    , offerPrefix = Just "$"
                    , offerSuffix = Nothing
                    , offerDescr = Nothing
                    }

    insert_ $ Role { roleStaff = e8
                   , roleService = s102
                   , roleName = "Aesthetician"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Role { roleStaff = e7
                   , roleService = s102
                   , roleName = "Aesthetician"
                   , roleDuration = 60 * (0 * 60 + 45)
                   , roleRating = Just 5
                   }

    insert_ $ Thumbnail { thumbnailService = s102
                        , thumbnailPhoto = $(embedFile "static/img/face-shaping.avif")
                        , thumbnailMime = "image/avif"
                        , thumbnailAttribution = Just [shamlet|
                              Designed by <a href="https://www.freepik.com/" target=_blank>Freepik</a>|]
                        }


    pass6 <- liftIO $ makePassword "pattyofurniture" 17
    c1 <- insert $ User { userName = "pattyofurniture"
                        , userPassword = decodeUtf8 pass6
                        , userAdmin = False
                        , userFullName = Just "Patty O’Furniture"
                        , userEmail = Just "pattyofurniture@mail.org"
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


    pass7 <- liftIO $ makePassword "raysin" 17
    c2 <- insert $ User { userName = "raysin"
                        , userPassword = decodeUtf8 pass7
                        , userAdmin = False
                        , userFullName = Just "Ray Sin"
                        , userEmail = Just "raysin@mail.org"
                        }

    insert_ $ UserPhoto { userPhotoUser = c2
                        , userPhotoPhoto = $(embedFile "static/img/customer-men-1.avif")
                        , userPhotoMime = "image/avif"
                        }

    let book2 = Book { bookOffer = o111
                     , bookRole = Just r111
                     , bookCustomer = c2
                     , bookDay = today
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
                     , bookDay = addDays 1 today
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

    return ()
  where
      duration :: String -> Maybe DiffTime
      duration = parseTimeM True defaultTimeLocale "%H:%M"
