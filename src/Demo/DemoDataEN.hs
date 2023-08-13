{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.DemoDataEN (populateEN) where

import qualified Data.ByteString.Base64 as B64 (decode)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad.IO.Class (MonadIO (liftIO))
import ClassyPrelude.Yesod (ReaderT)
import Yesod.Auth.Util.PasswordStore (makePassword)
import Database.Persist.Sql (SqlBackend)
import Database.Persist ( PersistStoreWrite(insert_, insert) )

import Model
    ( User (User, userName, userPassword, userEmail, userFullName)
    , Service
      ( Service, serviceName, serviceDescr, serviceGroup, serviceOverview
      , servicePublished
      )
    , Thumbnail (Thumbnail, thumbnailService, thumbnailPhoto, thumbnailMime)
    , Pricelist
      ( Pricelist, pricelistName, pricelistPrice, pricelistPrefix
      , pricelistSuffix, pricelistDescr, pricelistService
      )
    , Staff (Staff, staffName, staffPhone, staffMobile, staffEmail)
    , StaffPhoto (StaffPhoto, staffPhotoPhoto, staffPhotoMime, staffPhotoStaff)
    , Role (Role, roleStaff, roleService, roleName, roleRating)
    )
import Data.FileEmbed (embedFile)
import Demo.DemoPhotos
    ( man01, man02, man03, man04, man05, man06
    , woman01, woman02, woman03, woman04, woman05
    )

populateEN :: MonadIO m => ReaderT SqlBackend m ()
populateEN = do
    password <- liftIO $ makePassword "root" 17
    insert_ $ User { userName = "root"
                   , userPassword = decodeUtf8 password
                   , userFullName = Just "Smith James"
                   , userEmail = Just "jsmith@mail.en"
                   }

    e1 <- insert $ Staff { staffName = "Johnny Smith"
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "jsmith@mail.en"
                         }

    case B64.decode man01 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e1
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e2 <- insert $ Staff { staffName = "Mary Lopez"
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "mlopez@mail.en"
                         }

    case B64.decode woman01 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e2
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e3 <- insert $ Staff { staffName = "John Johnson"
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "jjohnson@mail.en"
                         }

    case B64.decode man02 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e3
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e4 <- insert $ Staff { staffName = "Patricia Brown"
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "pbrown@mail.en"
                         }

    case B64.decode woman02 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e4
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e5 <- insert $ Staff { staffName = "Chris Wilson"
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "cwilson@mail.en"
                         }

    case B64.decode man03 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e5
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e6 <- insert $ Staff { staffName = "Philip Davis"
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "phdavis@mail.en"
                         }

    case B64.decode man04 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e6
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e7 <- insert $ Staff { staffName = "Helen Taylor"
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "htaylor@mail.en"
                         }

    case B64.decode woman03 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e7
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e8 <- insert $ Staff { staffName = "Barbara Young"
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "byoung@mail.en"
                         }

    case B64.decode woman04 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e8
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e9 <- insert $ Staff { staffName = "Jorge Walker"
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "jwalker@mail.en"
                         }

    case B64.decode man05 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e9
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e10 <- insert $ Staff { staffName = "Robert William Evans"
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "revans@mail.en"
                         }

    case B64.decode man06 of
      Left _ -> return ()
      Right x -> insert_ $ StaffPhoto { staffPhotoStaff = e10
                                      , staffPhotoPhoto = x
                                      , staffPhotoMime = "image/avif"
                                      }

    e11 <- insert $ Staff { staffName = "Isabel Hughes"
                         , staffPhone = Just "0491 570 006"
                         , staffMobile = Just "0491 570 156"
                         , staffEmail = Just "ihughes@mail.en"
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
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s1
                        , thumbnailPhoto = $(embedFile "static/img/hair-care.svg")
                        , thumbnailMime = "image/svg+xml"
                        }

    s11 <- insert $ Service { serviceName = "Men hair cuts"
                            , servicePublished = True
                            , serviceOverview = Just "Hair cuts for men"
                            , serviceDescr = Just "Hair cuts for men"
                            , serviceGroup = Just s1
                            }

    insert_ $ Role { roleStaff = e1
                   , roleService = s11
                   , roleName = "Makeup artist"
                   , roleRating = Just 5
                   }

    insert_ $ Pricelist { pricelistService = s11
                        , pricelistName = "Price"
                        , pricelistPrice = 26
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Nothing
                        , pricelistDescr = Nothing
                        }

    insert_ $ Thumbnail { thumbnailService = s11
                        , thumbnailPhoto = $(embedFile "static/img/man-haircut.svg")
                        , thumbnailMime = "image/svg+xml"
                        }

    s12 <- insert $ Service { serviceName = "Women hair cuts (above shoulders)"
                            , servicePublished = True
                            , serviceOverview = Just "Hair cuts above shoulders for women"
                            , serviceDescr = Just "Hair cuts above shoulders for women"
                            , serviceGroup = Just s1
                            }

    insert_ $ Pricelist { pricelistService = s12
                        , pricelistName = "Price"
                        , pricelistPrice = 28
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Nothing
                        , pricelistDescr = Nothing
                        }

    insert_ $ Thumbnail { thumbnailService = s12
                        , thumbnailPhoto = $(embedFile "static/img/women-profile-hair-short.svg")
                        , thumbnailMime = "image/svg+xml"
                        }

    insert_ $ Role { roleStaff = e2
                   , roleService = s12
                   , roleName = "Barbers"
                   , roleRating = Just 4.8
                   }

    s13 <- insert $ Service { serviceName = "Women hair cuts (below shoulders)"
                            , servicePublished = True
                            , serviceOverview = Just "Hair cuts below shoulders for women"
                            , serviceDescr = Just "Hair cuts below shoulders for women"
                            , serviceGroup = Just s1
                            }

    insert_ $ Pricelist { pricelistService = s13
                        , pricelistName = "Price"
                        , pricelistPrice = 35
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Nothing
                        , pricelistDescr = Nothing
                        }

    insert_ $ Thumbnail { thumbnailService = s13
                        , thumbnailPhoto = $(embedFile "static/img/women-profile-hair-long.svg")
                        , thumbnailMime = "image/svg+xml"
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
                            , serviceGroup = Just s1
                            }

    insert_ $ Pricelist { pricelistService = s14
                        , pricelistName = "Price"
                        , pricelistPrice = 16
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Just "-$20 (depending on the length of their hair)"
                        , pricelistDescr = Nothing
                        }

    insert_ $ Thumbnail { thumbnailService = s14
                        , thumbnailPhoto = $(embedFile "static/img/child-haircut.svg")
                        , thumbnailMime = "image/svg+xml"
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
                            , serviceGroup = Just s1
                            }

    insert_ $ Thumbnail { thumbnailService = s15
                        , thumbnailPhoto = $(embedFile "static/img/conditioning.svg")
                        , thumbnailMime = "image/svg+xml"
                        }

    s151 <- insert $ Service { serviceName = "Conditioning"
                             , servicePublished = True
                             , serviceOverview = Just "Conditioning services"
                             , serviceDescr = Just "Conditioning"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s151
                        , thumbnailPhoto = $(embedFile "static/img/chemical-services.svg")
                        , thumbnailMime = "image/svg+xml"
                        }

    s1511 <- insert $ Service { serviceName = "After Perm Conditioner"
                              , servicePublished = True
                              , serviceOverview = Just "After Perm Conditioner"
                              , serviceDescr = Just "After Perm Conditioner"
                              , serviceGroup = Just s151
                              }

    insert_ $ Pricelist { pricelistService = s1511
                        , pricelistName = "Price"
                        , pricelistPrice = 99
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Just " & up"
                        , pricelistDescr = Nothing
                        }

    insert_ $ Role { roleStaff = e5
                   , roleService = s1511
                   , roleName = "Hairdresser"
                   , roleRating = Just 4.9
                   }

    s1512 <- insert $ Service { serviceName = "Before Perm Conditioner"
                              , servicePublished = True
                              , serviceOverview = Just "Before Perm Conditioner"
                              , serviceDescr = Just "Before Perm Conditioner"
                              , serviceGroup = Just s151
                              }

    insert_ $ Pricelist { pricelistService = s1512
                        , pricelistName = "Price"
                        , pricelistPrice = 110
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Just " & up"
                        , pricelistDescr = Nothing
                        }

    s152 <- insert $ Service { serviceName = "Highlights & Color"
                             , servicePublished = True
                             , serviceOverview = Just "Highlights & Color"
                             , serviceDescr = Just "Highlights & Color"
                             , serviceGroup = Just s15
                             }

    insert_ $ Thumbnail { thumbnailService = s152
                        , thumbnailPhoto = $(embedFile "static/img/hair-highlights-color.svg")
                        , thumbnailMime = "image/svg+xml"
                        }

    s1521 <- insert $ Service { serviceName = "Full"
                              , servicePublished = True
                              , serviceOverview = Just "Full"
                              , serviceDescr = Just "Full"
                              , serviceGroup = Just s152
                              }

    insert_ $ Pricelist { pricelistService = s1521
                        , pricelistName = "Price"
                        , pricelistPrice = 130
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Nothing
                        , pricelistDescr = Nothing
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
                              , serviceGroup = Just s152
                              }

    insert_ $ Pricelist { pricelistService = s1522
                        , pricelistName = "Price"
                        , pricelistPrice = 68
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Just " & up"
                        , pricelistDescr = Nothing
                        }

    s1523 <- insert $ Service { serviceName = "Permanent Color"
                              , servicePublished = True
                              , serviceOverview = Just "Permanent Color"
                              , serviceDescr = Just "Permanent Color"
                              , serviceGroup = Just s152
                              }

    insert_ $ Pricelist { pricelistService = s1523
                        , pricelistName = "Price"
                        , pricelistPrice = 68
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Just " & up"
                        , pricelistDescr = Nothing
                        }

    s153 <- insert $ Service { serviceName = "Perm"
                             , servicePublished = True
                             , serviceOverview = Just "Perm"
                             , serviceDescr = Just "Perm"
                             , serviceGroup = Just s15
                             }

    s1531 <- insert $ Service { serviceName = "Full Perm"
                              , servicePublished = True
                              , serviceOverview = Just "Full Perm"
                              , serviceDescr = Just "Full Perm"
                              , serviceGroup = Just s153
                              }

    insert_ $ Pricelist { pricelistService = s1531
                        , pricelistName = "Price"
                        , pricelistPrice = 79
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Just " & up"
                        , pricelistDescr = Nothing
                        }

    s1532 <- insert $ Service { serviceName = "Acid Repair Perm"
                              , servicePublished = True
                              , serviceOverview = Just "Acid Repair Perm"
                              , serviceDescr = Just "Acid Repair Perm"
                              , serviceGroup = Just s153
                              }

    insert_ $ Pricelist { pricelistService = s1532
                        , pricelistName = "Price"
                        , pricelistPrice = 89
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Just " & up"
                        , pricelistDescr = Nothing
                        }

    s1533 <- insert $ Service { serviceName = "Japanese Straightening Perm"
                              , servicePublished = True
                              , serviceOverview = Just "Japanese Straightening Perm"
                              , serviceDescr = Just "Japanese Straightening Perm"
                              , serviceGroup = Just s153
                              }

    insert_ $ Pricelist { pricelistService = s1533
                        , pricelistName = "Price"
                        , pricelistPrice = 250
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Just " & up"
                        , pricelistDescr = Nothing
                        }

    s2 <- insert $ Service { serviceName = "Facial Treatments"
                           , servicePublished = True
                           , serviceOverview = Just "Facial Treatments"
                           , serviceDescr = Just "<p>Your face is an expressive canvass that shows experience and emotion. At one of the best salons around, our palette holds nourishing treatments, which enhances, emphasizes beauty, youth and color for your body. Before any facial, our professional esthetician will give you a consolidation and work from there You won’t believe the difference!</p><p>All facial treatments include eyebrow shaping.</p>"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s2
                        , thumbnailPhoto = $(embedFile "static/img/facial-treatments.svg")
                        , thumbnailMime = "image/svg+xml"
                        }

    s21 <- insert $ Service { serviceName = "Basic Facial (60 min)"
                            , servicePublished = True
                            , serviceOverview = Just "Basic Facial (60 min)"
                            , serviceDescr = Just "Free Deep cleansing, exfoliation with steam treatment, followed by extractions, then eyebrow shaping; a de-stressing massage for the face, neck & shoulders. A custom mask, plus regular eye treatment, followed by moisturizer/sunscreen application. This relaxing but serious cleansing treatment will leave you with a clean, fresh, & glowing complexion."
                            , serviceGroup = Just s2
                            }

    insert_ $ Pricelist { pricelistService = s21
                        , pricelistName = "Price"
                        , pricelistPrice = 55
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Nothing
                        , pricelistDescr = Nothing
                        }

    insert_ $ Pricelist { pricelistService = s21
                        , pricelistName = "Package"
                        , pricelistPrice = 250
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Just "/5 sessions"
                        , pricelistDescr = Nothing
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
                            , serviceGroup = Just s2
                            }

    insert_ $ Pricelist { pricelistService = s22
                        , pricelistName = "Price"
                        , pricelistPrice = 75
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Nothing
                        , pricelistDescr = Nothing
                        }

    insert_ $ Pricelist { pricelistService = s22
                        , pricelistName = "Package"
                        , pricelistPrice = 350
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Just "/5 sessions"
                        , pricelistDescr = Nothing
                        }

    s23 <- insert $ Service { serviceName = "Pampering Facial (90 min)"
                            , servicePublished = True
                            , serviceOverview = Just "Pampering Facial (90 min)"
                            , serviceDescr = Just "A hydrating clinical treatment, creating a cooling effect on the skin to revitalize, moisturize, and soothe. Its thermo-cooling effect on the skin makes it a remarkable revitalizing treatment particularly for reducing redness. ALGOMASK+ offers instant radiance and long-lasting hydration."
                            , serviceGroup = Just s2
                            }

    insert_ $ Pricelist { pricelistService = s23
                        , pricelistName = "Price"
                        , pricelistPrice = 90
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Nothing
                        , pricelistDescr = Nothing
                        }

    insert_ $ Pricelist { pricelistService = s23
                        , pricelistName = "Package"
                        , pricelistPrice = 400
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Just "/5 sessions"
                        , pricelistDescr = Nothing
                        }

    s24 <- insert $ Service { serviceName = "Acne Treatment (120 min)"
                            , servicePublished = True
                            , serviceOverview = Just "Acne Treatment (120 min)"
                            , serviceDescr = Just "It is a very innovative and effective way of treating acne conditions that have not responded to other treatments and has produced many remarkable results. Urea peroxide, alpha-hydroxyl acids, and a special anti-androgen element are incorporated in Rejuvi Normalizing Formula."
                            , serviceGroup = Just s2
                            }

    insert_ $ Pricelist { pricelistService = s24
                        , pricelistName = "Price"
                        , pricelistPrice = 95
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Nothing
                        , pricelistDescr = Nothing
                        }

    insert_ $ Pricelist { pricelistService = s24
                        , pricelistName = "Package"
                        , pricelistPrice = 600
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Just "/7 sessions"
                        , pricelistDescr = Nothing
                        }

    s25 <- insert $ Service { serviceName = "24k Gold Whitening Treatment"
                            , servicePublished = True
                            , serviceOverview = Just "24k Gold Whitening Treatment"
                            , serviceDescr = Just "<p>A luxurious age defying facial treatment. This hydrating active age defying formula combines the power of pure vitamins, plant extracts and 24k gold. These ingredients effectively assist in the stimulation of collagen manufacturing. They form a continuous protective barrier to mimic the effects of surgery.</p><p>The mask fits like a “second skin” and perfectly adapts to the contours of the face. It delivers maximum hydration, fortifies the skin’s natural protective barrier, and rejuvenate your sensitive skin to reduce signs of aging.</p>"
                            , serviceGroup = Just s2
                            }

    insert_ $ Pricelist { pricelistService = s25
                        , pricelistName = "Price"
                        , pricelistPrice = 100
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Nothing
                        , pricelistDescr = Nothing
                        }

    insert_ $ Pricelist { pricelistService = s25
                        , pricelistName = "Package"
                        , pricelistPrice = 460
                        , pricelistPrefix = Just "$"
                        , pricelistSuffix = Just "/5 sessions"
                        , pricelistDescr = Nothing
                        }

    s3 <- insert $ Service { serviceName = "Advanced Facial Treatments"
                           , servicePublished = True
                           , serviceOverview = Just "Advanced Facial Treatments"
                           , serviceDescr = Just "Advanced Facial Treatments"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s3
                        , thumbnailPhoto = $(embedFile "static/img/advanced-facial-treatments.svg")
                        , thumbnailMime = "image/svg+xml"
                        }

    s4 <- insert $ Service { serviceName = "Anti-Aging Treatments"
                           , servicePublished = True
                           , serviceOverview = Just "Anti-Aging Treatments"
                           , serviceDescr = Just "Anti-Aging Treatments"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s4
                        , thumbnailPhoto = $(embedFile "static/img/anti-aging-treatments.svg")
                        , thumbnailMime = "image/svg+xml"
                        }

    s5 <- insert $ Service { serviceName = "Eye Treatment Center"
                           , servicePublished = True
                           , serviceOverview = Just "Eye Treatment Center"
                           , serviceDescr = Just "Eye Treatment Center"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s5
                        , thumbnailPhoto = $(embedFile "static/img/eye-treatment-center.svg")
                        , thumbnailMime = "image/svg+xml"
                        }

    s6 <- insert $ Service { serviceName = "Body Massage"
                           , servicePublished = True
                           , serviceOverview = Just "Body Massage"
                           , serviceDescr = Just "Body Massage"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s6
                        , thumbnailPhoto = $(embedFile "static/img/body-massage.svg")
                        , thumbnailMime = "image/svg+xml"
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
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s7
                        , thumbnailPhoto = $(embedFile "static/img/makeup-services.svg")
                        , thumbnailMime = "image/svg+xml"
                        }

    s8 <- insert $ Service { serviceName = "Waxing"
                           , servicePublished = True
                           , serviceOverview = Just "Waxing"
                           , serviceDescr = Just "Waxing"
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s8
                        , thumbnailPhoto = $(embedFile "static/img/waxing.svg")
                        , thumbnailMime = "image/svg+xml"
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
                           , serviceGroup = Nothing
                           }

    insert_ $ Thumbnail { thumbnailService = s9
                        , thumbnailPhoto = $(embedFile "static/img/nail-care.svg")
                        , thumbnailMime = "image/svg+xml"
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
                            , serviceGroup = Nothing
                            }

    insert_ $ Thumbnail { thumbnailService = s10
                        , thumbnailPhoto = $(embedFile "static/img/body-shaping-fitness.svg")
                        , thumbnailMime = "image/svg+xml"
                        }
        
    return ()
