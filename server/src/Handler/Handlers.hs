{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Handlers where

import Import

dbLookup key = runDB $ getEntity key

dbLookup404 key = do
  maybeValue <- dbLookup key
  case maybeValue of
    Just value -> pure value
    Nothing -> notFound


getDoctorR :: Int -> Handler Value
getDoctorR id = (dbLookup404 $ DoctorKey $ fromIntegral id) >>= returnJson

getPatientR :: Int -> Handler Value
getPatientR id = (dbLookup404 $ PatientKey $ fromIntegral id) >>= returnJson

postDoctorsR :: Handler Value
postDoctorsR = do
    -- requireJsonBody will parse the request body into the appropriate type,
    -- or return a 400 status code if the request JSON is invalid.
    doctor <- (requireJsonBody :: Handler Doctor)

    insertedDoctor <- runDB $ insertEntity doctor
    returnJson insertedDoctor

getDoctorsR :: Handler Value
getDoctorsR = do
  docs :: [Entity Doctor] <- runDB $ selectList [] []
  returnJson docs

postPatientsR :: Handler Value
postPatientsR = undefined

postRequestsR :: Handler Value
postRequestsR = undefined

postDoctorPatientsR :: Int -> Handler Value
postDoctorPatientsR = undefined
