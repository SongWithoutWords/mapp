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


-- requireJsonBody (used in post functions) parses the request body into the appropriate type,
-- or return a 400 status code if the request JSON is invalid.

postDoctorsR :: Handler Value
postDoctorsR = do
  doctor :: Doctor <- requireJsonBody
  doctorInserted <- runDB $ insertEntity doctor
  returnJson doctorInserted

postPatientsR :: Handler Value
postPatientsR =  do
  patient :: Patient <- requireJsonBody
  patientInserted <- runDB $ insertEntity patient
  returnJson patientInserted

postRequestsR :: Handler Value
postRequestsR =  do
  request :: RequestForDoctor <- requireJsonBody
  requestInserted <- runDB $ insertEntity request
  returnJson requestInserted

getDoctorsR :: Handler Value
getDoctorsR = do
  docs :: [Entity Doctor] <- runDB $ selectList [] []
  returnJson docs


postDoctorPatientsR :: Int -> Handler Value
postDoctorPatientsR = undefined
