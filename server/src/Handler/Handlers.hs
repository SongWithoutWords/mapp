{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Handlers where

import Import hiding(id)

dbLookup key = runDB $ getEntity key

dbLookup404 key = do
  maybeValue <- dbLookup key
  case maybeValue of
    Just value -> pure value
    Nothing -> notFound


getDoctorR :: Int -> Handler Value
getDoctorR id = dbLookup404 (doctorKey id) >>= returnJson

getPatientR :: Int -> Handler Value
getPatientR id = dbLookup404 (patientKey id) >>= returnJson

getDoctorsR :: Handler Value
getDoctorsR = do
  docs :: [Entity Doctor] <- runDB $ selectList [] []
  returnJson docs


-- requireJsonBody (used in post functions) parses the request body into the appropriate type,
-- or return a 400 status code if the request JSON is invalid.

postDoctorsR :: Handler Value
postDoctorsR = do
  PostDoctor email password fn ln <- requireJsonBody
  doctorInserted <- runDB $ insertEntity Doctor
    { doctorFirstName = fn
    , doctorLastName = ln
    , doctorPatients = []
    }
  let user = XUser email password "" (Left $ entityKey doctorInserted)
  _ <- runDB $ insertEntity user
  returnJson doctorInserted

postPatientsR :: Handler Value
postPatientsR =  do
  PostPatient _ _ fn ln bd <- requireJsonBody
  let patient = Patient fn ln bd
  patientInserted <- runDB $ insertEntity patient
  returnJson patientInserted

postRequestsR :: Handler Value
postRequestsR =  do
  request :: RequestForDoctor <- requireJsonBody
  requestInserted <- runDB $ insertEntity request
  returnJson requestInserted

postDoctorPatientsR :: Int -> Handler Value
postDoctorPatientsR doctorId = do
  let doctorId' = doctorKey doctorId

  doctor <- entityVal <$> dbLookup404 doctorId'

  patientIdToAdd :: PatientId <- requireJsonBody

  pendingRequests <- runDB $ selectList
    [ RequestForDoctorPatientFrom ==. patientIdToAdd
    , RequestForDoctorDoctorTo ==. doctorId'] []

  if null pendingRequests
    then invalidArgs ["No pending request from this patient to this doctor"]
    else do
      let doctor' = doctor {doctorPatients = patientIdToAdd : doctorPatients doctor}
      runDB $ replace doctorId' doctor'
      returnJson doctor'
