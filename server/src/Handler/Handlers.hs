{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Handlers where

import Import hiding(id)
import Control.Monad.Extra(mapMaybeM)

dbLookup key = runDB $ getEntity key

dbLookup404 key = do
  maybeValue <- dbLookup key
  case maybeValue of
    Just value -> pure value
    Nothing -> notFound

getDoctorWithPatients :: DoctorId -> Handler DoctorWithPatients
getDoctorWithPatients did = do
  Doctor fn ln <- runDB $ get404 did
  relations <- runDB $ selectList [DoctorPatientRelationDoctor ==. did] []
  let pids = doctorPatientRelationPatient . entityVal <$> relations
  patients' <- runDB $ mapMaybeM getEntity pids
  return $ DoctorWithPatients
    { id = did
    , firstName = fn
    , lastName = ln
    , patients = patients'
    }

getDoctorR :: Int -> Handler Value
getDoctorR = getDoctorWithPatients . doctorKey >=> returnJson

getPatientR :: Int -> Handler Value
getPatientR = dbLookup404 . patientKey >=> returnJson

getDoctorsR :: Handler Value
getDoctorsR = do
  docs :: [Entity Doctor] <- runDB $ selectList [] []
  returnJson docs


-- requireJsonBody (used in post functions) parses the request body into the appropriate type,
-- or return a 400 status code if the request JSON is invalid.

postDoctorsR :: Handler Value
postDoctorsR = do
  PostDoctor email' password' firstName' lastName' <- requireJsonBody
  doctorId <- runDB $ insert Doctor
    { doctorFirstName = firstName'
    , doctorLastName = lastName'
    }
  let user = XUser email' password' "" (Left doctorId)
  userInserted <- runDB $ insertUniqueEntity user

  case userInserted of
    Nothing -> do
      _ <- runDB $ delete doctorId
      invalidArgs ["Email already in use"]
    Just _ -> getDoctorWithPatients doctorId >>= returnJson

postPatientsR :: Handler Value
postPatientsR =  do
  PostPatient _ _ fn ln bd <- requireJsonBody
  let patient = Patient fn ln bd
  patientInserted <- runDB $ insertEntity patient
  returnJson patientInserted

postRequestsR :: Handler Value
postRequestsR =  do
  request :: DoctorPatientRequest <- requireJsonBody
  requestInserted <- runDB $ insertUniqueEntity request
  returnJson requestInserted

postRelationsR :: Handler Value
postRelationsR = do
  relation@(DoctorPatientRelation did pid) <- requireJsonBody

  pendingRequests <- runDB $ selectList
    [ DoctorPatientRequestDoctor ==. did
    , DoctorPatientRequestPatient ==. pid] []

  if null pendingRequests
    then invalidArgs ["No pending request from this patient to this doctor"]
    else do
      (runDB $ insertUniqueEntity relation) >>= returnJson
