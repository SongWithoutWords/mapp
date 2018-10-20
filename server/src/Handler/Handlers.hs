{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Handlers where

import Import

getDoctorR :: Int -> Handler Value
getDoctorR id = do
  mdoc <- runDB $ getEntity $ DoctorKey $ fromIntegral id
  case mdoc of
    Nothing -> returnJson mdoc
    Just doctor' ->  returnJson doctor'

postDoctorsR :: Handler Value
postDoctorsR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    doctor <- (requireJsonBody :: Handler Doctor)

    insertedDoctor <- runDB $ insertEntity doctor
    returnJson insertedDoctor

getDoctorsR :: Handler Value
getDoctorsR = do
  -- docs <- ((runDB $ selectList [] [Asc DoctorId]) :: Handler [Doctor])
  docs :: [Entity Doctor] <- runDB $ selectList [] []
  returnJson docs

getPatientR :: Int -> Handler Value
getPatientR = undefined

postPatientsR :: Handler Value
postPatientsR = undefined

postRequestsR :: Handler Value
postRequestsR = undefined

postDoctorPatientsR :: Int -> Handler Value
postDoctorPatientsR = undefined
