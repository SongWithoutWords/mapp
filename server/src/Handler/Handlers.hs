{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Handlers where

import Import

getDoctorGet :: Int -> Handler Value
getDoctorGet id = do
  mdoc <- runDB $ getEntity $ DoctorKey $ fromIntegral id
  case mdoc of
    Nothing -> returnJson mdoc
    Just doctor' ->  returnJson doctor'

postDoctors :: Handler Value
postDoctors = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    doctor <- (requireJsonBody :: Handler Doctor)

    insertedDoctor <- runDB $ insertEntity doctor
    returnJson insertedDoctor

getDoctors :: Handler Value
getDoctors = do
  -- docs <- ((runDB $ selectList [] [Asc DoctorId]) :: Handler [Doctor])
  docs :: [Entity Doctor] <- runDB $ selectList [] []
  returnJson docs

getPatientR :: Int -> Handler Value
getPatientR = undefined

postPatients :: Handler Value
postPatients = undefined

postRequests :: Handler Value
postRequests = undefined
