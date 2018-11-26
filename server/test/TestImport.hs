{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler, id)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Test            as X
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)

-- Wiping the database
import Database.Persist.Sqlite              (sqlDatabase, mkSqliteConnectionInfo, fkEnabled, createSqlitePoolFromInfo)
import Control.Monad.Logger                 (runLoggingT)
import Lens.Micro                           (set)
import Settings (appDatabaseConf)
import Yesod.Core (messageLoggerSource)

-- For JSON test functions
import Network.Wai.Test(SResponse(..))
import Data.Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Network.HTTP.Types as H
import qualified Test.HUnit as H
import Text.Show.Pretty(ppShow)
import Yesod.Core (Value, Yesod, RedirectUrl)


runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
    -- In order to wipe the database, we need to use a connection which has
    -- foreign key checks disabled.  Foreign key checks are enabled or disabled
    -- per connection, so this won't effect queries outside this function.
    --
    -- Aside: foreign key checks are enabled by persistent-sqlite, as of
    -- version 2.6.2, unless they are explicitly disabled in the
    -- SqliteConnectionInfo.

    let logFunc = messageLoggerSource app (appLogger app)

    let dbName = sqlDatabase $ appDatabaseConf $ appSettings app
        connInfo = set fkEnabled False $ mkSqliteConnectionInfo dbName

    pool <- runLoggingT (createSqlitePoolFromInfo connInfo 1) logFunc

    flip runSqlPersistMPool pool $ do
        tables <- getTables
        sqlBackend <- ask
        let queries = map (\t -> "DELETE FROM " ++ (connEscapeName sqlBackend $ DBName t)) tables
        forM_ queries (\q -> rawExecute q [])

getTables :: DB [Text]
getTables = do
    tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
    return (fmap unSingle tables)

failTest :: String -> YesodExample App ()
failTest = liftIO . H.assertFailure

jsonResponseIs :: (Show a, Eq a, FromJSON a, ToJSON a) => a -> YesodExample App ()
jsonResponseIs expected = withResponse $ \ (SResponse status _ bodyText) -> do
  statusIs 200
  liftIO $ case decode bodyText of
    Nothing -> H.assertFailure $ "Could not parse JSON response:\n" ++ show bodyText
    Just bodyJson -> case fromJSON bodyJson of
      Error message -> H.assertFailure $
        "Could not parse JSON response as correct type:\n" ++
        message ++ "\n" ++ ppShow bodyJson
      Success result ->
        if result == expected
          then pure ()
          else H.assertFailure $
               "Expected:\n" ++ ppShow expected ++
               "\nFound:\n" ++ ppShow result

postJson :: (ToJSON a) => RedirectUrl App url => url -> a -> YesodExample App ()
postJson url value = postBody url $ encode value

addBasicAuthHeader :: Text -> Text -> RequestBuilder site ()
addBasicAuthHeader email password = addRequestHeader $
  (H.hAuthorization, (asByteString "Basic ") ++ (B64.encode $ encodeUtf8 $ email ++ ":" ++ password))

postJsonAuth
  :: (ToJSON a, RedirectUrl App url)
  => (Text, Text) -> url -> a -> YesodExample App ()
postJsonAuth (email, password) url value = request $ do
  setMethod "POST"
  setUrl url
  addBasicAuthHeader email password
  setRequestBody $ encode value

getAuth :: RedirectUrl App url => (Text, Text) -> url -> YesodExample App ()
getAuth (email, password) url = request $ do
  setMethod "GET"
  setUrl url
  addBasicAuthHeader email password

