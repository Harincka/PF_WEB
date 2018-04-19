{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Clay as C
import Control.Monad.Trans (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.FromRow (FromRow, fromRow, field)
import GHC.Generics (Generic)
import Lucid
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty (middleware, scotty, get, json, html, param, ActionM)

dbName :: String
dbName = "mytracker.db"

data Tracker = Tracker 
  { site :: T.Text,
    page :: T.Text,
    hits :: Int
 } deriving (Show, Generic)

instance FromRow Tracker where
  fromRow = Tracker <$> field <*> field <*> field

instance ToJSON Tracker

selectAll :: IO [Tracker]
selectAll = do
  conn <- SQL.open dbName
  let sql = "SELECT * FROM tracker"
  res <- SQL.query_ conn sql :: IO [Tracker]     
  SQL.close conn
  return res

--divCss = C.div C.# C.td do
 -- C.border C.solid (C.px 1) C.black

mkpage :: Lucid.Html() -> Lucid.Html() -> L.Text
mkpage titleStr page = renderText $ do
  doctype_
  html_ $ do
    header_ $ do
      title_ titleStr
    body_ page

homeTracker :: [Tracker] -> Lucid.Html()
homeTracker trackers = do
  h1_ "mytracker"
  a_ [href_ "/getjson"] $ "get json"
  table_ $ do 
    tr_ $ mapM_ (td_ . toHtml . formatTracker) trackers 
      where formatTracker tracker = T.concat [site tracker, " ", page tracker, " "]
  --ul_ $ mapM_ (li_ . toHtml . formatTracker) trackers
    --  where formatTracker tracker = T.concat [site tracker, " ", page tracker, " " ]


main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  middleware simpleCors

  get "/" $ do
    trackers <- liftIO selectAll
    html $ mkpage "Tracker" $ homeTracker trackers
    
  --get "/getjson" $ json $ liftIO selectAll



