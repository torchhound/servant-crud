{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant.API
import Servant.Server
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Proxy

type Ccreate = "create" :> Put '[JSON] [Document]
--type Cread = "read" :> Capture "uuid" Integer :> Get '[JSON] [Document]
type Cread = "read" :> Get '[JSON] [Document]
type Cupdate = "update" :> Capture "uuid" Integer :> Patch '[JSON] [Document]
type Cdelete = "delete" :> Capture "uuid" Integer :> Delete '[JSON] [Document]

data Document = Document {document :: String} deriving (Show, Generic)

instance ToJSON Document

docs :: [Document]
docs = [Document "Test"]

server :: Server Cread
server = return docs

userAPI :: Proxy Cread
userAPI = Proxy

app :: Application
app = serve userAPI server

main :: IO ()
main = run 8081 app
