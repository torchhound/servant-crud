{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module CrudApi where

import Data.Proxy
import Data.Text
import Servant.API
import Database.Persist

import Model

type CrudApi = "create" :> ReqBody '[JSON] Document :> Put '[JSON] (Maybe (Key Document))
  :<|> "read" :> Capture "title" Text :> Get '[JSON] (Maybe Document)
  :<|> "update" :> ReqBody '[JSON] Document :> Patch '[JSON] [Char]
  :<|> "delete" :> Capture "title" Text :> Delete '[JSON] [Char]

documentApi :: Proxy CrudApi
documentApi = Proxy