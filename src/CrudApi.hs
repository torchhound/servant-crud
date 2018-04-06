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

type CrudApi = "create" :> Put '[JSON] (Maybe (Key Document))
	:<|> "read" :> Capture "title" Integer :> Get '[JSON] (Maybe Document)
	:<|> "update" :> Capture "title" Integer :> Patch '[JSON] (Maybe Document)
	:<|> "delete" :> Capture "title" Integer :> Delete '[JSON] (Maybe Document)

documentAPI :: Proxy CrudApi
documentAPI = Proxy