{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric #-}

module Model where

import Data.Aeson
import Data.Text
import Database.Persist.TH

share [mkPersist sqlSettings, mkMIgrate "migrateAll"] [persistLowerCase|
Document
  title Text
  body Text
  UniqueName uuid
  deriving Eq Read Show Generic
|]

instance FromJSON Document where
  parseJSON = withObject "Document" $ \ v ->
    Document <$> v .: "title"
             <*> v .: "body"

instance ToJSON Document where
  toJSON (Document title body) =
    object [ "title" .= title
           , "body" .= body ]