{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Servant
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite ( ConnectionPool, createSqlitePool
                               , runSqlPool, runSqlPersistMPool
                               , runMigration, selectFirst, (==.)
                               , insert, entityVal, deleteWhere
                               , updateWhere, (*=.))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.Wai.Handler.Warp as Warp

import CrudApi
import Model

server :: ConnectionPool -> Server CrudApi
server pool = _documentPost 
  :<|> _documentGet 
  :<|> _documentPatch 
  :<|> _documentDelete

  where
    _documentPost newDocument = liftIO $ documentPost newDocument
    _documentGet title = liftIO $ documentGet title
    _documentPatch patchDocument = liftIO $ documentPatch patchDocument
    _documentDelete deleteDocument = liftIO $ documentDelete deleteDocument

    documentPost :: Document -> IO (Maybe (Key Document))
    documentPost newDocument = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [UniqueTitle ==. (title newDocument)] []
      case exists of 
        Nothing -> Just <$> insert newDocument
        Just _ -> return Nothing

    documentGet :: Text -> IO (Maybe Document)
    documentGet title = flip runSqlPersistMPool pool $ do
      _Document <- selectFirst [UniqueTitle ==. title] []
      return $ entityVal <$> _Document

    documentPatch :: Document -> IO (Handler Text)
    documentPatch patchDocument = flip runSqlPersistMPool pool $ do
      updateWhere [UniqueTitle ==. (title patchDocument)] [Document *=. patchDocument]
      return "Document Patched"

    documentDelete :: Document -> IO (Handler Text)
    documentDelete deleteDocument = flip runSqlPersistMPool pool $ do
      deleteWhere [UniqueTitle ==. (title deleteDocument)]
      return "Document Deleted"

app :: ConnectionPool -> Application
app pool = serve documentAPI $ server pool

mkApp :: FilePath -> IO Application
mkApp file = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs file) 5

  runSqlPool (runMigration migrateAll) pool
  return $ app pool

sprint :: FilePath -> IO ()
sprint file = Warp.run 8081 =<< mkApp file

main :: IO ()
main = sprint "sqlite.db"