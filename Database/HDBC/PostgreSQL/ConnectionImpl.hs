module Database.HDBC.PostgreSQL.ConnectionImpl where

import qualified Database.HDBC.Statement as Types
import qualified Database.HDBC.Types as Types
import Database.HDBC.ColTypes as ColTypes

data Connection = 
    Connection {
                disconnect :: IO (),
                commit :: IO (),
                rollback :: IO (),
                run :: String -> [Types.SqlValue] -> IO Integer,
                prepare :: String -> IO Types.Statement,
                clone :: IO Connection,
                hdbcDriverName :: String,
                hdbcClientVer :: String,
                proxiedClientName :: String,
                proxiedClientVer :: String,
                dbServerVer :: String,
                dbTransactionSupport :: Bool,
                getTables :: IO [String],
                describeTable :: String -> IO [(String, ColTypes.SqlColDesc)]
               }

instance Types.IConnection Connection where
  disconnect = disconnect
  commit = commit
  rollback = rollback
  run = run
  prepare = prepare
  clone = clone
  hdbcDriverName = hdbcDriverName
  hdbcClientVer = hdbcClientVer
  proxiedClientName = proxiedClientName
  proxiedClientVer = proxiedClientVer
  dbServerVer = dbServerVer
  dbTransactionSupport = dbTransactionSupport
  getTables = getTables
  describeTable = describeTable
