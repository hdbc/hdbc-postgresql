-- -*- mode: haskell; -*-
{-# CFILES hdbc-postgresql-helper.c #-}
-- Above line for hugs
{-
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}

module Database.HDBC.PostgreSQL.Connection (connectPostgreSQL) where

import Database.HDBC.Types
import Database.HDBC
import Database.HDBC.PostgreSQL.Types
import Database.HDBC.PostgreSQL.Statement
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Database.HDBC.PostgreSQL.Utils
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word

#include <libpq-fe.h>
#include <pg_config.h>

{- | Connect to a PostgreSQL server.

See <http://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT> for the meaning
of the connection string. -}
connectPostgreSQL :: String -> IO Connection
connectPostgreSQL args = withCString args $
  \cs -> do ptr <- pqconnectdb cs
            status <- pqstatus ptr
            wrappedptr <- wrapconn ptr
            fptr <- newForeignPtr pqfinishptr wrappedptr
            case status of
                     #{const CONNECTION_OK} -> mkConn args fptr
                     _ -> raiseError "connectPostgreSQL" status ptr

-- FIXME: environment vars may have changed, should use pgsql enquiries
-- for clone.
mkConn :: String -> Conn -> IO Connection
mkConn args conn = withConn conn $
  \cconn -> 
    do begin_transaction conn
       protover <- pqprotocolVersion cconn
       serverver <- pqserverVersion cconn
       let clientver = #{const_str PG_VERSION}
       return $ Connection {
                            disconnect = fdisconnect conn,
                            commit = fcommit conn,
                            rollback = frollback conn,
                            run = frun conn,
                            prepare = newSth conn,
                            clone = connectPostgreSQL args,
                            hdbcDriverName = "postgresql",
                            hdbcClientVer = clientver,
                            proxiedClientName = "postgresql",
                            proxiedClientVer = show protover,
                            dbServerVer = show serverver,
                            getTables = fgetTables conn}

--------------------------------------------------
-- Guts here
--------------------------------------------------

begin_transaction :: Conn -> IO ()
begin_transaction o = frun o "BEGIN" [] >> return ()

frun o query args =
    do sth <- newSth o query
       res <- execute sth args
       finish sth
       return res

fcommit o = do frun o "COMMIT" []
               begin_transaction o
frollback o =  do frun o "ROLLBACK" []
                  begin_transaction o

fgetTables conn =
    do sth <- newSth conn "select table_name from information_schema.tables where table_schema = 'public'"
       execute sth []
       res1 <- fetchAllRows sth
       let res = map fromSql $ concat res1
       return $ seq (length res) res

fdisconnect conn = withRawConn pqfinish

foreign import ccall unsafe "libpq-fe.h PQconnectdb"
  pqconnectdb :: CString -> IO (Ptr CConn)

foreign import ccall unsafe "hdbc-postgresql-helper.h wrapobj"
  wrapconn :: Ptr CConn -> IO (Ptr WrappedCConn)

foreign import ccall unsafe "libpq-fe.h PQstatus"
  pqstatus :: Ptr CConn -> IO #{type ConnStatusType}

foreign import ccall unsafe "hdbc-postgresql-helper.h PQfinish_app"
  pqfinish :: Ptr WrappedCConn -> IO ()

foreign import ccall unsafe "hdbc-postgresql-helper.h &PQfinish_fptr"
  pqfinishptr :: FunPtr (Ptr WrappedCConn -> IO ())

foreign import ccall unsafe "libpq-fe.h PQprotocolVersion"
  pqprotocolVersion :: Ptr CConn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQserverVersion"
  pqserverVersion :: Ptr CConn -> IO CInt
