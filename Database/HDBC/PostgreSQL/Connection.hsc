-- -*- mode: haskell; -*-
{-# CFILES hdbc-postgresql-helper.c #-}
-- Above line for hugs

module Database.HDBC.PostgreSQL.Connection
	(connectPostgreSQL, withPostgreSQL,
         connectPostgreSQL', withPostgreSQL',
         Impl.begin, Impl.Connection())
 where

import Database.HDBC
import Database.HDBC.DriverUtils
import qualified Database.HDBC.PostgreSQL.ConnectionImpl as Impl
import Database.HDBC.PostgreSQL.Types
import Database.HDBC.PostgreSQL.Statement
import Database.HDBC.PostgreSQL.PTypeConv
import Foreign.C.Types
import Foreign.C.String
import Database.HDBC.PostgreSQL.Utils
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8
import Control.Monad (when)
import Control.Concurrent.MVar
import System.IO (stderr, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception(bracket) 

#include <libpq-fe.h>
#include <pg_config.h>


-- | A global lock only used when libpq is /not/ thread-safe.  In that situation
-- this mvar is used to serialize access to the FFI calls marked as /safe/.
globalConnLock :: MVar ()
{-# NOINLINE globalConnLock #-}
globalConnLock = unsafePerformIO $ newMVar ()

{- | Connect to a PostgreSQL server.

See <http://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT> for the meaning
of the connection string. -}
connectPostgreSQL :: String -> IO Impl.Connection
connectPostgreSQL = connectPostgreSQL_helper True

connectPostgreSQL' :: String -> IO Impl.Connection
connectPostgreSQL' = connectPostgreSQL_helper False

connectPostgreSQL_helper :: Bool -> String -> IO Impl.Connection
connectPostgreSQL_helper auto_transaction args =
  B.useAsCString (BUTF8.fromString args) $
  \cs -> do ptr <- pqconnectdb cs
            threadSafe <- pqisThreadSafe ptr
            connLock <- if threadSafe==0 -- Also check GHC.Conc.numCapabilities here?
                          then do hPutStrLn stderr "WARNING: libpq is not threadsafe, \
                                          \serializing all libpq FFI calls.  \
                                          \(Consider recompiling libpq with \
                                          \--enable-thread-safety.\n"
                                  return globalConnLock
                          else newMVar ()
            status <- pqstatus ptr
            wrappedptr <- wrapconn ptr nullPtr
            fptr <- newForeignPtr pqfinishptr wrappedptr
            case status of
                     #{const CONNECTION_OK} -> mkConn auto_transaction args (connLock,fptr)
                     _ -> raiseError "connectPostgreSQL" status ptr

-- FIXME: environment vars may have changed, should use pgsql enquiries
-- for clone.
mkConn :: Bool -> String -> Conn -> IO Impl.Connection
mkConn auto_transaction args conn = withConn conn $
  \cconn -> 
    do children <- newMVar []
       when auto_transaction $ begin_transaction conn children
       protover <- pqprotocolVersion cconn
       serverver <- pqserverVersion cconn
       let clientver = #{const_str PG_VERSION}
       let rconn = Impl.Connection {
                            Impl.disconnect = fdisconnect conn children,
                            Impl.begin = if auto_transaction
                                         then return ()
                                         else begin_transaction conn children,
                            Impl.commit = fcommit auto_transaction conn children,
                            Impl.rollback = frollback auto_transaction conn children,
                            Impl.runRaw = frunRaw conn children,
                            Impl.run = frun conn children,
                            Impl.prepare = newSth conn children,
                            Impl.clone = connectPostgreSQL args,
                            Impl.hdbcDriverName = "postgresql",
                            Impl.hdbcClientVer = clientver,
                            Impl.proxiedClientName = "postgresql",
                            Impl.proxiedClientVer = show protover,
                            Impl.dbServerVer = show serverver,
                            Impl.dbTransactionSupport = True,
                            Impl.getTables = fgetTables conn children,
                            Impl.describeTable = fdescribeTable conn children}
       _ <- quickQuery rconn "SET client_encoding TO utf8;" []
       return rconn

-- | Connect to a PostgreSQL server,  and automatically disconnect
-- if the handler exits normally or throws an exception.
withPostgreSQL :: String -> (Impl.Connection -> IO a) -> IO a
withPostgreSQL connstr = bracket (connectPostgreSQL connstr) (disconnect)

withPostgreSQL' :: String -> (Impl.Connection -> IO a) -> IO a
withPostgreSQL' connstr = bracket (connectPostgreSQL' connstr) (disconnect)

--------------------------------------------------
-- Guts here
--------------------------------------------------

begin_transaction :: Conn -> ChildList -> IO ()
begin_transaction o children = frun o children "BEGIN" [] >> return ()

frunRaw :: Conn -> ChildList -> String -> IO ()
frunRaw o children query =
    do sth <- newSth o children query
       executeRaw sth
       finish sth
       return ()

frun :: Conn -> ChildList -> String -> [SqlValue] -> IO Integer
frun o children query args =
    do sth <- newSth o children query
       res <- execute sth args
       finish sth
       return res

fcommit :: Bool -> Conn -> ChildList -> IO ()
fcommit begin o cl = do _ <- frun o cl "COMMIT" []
                        when begin $ begin_transaction o cl

frollback :: Bool -> Conn -> ChildList -> IO ()
frollback begin o cl =  do _ <- frun o cl "ROLLBACK" []
                           when begin $ begin_transaction o cl

fgetTables conn children =
    do sth <- newSth conn children 
              "select table_name from information_schema.tables where \
               \table_schema != 'pg_catalog' AND table_schema != \
               \'information_schema'"
       _ <- execute sth []
       res1 <- fetchAllRows' sth
       let res = map fromSql $ concat res1
       return $ seq (length res) res

fdescribeTable :: Conn -> ChildList -> String -> IO [(String, SqlColDesc)]
fdescribeTable o cl table = fdescribeSchemaTable o cl Nothing table

fdescribeSchemaTable :: Conn -> ChildList -> Maybe String -> String -> IO [(String, SqlColDesc)]
fdescribeSchemaTable o cl maybeSchema table =
    do sth <- newSth o cl 
              ("SELECT attname, atttypid, attlen, format_type(atttypid, atttypmod), attnotnull " ++
               "FROM pg_attribute, pg_class, pg_namespace ns " ++
               "WHERE relname = ? and attnum > 0 and attisdropped IS FALSE " ++
               (if isJust maybeSchema then "and ns.nspname = ? " else "") ++
               "and attrelid = pg_class.oid and relnamespace = ns.oid order by attnum")
       let params = toSql table : (if isJust maybeSchema then [toSql $ fromJust maybeSchema] else [])
       _ <- execute sth params
       res <- fetchAllRows' sth
       return $ map desccol res
    where
      desccol [attname, atttypid, attlen, formattedtype, attnotnull] =
          (fromSql attname, 
           colDescForPGAttr (fromSql atttypid) (fromSql attlen) (fromSql formattedtype) (fromSql attnotnull == False))
      desccol x =
          error $ "Got unexpected result from pg_attribute: " ++ show x
         

fdisconnect :: Conn -> ChildList -> IO ()
fdisconnect conn mchildren = 
    do closeAllChildren mchildren
       withRawConn conn $ pqfinish

foreign import ccall safe "libpq-fe.h PQconnectdb"
  pqconnectdb :: CString -> IO (Ptr CConn)

foreign import ccall unsafe "hdbc-postgresql-helper.h wrapobjpg"
  wrapconn :: Ptr CConn -> Ptr WrappedCConn -> IO (Ptr WrappedCConn)

foreign import ccall unsafe "libpq-fe.h PQstatus"
  pqstatus :: Ptr CConn -> IO #{type ConnStatusType}

foreign import ccall unsafe "hdbc-postgresql-helper.h PQfinish_app"
  pqfinish :: Ptr WrappedCConn -> IO ()

foreign import ccall unsafe "hdbc-postgresql-helper.h &PQfinish_finalizer"
  pqfinishptr :: FunPtr (Ptr WrappedCConn -> IO ())

foreign import ccall unsafe "libpq-fe.h PQprotocolVersion"
  pqprotocolVersion :: Ptr CConn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQserverVersion"
  pqserverVersion :: Ptr CConn -> IO CInt

foreign import ccall unsafe "libpq.fe.h PQisthreadsafe"
  pqisThreadSafe :: Ptr CConn -> IO Int
