{-# LANGUAGE
  TypeFamilies
, DeriveDataTypeable
, OverloadedStrings
  #-}

{- |
   Module     : Database.HDBC.PostgreSQL
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

HDBC driver interface for PostgreSQL 8.x

Written by John Goerzen, jgoerzen\@complete.org

/NOTE ON DATES AND TIMES/

The recommended correspondence between PostgreSQL date and time types and HDBC SqlValue
types is:

* SqlLocalDate: DATE

* SqlLocalTimeOfDay: TIME WITHOUT TIME ZONE

* SqlZonedLocalTimeOfDay: TIME WITH TIME ZONE

* SqlLocalTime: TIMESTAMP WITHOUT TIME ZONE

* SqlZonedTime: TIMESTAMP WITH TIME ZONE

* SqlUTCTime: TIMESTAMP WITH TIME ZONE

* SqlDiffTime: INTERVAL

* SqlPOSIXTime: NUMERIC

* SqlEpochTime: INTEGER

* SqlTimeDiff: INTERVAL

Other combinations are possible, and may even be converted automatically.
The above simply represents the types that seem the most logical correspondence,
and thus are tested by the HDBC-PostgreSQL test suite.

-}

module Database.HDBC.PostgreSQL where

import System.IO

import Database.HDBC
import Database.HDBC.DriverUtils
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Database.PostgreSQL.Simple.BuiltinTypes as PS

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Typeable
import Safe
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Blaze.ByteString.Builder.Char.Utf8 (fromLazyText, fromString)
import Blaze.ByteString.Builder (toByteString)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Data.Time
import System.Locale (defaultTimeLocale)
import Data.Word
import Data.Bits
import qualified Data.UUID as U


data PostgreConnection = PostgreConnection
                         { postNative     :: MVar (Maybe PQ.Connection) -- ^ LibPQ connection if established
                         , postStatements :: ChildList PostgreStatement -- ^ List of weak refs to statements to finalize on disconnect
                         , postConnString :: TL.Text                    -- ^ Original connection string
                         }
                         deriving (Typeable)

data PGStatementState =
  -- | Newly created or reseted statement
  STNew
  -- | Just executed statement
  | STExecuted
    { pgstResult :: ! PQ.Result
    }
    -- | Fetching rows is in progress
  | STFetching
    { pgstResult  :: ! PQ.Result -- ^ The result to fetch data from
    , pgstColumns :: ! PQ.Column -- ^ Columns count
    , pgstFormats :: ! [PQ.Format]
    , pgstTypes   :: ! [PQ.Oid]
    , pgstTuples  :: ! PQ.Row    -- ^ Tuples in result
    , pgstCurrent :: ! PQ.Row    -- ^ Row number waiting to fetch
    }
    -- | Statement is finished, can still be reseted
  | STFinished


data PostgreStatement = PostgreStatement
                        { stQuery :: TL.Text -- ^ Initial query
                        , stConnection :: PostgreConnection -- ^ Connection this statement working with
                        , stState :: MVar PGStatementState -- ^ State of statement
                        }
                        deriving (Typeable)

throwConnectionClosed :: IO a
throwConnectionClosed = throwIO $ SqlDriverError $ pgMsg "The connection is closed, can not operate"

withPGConnection :: PostgreConnection -> (PQ.Connection -> IO a) -> IO a
withPGConnection conn action = withMVar (postNative conn) $ \c -> case c of
  Nothing -> throwConnectionClosed
  Just con -> action con

sqlValueToNative :: SqlValue -> Maybe (PQ.Oid, B.ByteString, PQ.Format)
sqlValueToNative SqlNull = Nothing
sqlValueToNative x = Just $ tonative x
  where
    bshow :: (Show a) => a -> B.ByteString
    bshow = toByteString . fromString . show

    asText :: (Show a) => PS.BuiltinType -> a -> (PQ.Oid, B.ByteString, PQ.Format)
    asText t v = (PS.builtin2oid t, bshow v, PQ.Text)

    tonative :: SqlValue -> (PQ.Oid, B.ByteString, PQ.Format)
    tonative (SqlDecimal d)          = asText PS.Numeric d
    tonative (SqlWord32 w)           = asText PS.Int8 w
    tonative (SqlWord64 w)           = asText PS.Int8 w
    tonative (SqlInt32 i)            = asText PS.Int4 i
    tonative (SqlInt64 i)            = asText PS.Int8 i
    tonative (SqlInteger i)          = asText PS.Numeric i
    tonative (SqlDouble d)           = asText PS.Float8 d
    tonative (SqlText t)             = (PS.builtin2oid PS.Text, encodeUTF8 t, PQ.Text)
    tonative (SqlBlob b)             = (PS.builtin2oid PS.ByteA, b, PQ.Binary)
    tonative (SqlBool b)             = (PS.builtin2oid PS.Bool, if b then "TRUE" else "FALSE", PQ.Text)
    tonative (SqlBitField b)         = (PS.builtin2oid PS.VarBit, formatBits b, PQ.Text)
    tonative (SqlUUID u)             = (PS.builtin2oid PS.UUID, toByteString $ fromString $ U.toString u, PQ.Text)
    tonative (SqlUTCTime ut)         = (PS.builtin2oid PS.TimestampTZ, formatUTC ut, PQ.Text)
    tonative (SqlLocalDate d)        = (PS.builtin2oid PS.Date, formatDay d, PQ.Text)
    tonative (SqlLocalTimeOfDay tod) = (PS.builtin2oid PS.Time, formatT tod, PQ.Text)
    tonative (SqlLocalTime t)        = (PS.builtin2oid PS.Timestamp, formatDT t, PQ.Text)
    tonative SqlNow                  = (PS.builtin2oid PS.TimestampTZ, "NOW()", PQ.Text)
    tonative SqlNull                 = error "SqlNull is passed to 'tonative' internal function, this is a bug"

formatToBS :: (FormatTime a) => String -> a -> B.ByteString
formatToBS s d = toByteString $ fromString $ formatTime defaultTimeLocale s d

formatUTC :: UTCTime -> B.ByteString
formatUTC = formatToBS "%Y-%m-%d %H:%M:%S%Q %Z"

formatDay :: Day -> B.ByteString
formatDay = formatToBS  "%Y-%m-%d"

formatT :: TimeOfDay -> B.ByteString
formatT = formatToBS "%H:%M:%S%Q"

formatDT :: LocalTime -> B.ByteString
formatDT = formatToBS "%Y-%m-%d %H:%M:%S%Q"

formatBits :: Word64 -> B.ByteString
formatBits 0 = "0"
formatBits w = toByteString $ bits
  where
    bits = mconcat $ map toBS $ truncFalse $ wbits

    wbits = map (testBit w) [(bitSize w) - 1, (bitSize w) - 2 .. 0]

    truncFalse [] = []
    truncFalse (False:r) = truncFalse r
    truncFalse x@(True:_) = x

    toBS True = fromByteString "1"
    toBS False = fromByteString "0"

o2b :: PQ.Oid -> PS.BuiltinType
o2b fmt = case PS.oid2builtin fmt of
  Nothing -> throw $ SqlDriverError $ pgMsg $ "Could not understand returned type, OID=" ++ show fmt
  Just r  -> r


nativeToSqlValue :: PQ.Connection -> B.ByteString -> PQ.Format -> PQ.Oid -> IO SqlValue
nativeToSqlValue con b PQ.Text f = fromNative (o2b f) b
  where
    ftext x = SqlText $ decodeUTF8 x

    bread what x = case readMay $ val of
      Nothing -> throw $ SqlDriverError $ pgMsg $ "could not read \"" ++ val ++ "\" as " ++ what
      Just r  -> r
      where
        val = TL.unpack $ decodeUTF8 x


    fromNative PS.Bool x = case x of
      "t" -> return $ SqlBool True
      "f" -> return $ SqlBool False
      _ -> throwIO $ SqlDriverError $ pgMsg $ "could not parse " ++ (TL.unpack $ decodeUTF8 x) ++ " as Boolean"
    fromNative PS.ByteA x = do
      r <- PQ.unescapeBytea x
      case r of
        Nothing -> throwErrorMessage con
        Just ret -> return $ SqlBlob ret
    fromNative PS.Char x        = return $ ftext x
    fromNative PS.Int8 x        = return $ SqlInt64 $ bread "integer" x
    fromNative PS.Int4 x        = return $ SqlInt32 $ bread "integer" x
    fromNative PS.Int2 x        = return $ SqlInt32 $ bread "integer" x
    fromNative PS.Text x        = return $ ftext x
    fromNative PS.Xml  x        = return $ ftext x
    fromNative PS.Float4 x      = return $ SqlDouble $ bread "float" x
    fromNative PS.Float8 x      = return $ SqlDouble $ bread "float" x
    fromNative PS.BpChar x      = return $ ftext x
    fromNative PS.VarChar x     = return $ ftext x
    fromNative PS.Date x        = return $ SqlLocalDate $ parseD x
    fromNative PS.Time x        = return $ SqlLocalTimeOfDay $ parseT x
    fromNative PS.Timestamp x   = return $ SqlLocalTime $ parseDT x
    fromNative PS.TimestampTZ x = return $ SqlUTCTime $ parseUTC x
    fromNative PS.Bit x         = return $ SqlBitField $ parseBit x
    fromNative PS.VarBit x      = return $ SqlBitField $ parseBit x
    fromNative PS.Numeric x     = return $ SqlDecimal $ bread "numeric" x
    fromNative PS.Void _        = return $ SqlNull
    fromNative PS.UUID x        = SqlUUID <$> case U.fromString uval of
      Nothing -> throwIO $ SqlDriverError $ pgMsg $ "could not read " ++ uval ++ " as UUID"
      Just  r -> return r
      where
        uval = TL.unpack $ decodeUTF8 x
    fromNative _ x = return $ ftext x

nativeToSqlValue _ b PQ.Binary f = binFromNative (o2b f) b
  where
    binFromNative PS.ByteA x = return $ SqlBlob x
    binFromNative x _ = throwIO $ SqlDriverError $ pgMsg $ "Can not parse bytes as (Binary format) as " ++ show x

parseSome :: (ParseTime a) => String -> B.ByteString -> a
parseSome fmt dt = case parseTime defaultTimeLocale fmt val of
  Nothing -> throw $ SqlDriverError $ pgMsg $ "Could not parse " ++ val ++ " as Date/Time in format \"" ++ fmt ++ "\""
  Just r  -> r
  where
    val = TL.unpack $ decodeUTF8 dt


parseT :: B.ByteString -> TimeOfDay
parseT = parseSome "%H:%M:%S%Q"

parseDT :: B.ByteString -> LocalTime
parseDT = parseSome "%Y-%m-%d %H:%M:%S%Q"

parseD :: B.ByteString -> Day
parseD = parseSome "%Y-%m-%d"

parseUTC :: B.ByteString -> UTCTime
parseUTC = parseSome "%Y-%m-%d %H:%M:%S%Q %Z"

parseBit :: B.ByteString -> Word64
parseBit bt = foldl makeBit 0 $ zip [0..] $ take (bitSize (undefined :: Word64)) $ reverse val
  where
    makeBit l (n, '1') = setBit l n
    makeBit l (_, '0') = l
    makeBit _ _ = throw $ SqlDriverError $ pgMsg $ "Could not parse string \"" ++ val ++ "\" as bit field"
    val = TL.unpack $ decodeUTF8 bt

throwErrorMessage :: PQ.Connection -> IO a
throwErrorMessage con = do
  errm <- throwNoMessage =<< PQ.errorMessage con
  throwIO $ SqlError "" $ TL.unpack $ decodeUTF8 errm


-- | Throws appropriate SqlError when no PQ.Result is given
getPGResult :: PQ.Connection -> Maybe PQ.Result -> IO PQ.Result
getPGResult _ (Just x) = do
  st <- PQ.resultStatus x
  case st of
    PQ.FatalError -> throwResultError x
    PQ.EmptyQuery -> throwResultError x
    PQ.BadResponse -> throwResultError x
    _ -> return x
getPGResult con Nothing = throwErrorMessage con

-- | if Nothing throws error about no message
throwNoMessage :: Maybe e -> IO e
throwNoMessage Nothing = throwIO $ SqlDriverError $ pgMsg "Error has occured, but no error message received"
throwNoMessage (Just e) = return e

-- | Get error message from the result and throw it as 'SqlError'
throwResultError :: PQ.Result -> IO a
throwResultError res = do
  errm <- throwNoMessage =<< PQ.resultErrorMessage res
  ef <- throwNoMessage =<< PQ.resultErrorField res PQ.DiagSqlstate
  throwIO $ SqlError (TL.unpack $ decodeUTF8 ef) (TL.unpack $ decodeUTF8 errm)


-- | Establish new PostgreSQL connection
connectPostgreSQL :: TL.Text -- ^ Connection string according to the documentation
                  -> IO PostgreConnection
connectPostgreSQL constr = do
  con <- PQ.connectdb $ encodeUTF8 constr
  st <- PQ.status con
  case st of
    PQ.ConnectionOk -> do
      done <- PQ.setClientEncoding con "UTF8"
      when (not done) $ throwErrorMessage con
      PostgreConnection
        <$> (newMVar $ Just con)
        <*> newChildList
        <*> (return constr)
    _ -> throwErrorMessage con

encodeUTF8 :: TL.Text -> B.ByteString
encodeUTF8 = toByteString . fromLazyText

decodeUTF8 :: B.ByteString -> TL.Text
decodeUTF8 = TL.decodeUtf8 . BL.fromChunks . (:[])

pgRun :: PostgreConnection -> TL.Text -> [SqlValue] -> IO PQ.Result
pgRun conn query values = withPGConnection conn $ \con -> do
  r <- PQ.execParams con (encodeUTF8 query) binvals PQ.Text
  getPGResult con r     -- Throwing error if failed
  where
    binvals = map sqlValueToNative values

pgRunRaw :: PostgreConnection -> TL.Text -> IO PQ.Result
pgRunRaw conn query = withPGConnection conn $ \con -> do
    r <- PQ.exec con $ encodeUTF8 query
    getPGResult con r

pgRunMany :: PostgreConnection -> TL.Text -> [[SqlValue]] -> IO ()
pgRunMany conn query values = withPGConnection conn $ \con -> do
  forM_ values $ \val -> do
    r <- PQ.execParams con binq (binv val) PQ.Text
    res <- getPGResult con r
    PQ.unsafeFreeResult res
    return ()
  where
    binq = encodeUTF8 query
    binv = map sqlValueToNative

pgMsg :: String -> String
pgMsg = ("hdbc-postgresql: " ++)

instance Connection PostgreConnection where
  type ConnStatement PostgreConnection = PostgreStatement

  disconnect conn = modifyMVar_ (postNative conn) $ \con -> do
    case con of
      Nothing -> return Nothing
      Just c -> do
        closeAllChildren $ postStatements conn
        PQ.finish c
        return Nothing

  begin conn = runRaw conn "BEGIN"
  commit conn = runRaw conn "COMMIT"
  rollback conn = runRaw conn "ROLLBACK"
  inTransaction conn = withPGConnection conn $ \con -> do
    st <- PQ.transactionStatus con
    case st of
      PQ.TransInTrans -> return True
      PQ.TransInError -> do
        hPutStrLn stderr "Transaction is in error status"
        return True
      PQ.TransUnknown -> throwIO $ SqlDriverError "Could not determine the transaction status (TransUnknown)"
      _ -> return False
  connStatus conn = withMVar (postNative conn) $ \con -> case con of
    Nothing -> return ConnDisconnected
    Just c -> do
      st <- PQ.status c
      case st of
        PQ.ConnectionOk -> return ConnOK

        PQ.ConnectionBad -> return ConnBad
        x -> throwIO $ SqlDriverError $ pgMsg $ "unexpected status code (" ++ show x ++ ")"

  prepare conn query = do
    st <- PostgreStatement
          <$> return query
          <*> return conn
          <*> newMVar STNew
    addChild (postStatements conn) st
    return st

  run conn query values = do
    res <- pgRun conn query values
    PQ.unsafeFreeResult res
    return ()

  runRaw conn query = do
    res <- pgRunRaw conn query
    PQ.unsafeFreeResult res
    return ()

  runMany = pgRunMany

  clone conn = connectPostgreSQL $ postConnString conn

  hdbcDriverName = const "postgresql"

  dbTransactionSupport = const True

pgstName :: PGStatementState -> String
pgstName STNew = "New"
pgstName (STExecuted {}) = "Executed"
pgstName (STFetching {}) = "Fetching state"
pgstName STFinished = "Finished"

withPGSTNew :: PGStatementState -> IO a -> IO a
withPGSTNew st action = case st of
  STNew -> action
  x -> throwIO $ SqlDriverError $ pgMsg $ "Statement is not in new state (" ++ (pgstName x) ++ ")"

pgstGetResult :: String -> PGStatementState -> PQ.Result
pgstGetResult _ STExecuted { pgstResult = result } = result
pgstGetResult _ STFetching { pgstResult = result } = result
pgstGetResult msg st = throw $ SqlDriverError $ pgMsg $ msg ++ " (" ++ pgstName st ++ ")"

instance Statement PostgreStatement where

  execute stmt values = modifyMVar_ (stState stmt)
                        $ \st -> withPGSTNew st $ do
    res <- pgRun (stConnection stmt) (stQuery stmt) values
    return $ STExecuted res

  executeRaw stmt = modifyMVar_ (stState stmt)
                    $ \st -> withPGSTNew st $ do
    res <- pgRunRaw (stConnection stmt) (stQuery stmt)
    return $ STExecuted res

  executeMany stmt vals = withMVar (stState stmt)
                          $ \st -> withPGSTNew st
                          $ pgRunMany (stConnection stmt) (stQuery stmt) vals

  statementStatus stmt = withMVar (stState stmt)
                         $ \st -> case st of
    STNew         -> return StatementNew
    STExecuted {} -> return StatementExecuted
    STFetching { pgstCurrent = c
               , pgstTuples  = t} -> if c >= t
                                     then return StatementFetched
                                     else return StatementExecuted
    STFinished    -> return StatementFinished

  affectedRows stmt = withMVar (stState stmt)
                      $ \st -> do
    let res = pgstGetResult "Statement is not executed or already finished" st
    tpls <- PQ.cmdTuples res
    case tpls of
      Nothing -> withPGConnection (stConnection stmt) throwErrorMessage
      Just "" -> return 0        -- when query was SELECT libPQ return empty string
      Just r  -> let rx = TL.unpack $ decodeUTF8 r
                 in case readMay rx of
                   Just x -> return x
                   Nothing -> throwIO $ SqlDriverError $ pgMsg $ "Could not read binding output as integer \"" ++ rx ++ "\""

  finish stmt = modifyMVar_ (stState stmt) doFinishStatement

  reset stmt = modifyMVar_ (stState stmt) $ \st -> do
    _ <- doFinishStatement st
    return STNew

  fetchRow stmt = modifyMVar (stState stmt) _fetchRow
    where
      _fetchRow STExecuted {pgstResult = result} = do
        fields <- PQ.nfields result
        s <- STFetching
             <$> return result
             <*> return fields
             <*> forM [0..fields-1] (PQ.fformat result)
             <*> forM [0..fields-1] (PQ.ftype result)
             <*> PQ.ntuples result
             <*> (return $ PQ.toRow (0 :: Int))
        _fetchRow s

      _fetchRow x@(STFetching result cols formats oids tpls current) =
        if current >= tpls
        then return (x, Nothing)
        else do
          ret <- forM (zip3 [0..cols-1] formats oids) $ \(col, fmt, oid) -> do
            mval <- PQ.getvalue' result current col
            case mval of
              Nothing -> withPGConnection (stConnection stmt) throwErrorMessage
              Just val -> withPGConnection (stConnection stmt) $ \con -> nativeToSqlValue con val fmt oid
          return (x {pgstCurrent = current + 1}, Just ret)

      _fetchRow x = throwIO
                    $ SqlDriverError
                    $ pgMsg $ "Statement is in wrong state (" ++ pgstName x ++ ") to fetch row"

  getColumnNames stmt = withMVar (stState stmt) $ \st -> do
    let res = pgstGetResult "Statement is in wrong state to get column names" st
    cols <- PQ.nfields res
    forM [0 .. cols-1] $ \col -> do
      mname <- PQ.fname res col
      case mname of
        Nothing   -> withPGConnection (stConnection stmt) throwErrorMessage
        Just name -> return $ decodeUTF8 name

  getColumnsCount stmt = withMVar (stState stmt) $ \st -> do
    let res = pgstGetResult "Statement is in wrong state to get columns count" st
    ret <- PQ.nfields res
    return $ toEnum $ fromEnum ret

  originalQuery = stQuery

doFinishStatement :: PGStatementState -> IO PGStatementState
doFinishStatement st = case st of
  STExecuted {pgstResult = r} -> _finish r
  STFetching {pgstResult = r} -> _finish r
  _ -> return STFinished
  where
    _finish res = do
      PQ.unsafeFreeResult res
      return STFinished
