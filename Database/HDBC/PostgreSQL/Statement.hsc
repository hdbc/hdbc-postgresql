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
module Database.HDBC.PostgreSQL.Statement where
import Database.HDBC.Types
import Database.HDBC
import Database.HDBC.PostgreSQL.Types
import Database.HDBC.PostgreSQL.Utils
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Concurrent.MVar
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Monad
import Data.List
import Data.Word
import Control.Exception
import System.IO
import Database.HDBC.PostgreSQL.Parser(convertSQL)
import Database.HDBC.DriverUtils
import Database.HDBC.PostgreSQL.PTypeConv

l _ = return ()
--l m = hPutStrLn stderr ("\n" ++ m)

#include <libpq-fe.h>

data SState = 
    SState { stomv :: MVar (Maybe Stmt),
             nextrowmv :: MVar (CInt), -- -1 for no next row (empty); otherwise, next row to read.
             dbo :: Conn,
             squery :: String,
             coldefmv :: MVar [(String, SqlColDef)]}

-- FIXME: we currently do no prepare optimization whatsoever.

newSth :: Conn -> ChildList -> String -> IO Statement               
newSth indbo mchildren query = 
    do l "in newSth"
       newstomv <- newMVar Nothing
       newnextrowmv <- newMVar (-1)
       newcoldefmv <- newMVar []
       usequery <- case convertSQL query of
                      Left errstr -> throwDyn $ SqlError
                                      {seState = "",
                                       seNativeError = (-1),
                                       seErrorMsg = "hdbc prepare: " ++ 
                                                    show errstr}
                      Right converted -> return converted
       let sstate = SState {stomv = newstomv, nextrowmv = newnextrowmv,
                            dbo = indbo, squery = usequery,
                            coldefmv = newcoldefmv}
       let retval = 
                Statement {execute = fexecute sstate,
                           executeMany = fexecutemany sstate,
                           finish = public_ffinish sstate,
                           fetchRow = ffetchrow sstate,
                           originalQuery = query,
                           getColumnNames = readMVar (coldefmv sstate)}
       addChild mchildren retval
       return retval

{- For now, we try to just  handle things as simply as possible.
FIXME lots of room for improvement here (types, etc). -}
fexecute sstate args = withConn (dbo sstate) $ \cconn ->
                       withCString (squery sstate) $ \cquery ->
                       withCStringArr0 args $ \cargs ->
    do l "in fexecute"
       public_ffinish sstate    -- Sets nextrowmv to -1
       resptr <- pqexecParams cconn cquery
                 (genericLength args) nullPtr cargs nullPtr nullPtr 0
       status <- pqresultStatus resptr
       case status of
         #{const PGRES_EMPTY_QUERY} ->
             do l $ "PGRES_EMPTY_QUERY: " ++ squery sstate
                pqclear_raw resptr
                swapMVar (coldefmv sstate) []
                return 0
         #{const PGRES_COMMAND_OK} ->
             do l $ "PGRES_COMMAND_OK: " ++ squery sstate
                rowscs <- pqcmdTuples resptr
                rows <- peekCString rowscs
                pqclear_raw resptr
                swapMVar (coldefmv sstate) []
                return $ case rows of
                                   "" -> 0
                                   x -> read x
         #{const PGRES_TUPLES_OK} -> 
             do l $ "PGRES_TUPLES_OK: " ++ squery sstate
                fgetcoldef resptr >>= swapMVar (coldefmv sstate) 
                numrows <- pqntuples resptr
                if numrows < 1
                   then do pqclear_raw resptr
                           return 0
                   else do 
                        wrappedptr <- withRawConn (dbo sstate) 
                                      (\rawconn -> wrapstmt resptr rawconn)
                        fresptr <- newForeignPtr pqclearptr wrappedptr
                        swapMVar (nextrowmv sstate) 0
                        swapMVar (stomv sstate) (Just fresptr)
                        return 0
         _ -> do l $ "PGRES ERROR: " ++ squery sstate
                 csstatusmsg <- pqresStatus status
                 cserrormsg <- pqresultErrorMessage resptr
                 statusmsg <- peekCString csstatusmsg
                 errormsg <- peekCString cserrormsg
                 pqclear_raw resptr
                 throwDyn $ 
                          SqlError {seState = "",
                                    seNativeError = fromIntegral status,
                                    seErrorMsg = "execute: " ++ statusmsg ++
                                                 ": " ++ errormsg}
{- General algorithm: find out how many columns we have, check the type
of each to see if it's NULL.  If it's not, fetch it as text and return that.
-}

ffetchrow :: SState -> IO (Maybe [SqlValue])
ffetchrow sstate = modifyMVar (nextrowmv sstate) dofetchrow
    where dofetchrow (-1) = l "ffr -1" >> return ((-1), Nothing)
          dofetchrow nextrow = modifyMVar (stomv sstate) $ \stmt -> 
             case stmt of
               Nothing -> l "ffr nos" >> return (stmt, ((-1), Nothing))
               Just cmstmt -> withStmt cmstmt $ \cstmt ->
                 do l $ "ffetchrow: " ++ show nextrow
                    numrows <- pqntuples cstmt
                    l $ "numrows: " ++ show numrows
                    if nextrow >= numrows
                       then do l "no more rows"
                               -- Don't use public_ffinish here
                               ffinish cmstmt
                               return (Nothing, ((-1), Nothing))
                       else do l "getting stuff"
                               ncols <- pqnfields cstmt
                               res <- mapM (getCol cstmt nextrow) 
                                      [0..(ncols - 1)]
                               return (stmt, (nextrow + 1, Just res))
          getCol p row icol = 
             do isnull <- pqgetisnull p row icol
                if isnull /= 0
                   then return SqlNull
                   else do text <- pqgetvalue p row icol
                           s <- peekCString text
                           return (SqlString s)

fgetcolnames cstmt =
    do ncols <- pqnfields cstmt
       mapM (\i -> pqfname cstmt i >>= peekCString) [0..(ncols - 1)]

fgetcoldef cstmt =
    do ncols <- pqnfields cstmt
       mapM desccol [0..(ncols - 1)]
    where desccol i =
              do colname <- (pqfname cstmt i >>= peekCString)
                 coltype <- pqftype cstmt i
                 --coloctets <- pqfsize
                 let coltype = oidToColDef coltype
                 return (colname, coltype)

-- FIXME: needs a faster algorithm.
fexecutemany :: SState -> [[SqlValue]] -> IO ()
fexecutemany sstate arglist =
    mapM_ (fexecute sstate) arglist >> return ()

-- Finish and change state
public_ffinish sstate = 
    do l "public_ffinish"
       swapMVar (nextrowmv sstate) (-1)
       modifyMVar_ (stomv sstate) worker
    where worker Nothing = return Nothing
          worker (Just sth) = ffinish sth >> return Nothing

ffinish :: Stmt -> IO ()
ffinish p = withRawStmt p $ pqclear

foreign import ccall unsafe "libpq-fe.h PQresultStatus"
  pqresultStatus :: (Ptr CStmt) -> IO #{type ExecStatusType}

foreign import ccall unsafe "libpq-fe.h PQexecParams"
  pqexecParams :: (Ptr CConn) -> CString -> CInt ->
                  (Ptr #{type Oid}) ->
                  (Ptr CString) ->
                  (Ptr CInt) ->
                  (Ptr CInt) ->
                  CInt ->
                  IO (Ptr CStmt)

foreign import ccall unsafe "hdbc-postgresql-helper.h PQclear_app"
  pqclear :: Ptr WrappedCStmt -> IO ()

foreign import ccall unsafe "hdbc-postgresql-helper.h &PQclear_finalizer"
  pqclearptr :: FunPtr (Ptr WrappedCStmt -> IO ())

foreign import ccall unsafe "libpq-fe.h PQclear"
  pqclear_raw :: Ptr CStmt -> IO ()

foreign import ccall unsafe "hdbc-postgresql-helper.h wrapobj"
  wrapstmt :: Ptr CStmt -> Ptr WrappedCConn -> IO (Ptr WrappedCStmt)

foreign import ccall unsafe "libpq-fe.h PQcmdTuples"
  pqcmdTuples :: Ptr CStmt -> IO CString
foreign import ccall unsafe "libpq-fe.h PQresStatus"
  pqresStatus :: #{type ExecStatusType} -> IO CString

foreign import ccall unsafe "libpq-fe.h PQresultErrorMessage"
  pqresultErrorMessage :: (Ptr CStmt) -> IO CString

foreign import ccall unsafe "libpq-fe.h PQntuples"
  pqntuples :: Ptr CStmt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQnfields"
  pqnfields :: Ptr CStmt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQgetisnull"
  pqgetisnull :: Ptr CStmt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQgetvalue"
  pqgetvalue :: Ptr CStmt -> CInt -> CInt -> IO CString

foreign import ccall unsafe "libpq-fe.h PQfname"
  pqfname :: Ptr CStmt -> CInt -> IO CString
