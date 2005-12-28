-- -*- mode: haskell; -*-
{-# CFILES hugs-postgresql-helper.c #-}
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

l _ = return ()
--l m = hPutStrLn stderr ("\n" ++ m)

#include <libpq-fe.h>

data SState = 
    SState { stomv :: MVar (Maybe Stmt),
#ifdef __HUGS__
             envmv :: MVar (Maybe (Ptr CInt)),
#endif
             nextrowmv :: MVar (CInt), -- -1 for no next row (empty); otherwise, next row to read.
             dbo :: Conn,
             squery :: String,
             colnamemv :: MVar [String]}

-- FIXME: we currently do no prepare optimization whatsoever.

newSth :: Conn -> String -> IO Statement               
newSth indbo query = 
    do l "in newSth"
       newstomv <- newMVar Nothing
#ifdef __HUGS__
       newenvmv <- newMVar Nothing
#endif
       newnextrowmv <- newMVar (-1)
       newcolnamemv <- newMVar []
       usequery <- case convertSQL query of
                      Left errstr -> throwDyn $ SqlError
                                      {seState = "",
                                       seNativeError = (-1),
                                       seErrorMsg = "hdbc prepare: " ++ 
                                                    show errstr}
                      Right converted -> return converted
       let sstate = SState {stomv = newstomv, nextrowmv = newnextrowmv,
                            dbo = indbo, squery = usequery,
#ifdef __HUGS__
                            envmv = newenvmv,
#endif
                            colnamemv = newcolnamemv}
       return $ Statement {execute = fexecute sstate,
                           executeMany = fexecutemany sstate,
                           finish = public_ffinish sstate,
                           fetchRow = ffetchrow sstate,
                           originalQuery = query,
                           getColumnNames = readMVar (colnamemv sstate)}

{- For now, we try to just  handle things as simply as possible.
FIXME lots of room for improvement here (types, etc). -}
fexecute sstate args = withForeignPtr (dbo sstate) $ \cconn ->
                       withCString (squery sstate) $ \cquery ->
                       withCStringArr0 args $ \paramvalues ->
                       withArray (map convlengths args) $ \paramlengths ->
                       withArray (map convformats args) $ \paramformats ->
    do l "in fexecute"
       public_ffinish sstate    -- Sets nextrowmv to -1
       resptr <- pqexecParams cconn cquery
                 (genericLength args) nullPtr paramvalues paramlengths
                 paramformats 0
       status <- pqresultStatus resptr
       case status of
         #{const PGRES_EMPTY_QUERY} ->
             do l $ "PGRES_EMPTY_QUERY: " ++ squery sstate
                pqclear resptr
                swapMVar (colnamemv sstate) []
                return 0
         #{const PGRES_COMMAND_OK} ->
             do l $ "PGRES_COMMAND_OK: " ++ squery sstate
                rowscs <- pqcmdTuples resptr
                rows <- peekCString rowscs
                pqclear resptr
                swapMVar (colnamemv sstate) []
                return $ case rows of
                                   "" -> 0
                                   x -> read x
         #{const PGRES_TUPLES_OK} -> 
             do l $ "PGRES_TUPLES_OK: " ++ squery sstate
                fgetcolnames resptr >>= swapMVar (colnamemv sstate) 
                numrows <- pqntuples resptr
                if numrows < 1
                   then do pqclear resptr
                           return 0
                   else do 
#ifdef __HUGS__
                        env <- pqhugs_alloc_env
                        fresptr <- newForeignPtrEnv pqclearptr env resptr
#else
                        fresptr <- newForeignPtr pqclearptr resptr
#endif
                        swapMVar (nextrowmv sstate) 0
                        swapMVar (stomv sstate) (Just fresptr)
#ifdef __HUGS__
                        swapMVar (envmv sstate) (Just env)
#endif
                        return 0
         _ -> do l $ "PGRES ERROR: " ++ squery sstate
                 csstatusmsg <- pqresStatus status
                 cserrormsg <- pqresultErrorMessage resptr
                 statusmsg <- peekCString csstatusmsg
                 errormsg <- peekCString cserrormsg
                 pqclear resptr
                 throwDyn $ 
                          SqlError {seState = "",
                                    seNativeError = fromIntegral status,
                                    seErrorMsg = "execute: " ++ statusmsg ++
                                                 ": " ++ errormsg}
    where convlengths :: SqlValue -> CInt
          convlengths (SqlString x) = genericLength x
          convlengths _ = 0
          convformats :: SqlValue -> CInt
          convformats (SqlString _) = 1
          convformats _ = 0
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
#ifdef __HUGS__
                               ffinish (envmv sstate) cmstmt
#else
                               ffinish cmstmt
#endif
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
                   else do len <- pqgetlength p row icol
                           text <- pqgetvalue p row icol
                           s <- peekCStringLen (text, fromIntegral len)
                           return (SqlString s)

fgetcolnames cstmt =
    do ncols <- pqnfields cstmt
       mapM (\i -> pqfname cstmt i >>= peekCString) [0..(ncols - 1)]

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
#ifdef __HUGS__
          worker (Just sth) = ffinish (envmv sstate) sth >> return Nothing
#else
          worker (Just sth) = ffinish sth >> return Nothing
#endif

#ifdef __HUGS__
ffinish :: MVar (Maybe (Ptr CInt)) -> Stmt -> IO ()
ffinish envmv p = modifyMVar_ envmv $ \mcenv ->
    do case mcenv of
          Nothing -> fail "Internal error, HDBC PostgreSQL hugs: ffinish called when no cenv present"
          Just cenv -> do withStmt p $ \cstmt -> pqclear_hugs cenv cstmt
                          return Nothing
#else
ffinish :: Stmt -> IO ()
ffinish p = l "ffinish" >> finalizeForeignPtr p
#endif

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

#ifdef __HUGS__
foreign import ccall unsafe "hugs-postgresql-helper.h PQhugs_alloc_env"
  pqhugs_alloc_env :: IO (Ptr CInt)

foreign import ccall unsafe "hugs-postgresql-helper.h PQclear_hugs_app"
  pqclear_hugs :: Ptr CInt -> Ptr CStmt -> IO ()

foreign import ccall unsafe "hugs-postgresql-helper.h &PQclear_hugs_fptr"
  pqclearptr :: FunPtr (Ptr CInt -> Ptr CStmt -> IO ())
#else
foreign import ccall unsafe "libpq-fe.h &PQclear"
  pqclearptr :: FunPtr ((Ptr CStmt) -> IO ())
#endif

foreign import ccall unsafe "libpq-fe.h PQclear"
  pqclear :: Ptr CStmt -> IO ()

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

foreign import ccall unsafe "libpq-fe.h PQgetlength"
  pqgetlength :: Ptr CStmt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQfname"
  pqfname :: Ptr CStmt -> CInt -> IO CString
