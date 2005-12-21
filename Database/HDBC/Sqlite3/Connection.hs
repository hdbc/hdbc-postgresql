{-
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and\/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

-}

module Database.HDBC.Sqlite3.Connection where

import Database.HDBC.Types
import Database.HDBC
import Database.HDBC.Sqlite3.Types
import Database.HDBC.Sqlite3.Statement
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Database.HDBC.Sqlite3.Utils
import Foreign.ForeignPtr
import Foreign.Ptr

connectSqlite3 :: FilePath -> IO Connection
connectSqlite3 fp = 
    withCString fp 
        (\cs -> alloca 
         (\(p::Ptr (Ptr CSqlite3)) ->
              do res <- sqlite3_open cs p
                 o <- peek p
                 fptr <- newForeignPtr sqlite3_closeptr o
                 newconn <- mkConn fptr
                 checkError ("connectSqlite3 " ++ fp) fptr res
                 return newconn
         )
        )

mkConn :: Sqlite3 -> IO Connection
mkConn obj =
    do begin_transaction obj
       return $ Connection {
                            disconnect = fdisconnect obj,
                            commit = fcommit obj,
                            rollback = frollback obj,
                            sRun = fsrun obj,
                            prepare = newSth obj}

--------------------------------------------------
-- Guts here
--------------------------------------------------

begin_transaction :: Sqlite3 -> IO ()
begin_transaction o = fsrun o "BEGIN" [] >> return ()

fsrun o query args =
    do sth <- newSth o query
       res <- sExecute sth args
       finish sth
       return res

fcommit o = do fsrun o "COMMIT" []
               begin_transaction o
frollback o =  do fsrun o "COMMIT" []
                  begin_transaction o
fdisconnect o = withForeignPtr o (\p -> do r <- sqlite3_close p
                                           checkError "disconnect" o r)

foreign import ccall unsafe "sqlite3.h sqlite3_open"
  sqlite3_open :: CString -> (Ptr (Ptr CSqlite3)) -> IO CInt

foreign import ccall unsafe "sqlite3.h &sqlite3_close"
  sqlite3_closeptr :: FunPtr ((Ptr CSqlite3) -> IO ())

foreign import ccall unsafe "sqlite3.h sqlite3_close"
  sqlite3_close :: Ptr CSqlite3 -> IO CInt