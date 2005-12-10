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

{- |
   Module     : Database.HDBC.Sqlite3
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU LGPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

HDBC driver interface for Sqlite 3.x.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Database.HDBC.Sqlite3
    (
     connectSqlite3
    )

where

import Database.HDBC.Types
import Database.HDBC
import Foreign.C.Types
import Foreign.C.ForeignPtr
import Foreign.Ptr

connectSqlite3 :: FilePath -> IO Connection
connectSqlite3 fp = 
    withCString fp 
        (\cs -> alloca 
         (\(p::Ptr (Ptr CSqlite3)) ->
              do res <- sqlite3_open cs p
                 checkError res
                 o <- peek p
                 fptr <- newForeignPtr sqlite3_finalize p
                 mkConn fptr
         )
        )

mkConn :: Sqlite3 -> IO Connection
mkConn obj =
    do begin_transaction obj
       return $ Connection {
                            disconnect = fdisconnect obj,
                            commit = fcommit obj,
                            rollback = frollback obj,
                            run = frun obj,
                            prepare = fprepare obj}
                    
data CSqlite3
type Sqlite3 = ForeignPtr CSqlite3

data CStmt
type Stmt = ForeignPtr CStmt

foreign import ccall unsafe "sqlite3.h sqlite3_open"
  sqlite3_open :: CString -> (Ptr (Ptr CSqlite3)) -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_errmsg"
  sqlite3_errmsg :: (Ptr CSqlite3) -> IO CString

foreign import ccall unsafe "sqlite3.h &sqlite3_finalize"
  sqlite3_finalize :: FunPtr (Ptr CStmt) -> IO Cint
