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
module Database.HDBC.Sqlite3.Statement where
import Database.HDBC.Types
import Database.HDBC
import Database.HDBC.Sqlite3.Types
import Database.HDBC.Sqlite3.Statement
import Foreign.C.Types
import Foreign.C.ForeignPtr
import Foreign.Ptr
import Control.Concurrent.MVar

fprepare o str = withForeignPtr 
  (\p -> withCStringLen
   (\cslen -> alloca
    (\(newp::Ptr (Ptr CStmt)) ->
     (do res <- sqlite3_prepare p (fst cslen) (snd cslen) newp nullPtr
         checkError ("prepare " ++ str) res
         o <- peek newp
         fptr <- newForeignPtr sqlite3_finalizeptr o
         mkstmt fptr
     )
     )
   )
   )
                    
mkstmt o = 
    do mv <- newMVar False
           Statement {sExecute = fexecute mv o,
                      sExecuteMany = fexecutemany mv o,
                      finish = ffinish o,
                      fetchRow = ffetchrow mv o}

fexecute mv o args = withForeignPtr o
  (\p -> do c <- sqlite3_bind_parameter_count p
            when (c /= genericLength args)
                 (error "Wrong number of bind args")
            modifyMVar_ mv (\_ -> return False)
            sqlite3_reset p >>= checkError "execute"
            bindArgs p
            r <- sqlite3_step p
            case r of
              #{const SQLITE_ROW} -> modifyMVar_ mv (\_ -> return True)
              #{const SQLITE_DONE} -> return ()
              x -> checkError x >> error "Invalid result from sqlite3_step"
            return (-1)

fexecutemany mv o arglist =
    mapM_ (fexecute mv o) args

ffinish o = withForeignPtr o (\p -> sqlite3_finalize p >>= checkError "finish")

foreign import ccall unsafe "sqlite3.h &sqlite3_finalize"
  sqlite3_finalizeptr :: FunPtr ((Ptr CStmt) -> IO CInt)

foreign import ccall unsafe "sqlite3.h sqlite3_finalize"
  sqlite3_finalize :: (Ptr CStmt) -> IO Cint

foreign import ccall unsafe "sqlite3.h sqlite3_prepare"
  sqlite3_prepare :: (Ptr CSqlite3) -> CString -> CInt -> Ptr (Ptr CStmt) -> Ptr (Ptr CString) -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_bind_parameter_count"
  sqlite3_bind_parameter_count :: (Ptr CStmt) -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_step"
  sqlite3_step :: (Ptr CStmt) -> IO CInt
