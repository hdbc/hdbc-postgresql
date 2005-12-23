{- -*- mode: haskell; -*- 
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

module Database.HDBC.PostgreSQL.Utils where
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Database.HDBC.Types
import Database.HDBC.PostgreSQL.Types
import Foreign.C.Types
import Control.Exception
import Foreign.Storable

#include "hdbc-sqlite3-helper.h"

checkError :: String -> Sqlite3 -> CInt -> IO ()
checkError _ _ 0 = return ()
checkError msg o res =
    withSqlite3 o
     (\p -> do rc <- sqlite3_errmsg p
               str <- peekCString rc
               throwDyn $ SqlError {seState = "",
                                    seNativeError = fromIntegral res,
                                    seErrorMsg = msg ++ ": " ++ str}
     )

{- This is a little hairy.

We have a CSqlite3 object that is actually a finalizeonce wrapper around
the real object.  We use withSqlite3 to dereference the foreign pointer,
and then extract the pointer to the real object from the finalizeonce struct.

But, when we close the connection, we need the finalizeonce struct, so that's
done by withRawSqlite3.

Ditto for statements. -}

withSqlite3 :: Sqlite3 -> (Ptr CSqlite3 -> IO b) -> IO b
withSqlite3 = genericUnwrap

withRawSqlite3 :: Sqlite3 -> (Ptr CSqlite3 -> IO b) -> IO b
withRawSqlite3 = withForeignPtr

withStmt :: Stmt -> (Ptr CStmt -> IO b) -> IO b
withStmt = genericUnwrap

withRawStmt :: Stmt -> (Ptr CStmt -> IO b) -> IO b
withRawStmt = withForeignPtr


genericUnwrap :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
genericUnwrap fptr action = withForeignPtr fptr (\structptr ->
    do objptr <- #{peek finalizeonce, encapobj} structptr
       action objptr
                                                )

foreign import ccall unsafe "sqlite3.h sqlite3_errmsg"
  sqlite3_errmsg :: (Ptr CSqlite3) -> IO CString

