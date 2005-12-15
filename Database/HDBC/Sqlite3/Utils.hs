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

module Database.HDBC.Sqlite3.Utils where
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Database.HDBC.Types
import Database.HDBC.Sqlite3.Types
import Foreign.C.Types
import Control.Exception

checkError :: String -> Sqlite3 -> CInt -> IO ()
checkError msg _ 0 = return ()
checkError msg o res =
    withForeignPtr o
     (\p -> do rc <- sqlite3_errmsg p
               str <- peekCString rc
               throwDyn $ SqlError {seState = "",
                                    seNativeError = fromIntegral res,
                                    seErrorMsg = str}
     )

foreign import ccall unsafe "sqlite3.h sqlite3_errmsg"
  sqlite3_errmsg :: (Ptr CSqlite3) -> IO CString

