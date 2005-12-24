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

raiseError :: String -> CInt -> (Ptr CConn) -> IO a
raiseError msg cconn code =
    do rc <- pqerrorMessage cconn
       str <- peekCString rc
       throwDyn $ SqlError {seState = "",
                            seNativeError = fromIntegral code,
                            seErrorMsg = msg ++ ": " ++ str}

withCConn :: Conn -> (Ptr CConn -> IO b) -> IO b
withCConn = withForeignPtr

withStmt :: Stmt -> (Ptr CStmt -> IO b) -> IO b
withStmt = withForeignPtr

foreign import ccall unsafe "libpq-fe.h PQerrorMessage"
  pqerrorMessage :: Ptr CConn -> IO CString
