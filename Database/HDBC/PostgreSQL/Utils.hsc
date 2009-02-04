{- -*- mode: haskell; -*- 
Copyright (C) 2005-2009 John Goerzen <jgoerzen@complete.org>

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

module Database.HDBC.PostgreSQL.Utils where
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Database.HDBC(throwSqlError)
import Database.HDBC.Types
import Database.HDBC.PostgreSQL.Types
import Foreign.C.Types
import Control.Exception
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Data.Word
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.ByteString as B
#ifndef __HUGS__
-- Hugs includes this in Data.ByteString
import qualified Data.ByteString.Unsafe as B
#endif

#include "hdbc-postgresql-helper.h"

raiseError :: String -> Word32 -> (Ptr CConn) -> IO a
raiseError msg code cconn =
    do rc <- pqerrorMessage cconn
       bs <- B.packCString rc
       let str = BUTF8.toString bs
       throwSqlError $ SqlError {seState = "",
                                 seNativeError = fromIntegral code,
                                 seErrorMsg = msg ++ ": " ++ str}

{- This is a little hairy.

We have a Conn object that is actually a finalizeonce wrapper around
the real object.  We use withConn to dereference the foreign pointer,
and then extract the pointer to the real object from the finalizeonce struct.

But, when we close the connection, we need the finalizeonce struct, so that's
done by withRawConn.

Ditto for statements. -}

withConn :: Conn -> (Ptr CConn -> IO b) -> IO b
withConn = genericUnwrap

withRawConn :: Conn -> (Ptr WrappedCConn -> IO b) -> IO b
withRawConn = withForeignPtr

withStmt :: Stmt -> (Ptr CStmt -> IO b) -> IO b
withStmt = genericUnwrap

withRawStmt :: Stmt -> (Ptr WrappedCStmt -> IO b) -> IO b
withRawStmt = withForeignPtr

withCStringArr0 :: [SqlValue] -> (Ptr CString -> IO a) -> IO a
withCStringArr0 inp action = withAnyArr0 convfunc freefunc inp action
    where convfunc SqlNull = return nullPtr
{-
          convfunc y@(SqlZonedTime _) = convfunc (SqlString $ 
                                                "TIMESTAMP WITH TIME ZONE '" ++ 
                                                fromSql y ++ "'")
-}
          convfunc y@(SqlUTCTime _) = convfunc (SqlZonedTime (fromSql y))
          convfunc y@(SqlEpochTime _) = convfunc (SqlZonedTime (fromSql y))
          convfunc x = cstrUtf8BString (fromSql x)
          freefunc x =
              if x == nullPtr
                 then return ()
                 else free x

withAnyArr0 :: (a -> IO (Ptr b)) -- ^ Function that transforms input data into pointer
            -> (Ptr b -> IO ())  -- ^ Function that frees generated data
            -> [a]               -- ^ List of input data
            -> (Ptr (Ptr b) -> IO c) -- ^ Action to run with the C array
            -> IO c             -- ^ Return value
withAnyArr0 input2ptract freeact inp action =
    bracket (mapM input2ptract inp)
            (\clist -> mapM_ freeact clist)
            (\clist -> withArray0 nullPtr clist action)

cstrUtf8BString :: B.ByteString -> IO CString
cstrUtf8BString bs = do
    B.unsafeUseAsCStringLen bs $ \(s,len) -> do
        res <- mallocBytes (len+1)
        -- copy in
        copyBytes res s len
        -- null terminate
        poke (plusPtr res len) (0::CChar)
        -- return ptr
        return res

genericUnwrap :: ForeignPtr (Ptr a) -> (Ptr a -> IO b) -> IO b
genericUnwrap fptr action = withForeignPtr fptr (\structptr ->
    do objptr <- #{peek finalizeonce, encapobj} structptr
       action objptr
                                                )
          
foreign import ccall unsafe "libpq-fe.h PQerrorMessage"
  pqerrorMessage :: Ptr CConn -> IO CString

