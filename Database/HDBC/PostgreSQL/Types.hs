module Database.HDBC.PostgreSQL.Types
where

import Foreign
import Control.Concurrent.MVar

type ConnLock = MVar ()

data CConn = CConn
type WrappedCConn = Ptr CConn
type Conn = (ConnLock, ForeignPtr WrappedCConn)

data CStmt = CStmt
type WrappedCStmt = Ptr CStmt
type Stmt = ForeignPtr WrappedCStmt
type ResultStatus = Word32

