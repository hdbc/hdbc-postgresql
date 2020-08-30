module Database.HDBC.PostgreSQL.Types
where

import Foreign
import Control.Concurrent.MVar

type ConnLock = MVar ()

data CConn = CConn
type Conn = (ConnLock, ForeignPtr CConn)

data CStmt = CStmt
type Stmt = ForeignPtr CStmt
type ResultStatus = Word32

