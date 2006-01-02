module Database.HDBC.PostgreSQL.Types
where

import Foreign.ForeignPtr

data CConn = CConn
data WrappedCConn = Ptr CConn
type Conn = ForeignPtr WrappedCConn

data CStmt = CStmt
data WrappedCStmt = Ptr CStmr
type Stmt = ForeignPtr WrappedCStmt

