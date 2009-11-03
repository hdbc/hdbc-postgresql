module Database.HDBC.PostgreSQL.Types
where

import Foreign

data CConn = CConn
type WrappedCConn = Ptr CConn
type Conn = ForeignPtr WrappedCConn

data CStmt = CStmt
type WrappedCStmt = Ptr CStmt
type Stmt = ForeignPtr WrappedCStmt
type ResultStatus = Word32

