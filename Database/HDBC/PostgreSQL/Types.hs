module Database.HDBC.PostgreSQL.Types
where

import Foreign.ForeignPtr

data CConn = CConn
type Conn = ForeignPtr CConn

data CStmt = CStmt
type Stmt = ForeignPtr CStmt

