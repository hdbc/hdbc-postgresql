module Database.HDBC.Sqlite3.Types
where

import Foreign.C.Types
import Foreign.C.ForeignPtr
import Foreign.Ptr

data CSqlite3
type Sqlite3 = ForeignPtr CSqlite3

data CStmt
type Stmt = ForeignPtr CStmt

