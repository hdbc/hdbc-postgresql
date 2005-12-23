module Database.HDBC.Sqlite3.Types
where

import Foreign.ForeignPtr

data CSqlite3
type Sqlite3 = ForeignPtr CSqlite3

data CStmt
type Stmt = ForeignPtr CStmt

