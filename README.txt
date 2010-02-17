Welcome to HDBC, Haskell Database Connectivity.

This package provides a database backend driver for PostgreSQL.

Please see HDBC itself for documentation on use.

This package provides one function in module Database.HDBC.PostgreSQL:

{- | Connect to a PostgreSQL server.

See <http://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT> for the meaning
of the connection string. -}
connectPostgreSQL :: String -> IO Connection

An example would be:
dbh <- connectPostgreSQL "host=localhost dbname=testdb user=foo"

DIFFERENCES FROM HDBC STANDARD
------------------------------

None known at this time.
