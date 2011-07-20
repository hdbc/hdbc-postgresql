{- |
   Module     : Database.HDBC.PostgreSQL
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

HDBC driver interface for PostgreSQL 8.x

Written by John Goerzen, jgoerzen\@complete.org

/NOTE ON DATES AND TIMES/

The recommended correspondence between PostgreSQL date and time types and HDBC SqlValue
types is:

* SqlLocalDate: DATE

* SqlLocalTimeOfDay: TIME WITHOUT TIME ZONE

* SqlZonedLocalTimeOfDay: TIME WITH TIME ZONE

* SqlLocalTime: TIMESTAMP WITHOUT TIME ZONE

* SqlZonedTime: TIMESTAMP WITH TIME ZONE

* SqlUTCTime: TIMESTAMP WITH TIME ZONE

* SqlDiffTime: INTERVAL

* SqlPOSIXTime: NUMERIC

* SqlEpochTime: INTEGER

* SqlTimeDiff: INTERVAL

Other combinations are possible, and may even be converted automatically.
The above simply represents the types that seem the most logical correspondence,
and thus are tested by the HDBC-PostgreSQL test suite.

-}

module Database.HDBC.PostgreSQL
    (
     -- * Connecting to Databases
     connectPostgreSQL, withPostgreSQL,
     connectPostgreSQL', withPostgreSQL',
     Connection,

     -- * Transactions
     begin,

     -- * PostgreSQL Error Codes
     --
     -- |When an @SqlError@ is thrown, the field @seState@ is set to one of the following
     -- error codes.
     module Database.HDBC.PostgreSQL.ErrorCodes,

     -- * Threading
     -- $threading
    )

where

import Database.HDBC.PostgreSQL.Connection(connectPostgreSQL, withPostgreSQL,
                                           connectPostgreSQL', withPostgreSQL',
                                           begin, Connection())
import Database.HDBC.PostgreSQL.ErrorCodes

{- $threading
   
   Provided the local libpq library is thread-safe, multiple 'Connection's may be used
   to have concurrent database queries.  Concurrent queries issued on a single 
   'Connection' will be performed serially.

   When the local libpq library is not thread-safe (ie. it has not been compiled with 
   --enable-thread-safety), only a single database function will be performed at a time.
  
-}

