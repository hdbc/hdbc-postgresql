{-
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

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

{- |
   Module     : Database.HDBC.PostgreSQL
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU LGPL, version 2.1 or above

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

/NOTE ON ERROR HANDLING/

When an @SqlError@ is thrown, the field @seState@ is set to one of the error codes
described at
<http://www.postgresql.org/docs/8.1/static/errcodes-appendix.html>.
-}

module Database.HDBC.PostgreSQL
    (
     connectPostgreSQL, Connection
    )

where

import Database.HDBC.PostgreSQL.Connection(connectPostgreSQL, Connection())
