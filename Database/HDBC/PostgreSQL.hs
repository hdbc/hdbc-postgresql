{-# LANGUAGE
  TypeFamilies
, DeriveDataTypeable
, OverloadedStrings
  #-}

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
         PostgreConnection(..)
       , PGStatementState(..)
       , PostgreStatement(..)
       , connectPostgreSQL
       ) where

import Database.HDBC.PostgreSQL.Implementation

