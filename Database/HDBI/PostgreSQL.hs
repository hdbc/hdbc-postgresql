{-# LANGUAGE
  TypeFamilies
, DeriveDataTypeable
, OverloadedStrings
  #-}

{- |
   Module     : Database.HDBI.PostgreSQL
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : experimental
   Portability: portable

HDBI driver interface for PostgreSQL 8.x

Written by John Goerzen, jgoerzen\@complete.org

-}

module Database.HDBI.PostgreSQL
       (
         PostgreConnection(..)
       , PGStatementState(..)
       , PostgreStatement(..)
       , connectPostgreSQL
       ) where

import Database.HDBI.PostgreSQL.Implementation

