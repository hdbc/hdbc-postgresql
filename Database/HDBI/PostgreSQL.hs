{-# LANGUAGE
  TypeFamilies
, DeriveDataTypeable
, OverloadedStrings
  #-}

{- |
   Module     : Database.HDBI.PostgreSQL
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : Aleksey Uymanov <s9gf4ult@gmail.com>
   Stability  : experimental
   Portability: portable

HDBI driver interface for PostgreSQL 8.x and above

Written by John Goerzen, jgoerzen\@complete.org

-}

module Database.HDBI.PostgreSQL
       (
         PostgreConnection(..)
       , PostgreStatement(..)
       , PGStatementState(..)
       , connectPostgreSQL
       ) where

import Database.HDBI.PostgreSQL.Implementation
