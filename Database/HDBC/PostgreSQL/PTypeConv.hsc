-- -*- mode: haskell; -*-
module Database.HDBC.PostgreSQL.PTypeConv where
import Database.HDBC.ColTypes
import Data.Word
import Data.Int

#include "pgtypes.h"
#include <libpq-fe.h>


colDescForPGAttr :: #{type Oid} -> Int -> String -> Bool -> SqlColDesc
colDescForPGAttr atttypeid attlen formattedtype attnotnull =
    let
        coltype = oidToColType atttypeid

        size = if attlen == -1 then maybeExtractFirstParenthesizedNumber formattedtype
               else Just attlen

        decDigs = if coltype == SqlNumericT then maybeExtractSecondParenthesizedNumber formattedtype
                  else Nothing
    in
      SqlColDesc { colType = coltype,
                   colSize = size,
                   colOctetLength = Nothing, -- not available in postgres
                   colDecDigits = decDigs,
                   colNullable = Just attnotnull }
    where
      maybeExtractFirstParenthesizedNumber s = case extractParenthesizedInts s of n:_ -> Just n; _ -> Nothing

      maybeExtractSecondParenthesizedNumber s = case extractParenthesizedInts s of _:n2:_ -> Just n2; _ -> Nothing

      extractParenthesizedInts :: String -> [Int]
      extractParenthesizedInts s =
          case takeWhile (/=')') $ dropWhile (/='(') s of
            '(':textBetweenParens ->
                case map fst $ reads $ "[" ++ textBetweenParens ++ "]" of
                  l:_ -> l
                  [] -> []
            _ -> []



oidToColDef :: #{type Oid} -> SqlColDesc
oidToColDef oid =
    SqlColDesc {colType = (oidToColType oid),
                colSize = Nothing,
                colOctetLength = Nothing,
                colDecDigits = Nothing,
                colNullable = Nothing}

oidToColType :: #{type Oid} -> SqlTypeId
oidToColType oid =
    case oid of
      #{const PG_TYPE_CHAR} -> SqlCharT
      #{const PG_TYPE_CHAR2} -> SqlCharT
      #{const PG_TYPE_CHAR4} -> SqlCharT
      #{const PG_TYPE_CHAR8} -> SqlCharT
      #{const PG_TYPE_NAME} -> SqlVarCharT
      #{const PG_TYPE_BPCHAR} -> SqlCharT
      #{const PG_TYPE_VARCHAR} -> SqlVarCharT
      #{const PG_TYPE_TEXT} -> SqlVarCharT
      #{const PG_TYPE_XML} -> SqlVarCharT
      #{const PG_TYPE_BYTEA} -> SqlVarBinaryT
      #{const PG_TYPE_INT2} -> SqlSmallIntT
      #{const PG_TYPE_OID} -> SqlIntegerT
      #{const PG_TYPE_XID} -> SqlIntegerT
      #{const PG_TYPE_INT4} -> SqlBigIntT
      #{const PG_TYPE_INT8} -> SqlBigIntT
      #{const PG_TYPE_NUMERIC} -> SqlNumericT
      #{const PG_TYPE_FLOAT4} -> SqlRealT
      #{const PG_TYPE_FLOAT8} -> SqlFloatT
      #{const PG_TYPE_DATE} -> SqlDateT
      #{const PG_TYPE_ABSTIME} -> SqlTimestampWithZoneT

      #{const PG_TYPE_DATETIME} -> SqlTimestampWithZoneT
      #{const PG_TYPE_TIMESTAMP_NO_TMZONE} -> SqlTimestampT
      #{const PG_TYPE_TIMESTAMP} -> SqlTimestampT

      #{const PG_TYPE_TIME} -> SqlTimeT
      #{const PG_TYPE_TIME_WITH_TMZONE} -> SqlTimeWithZoneT

      #{const PG_TYPE_TINTERVAL} -> SqlIntervalT SqlIntervalMonthT -- SqlIntervalMonthT chosen arbitrarily in these two. PG allows any parts
      #{const PG_TYPE_RELTIME}   -> SqlIntervalT SqlIntervalMonthT -- of an interval (microsecond to millennium) to be specified together.
      1186 -> SqlIntervalT SqlIntervalMonthT
      #{const PG_TYPE_BOOL} -> SqlBitT
      x -> SqlUnknownT (show x)
