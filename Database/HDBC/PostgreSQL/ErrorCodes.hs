-- Generated from "/usr/include/postgresql/utils/errcodes.h" (PostgreSQL 8.3).
--
-- The following vim regexp substitutions map "errorcodes.h" to this file:
--
-- " remove all comments
-- :%s/\v\/\*\_.{-}\*\/
--
-- " remove empty lines
-- :%s/^\s*\n/
--
-- " #defines -> functions
-- :%s/\v#define ERRCODE_(\S*)\t*/\L\1 = /
--
-- " expand MAKE_SQLSTATE macro
-- :%s/\vMAKE_SQLSTATE\('(.)',\s*'(.)',\s*'(.)',\s*'(.)',\s*'(.)'\)/"\1\2\3\4\5"/
--
-- " aliases
-- :%s/\vERRCODE_(\S*)/\L\1/
--
-- " type signatures
-- :%s/\v^(\S*)/\r\1 :: String\r\1/
--
-- " to mixedCase
-- :%s/\v_(\l)/\u\1/g
--
-- " got ride of additional whitespace
-- :%s/\s+=\s+/ = /
--
-- " Documentation
-- :%s/\v(\a+) :: String\n\1 \= "(.*)"/-- |Is set to @\\"\2\\"@.\r\0
--
-- " Documentation for aliases
-- :%s/\v(\a+) :: String\n\1 \= (\a+)/-- |Same as '\2'.\r\0
--

module Database.HDBC.PostgreSQL.ErrorCodes where

-- |Is set to @\"00000\"@.
successfulCompletion :: String
successfulCompletion = "00000"

-- |Is set to @\"01000\"@.
warning :: String
warning = "01000"

-- |Is set to @\"0100C\"@.
warningDynamicResultSetsReturned :: String
warningDynamicResultSetsReturned = "0100C"

-- |Is set to @\"01008\"@.
warningImplicitZeroBitPadding :: String
warningImplicitZeroBitPadding = "01008"

-- |Is set to @\"01003\"@.
warningNullValueEliminatedInSetFunction :: String
warningNullValueEliminatedInSetFunction = "01003"

-- |Is set to @\"01007\"@.
warningPrivilegeNotGranted :: String
warningPrivilegeNotGranted = "01007"

-- |Is set to @\"01006\"@.
warningPrivilegeNotRevoked :: String
warningPrivilegeNotRevoked = "01006"

-- |Is set to @\"01004\"@.
warningStringDataRightTruncation :: String
warningStringDataRightTruncation = "01004"

-- |Is set to @\"01P01\"@.
warningDeprecatedFeature :: String
warningDeprecatedFeature = "01P01"

-- |Is set to @\"02000\"@.
noData :: String
noData = "02000"

-- |Is set to @\"02001\"@.
noAdditionalDynamicResultSetsReturned :: String
noAdditionalDynamicResultSetsReturned = "02001"

-- |Is set to @\"03000\"@.
sqlStatementNotYetComplete :: String
sqlStatementNotYetComplete = "03000"

-- |Is set to @\"08000\"@.
connectionException :: String
connectionException = "08000"

-- |Is set to @\"08003\"@.
connectionDoesNotExist :: String
connectionDoesNotExist = "08003"

-- |Is set to @\"08006\"@.
connectionFailure :: String
connectionFailure = "08006"

-- |Is set to @\"08001\"@.
sqlclientUnableToEstablishSqlconnection :: String
sqlclientUnableToEstablishSqlconnection = "08001"

-- |Is set to @\"08004\"@.
sqlserverRejectedEstablishmentOfSqlconnection :: String
sqlserverRejectedEstablishmentOfSqlconnection = "08004"

-- |Is set to @\"08007\"@.
transactionResolutionUnknown :: String
transactionResolutionUnknown = "08007"

-- |Is set to @\"08P01\"@.
protocolViolation :: String
protocolViolation = "08P01"

-- |Is set to @\"09000\"@.
triggeredActionException :: String
triggeredActionException = "09000"

-- |Is set to @\"0A000\"@.
featureNotSupported :: String
featureNotSupported = "0A000"

-- |Is set to @\"0B000\"@.
invalidTransactionInitiation :: String
invalidTransactionInitiation = "0B000"

-- |Is set to @\"0F000\"@.
locatorException :: String
locatorException = "0F000"

-- |Is set to @\"0F001\"@.
lEInvalidSpecification :: String
lEInvalidSpecification = "0F001"

-- |Is set to @\"0L000\"@.
invalidGrantor :: String
invalidGrantor = "0L000"

-- |Is set to @\"0LP01\"@.
invalidGrantOperation :: String
invalidGrantOperation = "0LP01"

-- |Is set to @\"0P000\"@.
invalidRoleSpecification :: String
invalidRoleSpecification = "0P000"

-- |Is set to @\"21000\"@.
cardinalityViolation :: String
cardinalityViolation = "21000"

-- |Is set to @\"22000\"@.
dataException :: String
dataException = "22000"

-- |Is set to @\"2202E\"@.
arrayElementError :: String
arrayElementError = "2202E"

-- |Same as 'arrayElementError'.
arraySubscriptError :: String
arraySubscriptError = arrayElementError

-- |Is set to @\"22021\"@.
characterNotInRepertoire :: String
characterNotInRepertoire = "22021"

-- |Is set to @\"22008\"@.
datetimeFieldOverflow :: String
datetimeFieldOverflow = "22008"

-- |Same as 'datetimeFieldOverflow'.
datetimeValueOutOfRange :: String
datetimeValueOutOfRange = datetimeFieldOverflow

-- |Is set to @\"22012\"@.
divisionByZero :: String
divisionByZero = "22012"

-- |Is set to @\"22005\"@.
errorInAssignment :: String
errorInAssignment = "22005"

-- |Is set to @\"2200B\"@.
escapeCharacterConflict :: String
escapeCharacterConflict = "2200B"

-- |Is set to @\"22022\"@.
indicatorOverflow :: String
indicatorOverflow = "22022"

-- |Is set to @\"22015\"@.
intervalFieldOverflow :: String
intervalFieldOverflow = "22015"

-- |Is set to @\"2201E\"@.
invalidArgumentForLog :: String
invalidArgumentForLog = "2201E"

-- |Is set to @\"2201F\"@.
invalidArgumentForPowerFunction :: String
invalidArgumentForPowerFunction = "2201F"

-- |Is set to @\"2201G\"@.
invalidArgumentForWidthBucketFunction :: String
invalidArgumentForWidthBucketFunction = "2201G"

-- |Is set to @\"22018\"@.
invalidCharacterValueForCast :: String
invalidCharacterValueForCast = "22018"

-- |Is set to @\"22007\"@.
invalidDatetimeFormat :: String
invalidDatetimeFormat = "22007"

-- |Is set to @\"22019\"@.
invalidEscapeCharacter :: String
invalidEscapeCharacter = "22019"

-- |Is set to @\"2200D\"@.
invalidEscapeOctet :: String
invalidEscapeOctet = "2200D"

-- |Is set to @\"22025\"@.
invalidEscapeSequence :: String
invalidEscapeSequence = "22025"

-- |Is set to @\"22P06\"@.
nonstandardUseOfEscapeCharacter :: String
nonstandardUseOfEscapeCharacter = "22P06"

-- |Is set to @\"22010\"@.
invalidIndicatorParameterValue :: String
invalidIndicatorParameterValue = "22010"

-- |Is set to @\"22020\"@.
invalidLimitValue :: String
invalidLimitValue = "22020"

-- |Is set to @\"22023\"@.
invalidParameterValue :: String
invalidParameterValue = "22023"

-- |Is set to @\"2201B\"@.
invalidRegularExpression :: String
invalidRegularExpression = "2201B"

-- |Is set to @\"22009\"@.
invalidTimeZoneDisplacementValue :: String
invalidTimeZoneDisplacementValue = "22009"

-- |Is set to @\"2200C\"@.
invalidUseOfEscapeCharacter :: String
invalidUseOfEscapeCharacter = "2200C"

-- |Is set to @\"2200G\"@.
mostSpecificTypeMismatch :: String
mostSpecificTypeMismatch = "2200G"

-- |Is set to @\"22004\"@.
nullValueNotAllowed :: String
nullValueNotAllowed = "22004"

-- |Is set to @\"22002\"@.
nullValueNoIndicatorParameter :: String
nullValueNoIndicatorParameter = "22002"

-- |Is set to @\"22003\"@.
numericValueOutOfRange :: String
numericValueOutOfRange = "22003"

-- |Is set to @\"22026\"@.
stringDataLengthMismatch :: String
stringDataLengthMismatch = "22026"

-- |Is set to @\"22001\"@.
stringDataRightTruncation :: String
stringDataRightTruncation = "22001"

-- |Is set to @\"22011\"@.
substringError :: String
substringError = "22011"

-- |Is set to @\"22027\"@.
trimError :: String
trimError = "22027"

-- |Is set to @\"22024\"@.
unterminatedCString :: String
unterminatedCString = "22024"

-- |Is set to @\"2200F\"@.
zeroLengthCharacterString :: String
zeroLengthCharacterString = "2200F"

-- |Is set to @\"22P01\"@.
floatingPointException :: String
floatingPointException = "22P01"

-- |Is set to @\"22P02\"@.
invalidTextRepresentation :: String
invalidTextRepresentation = "22P02"

-- |Is set to @\"22P03\"@.
invalidBinaryRepresentation :: String
invalidBinaryRepresentation = "22P03"

-- |Is set to @\"22P04\"@.
badCopyFileFormat :: String
badCopyFileFormat = "22P04"

-- |Is set to @\"22P05\"@.
untranslatableCharacter :: String
untranslatableCharacter = "22P05"

-- |Is set to @\"2200L\"@.
notAnXmlDocument :: String
notAnXmlDocument = "2200L"

-- |Is set to @\"2200M\"@.
invalidXmlDocument :: String
invalidXmlDocument = "2200M"

-- |Is set to @\"2200N\"@.
invalidXmlContent :: String
invalidXmlContent = "2200N"

-- |Is set to @\"2200S\"@.
invalidXmlComment :: String
invalidXmlComment = "2200S"

-- |Is set to @\"2200T\"@.
invalidXmlProcessingInstruction :: String
invalidXmlProcessingInstruction = "2200T"

-- |Is set to @\"23000\"@.
integrityConstraintViolation :: String
integrityConstraintViolation = "23000"

-- |Is set to @\"23001\"@.
restrictViolation :: String
restrictViolation = "23001"

-- |Is set to @\"23502\"@.
notNullViolation :: String
notNullViolation = "23502"

-- |Is set to @\"23503\"@.
foreignKeyViolation :: String
foreignKeyViolation = "23503"

-- |Is set to @\"23505\"@.
uniqueViolation :: String
uniqueViolation = "23505"

-- |Is set to @\"23514\"@.
checkViolation :: String
checkViolation = "23514"

-- |Is set to @\"24000\"@.
invalidCursorState :: String
invalidCursorState = "24000"

-- |Is set to @\"25000\"@.
invalidTransactionState :: String
invalidTransactionState = "25000"

-- |Is set to @\"25001\"@.
activeSqlTransaction :: String
activeSqlTransaction = "25001"

-- |Is set to @\"25002\"@.
branchTransactionAlreadyActive :: String
branchTransactionAlreadyActive = "25002"

-- |Is set to @\"25008\"@.
heldCursorRequiresSameIsolationLevel :: String
heldCursorRequiresSameIsolationLevel = "25008"

-- |Is set to @\"25003\"@.
inappropriateAccessModeForBranchTransaction :: String
inappropriateAccessModeForBranchTransaction = "25003"

-- |Is set to @\"25004\"@.
inappropriateIsolationLevelForBranchTransaction :: String
inappropriateIsolationLevelForBranchTransaction = "25004"

-- |Is set to @\"25005\"@.
noActiveSqlTransactionForBranchTransaction :: String
noActiveSqlTransactionForBranchTransaction = "25005"

-- |Is set to @\"25006\"@.
readOnlySqlTransaction :: String
readOnlySqlTransaction = "25006"

-- |Is set to @\"25007\"@.
schemaAndDataStatementMixingNotSupported :: String
schemaAndDataStatementMixingNotSupported = "25007"

-- |Is set to @\"25P01\"@.
noActiveSqlTransaction :: String
noActiveSqlTransaction = "25P01"

-- |Is set to @\"25P02\"@.
inFailedSqlTransaction :: String
inFailedSqlTransaction = "25P02"

-- |Is set to @\"26000\"@.
invalidSqlStatementName :: String
invalidSqlStatementName = "26000"

-- |Is set to @\"27000\"@.
triggeredDataChangeViolation :: String
triggeredDataChangeViolation = "27000"

-- |Is set to @\"28000\"@.
invalidAuthorizationSpecification :: String
invalidAuthorizationSpecification = "28000"

-- |Is set to @\"2B000\"@.
dependentPrivilegeDescriptorsStillExist :: String
dependentPrivilegeDescriptorsStillExist = "2B000"

-- |Is set to @\"2BP01\"@.
dependentObjectsStillExist :: String
dependentObjectsStillExist = "2BP01"

-- |Is set to @\"2D000\"@.
invalidTransactionTermination :: String
invalidTransactionTermination = "2D000"

-- |Is set to @\"2F000\"@.
sqlRoutineException :: String
sqlRoutineException = "2F000"

-- |Is set to @\"2F005\"@.
sREFunctionExecutedNoReturnStatement :: String
sREFunctionExecutedNoReturnStatement = "2F005"

-- |Is set to @\"2F002\"@.
sREModifyingSqlDataNotPermitted :: String
sREModifyingSqlDataNotPermitted = "2F002"

-- |Is set to @\"2F003\"@.
sREProhibitedSqlStatementAttempted :: String
sREProhibitedSqlStatementAttempted = "2F003"

-- |Is set to @\"2F004\"@.
sREReadingSqlDataNotPermitted :: String
sREReadingSqlDataNotPermitted = "2F004"

-- |Is set to @\"34000\"@.
invalidCursorName :: String
invalidCursorName = "34000"

-- |Is set to @\"38000\"@.
externalRoutineException :: String
externalRoutineException = "38000"

-- |Is set to @\"38001\"@.
eREContainingSqlNotPermitted :: String
eREContainingSqlNotPermitted = "38001"

-- |Is set to @\"38002\"@.
eREModifyingSqlDataNotPermitted :: String
eREModifyingSqlDataNotPermitted = "38002"

-- |Is set to @\"38003\"@.
eREProhibitedSqlStatementAttempted :: String
eREProhibitedSqlStatementAttempted = "38003"

-- |Is set to @\"38004\"@.
eREReadingSqlDataNotPermitted :: String
eREReadingSqlDataNotPermitted = "38004"

-- |Is set to @\"39000\"@.
externalRoutineInvocationException :: String
externalRoutineInvocationException = "39000"

-- |Is set to @\"39001\"@.
eRIEInvalidSqlstateReturned :: String
eRIEInvalidSqlstateReturned = "39001"

-- |Is set to @\"39004\"@.
eRIENullValueNotAllowed :: String
eRIENullValueNotAllowed = "39004"

-- |Is set to @\"39P01\"@.
eRIETriggerProtocolViolated :: String
eRIETriggerProtocolViolated = "39P01"

-- |Is set to @\"39P02\"@.
eRIESrfProtocolViolated :: String
eRIESrfProtocolViolated = "39P02"

-- |Is set to @\"3B000\"@.
savepointException :: String
savepointException = "3B000"

-- |Is set to @\"3B001\"@.
sEInvalidSpecification :: String
sEInvalidSpecification = "3B001"

-- |Is set to @\"3D000\"@.
invalidCatalogName :: String
invalidCatalogName = "3D000"

-- |Is set to @\"3F000\"@.
invalidSchemaName :: String
invalidSchemaName = "3F000"

-- |Is set to @\"40000\"@.
transactionRollback :: String
transactionRollback = "40000"

-- |Is set to @\"40002\"@.
tRIntegrityConstraintViolation :: String
tRIntegrityConstraintViolation = "40002"

-- |Is set to @\"40001\"@.
tRSerializationFailure :: String
tRSerializationFailure = "40001"

-- |Is set to @\"40003\"@.
tRStatementCompletionUnknown :: String
tRStatementCompletionUnknown = "40003"

-- |Is set to @\"40P01\"@.
tRDeadlockDetected :: String
tRDeadlockDetected = "40P01"

-- |Is set to @\"42000\"@.
syntaxErrorOrAccessRuleViolation :: String
syntaxErrorOrAccessRuleViolation = "42000"

-- |Is set to @\"42601\"@.
syntaxError :: String
syntaxError = "42601"

-- |Is set to @\"42501\"@.
insufficientPrivilege :: String
insufficientPrivilege = "42501"

-- |Is set to @\"42846\"@.
cannotCoerce :: String
cannotCoerce = "42846"

-- |Is set to @\"42803\"@.
groupingError :: String
groupingError = "42803"

-- |Is set to @\"42830\"@.
invalidForeignKey :: String
invalidForeignKey = "42830"

-- |Is set to @\"42602\"@.
invalidName :: String
invalidName = "42602"

-- |Is set to @\"42622\"@.
nameTooLong :: String
nameTooLong = "42622"

-- |Is set to @\"42939\"@.
reservedName :: String
reservedName = "42939"

-- |Is set to @\"42804\"@.
datatypeMismatch :: String
datatypeMismatch = "42804"

-- |Is set to @\"42P18\"@.
indeterminateDatatype :: String
indeterminateDatatype = "42P18"

-- |Is set to @\"42809\"@.
wrongObjectType :: String
wrongObjectType = "42809"

-- |Is set to @\"42703\"@.
undefinedColumn :: String
undefinedColumn = "42703"

-- |Same as 'invalidCursorName'.
undefinedCursor :: String
undefinedCursor = invalidCursorName

-- |Same as 'invalidCatalogName'.
undefinedDatabase :: String
undefinedDatabase = invalidCatalogName

-- |Is set to @\"42883\"@.
undefinedFunction :: String
undefinedFunction = "42883"

-- |Same as 'invalidSqlStatementName'.
undefinedPstatement :: String
undefinedPstatement = invalidSqlStatementName

-- |Same as 'invalidSchemaName'.
undefinedSchema :: String
undefinedSchema = invalidSchemaName

-- |Is set to @\"42P01\"@.
undefinedTable :: String
undefinedTable = "42P01"

-- |Is set to @\"42P02\"@.
undefinedParameter :: String
undefinedParameter = "42P02"

-- |Is set to @\"42704\"@.
undefinedObject :: String
undefinedObject = "42704"

-- |Is set to @\"42701\"@.
duplicateColumn :: String
duplicateColumn = "42701"

-- |Is set to @\"42P03\"@.
duplicateCursor :: String
duplicateCursor = "42P03"

-- |Is set to @\"42P04\"@.
duplicateDatabase :: String
duplicateDatabase = "42P04"

-- |Is set to @\"42723\"@.
duplicateFunction :: String
duplicateFunction = "42723"

-- |Is set to @\"42P05\"@.
duplicatePstatement :: String
duplicatePstatement = "42P05"

-- |Is set to @\"42P06\"@.
duplicateSchema :: String
duplicateSchema = "42P06"

-- |Is set to @\"42P07\"@.
duplicateTable :: String
duplicateTable = "42P07"

-- |Is set to @\"42712\"@.
duplicateAlias :: String
duplicateAlias = "42712"

-- |Is set to @\"42710\"@.
duplicateObject :: String
duplicateObject = "42710"

-- |Is set to @\"42702\"@.
ambiguousColumn :: String
ambiguousColumn = "42702"

-- |Is set to @\"42725\"@.
ambiguousFunction :: String
ambiguousFunction = "42725"

-- |Is set to @\"42P08\"@.
ambiguousParameter :: String
ambiguousParameter = "42P08"

-- |Is set to @\"42P09\"@.
ambiguousAlias :: String
ambiguousAlias = "42P09"

-- |Is set to @\"42P10\"@.
invalidColumnReference :: String
invalidColumnReference = "42P10"

-- |Is set to @\"42611\"@.
invalidColumnDefinition :: String
invalidColumnDefinition = "42611"

-- |Is set to @\"42P11\"@.
invalidCursorDefinition :: String
invalidCursorDefinition = "42P11"

-- |Is set to @\"42P12\"@.
invalidDatabaseDefinition :: String
invalidDatabaseDefinition = "42P12"

-- |Is set to @\"42P13\"@.
invalidFunctionDefinition :: String
invalidFunctionDefinition = "42P13"

-- |Is set to @\"42P14\"@.
invalidPstatementDefinition :: String
invalidPstatementDefinition = "42P14"

-- |Is set to @\"42P15\"@.
invalidSchemaDefinition :: String
invalidSchemaDefinition = "42P15"

-- |Is set to @\"42P16\"@.
invalidTableDefinition :: String
invalidTableDefinition = "42P16"

-- |Is set to @\"42P17\"@.
invalidObjectDefinition :: String
invalidObjectDefinition = "42P17"

-- |Is set to @\"44000\"@.
withCheckOptionViolation :: String
withCheckOptionViolation = "44000"

-- |Is set to @\"53000\"@.
insufficientResources :: String
insufficientResources = "53000"

-- |Is set to @\"53100\"@.
diskFull :: String
diskFull = "53100"

-- |Is set to @\"53200\"@.
outOfMemory :: String
outOfMemory = "53200"

-- |Is set to @\"53300\"@.
tooManyConnections :: String
tooManyConnections = "53300"

-- |Is set to @\"54000\"@.
programLimitExceeded :: String
programLimitExceeded = "54000"

-- |Is set to @\"54001\"@.
statementTooComplex :: String
statementTooComplex = "54001"

-- |Is set to @\"54011\"@.
tooManyColumns :: String
tooManyColumns = "54011"

-- |Is set to @\"54023\"@.
tooManyArguments :: String
tooManyArguments = "54023"

-- |Is set to @\"55000\"@.
objectNotInPrerequisiteState :: String
objectNotInPrerequisiteState = "55000"

-- |Is set to @\"55006\"@.
objectInUse :: String
objectInUse = "55006"

-- |Is set to @\"55P02\"@.
cantChangeRuntimeParam :: String
cantChangeRuntimeParam = "55P02"

-- |Is set to @\"55P03\"@.
lockNotAvailable :: String
lockNotAvailable = "55P03"

-- |Is set to @\"57000\"@.
operatorIntervention :: String
operatorIntervention = "57000"

-- |Is set to @\"57014\"@.
queryCanceled :: String
queryCanceled = "57014"

-- |Is set to @\"57P01\"@.
adminShutdown :: String
adminShutdown = "57P01"

-- |Is set to @\"57P02\"@.
crashShutdown :: String
crashShutdown = "57P02"

-- |Is set to @\"57P03\"@.
cannotConnectNow :: String
cannotConnectNow = "57P03"

-- |Is set to @\"58030\"@.
ioError :: String
ioError = "58030"

-- |Is set to @\"58P01\"@.
undefinedFile :: String
undefinedFile = "58P01"

-- |Is set to @\"58P02\"@.
duplicateFile :: String
duplicateFile = "58P02"

-- |Is set to @\"F0000\"@.
configFileError :: String
configFileError = "F0000"

-- |Is set to @\"F0001\"@.
lockFileExists :: String
lockFileExists = "F0001"

-- |Is set to @\"P0000\"@.
plpgsqlError :: String
plpgsqlError = "P0000"

-- |Is set to @\"P0001\"@.
raiseException :: String
raiseException = "P0001"

-- |Is set to @\"P0002\"@.
noDataFound :: String
noDataFound = "P0002"

-- |Is set to @\"P0003\"@.
tooManyRows :: String
tooManyRows = "P0003"

-- |Is set to @\"XX000\"@.
internalError :: String
internalError = "XX000"

-- |Is set to @\"XX001\"@.
dataCorrupted :: String
dataCorrupted = "XX001"

-- |Is set to @\"XX002\"@.
indexCorrupted :: String
indexCorrupted = "XX002"
