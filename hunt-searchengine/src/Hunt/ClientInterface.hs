{- |
  Module     : Hunt.ClientInterface
  License    : MIT

  Maintainer : Uwe Schmidt
  Stability  : experimental
  Portability: none portable

  Common data types and and smart constructors
  for calling a hunt server from a client.

  Values of the Command datatype and its component types, e.g
  Query, ApiDocument, and others
  can be constructed with the "smart" construtors
  defined in this module

  The module is intended to be imported qualified,
  eg like @import qualified Hunt.ClientInterface as HI@.

-}

-- ----------------------------------------------------------------------------

module Hunt.ClientInterface
    (
    -- * types used in commands
      Command
    , ApiDocument(..)   -- also used in results
    , Content
    , Context
    , ContextSchema
    , Description
    , IndexMap
    , Query
    , RegEx
    , StatusCmd
    , URI
    , Weight

    -- * types used in results
    , CmdError(..)
    , CmdRes(..)
    , CmdResult(..)
    , LimitedResult(..)
    , Score

    -- * command construction
    , cmdSearch
    , cmdCompletion
    , cmdSelect
    , cmdInsertDoc
    , cmdUpdateDoc
    , cmdDeleteDoc
    , cmdDeleteDocsByQuery
    , cmdLoadIndex
    , cmdStoreIndex
    , cmdInsertContext
    , cmdDeleteContext
    , cmdStatus
    , cmdSequence
    , cmdNOOP

    -- * configuration options for search and completion
    , setSelectedFields
    , setMaxResults
    , setResultOffset
    , setWeightIncluded


    -- * ApiDocument construction, configuration and access
    , mkApiDoc
    , setDescription
    , getDescription
    , addDescription
    , changeDescription
    , setIndex
    , addToIndex
    , getFromIndex
    , changeIndex
    , setDocWeight

    -- * description construction
    , mkDescription
    , emptyDescription
    , fromDescription

    -- * query construction
    , qWord
    , qPhrase
    , qRange
    , qAnd
    , qAnds
    , qOr
    , qOrs
    , qAndNot
    , qAndNots
    , setNoCaseSearch
    , setFuzzySearch
    , setContext
    , setContexts
    , setBoost
    , withinContexts    -- deprecated
    , withinContext     -- deprecated
    , withBoost         -- deprecated
    , qContext

    -- ** pretty printing
    , printQuery

    -- * schema definition
    , mkSchema
    , setCxNoDefault
    , setCxWeight
    , setCxRegEx
    , setCxUpperCase
    , setCxLowerCase
    , setCxZeroFill
    , setCxText
    , setCxInt
    , setCxDate
    , setCxPosition
    )
where

import           Data.Default
import qualified Data.Map.Strict             as SM
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Hunt.Common.ApiDocument     (ApiDocument (..), IndexMap,
                                              LimitedResult (..),
                                              emptyApiDocDescr,
                                              emptyApiDocIndexMap)
import           Hunt.Common.BasicTypes      (Content, Context, Description,
                                              RegEx, Score, URI, Weight,
                                              mkWeight, noWeight)
import           Hunt.Common.DocDesc         (DocDesc (..))
import qualified Hunt.Common.DocDesc         as DD
import           Hunt.Index.Schema
import           Hunt.Interpreter.Command
import           Hunt.Query.Language.Grammar

-- ------------------------------------------------------------
-- lookup commands


-- | create simple search command

cmdSearch :: Query -> Command
cmdSearch q
    = Search { icQuery    = q
             , icOffsetSR = 0
             , icMaxSR    = (-1)        -- unlimited
             , icWeight   = False
             , icFields   = Nothing
             }

-- | Create simple completion command

cmdCompletion :: Query -> Command
cmdCompletion q
    = Completion { icPrefixCR = q
                 , icMaxCR    = (-1)    -- unlimited
                 }

cmdSelect :: Query -> Command
cmdSelect = Select

-- ------------------------------------------------------------
-- modifying commands

-- | insert document

cmdInsertDoc :: ApiDocument -> Command
cmdInsertDoc = Insert

-- | update document

cmdUpdateDoc :: ApiDocument -> Command
cmdUpdateDoc = Update

-- | delete document identified by an URI

cmdDeleteDoc :: URI -> Command
cmdDeleteDoc = Delete

-- | delete all documents idenitfied by a query

cmdDeleteDocsByQuery :: Query -> Command
cmdDeleteDocsByQuery = DeleteByQuery

-- ------------------------------------------------------------
-- index schema manipulation

cmdInsertContext :: Context -> ContextSchema -> Command
cmdInsertContext cx sc
    = InsertContext { icIContext = cx
                    , icSchema   = sc
                    }

cmdDeleteContext :: Context -> Command
cmdDeleteContext cx
    = DeleteContext { icDContext = cx }

-- ------------------------------------------------------------
-- index persistance

cmdLoadIndex :: FilePath -> Command
cmdLoadIndex = LoadIx

cmdStoreIndex :: FilePath -> Command
cmdStoreIndex = StoreIx

-- ------------------------------------------------------------
-- status and control commands

cmdStatus :: StatusCmd -> Command
cmdStatus = Status

cmdSequence :: [Command] -> Command
cmdSequence = Sequence

cmdNOOP :: Command
cmdNOOP = NOOP

-- ------------------------------------------------------------

-- | configure search and completion command: set the max # of results

setMaxResults :: Int -> Command -> Command
setMaxResults mx q@Search{}
    = q { icMaxSR    = mx }
setMaxResults mx q@Completion{}
    = q { icMaxCR    = mx }
setMaxResults _ q
    = q

-- | configure search command: set the starting offset of the result list
setResultOffset :: Int -> Command -> Command
setResultOffset off q@Search{}
    = q { icOffsetSR = off }
setResultOffset _ q
    = q

-- | configure search command: set the list of attributes of the document decription
-- to be included in the result list
--
-- example: @setSelectedFields ["title", "date"]@ restricts the documents
-- attributes to these to fields

setSelectedFields :: [Text] -> Command -> Command
setSelectedFields fs q@Search{}
    = q { icFields = Just fs }

setSelectedFields _ q
    = q

-- |  configure search command: include document weight in result list

setWeightIncluded :: Command -> Command
setWeightIncluded q@Search{}
    = q { icWeight = True }
setWeightIncluded q
    = q

-- ------------------------------------------------------------

-- | build an api document with an uri as key and a description
-- map as contents

mkApiDoc :: URI -> ApiDocument
mkApiDoc u
    = ApiDocument
      { adUri   = u
      , adIndex = emptyApiDocIndexMap
      , adDescr = emptyApiDocDescr
      , adWght  = noWeight
      , adScore = 1.0
      }

-- | add an index map containing the text parts to be indexed

setDescription :: Description -> ApiDocument -> ApiDocument
setDescription descr d
    = d { adDescr = descr }

getDescription :: ApiDocument -> Description
getDescription = adDescr

addDescription :: Text -> Text -> ApiDocument -> ApiDocument
addDescription k v
    | T.null v  = changeDescription $ (DocDesc . SM.delete k   . unDesc)
    | otherwise = changeDescription $ (DocDesc . SM.insert k v . unDesc)

changeDescription :: (Description -> Description) -> ApiDocument -> ApiDocument
changeDescription f a = a { adDescr = f . adDescr $ a }

-- | add an index map containing the text parts to be indexed

setIndex :: IndexMap -> ApiDocument -> ApiDocument
setIndex im d
    = d { adIndex = im }

addToIndex :: Context -> Content -> ApiDocument -> ApiDocument
addToIndex cx ct d
    | T.null ct = d
    | otherwise = changeIndex (SM.insert cx ct) d

getFromIndex :: Context -> ApiDocument -> Text
getFromIndex cx d
    = maybe "" id . SM.lookup cx . adIndex $ d

changeIndex :: (IndexMap -> IndexMap) -> ApiDocument -> ApiDocument
changeIndex f a = a { adIndex = f $ adIndex a }

-- | add a document weight

setDocWeight :: Weight -> ApiDocument -> ApiDocument
setDocWeight w d
    = d { adWght = mkWeight w }

-- ------------------------------------------------------------
-- document description

mkDescription :: [(Text, Text)] -> Description
mkDescription = DD.fromList . filter (not . T.null . snd)

emptyDescription :: Description
emptyDescription = DD.empty

fromDescription :: Description ->  [(Text, Text)]
fromDescription = DD.toList

-- ------------------------------------------------------------
-- query construction

-- | prefix search of a single word

qWord :: Text -> Query
qWord = QWord QCase

-- | exact search of a word
qPhrase :: Text -> Query
qPhrase = QPhrase QCase

-- | search a range of words or an intervall for numeric contexts
qRange :: Text -> Text -> Query
qRange = QRange

-- | shortcut for case sensitive context search
qContext :: Context -> Text -> Query
qContext c w = QContext [c] $ QWord QCase w

-- | and query
qAnd :: Query -> Query -> Query
qAnd = QBinary And

--  | multiple @and@ queries. The list must not be emtpy
qAnds :: [Query] -> Query
qAnds = foldl1 qAnd     -- foldl more efficient (?) than foldr

-- | or query
qOr :: Query -> Query -> Query
qOr = QBinary Or

--  | multiple @or@ queries. The list must not be emtpy
qOrs :: [Query] -> Query
qOrs = foldl1 qOr       -- foldl more efficient (?) than foldr

-- | and not query
qAndNot :: Query -> Query -> Query
qAndNot = QBinary AndNot

--  | multiple @and-not@ queries. The list must not be emtpy
qAndNots :: [Query] -> Query
qAndNots = foldl1 qAndNot       -- foldl due to left associativity of AndNot

-- ------------------------------------------------------------
-- configure simple search queries

-- | case insensitve search, only sensible for word and phrase queries

setNoCaseSearch :: Query -> Query
setNoCaseSearch (QWord   _ w) = QWord   QNoCase w
setNoCaseSearch (QPhrase _ w) = QPhrase QNoCase w
setNoCaseSearch q             = q

-- | fuzzy search, only sensible for word and phrase queries

setFuzzySearch :: Query -> Query
setFuzzySearch (QWord   _ w) = QWord   QFuzzy w
setFuzzySearch (QPhrase _ w) = QPhrase QFuzzy w
setFuzzySearch q             = q

-- | restrict search to list of contexts

setContexts :: [Context] -> Query -> Query
setContexts = QContext

withinContexts :: [Context] -> Query -> Query
withinContexts = QContext
{-# DEPRECATED withinContexts "Don't use this, use setContexts" #-}

-- | restrict search to a single context

setContext :: Context -> Query -> Query
setContext cx = withinContexts [cx]

withinContext :: Context -> Query -> Query
withinContext cx = setContexts [cx]
{-# DEPRECATED withinContext "Don't use this, use setContext" #-}


-- | boost the search results by a factor

setBoost :: Weight -> Query -> Query
setBoost = QBoost

withBoost :: Weight -> Query -> Query
withBoost = QBoost
{-# DEPRECATED withBoost "Don't use this, use setBoost" #-}

-- ------------------------------------------------------------
-- context schema construction

-- | the default schema: context type is text, no normalizers,
-- weigth is 1.0, context is always searched by queries without context spec

mkSchema :: ContextSchema
mkSchema = def

-- | prevent searching in context, when not explicitly set in query

setCxNoDefault :: ContextSchema -> ContextSchema
setCxNoDefault sc
    = sc { cxDefault = False }

-- | set the regex for splitting a text into words

setCxWeight :: Float -> ContextSchema -> ContextSchema
setCxWeight w sc
    = sc { cxWeight = w }

-- | set the regex for splitting a text into words

setCxRegEx :: RegEx -> ContextSchema -> ContextSchema
setCxRegEx re sc
    = sc { cxRegEx = Just re }

-- | add a text normalizer for transformation into uppercase

setCxUpperCase :: ContextSchema -> ContextSchema
setCxUpperCase sc
    = sc { cxNormalizer = cnUpperCase : cxNormalizer sc }

-- | add a text normalizer for transformation into lowercase

setCxLowerCase :: ContextSchema -> ContextSchema
setCxLowerCase sc
    = sc { cxNormalizer = cnLowerCase : cxNormalizer sc }

-- | add a text normalizer for transformation into lowercase

setCxZeroFill :: ContextSchema -> ContextSchema
setCxZeroFill sc
    = sc { cxNormalizer = cnZeroFill : cxNormalizer sc }

-- | set the type of a context to text

setCxText :: ContextSchema -> ContextSchema
setCxText sc
    = sc { cxType = ctText }

-- | set the type of a context to Int

setCxInt :: ContextSchema -> ContextSchema
setCxInt sc
    = sc { cxType = ctInt }

-- | set the type of a context to Date

setCxDate :: ContextSchema -> ContextSchema
setCxDate sc
    = sc { cxType = ctDate }

-- | set the type of a context to Int

setCxPosition :: ContextSchema -> ContextSchema
setCxPosition sc
    = sc { cxType = ctPosition }

-- ------------------------------------------------------------
