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
    , withSelectedFields
    , withMaxResults
    , withResultOffset
    , withWeightIncluded

    -- * ApiDocument construction and configuration
    , mkApiDoc
    , withDescription
    , addDescription
    , changeDescription
    , withIndex
    , addToIndex
    , changeIndex
    , withDocWeight

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
    , withNoCaseSearch
    , withFuzzySearch
    , withinContext
    , withinContexts
    , withBoost
    , qContext

    -- ** pretty printing
    , printQuery

    -- * schema definition
    , mkSchema
    , withoutCxDefault
    , withCxWeight
    , withCxRegEx
    , withCxUpperCase
    , withCxLowerCase
    , withCxZeroFill
    , withCxText
    , withCxInt
    , withCxDate
    , withCxPosition
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
                                              RegEx, Score, URI, Weight)
import           Hunt.Common.DocDesc         (DocDesc (..))
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

withMaxResults :: Int -> Command -> Command
withMaxResults mx q@Search{}
    = q { icMaxSR    = mx }
withMaxResults mx q@Completion{}
    = q { icMaxCR    = mx }
withMaxResults _ q
    = q

-- | configure search command: set the starting offset of the result list
withResultOffset :: Int -> Command -> Command
withResultOffset off q@Search{}
    = q { icOffsetSR = off }
withResultOffset _ q
    = q

-- | configure search command: set the list of attributes of the document decription
-- to be included in the result list
--
-- example: @withSelectedFields ["title", "date"]@ restricts the documents
-- attributes to these to fields

withSelectedFields :: [Text] -> Command -> Command
withSelectedFields fs q@Search{}
    = q { icFields = Just fs }

withSelectedFields _ q
    = q

-- |  configure search command: include document weight in result list

withWeightIncluded :: Command -> Command
withWeightIncluded q@Search{}
    = q { icWeight = True }
withWeightIncluded q
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
      , adWght  = Nothing
      }

-- | add an index map containing the text parts to be indexed

withDescription :: Description -> ApiDocument -> ApiDocument
withDescription descr d
    = d { adDescr = descr }

addDescription :: Text -> Text -> ApiDocument -> ApiDocument
addDescription k v
    | T.null v  = changeDescription $ (DocDesc . SM.delete k   . unDesc)
    | otherwise = changeDescription $ (DocDesc . SM.insert k v . unDesc)

changeDescription :: (Description -> Description) -> ApiDocument -> ApiDocument
changeDescription f a = a { adDescr = f . adDescr $ a }

-- | add an index map containing the text parts to be indexed

withIndex :: IndexMap -> ApiDocument -> ApiDocument
withIndex im d
    = d { adIndex = im }

addToIndex :: Context -> Content -> ApiDocument -> ApiDocument
addToIndex cx ct d
    | T.null ct = d
    | otherwise = changeIndex (SM.insert cx ct) d

changeIndex :: (IndexMap -> IndexMap) -> ApiDocument -> ApiDocument
changeIndex f a = a { adIndex = f $ adIndex a }

-- | add a document weight

withDocWeight :: Float -> ApiDocument -> ApiDocument
withDocWeight w d
    | w == 1.0  = d
    | otherwise = d { adWght = Just w }

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

-- | and query
qAnd :: Query -> Query -> Query
qAnd = QBinary And

--  | multiple @and@ queries. The list must not be emtpy
qAnds :: [Query] -> Query
qAnds = foldr1 (QBinary And)

-- | or query
qOr :: Query -> Query -> Query
qOr = QBinary Or

--  | multiple @or@ queries. The list must not be emtpy
qOrs :: [Query] -> Query
qOrs = foldr1 (QBinary Or)

-- | and not query
qAndNot :: Query -> Query -> Query
qAndNot = QBinary AndNot

--  | multiple @and-not@ queries. The list must not be emtpy
qAndNots :: [Query] -> Query
qAndNots = foldr1 (QBinary AndNot)



-- ------------------------------------------------------------
-- configure simple search queries

-- | case insensitve search, only sensible for word and phrase queries

withNoCaseSearch :: Query -> Query
withNoCaseSearch (QWord   _ w) = QWord   QNoCase w
withNoCaseSearch (QPhrase _ w) = QPhrase QNoCase w
withNoCaseSearch q             = q

-- | fuzzy search, only sensible for word and phrase queries

withFuzzySearch :: Query -> Query
withFuzzySearch (QWord   _ w) = QWord   QFuzzy w
withFuzzySearch (QPhrase _ w) = QPhrase QFuzzy w
withFuzzySearch q             = q

-- | restrict search to list of contexts

withinContexts :: [Context] -> Query -> Query
withinContexts = QContext

-- | restrict search to a single context

withinContext :: Context -> Query -> Query
withinContext cx = withinContexts [cx]

-- | boost the search results by a factor

withBoost :: Weight -> Query -> Query
withBoost = QBoost

-- | Shortcut for case sensitive context search
qContext :: Context -> Text -> Query
qContext c w = QContext [c] $ QWord QCase w


-- ------------------------------------------------------------
-- context schema construction

-- | the default schema: context type is text, no normalizers,
-- weigth is 1.0, context is always searched by queries without context spec

mkSchema :: ContextSchema
mkSchema = def

-- | prevent searching in context, when not explicitly set in query

withoutCxDefault :: ContextSchema -> ContextSchema
withoutCxDefault sc
    = sc { cxDefault = False }

-- | set the regex for splitting a text into words

withCxWeight :: Weight -> ContextSchema -> ContextSchema
withCxWeight w sc
    = sc { cxWeight = w }

-- | set the regex for splitting a text into words

withCxRegEx :: RegEx -> ContextSchema -> ContextSchema
withCxRegEx re sc
    = sc { cxRegEx = Just re }

-- | add a text normalizer for transformation into uppercase

withCxUpperCase :: ContextSchema -> ContextSchema
withCxUpperCase sc
    = sc { cxNormalizer = cnUpperCase : cxNormalizer sc }

-- | add a text normalizer for transformation into lowercase

withCxLowerCase :: ContextSchema -> ContextSchema
withCxLowerCase sc
    = sc { cxNormalizer = cnLowerCase : cxNormalizer sc }

-- | add a text normalizer for transformation into lowercase

withCxZeroFill :: ContextSchema -> ContextSchema
withCxZeroFill sc
    = sc { cxNormalizer = cnZeroFill : cxNormalizer sc }

-- | set the type of a context to text

withCxText :: ContextSchema -> ContextSchema
withCxText sc
    = sc { cxType = ctText }

-- | set the type of a context to Int

withCxInt :: ContextSchema -> ContextSchema
withCxInt sc
    = sc { cxType = ctInt }

-- | set the type of a context to Date

withCxDate :: ContextSchema -> ContextSchema
withCxDate sc
    = sc { cxType = ctDate }

-- | set the type of a context to Int

withCxPosition :: ContextSchema -> ContextSchema
withCxPosition sc
    = sc { cxType = ctPosition }

-- ------------------------------------------------------------
