{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS
   -fno-warn-orphans
   -fno-warn-missing-signatures
   -fno-warn-missing-methods
   -fno-warn-unused-matches
   -fno-warn-type-defaults
#-}
module Main where

import           Control.Applicative                  ((<$>))

import           Test.Framework                       hiding (Test)
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

import           Control.Monad
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Holumbus.Query.Language.Parser       as P
import           Holumbus.Query.Language.Grammar


-- ----------------------------------------------------------------------------
-- helper
--

a :: Query -> Query -> Query
a = QBinary And

o :: Query -> Query -> Query
o = QBinary Or

an :: Query -> Query -> Query
an = QBinary AndNot

w :: Text -> Query
w = QWord QNoCase

p :: Text -> Query
p = QPhrase QNoCase

s :: [Text] -> Query -> Query
s = QContext

cw :: Text -> Query
cw = QWord QCase

cp :: Text -> Query
cp = QPhrase QCase

fw :: Text -> Query
fw = QWord QFuzzy

rg :: Text -> Text -> Query
rg = QRange

bst :: Float -> Query -> Query
bst = QBoost

andTests :: Test
andTests = TestList
  [ TestCase (assertEqual "Simple two term 'and' query"
  (Right (a (w "abc") (w "def")))
  (P.parseQuery "abc def"))

  , TestCase (assertEqual "Concatenating 'and' terms"
  (Right (a (w "abc") (a (w "def") (w "ghi"))))
  (P.parseQuery "abc def ghi"))

  , TestCase (assertEqual "Ignoring whitespace"
  (Right (a (w "abc") (a (w "def") (a (w "ghi") (w "jkl")))))
  (P.parseQuery " \rabc \r  def  \tghi \njkl \r\n "))

  , TestCase (assertEqual "Priorities"
  (Right (a (s ["wurst"] (w "abc")) (a (w "def") (a (w "ghi") (s ["wurst"] (w "jkl"))))))
  (P.parseQuery "wurst:abc def ghi wurst:jkl"))

  , TestCase (assertEqual "Confusing operator"
  (Right (a (w "Apple") (a (w "Anna") (w "ANDroid"))))
  (P.parseQuery "Apple Anna ANDroid"))

  , TestCase (assertEqual "Explicit operator"
  (Right (a (w "abc") (w "def")))
  (P.parseQuery "abc AND def"))
  ]

orTests :: Test
orTests = TestList
  [ TestCase (assertEqual "Simple two term 'or' query"
  (Right (o (w "abc") (w "def")))
  (P.parseQuery "abc OR def"))

  , TestCase (assertEqual "Concatenating 'or' terms"
  (Right (o (w "abc") (o (w "def") (w "ghi"))))
  (P.parseQuery "abc OR def OR ghi"))

  , TestCase (assertEqual "Ignoring whitespace"
  (Right (o (w "abc") (o (w "def") (o (w "ghi") (w "jkl")))))
  (P.parseQuery " \rabc \rOR  def OR \tghi OR\njkl \r\n "))

  , TestCase (assertEqual "Priorities"
  (Right (o (s ["wurst"] (w "abc")) (o (w "def") (o (w "ghi") (s ["wurst"] (w "jkl"))))))
  (P.parseQuery "wurst:abc OR def OR ghi OR wurst:jkl"))

  , TestCase (assertEqual "Operator precedence"
  (P.parseQuery "wurst:abc (def OR ghi) wurst:jkl")
  (P.parseQuery "wurst:abc def OR ghi wurst:jkl"))

  , TestCase (assertEqual "Confusing operator"
  (Right (a (w "Operation") (w "ORganism")))
  (P.parseQuery "Operation ORganism"))
  ]

specifierTests :: Test
specifierTests = TestList
  [ TestCase (assertEqual "Specifier with whitespace"
  (Right (a (s ["wurst"] (w "abc")) (s ["batzen"] (w "def"))))
  (P.parseQuery " wurst:\t abc \nbatzen : \r def "))

  , TestCase (assertEqual "Specifier priority"
  (Right (a (w "abc") (a (s ["wurst"] (w "def")) (o (s ["wurst"] (w "ghi")) (s ["wurst"] (w "jkl"))))))
  (P.parseQuery "abc wurst: def wurst: ghi OR wurst: jkl"))

  ,TestCase (assertEqual "Specifier and brackets"
  (Right (a (s ["wurst"] (a (w "abc") (a (w "def") (w "ghi")))) (s ["batzen"] (o (w "abc") (w "def")))))
  (P.parseQuery "wurst: (abc def ghi) batzen: (abc OR def)"))

  ,TestCase (assertEqual "Specifier and brackets"
  (Right (a (s ["wurst"] (a (w "abc") (a (w "def") (w "ghi")))) (s ["batzen"] (o (w "abc") (w "def")))))
  (P.parseQuery "wurst: (abc def ghi) batzen: (abc OR def)"))

  ,TestCase (assertEqual "Specifier and space"
  (Right (a (s ["wurst"] (a (w "abc") (a (w "def") (w "ghi")))) (s ["batzen"] (o (w "abc") (w "def")))))
  (P.parseQuery "wurst \t: (abc def ghi) batzen \n : (abc OR def)"))

  ,TestCase (assertEqual "Specifier lists"
  (Right (s ["wurst","batzen","schinken"] (a (w "abc") (a (w "def") (w "ghi")))))
  (P.parseQuery "wurst,batzen,schinken: (abc def ghi)"))

  ,TestCase (assertEqual "Specifier lists with space"
  (Right (s ["wurst","batzen","schinken"] (a (w "abc") (a (w "def") (w "ghi")))))
  (P.parseQuery "wurst , \n batzen \t, schinken: (abc def ghi)"))

  ,TestCase (assertEqual "Specifier lists with phrase"
  (Right (s ["wurst","batzen","schinken"] (p "this is A Test")))
  (P.parseQuery "wurst , \n batzen \t, schinken: \"this is A Test\""))
  ]

andNotTests :: Test
andNotTests = TestList
  [ TestCase (assertEqual "Simple two term 'and not' query"
  (Right (an (w "abc") (w "def")))
  (P.parseQuery "abc AND NOT def"))

  , TestCase (assertEqual "Concatenating 'and' terms"
  (Right (an (w "abc") (an (w "def") (w "ghi"))))
  (P.parseQuery "abc AND NOT def AND NOT ghi"))

  , TestCase (assertEqual "Ignoring whitespace"
  (Right (an (w "abc") (an (w "def") (an (w "ghi") (w "jkl")))))
  (P.parseQuery " \rabc AND NOT\r  def  \tAND NOT ghi AND NOT \njkl \r\n "))

  , TestCase (assertEqual "Priorities"
  (Right (an (s ["wurst"] (w "abc")) (an (w "def") (an (w "ghi") (s ["wurst"] (w "jkl"))))))
  (P.parseQuery "wurst:abc AND NOT def AND NOT ghi AND NOT wurst:jkl"))

  , TestCase (assertEqual "Confusing operator"
  (Right (an (w "Apple") (a (w "Anna") (w "ANDNOTtingham"))))
  (P.parseQuery "Apple AND NOT Anna ANDNOTtingham"))
  ]

{-
notTests :: Test
notTests = TestList
  [ TestCase (assertEqual "Simple not query"
  (Right (n (w "batzen")))
  (P.parseQuery "NOT batzen"))

  , TestCase (assertEqual "Operator precedence"
  (Right (a (n (w "batzen")) (w "wurst")))
  (P.parseQuery "NOT batzen wurst"))

  , TestCase (assertEqual "Operator precedence with and"
  (Right (a (w "test") (a (n (w "batzen")) (w "wurst"))))
  (P.parseQuery "test NOT batzen wurst"))

  , TestCase (assertEqual "Operator precedence with or"
  (Right (o (w "test") (o (n (w "batzen")) (w "wurst"))))
  (P.parseQuery "test OR NOT batzen OR wurst"))

  , TestCase (assertEqual "Confusing operator"
  (Right (a (w "Nail") (a (w "NOrthpole") (w "NOTtingham"))))
  (P.parseQuery "Nail NOrthpole NOTtingham"))
  ]
-}

caseTests :: Test
caseTests = TestList
  [ TestCase (assertEqual "Simple case-sensitive word"
  (Right (cw "batzen"))
  (P.parseQuery "!batzen"))

  ,TestCase (assertEqual "Simple case-sensitive phrase"
  (Right (cp "this is a test"))
  (P.parseQuery "!\"this is a test\""))

  ,TestCase (assertEqual "Case sensitive word with whitespace"
  (Right (cw "test"))
  (P.parseQuery " ! test"))
  ]

boostTests :: Test
boostTests = TestList
  [ TestCase $ assertEqual "Boosting a word"
    (Right (bst 9 $ w "word"))
    ( P.parseQuery "word^9")

  , TestCase $ assertEqual "Boosting and more"
    (Right (a (bst 2 $ w "foo") (w "bar")))
    ( P.parseQuery "foo^2 bar")

  , TestCase $ assertEqual "Boosting a word with a proper float"
    (Right (bst 9.5 $ w "word"))
    ( P.parseQuery "word^9.5")

  , TestCase $ assertEqual "Boosting a phrase"
    (Right (bst 9 $ p "word"))
    ( P.parseQuery "\"word\"^9")

  , TestCase $ assertEqual "Boosting a binary query with parantheses"
    (Right (bst 9 $ a (w "w") (o (w "k") (w "p"))))
    ( P.parseQuery "(w AND k OR p)^9")

  , TestCase $ assertEqual "Boosting a context"
    (Right (bst 9 $ s ["con"] (w "word")))
    ( P.parseQuery "(con:word)^9")

  ]

rangeTests :: Test
rangeTests = TestList
  [ TestCase $ assertEqual "Simple Range Query without meta"
    (Right (rg "30" "40"))
    ( P.parseQuery "[30 TO 40]")

  , TestCase $ assertEqual "Range with context"
    (Right (s ["con"] (rg "30" "40")))
    ( P.parseQuery "con:[30 TO 40]")

  , TestCase $ assertEqual "Range with contexts"
    (Right (s ["con1", "con2"] (rg "30" "40")))
    ( P.parseQuery "con1,con2:[30 TO 40]")

  , TestCase $ assertEqual "complex query with ranges"
    (Right (a (s ["con1"] (rg "30" "40")) (s ["con2"] (rg "59" "100"))))
    ( P.parseQuery "con1:[30 TO 40] AND con2:[59 TO 100]")

  ]


parentheseTests :: Test
parentheseTests = TestList
  [ TestCase (assertEqual "Parentheses without effect"
  (P.parseQuery "abc def OR ghi")
  (P.parseQuery "abc (def OR ghi)"))

  , TestCase (assertEqual "Parentheses changing priority of OR"
  (Right (a (o (w "abc") (w "def")) (w "ghi")))
  (P.parseQuery "(abc OR def) ghi"))

  , TestCase (assertEqual "Parentheses with whitespace and OR"
  (Right (o (w "abc") (w "def")))
  (P.parseQuery " ( abc OR def ) "))

  , TestCase (assertEqual "Parentheses with whitespace and AND"
  (Right (a (w "abc") (w "def")))
  (P.parseQuery " ( abc def ) "))
  ]

fuzzyTests :: Test
fuzzyTests = TestList
  [ TestCase (assertEqual "Simple fuzzy query"
  (Right (fw "test"))
  (P.parseQuery "~test"))

  , TestCase (assertEqual "Fuzzy query with whitespace"
  (Right (fw "test"))
  (P.parseQuery " ~ test"))
  ]

phraseTests :: Test
phraseTests = TestList
  [ TestCase (assertEqual "Ignoring whitespace without case operator"
  (Right (p "wurst schinken batzen"))
  (P.parseQuery "  \t \n \"wurst schinken batzen\" \t "))

  , TestCase (assertEqual "Ignoring whitespace with case operator"
  (Right (cp "wurst schinken batzen"))
  (P.parseQuery "  \t \n ! \"wurst schinken batzen\" \t "))
  ]
{--
instance Arbitrary Char where
  arbitrary     = oneof [choose ('\65', '\90') ,choose ('\97', '\122')]
  shrink c  = [ c' | c' <- ['a','b','c'], c' < c || not (isLower c) ]
--}
instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary
    shrink xs = T.pack <$> shrink (T.unpack xs)

instance Arbitrary Query where
  arbitrary = sized query

query :: Int -> Gen Query
query num | num == 0 = liftM (QWord QCase) word
          | num < 0 = query (abs num)
          | num > 0 = frequency [ (4, liftM (QWord QNoCase) word)
                                , (1, liftM (QWord QCase) word)
                                , (1, liftM (QWord QFuzzy) word)
                                , (2, liftM (QPhrase QNoCase) phrase)
                                , (1, liftM (QPhrase QCase) phrase)
                                , (1, liftM2 QContext specs subQuery)
                                , (4, liftM3 QBinary op subQuery subQuery)
                                ]
query _ = error "Error in query generator!"



op = frequency [(3, return (And)), (1, return (Or)), (1, return (AndNot))]
subQuery = sized (\num -> query (num `div` 2))

specs = sequence [ word | i <- [1..2] ]

word :: Gen Text
word = fmap T.pack . listOf1 . elements $ concat [['0'..'9'], ['A'..'Z'], ['a'..'z']]

phrase = do
         ws <- sequence [ word | i <- [1..3] ]
         return (T.intercalate " " ws)

showQuery :: (BinOp -> String) -> Query -> Text
showQuery _ (QWord QNoCase st)   = st
showQuery _ (QPhrase QNoCase st) = T.concat ["\"", st, "\""]
showQuery _ (QWord QCase st)     = T.concat ["!", st]
showQuery _ (QPhrase QCase st)   = T.concat ["!\"", st, "\""]
showQuery _ (QWord QFuzzy st)    = T.concat ["~", st]
showQuery _ (QPhrase QFuzzy st)  = T.concat ["~\"", st, "\""]
showQuery f (QContext c q)       = T.concat [(T.intercalate "," c), ":(", (showQuery f q), ")"]
showQuery f (QBinary opr q1 q2)  = T.concat ["(", (showQuery f q1)
                                            ," " , (T.pack $ f opr)
                                            ," " , (showQuery f q2) , ")"]
showQuery f (QRange l u)         = T.concat [ "[", l, " TO ", u, "]"]
showQuery f (QBoost factor q)    = T.concat [ showQuery f q, "^", (T.pack . show $ factor) ]

showOpAnd And    = "AND"
showOpAnd Or     = "OR"
showOpAnd AndNot = "AND NOT"

showOpSpace And    = " "
showOpSpace Or     = "OR"
showOpSpace AndNot = "AND NOT"


prop_ParseAnd q = P.parseQuery (T.unpack $ showQuery showOpAnd q) == Right q
prop_ParseSpace q = P.parseQuery (T.unpack $ showQuery showOpSpace q) == Right q

allProperties = testGroup "Query Parser Properties"
                [ testProperty "prop_ParseAnd" prop_ParseAnd
                , testProperty "prop_ParseSpace" prop_ParseSpace
                ]

allUnitTests = testGroup "Query Parser Hunit tests" $ hUnitTestToTests $ TestList
  [ TestLabel "And tests"         andTests
  , TestLabel "Or tests"          orTests
  , TestLabel "And Not tests"     andNotTests
  --, TestLabel "Not tests"         notTests
  , TestLabel "Specifier tests"   specifierTests
  , TestLabel "Case tests"        caseTests
  , TestLabel "Parenthese tests"  parentheseTests
  , TestLabel "Phrase tests"      phraseTests
  , TestLabel "Fuzzy tests"       fuzzyTests
  , TestLabel "Range tests"       rangeTests
  , TestLabel "Boost tests"       boostTests
  ]
--
main :: IO ()
main = defaultMain $ [ allProperties
                     , allUnitTests
                     ]
