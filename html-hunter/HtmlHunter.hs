{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

-- ------------------------------------------------------------

{- |
   Module     : HTML Extractor for the Hunt System
   Copyright  : Copyright (C) 2014 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt
   Maintainer : uwe@fh-wedel.de
   Stability  : experimental

-}

-- ------------------------------------------------------------

module Main
    (main)
where

import           Control.Applicative
import qualified Control.Monad                        as CM (when)
import           Control.Monad.Error                  hiding (when)
import           Control.Monad.IO.Class               ()
import           Control.Monad.Reader                 hiding (when)

import           Data.Char                            (isAlphaNum)
import           Data.Text                            (Text)
import qualified Data.Text                            as T

import           Hunt.ClientInterface

import           Text.Regex.XMLSchema.String          (match)
import           Text.XML.HXT.Arrow.XmlState.TypeDefs
import           Text.XML.HXT.Core
import           Text.XML.HXT.HTTP
import           Text.XML.HXT.XPath                   (getXPathTreesInDoc,
                                                       parseXPathExpr)

import           System.Console.CmdArgs
import           System.Exit
import           System.IO

-- ------------------------------------------------------------

data AppOpts
     = AO
       { proxy          :: String
       , redirect       :: Bool
       , parse_html     :: Bool
       , trace_level    :: Int
       , text_body      :: Bool
       , text_title     :: Bool
       , text_headlines :: Bool
       , contexts       :: [String]
       , output         :: Maybe String
       , hunt_server    :: Maybe String
       , source         :: [String]
       }
    deriving (Data, Typeable, Show)

type HIO = ReaderT AppOpts (ErrorT String IO)

cmdArgsDescr :: Mode (CmdArgs AppOpts)
cmdArgsDescr
    = cmdArgsMode $
      AO { parse_html
               = False
                 &= help "force HTML parsing"
         , proxy
             = ""
               &= name "p"
               &= typ  "URL"
               &= help "HTTP acces via proxy"
         , redirect
             = False
               &= name "r"
               &= help "allow automatic redirects"
         , trace_level
             = 0
               &= name "t"
               &= typ "INT"
               &= help "trace level"
         , text_body
             = False
               &= help "index all text within <body> element into a body context"
         , text_title
             = False
               &= help "index all text within <title> element into a title context"
         , text_headlines
             = False
               &= help "index all text within <h1> to <h6> elements into a headlines context"
         , contexts
             = []
               &= name "c"
               &= typ "CONTEXT:XPATH"
               &= help
                  (unwords [ "additional contexts with an XPATH selector for the parts to be indexed,"
                           , "several contexts may be given,"
                           , "example for selecting all text within the"
                           , "body-part of a document: \"-c body:/html/body\" or"
                           , "shorter \"body://body\""
                           , "(same as option \"--text-body\")"
                           ]
                  )
         , output
             = Nothing
               &= typ  "FILE"
               &= help "output file, \"-\" for stdout"
         , hunt_server
             = Nothing
               &= typ "URL"
               &= help "hunt server url"
         , source
             = []
               &= args
               &= typ  "FILES/URLS"
         }
         &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
         &= help    _PROGRAM_ABOUT
         &= program _PROGRAM_NAME
    where
      _PROGRAM_NAME
          = "html-hunter"
      _PROGRAM_VERSION
          = "0.0.0.1"
      _PROGRAM_INFO
          = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
      _PROGRAM_ABOUT
          = "a simple HTML extractor for the Hunt search engine system"
      _COPYRIGHT
          = "(C) Uwe Schmidt 2014"

main :: IO ()
main
    = do argl <- cmdArgsRun cmdArgsDescr
         print argl
         res  <- runErrorT $ runReaderT doTheWork argl
         either (failure) (const exitSuccess) res
    where
      failure s
          = do hPutStrLn stderr $ "program aborted: " ++ s
               exitFailure

-- ------------------------------------------------------------
--
-- the real main

doTheWork :: HIO ()
doTheWork
    = do inputs <- asks source
         docs   <- mapM processDoc inputs
         cmd    <- buildRes docs
         emitRes cmd
    where
      buildRes :: [ApiDocument] -> HIO Command
      buildRes ds
          = return (cmdSequence . map cmdInsertDoc $ ds)


processDoc :: String -> HIO ApiDocument
processDoc url
    = do dom  <- getDom url
         doc  <- extractText url dom
         return doc

emitRes :: Command -> HIO ()
emitRes cmd
    = do f <- asks output
         s <- asks hunt_server
         liftIO $ (sendCmd f s) cmd
    where
      sendCmd _ (Just s) = sendCmdToServer s
      sendCmd (Just f) _ = sendCmdToFile f
      sendCmd _ _        = sendCmdToFile ""

notice :: String -> HIO ()
notice msg
    = do l <- asks trace_level
         if l >= 1
            then liftIO $ hPutStrLn stderr msg
            else return ()

-- ------------------------------------------------------------

-- the hxt input and parsing stuff

getDom :: String -> HIO XmlTree
getDom url
    = do notice' $ "start reading"
         ic <- configX
         rs <- liftIO $ runX
               ( configSysVars ic
                 >>>
                 readDocument [ withValidate          no
                              , withRemoveWS          yes
                              , withWarnings          no
                              , withSubstDTDEntities  no
                              , withSubstHTMLEntities yes
                              , withHTTP     []
                              ] url
                 >>>
                 processTopDown normalize
                 >>>
                 traceSource >>> traceTree
                 >>>
                 (this &&& getErrStatus)
               )
         case rs of
           ((x, rc) : _)
               | rc < c_err  -> notice' "document read" >> return x
           _                 -> error' "errors while reading"

    where
      msg' m            = unwords [m, show url]
      notice'           = notice . msg'
      error' m          = notice' m >> throwError (msg' m)

      normalize
          = fromLA $
            choiceA
            [ hasName "script"   :-> none
            , hasName "noscript" :-> none
            , this               :-> this
            ]
      configX
          = do p <- setProxy    <$> asks proxy
               r <- setRedirect <$> asks redirect
               h <- setParser   <$> asks parse_html
               t <- setTraceLev <$> asks trace_level
               return $ p ++ r ++ h ++ t
          where
            setProxy ""         = []
            setProxy x          = [withProxy x]

            setRedirect b       = [withRedirect  b]
            setParser   b       = [withParseHTML b]
            setTraceLev x       = [withTrace     x]

-- ------------------------------------------------------------

extractText :: String -> XmlTree -> HIO ApiDocument
extractText url dom
    = (toDoc . map (uncurry toText)) <$> getContexts
    where
      absUrl
          | null tr   = url
          | otherwise = tr
          where
            tr = concat $ runLA getTransferUrl dom

      toText cx sel
          = (cx, extr sel)

      toDoc :: [(Context, (Text, Text))] -> ApiDocument
      toDoc
          = foldr setCx (mkApiDoc $ T.pack absUrl)
            where
              setCx (cx, (descr, ixws)) doc
                  | T.null descr
                    &&
                    T.null ixws
                      = doc
                  | otherwise
                      = addToIndex cx ixws
                        . addDescription cx descr
                        $ doc

      extr :: LA XmlTree String -> (Text, Text)
      extr sel
          = ( (T.pack . normalizeWS)
              &&&
              (T.pack . normalizeWS . filterAlphaNum)
            )
            . concat
            . runLA sel
            $ dom
          where
            normalizeWS
                = unwords . words
            filterAlphaNum
                = map (\ x -> if isAlphaNum x then x else ' ')

getContexts :: HIO [(Text, LA XmlTree String)]
getContexts
    = do cx <- asks contexts
         tb <- asks text_body
         tt <- asks text_title
         th <- asks text_headlines
         if null cx && not tb && not tt && not th
            then return defCx
            else addSpecial tb tt th <$> mapM compSelect cx
    where
      addSpecial tb tt th xs
          = (if tb then [ctb] else [])
            ++
            (if tt then [ctt] else [])
            ++
            (if th then [cth] else [])
            ++
            xs

      defCx@[ctb, ctt, cth]
          = [ ("body",      getHtmlPlainText)
            , ("title",     getHtmlTitle)
            , ("headlines", getHtmlHeadlines)
            ]

      compSelect cxSpec
          = do CM.when (not . match "\\p{L}(\\p{L}|\\p{N}|_)*" $ cx) $
                 throwError (unwords ["context name must be an identifier, found"
                                     , show cx
                                     , "in:"
                                     , show cxSpec
                                     ]
                            )
               CM.when (not $ xpathOk xp) $
                 throwError (unwords ["not a leagal XPath expression, found"
                                     , show xp
                                     , "in:"
                                     , show cxSpec
                                     ]
                            )
               return (T.pack cx, getAllText (getXPathTreesInDoc xp))
          where
            (cx, rs1) = span (/= ':') cxSpec
            xp        = drop 1 rs1
            xpathOk s
                = either (const False) (const True)
                  $ parseXPathExpr s

-- ------------------------------------------------------------
--
-- HTML helper arrows

getByPath                       :: ArrowXml a => [String] -> a XmlTree XmlTree
getByPath                       = seqA . map (\ n -> getChildren >>> hasName n)

getHtmlTitle                    :: ArrowXml a => a XmlTree String
getHtmlTitle                    = getAllText $
                                  getByPath ["html", "head", "title"]

getHtmlPlainText                :: ArrowXml a => a XmlTree String
getHtmlPlainText                = getAllText $
                                  getByPath ["html", "body"]

getHtmlHeadlines                :: ArrowXml a => a XmlTree String
getHtmlHeadlines                = getAllText $
                                  getByPath ["html", "body"]
                                  >>> deep
                                      (foldr1 (<+>) (map (hasName . ("h" ++) . show) [(1::Int)..6]))

getAllText                      :: ArrowXml a => a XmlTree XmlTree -> a XmlTree String
getAllText getText'             = ( getText'
                                    >>>
                                    ( fromLA $ deep getText )
                                    >>^
                                    (" " ++)                  -- text parts are separated by a space
                                  )
                                  >. concat

getTransferUrl                  :: ArrowXml a => a XmlTree String
getTransferUrl                  = getAttrValue transferURI

-- ------------------------------------------------------------
{-
todo :: String -> a
todo = error . ("not yet implemented: " ++)
-- -}
-- ------------------------------------------------------------
