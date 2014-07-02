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
import           Control.Monad.Error                  hiding (when)
import           Control.Monad.IO.Class               ()
import           Control.Monad.Reader                 hiding (when)

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy                 as LB
import qualified Data.ByteString.Lazy.Char8           as LC
import           Data.Char                            (isAlphaNum)
import           Data.Map                             (Map)
import qualified Data.Map                             as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                            (Text)
import qualified Data.Text                            as T

import           Hunt.ClientInterface

import           Text.XML.HXT.Arrow.XmlState.TypeDefs
import           Text.XML.HXT.Core
import           Text.XML.HXT.HTTP

import           System.Console.CmdArgs
import           System.Exit
import           System.IO

-- ------------------------------------------------------------

data AppOpts
     = AO
       { proxy       :: String
       , redirect    :: Bool
       , parse_html  :: Bool
       , trace_level :: Int
       , output      :: Maybe String
       , hunt_server :: Maybe String
       , source      :: [String]
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
         -- print argl
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
    = do return defCx
    where
      defCx
          = [ ("contents", getHtmlPlainText)
            , ("title",   getHtmlTitle)
            ]

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

todo :: String -> a
todo = error . ("not yet implemented: " ++)

-- ------------------------------------------------------------
