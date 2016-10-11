{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
module Hunt.SegmentIndex.Commit where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocDesc               (FieldValue (..))
import qualified Hunt.Common.DocDesc               as DocDesc
import           Hunt.Common.DocId
import qualified Hunt.Common.DocIdMap              as DocIdMap
import           Hunt.Common.Document
import           Hunt.Common.Occurrences           (Occurrences)
import qualified Hunt.Common.Positions             as Positions
import           Hunt.IO.Buffer                    (Buffer, Flush)
import qualified Hunt.IO.Buffer                    as Buffer
import           Hunt.IO.Files
import           Hunt.IO.Write
import           Hunt.IO.Writer
import           Hunt.Scoring.SearchResult
import           Hunt.SegmentIndex.Types.SegmentId

import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as ByteString
import           Data.Foldable
import           Data.Map                          (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Text
import qualified Data.Vector                       as V
import           Data.Word                         hiding (Word)
import           Foreign.Ptr
import           System.FilePath

import           Prelude                           hiding (Word)

type ContextNum = Int

type Offset = Int

type BytesWritten = Int

-- | 'encWriter' encodes 'a's with a given 'Write' to a 'Buffer'.
-- If the buffer is full it will be flushed. Returns the number
-- of bytes written.
encWriter :: Buffer
          -> Flush BytesWritten
          -> Write a
          -> Writer a BytesWritten
encWriter buffer flush (W size write) = WR start step stop
  where
    start = pure 0

    step bytesWritten a = do
      notFull <- Buffer.hasEnoughBytes buffer (size a)
      case notFull of
        True -> do
          _ <- Buffer.put buffer (write a)
          return bytesWritten
        False -> do
          n <- Buffer.flush buffer flush
          step (bytesWritten + n) a

    stop bytesWritten = do
      isNull <- Buffer.null buffer
      case isNull of
        True  -> return bytesWritten
        False -> do
          n <- Buffer.flush buffer flush
          return (bytesWritten + n)


writeIndex :: FilePath
           -> (Context -> ContextNum)
           -> SegmentId
           -> [(Word, Map Context SearchResult)]
           -> IO ()
writeIndex ixDir contextNum sid wx = do

  -- writeIndex master plan:
  --
  -- 1. Create three append-only files:
  --   * .tv
  --   * .occ
  --   * .pos
  --
  -- 2. Loop over all words and their contexts
  --   * Write positions
  --   * Write occurrences
  --   * Write word itself

  let
    -- Writes the terms in delta encoded form. Returns the number
    -- of terms written.
    termWriter :: Writer (Int, (ByteString, (ContextNum, (Int, Offset)))) a
               -> Writer (Text, ContextNum, Int, Offset) Int
    termWriter wr = case wr of
      WR fstart fstep fstop -> WR start step stop
        where
          start = T3 0 Text.empty <$> fstart

          step (T3 nterms lastWord fs) (word, cx, noccs, occoff) = do

            -- We encode the terms in a prefix free fashion. We look
            -- for a common prefix with the previous insert word
            -- and only encode the suffix.
            let (prefix, suffix) = case Text.commonPrefixes lastWord word of
                  Just (p, _, s) -> (p, s)
                  Nothing        -> (Text.empty, word)

            -- delegate the term info to the underlying 'Writer' to
            -- write the record to a buffer/disk.
            fs' <- fstep fs
                    ( ByteString.length (Text.encodeUtf8 prefix)
                    , (Text.encodeUtf8 suffix
                      , (cx
                        , ( noccs
                          , occoff
                          )
                        )
                      )
                    )
            return ( T3 (nterms + 1) word fs' )

          stop (T3 nterms _lastWord fs) = do
            _ <- fstop fs
            return nterms

    -- The big glue which ties terms, occurrences and positions
    -- together.
    indexWriter :: Writer Position (Int, BytesWritten)
                -> Writer (DocId, (Int, Offset)) (Int, BytesWritten)
                -> Writer (Text, ContextNum, Int, Offset) Int
                -> Writer (Text, ContextNum, Occurrences) Int
    indexWriter pw ow tw = case ow of
      WR ostart ostep ostop -> case tw of
        WR tstart tstep tstop -> WR start step stop where

          start = T3 0 0 <$> tstart

          step (T3 poff0 ooff0 ts) (word, cx, occs) = do

            -- for every occurrence we need to write its
            -- positions. Thus we need to manually do the
            -- fold over 'ow'.
            os0 <- ostart
            T2 os1 poff' <-
              foldlM (\(T2 os poff) (did, pos) -> do
                         -- write positions and remember how
                         -- many bytes actually were written
                         (npos, posBytesWritten) <-
                           runWriter pw (Positions.toAscList pos)

                         -- write the occurrences
                         os' <- ostep os (did, (npos , poff))
                         return $! T2 os' (poff + posBytesWritten)
                     ) (T2 os0 poff0) (DocIdMap.toList occs)

            -- number and bytes written for occurrences
            (noccs, occsBytesWritten) <- ostop os1

            -- write the term itself
            ts' <- tstep ts (word, cx, noccs, ooff0)
            return $! T3 poff' (ooff0 + occsBytesWritten) ts'

          stop (T3 _ _ ts) = tstop ts

  withAppendFile (ixDir </> termVectorFile sid) $ \termsFile ->
    withAppendFile (ixDir </> occurrencesFile sid) $ \occsFile ->
    withAppendFile (ixDir </> positionsFile sid) $ \posFile -> do

    Buffer.withBuffer (16 * 1024) $ \termBuf ->
      Buffer.withBuffer (8  * 1024) $ \occBuf ->
      Buffer.withBuffer (4  * 1024) $ \posBuf -> do

      -- 'posWrite', 'occWrite', 'termWrite' describe the
      -- schemas for the on-disk representation of terms,
      -- positions and occurrences.

      let

        vint :: Write Int
        vint = fromIntegral >$< varint64

        posWrite :: Write Position
        posWrite = vint

        occWrite :: Write (DocId, (Int, Offset))
        occWrite = (unDocId >$< vint) >*< vint >*< vint

        termWrite :: Write (Int, (ByteString, (ContextNum, (Int, Offset))))
        termWrite = vint >*< bytestring' >*< vint >*< vint >*< vint

        -- here we tie everything together, the flush for 'encWriter'
        -- always appends to the corresponding files on disk.
        iw = indexWriter
             ( (,) <$> count <*> encWriter posBuf (append posFile) posWrite )
             ( (,) <$> count <*> encWriter occBuf (append occsFile) occWrite )
             ( termWriter ( encWriter termBuf (append termsFile) termWrite ) )

      -- again we need to hand-roll the fold as we have a one-to-many relationship
      -- in context -> occurrences. This has potential for optimization as we
      -- do the common prefix check over and over again for the same words if they
      -- are present in multiple contexts.
      _ <- case iw of
        WR iwstart iwstep iwstop -> do
          iws0 <- iwstart
          iws1 <- foldlM (\iws (word, cxs) -> do
                             foldlM (\iws' (cx, sr) -> do
                                        iwstep iws' ( word
                                                    , contextNum cx
                                                    , searchResultToOccurrences sr
                                                    )
                                    ) iws (Map.toAscList cxs)
                         ) iws0 wx
          nterms <- iwstop iws1
          return nterms

      return ()
    return ()

type SortedFields = V.Vector Field

writeDocuments :: FilePath
               -> SegmentId
               -> SortedFields
               -> [Document]
               -> IO ()
writeDocuments ixDir sid fields docs = do

  withAppendFile (ixDir </> fieldDataFile sid) $ \fdtFile ->
    withAppendFile (ixDir </> fieldIndexFile sid) $ \fdxFile -> do

    Buffer.withBuffer (2 * 1024) $ \fdxBuffer -> do
      Buffer.withBuffer (16 * 1024) $ \fdtBuffer -> do

        let
          fdtFlush :: Flush BytesWritten
          fdtFlush = append fdtFile

          fdxFlush :: Flush BytesWritten
          fdxFlush = append fdxFile

          -- For each document write its fields to the
          -- field data buffer
          foldDocs bytesWritten doc = do
            let
              descr = desc doc
              !size = DocDesc.size descr

              -- for each field of a document write the fields one
              -- by one, sorted in 'fields' order.
              foldFields docBytesWritten fieldRank field = do
                case DocDesc.lookupValue field descr of
                  FV_Null -> return docBytesWritten
                  value   -> do
                    let
                      loop = do
                        notFull <- Buffer.hasEnoughBytes
                                   fdtBuffer
                                   (fvSize (fieldRank, value))
                        case notFull of
                          True -> do
                            n <- Buffer.put
                                 fdtBuffer
                                 (fvWrite (fieldRank, value))
                            return (docBytesWritten + n)
                          False -> do
                            _ <- Buffer.flush fdtBuffer fdtFlush
                            loop
                    loop

            lenBytes <- putFlush
                        fdtBuffer
                        fdtFlush
                        (vintSize size)
                        (vintWrite size)

            docBytes <- V.ifoldM' foldFields 0 fields

            -- write the offset of the document field data
            -- to the document field index.
            _ <- putFlush
                 fdxBuffer
                 fdxFlush
                 (w64Size (fromIntegral bytesWritten))
                 (w64Write (fromIntegral bytesWritten))

            return (lenBytes + docBytes + bytesWritten)

        _ <- foldlM foldDocs 0 docs

        Buffer.flush fdtBuffer fdtFlush
        Buffer.flush fdxBuffer fdxFlush
        return ()
  return ()

  where
    W word8Size word8Write = word8
    W w64Size w64Write     = word64
    vint                   = fromIntegral >$< varint64
    W vintSize vintWrite   = vint
    W bsSize bsWrite       = bytestring'
    W fvSize fvWrite       = vint >*< W size write
      where
        size (FV_Int i)    = vintSize i + tagSize
        size (FV_Float _f) = undefined
        size (FV_Text s)   = bsSize s + tagSize
        size (FV_Binary b) = bsSize b + tagSize
        size FV_Null       = 0

        write (FV_Int i) op      = word8Write 0 op
                                   >>= vintWrite i
        write (FV_Float _f) op    = word8Write 2 op
                                   >>= undefined
        write (FV_Text s)op      = word8Write 3 op
                                   >>= bsWrite s
        write (FV_Binary b) op   = word8Write 4 op
                                   >>= bsWrite b
        write FV_Null op         = return op

        tagSize = word8Size 0

    -- Helper which flushes and retries if buffer
    -- is full
    putFlush buffer flush size write = do
      notFull <- Buffer.hasEnoughBytes buffer size
      case notFull of
        True -> Buffer.put buffer write
        False -> do _ <- Buffer.flush buffer flush
                    putFlush buffer flush size write

termVectorFile :: SegmentId -> FilePath
termVectorFile sid = show sid <.> "tv"

occurrencesFile :: SegmentId -> FilePath
occurrencesFile sid = show sid <.> "occ"

positionsFile :: SegmentId -> FilePath
positionsFile sid = show sid <.> "pos"

fieldIndexFile :: SegmentId -> FilePath
fieldIndexFile sid = show sid <.> "fdx"

fieldDataFile :: SegmentId -> FilePath
fieldDataFile sid = show sid <.> "fdt"
