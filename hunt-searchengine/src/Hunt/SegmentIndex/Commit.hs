{-# LANGUAGE PatternGuards #-}
module Hunt.SegmentIndex.Commit where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import qualified Hunt.Common.DocIdMap              as DocIdMap
import           Hunt.Common.Occurrences           (Occurrences)
import qualified Hunt.Common.Positions             as Positions
import           Hunt.Index.Schema
import           Hunt.IO.Buffer                    (Buffer)
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
          -> (Ptr Word8 -> Int -> IO Int)
          -> Write a
          -> Writer a Int
encWriter buf0 flush (W size write) = WR start step stop
  where
    start = pure ( T2 0 buf0 )

    step (T2 nbytes buf) a
      | Buffer.hasEnoughBytes buf (size a) = do
          buf' <- Buffer.put buf (write a)
          return (T2 nbytes buf')
      | otherwise = do
          n <- Buffer.flush flush buf
          step ( T2 (nbytes + n) (Buffer.reset buf) ) a

    stop (T2 nbytes buf)
      | Buffer.null buf = return nbytes
      | otherwise = do
          n <- Buffer.flush flush buf
          return (n + nbytes)
{-# INLINE encWriter #-}

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

  withAppendFile (ixDir </> termVectorFile sid) $ \termsFile ->
    withAppendFile (ixDir </> occurrencesFile sid) $ \occsFile ->
    withAppendFile (ixDir </> positionsFile sid) $ \posFile -> do

    Buffer.withBuffer (16 * 1024) $ \termBuf ->
      Buffer.withBuffer (8  * 1024) $ \occBuf ->
      Buffer.withBuffer (4  * 1024) $ \posBuf -> do

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
                T2 os1 poff'  <-
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

        -- 'posWrite', 'occWrite', 'termWrite' describe the
        -- schemas for the on-disk representation of terms,
        -- positions and occurrences.

        vint :: Write Int
        vint = fromIntegral >$< varint64

        posWrite :: Write Position
        posWrite = vint

        occWrite :: Write (DocId, (Int, Offset))
        occWrite = (unDocId >$< vint) >*< vint >*< vint

        termWrite :: Write (Int, (ByteString, (ContextNum, (Int, Offset))))
        termWrite = vint >*< bytestring' >*< vint >*< vint >*< vint

      let
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

termVectorFile :: SegmentId -> FilePath
termVectorFile sid = show sid <.> "tv"

occurrencesFile :: SegmentId -> FilePath
occurrencesFile sid = show sid <.> "occ"

positionsFile :: SegmentId -> FilePath
positionsFile sid = show sid <.> "pos"
