{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}

module GHC.Stats.Json where

import           Data.Aeson

import           GHC.Stats

-- ----------------------------------------------------------------------------

instance ToJSON GCStats where
  toJSON o = object
    [ "bytesAllocated"          .= bytesAllocated o
    , "numGcs"                  .= numGcs o
    , "maxBytesUsed"            .= maxBytesUsed o
    , "numByteUsageSamples"     .= numByteUsageSamples o
    , "cumulativeBytesUsed"     .= cumulativeBytesUsed o
    , "bytesCopied"             .= bytesCopied o
    , "currentBytesUsed"        .= currentBytesUsed o
    , "currentBytesSlop"        .= currentBytesSlop o
    , "maxBytesSlop"            .= maxBytesSlop o
    , "peakMegabytesAllocated"  .= peakMegabytesAllocated o
    , "mutatorCpuSeconds"       .= mutatorCpuSeconds o
    , "mutatorWallSeconds"      .= mutatorWallSeconds o
    , "gcCpuSeconds"            .= gcCpuSeconds o
    , "gcWallSeconds"           .= gcWallSeconds o
    , "cpuSeconds"              .= cpuSeconds o
    , "wallSeconds"             .= wallSeconds o
    , "parTotBytesCopied"       .= parTotBytesCopied o
    , "parMaxBytesCopied"       .= parMaxBytesCopied o
    ]

-- ----------------------------------------------------------------------------
