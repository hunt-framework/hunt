{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE Rank2Types        #-}

module Holumbus.Index.TextIndex
(TextIndex, ContextTextIndex)
where

import           Holumbus.Index.Common
import           Holumbus.Index.Index
import           Holumbus.Index.Proxy.ContextIndex   (ContextIxCon)
-- ----------------------------------------------------------------------------

-- Requires 'ConstraintKinds' extension
type TextIndex i v
  = ( Index i
    , ICon i v
    , v ~ IVal i v
    , v ~ Occurrences
    , IKey i v ~ Word
    )

type ContextTextIndex i v = ( ContextIxCon i v
                            , Index i
                            , ICon i Occurrences
                            , IKey i Occurrences ~ Word
                            , TextIndex i v
                            )
