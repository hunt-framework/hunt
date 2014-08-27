{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Hunt.IndexTests
(contextTypeTests)
where

--import qualified Data.Map                       as M

import           Test.Framework

import qualified Hunt.Index.Default             as Default
import qualified Hunt.Index.IndexValueTests     as Value

-- ----------------------------------------------------------------------------

contextTypeTests :: [Test]
contextTypeTests = concat [ Default.tests
                          , Value.tests
                          ]
