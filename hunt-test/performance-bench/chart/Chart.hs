module Main where

import Data.Binary
import Data.Int
import Data.Text (Text)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Plot.Lines
import Data.List
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
import System.Random
import Data.Colour.SRGB
import qualified Data.Map as M

main :: IO ()
main = do
  -- insert benchmark (time,memory)
  unc   <- decodeFile "results_unc.bin"     :: IO [(Double, Int64)]
  bs    <- decodeFile "results_bs.bin"      :: IO [(Double, Int64)]
  bzip  <- decodeFile "results_bzip.bin"    :: IO [(Double, Int64)]
  cache <- decodeFile "results_cache.bin"   :: IO [(Double, Int64)]
  btree <- decodeFile "results_bintree.bin" :: IO [(Double, Int64)]
  
  -- query performance benchmark (time for 1000 queries)
  q_unc   <- decodeFile "results_query_unc.bin"     :: IO [Double]
  q_btree <- decodeFile "results_query_bintree.bin" :: IO [Double]
  q_bs    <- decodeFile "results_query_bs.bin"      :: IO [Double]
  q_bzip  <- decodeFile "results_query_bzip.bin"    :: IO [Double]

  -- format data
  let memory_fst = zipWith3 (\a b c -> [snd' a, snd' b, snd' c]) unc bs bzip
  let time_fst   = zipWith3 (\a b c -> [fst  a, fst  b, fst  c]) unc bs bzip

  let memory_all = zipWith4 (\a b c d  -> [snd' a, snd' b, snd' c, snd' d ]) unc btree bs bzip
  let time_all   = zipWith4 (\a b c d  -> [fst  a, fst  b, fst  c, fst  d ]) unc btree bs bzip

  let query      = zipWith4 (\a b c d  -> [a,b,c,d]) q_unc q_btree q_bs q_bzip

  -- memory diagram with uncompr. + bytestring + bzip
  let memory1 = def
            & layout_title  .~ "Memory footprint" 
            & layout_plots  .~ [ plotBars $ barDiagram3
                                      (reverse [ "BZIP"
                                      , "ByteString"
                                      , "Uncompressed"
                                      ]) memory_fst
                               ]
            & layout_x_axis .~ (def    & laxis_title .~ "Number of indexed Documents"
                                       & laxis_generate .~ (autoIndexAxis $ map toDocs [1..11])
                               )
            & layout_y_axis .~ (def & laxis_title .~ "Index Size in Megabyte")

  -- performance benchmark for uncompr + bs + bzip
  let time1 = def
            & layout_title  .~ "Execution times of Insert command" 
            & layout_plots  .~ [ plotBars $ barDiagram3
                                      (reverse [ "BZIP"
                                      , "ByteString"
                                      , "Uncompressed"
                                      ]) time_fst
                               ]
            & layout_x_axis .~ (def    & laxis_title .~ "Number of indexed Documents"
                                       & laxis_generate .~ (autoIndexAxis $ map toDocs [1..11])
                               )
            & layout_y_axis .~ (def & laxis_title .~ "Execution time in seconds")


  -- memory diagram with all index types
  let memory2 = def
            & layout_title  .~ "Memory footprint" 
            & layout_plots  .~ [ plotBars $ barDiagram4
                                      (reverse [ "BZIP"
                                      , "ByteString"
                                      , "Cache + Binary Tree"
                                    --  , "Cache"
                                      , "Uncompressed" 
                                      ]) memory_all 
                               ]
            & layout_x_axis .~ (def    & laxis_title .~ "Number of indexed documents"
                                       & laxis_generate .~ (autoIndexAxis $ map toDocs [1..11])
                               )
            & layout_y_axis .~ (def & laxis_title .~ "Index Size in Megabyte")

  -- performance diagram for all index types
  let time2 = def
            & layout_title  .~ "Execution time of Insert command" 
            & layout_plots  .~ [ plotBars $ barDiagram4 
                                      (reverse [ "BZIP"
                                      , "ByteString"
                                      , "Cache + Binary Tree"
                                   --   , "Cache"
                                      , "Uncompressed" 
                                      ]) time_all
                               ]
            & layout_x_axis .~ (def    & laxis_title .~ "Number of indexed documents"
                                       & laxis_generate .~ (autoIndexAxis $ map toDocs [1..11])
                               )
            & layout_y_axis .~ (def & laxis_title .~ "Execution time in seconds")

  -- query benchmark
  
  let q_bars = [plotBars $ barDiagram4 (reverse [ "BZIP"
                                      , "ByteString"
                                      , "Cache + Binary Tree"
                                      , "Uncompressed" 
                                      ]) query]

  let q_lines =  [ toPlot $ lineDiagram "BZIP" q_bzip
                 , toPlot $ lineDiagram "ByteString" q_bs
                 , toPlot $ lineDiagram "Cache + Binary Tree" q_btree
                 , toPlot $ lineDiagram "Uncompressed" q_unc
                 ]

  let query1 = def
            & layout_title  .~ "Execution times of 1000 queries for 'a'" 
            & layout_plots  .~ q_lines
            & layout_x_axis .~ (def    & laxis_title .~ "Number of indexed documents"
                                       & laxis_generate .~ (autoIndexAxis $ map toDocs [1..11])
                               )
            & layout_y_axis .~ (def & laxis_title .~ "Execution time in seconds")

  mapM (\(plot,file) -> renderableToFile (FileOptions (640,300) SVG M.empty) (toRenderable plot) file) 
    [ (memory1, "memory1.svg")
    , (memory2, "memory2.svg")
    , (time1  , "time1.svg")
    , (time2  , "time2.svg")
    , (query1 , "query.svg")
    ]   

  return ()

--------------------
-- helper

snd' :: Integral b => (a,b) -> Double
snd' (a,b) = fromIntegral b

toDocs :: Int -> String
toDocs i = show $ i * 3000

barDiagram3 :: [String] -> [[Double]] -> PlotBars PlotIndex Double
barDiagram3 titles x = 
  def & plot_bars_titles .~ titles
      & plot_bars_values .~ addIndexes x
      & plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq1)

barDiagram4 :: [String] -> [[Double]] -> PlotBars PlotIndex Double
barDiagram4 titles x = 
  def & plot_bars_titles .~ titles
      & plot_bars_values .~ addIndexes x
      & plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq2)

lineDiagram :: String -> [Double] -> PlotLines PlotIndex Double
lineDiagram title res = 
  def & plot_lines_title  .~ title
      & plot_lines_style  .~ def
      & plot_lines_values .~ [addIndexes res]
--      & plot_lines_item_styles .~ map mkstyle (cycle defaultColorSeq2)

bstyle = Just (solidLine 1.0 $ opaque black)
mkstyle c = (solidFillStyle c, bstyle)

defaultColorSeq1 = cycle $ map opaque [blue, red, green, magenta] 
defaultColorSeq2 = cycle $ map opaque [blue, yellow, red, green] 
