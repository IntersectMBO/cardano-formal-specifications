{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Default.Class
import PraosModel
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import System.Process

main :: IO ()
main = do 
    renderableToPDF "app/Inserts/oneHopDelays" (toRenderable oneHopCDF)
    renderableToPDF "app/Inserts/multi-hop64k-plots" (toRenderable multiHopCDF64k)
    renderableToPDF "app/Inserts/multi-hop-1024k-plots" (toRenderable multiHopCDF1024k)
    renderableToPDF "app/Inserts/blended-hop-blocksizes" (toRenderable blendedHopCDFNode10)
    renderableToPDF "app/Inserts/verified-hop-blocksizes" (toRenderable blendedHopCDFNode10')
    renderableToPDF "app/Inserts/pipelined-hop-script" (toRenderable pipelindedMultiHopScript)
    renderableToPDF "app/Inserts/pipelined-hop-value" (toRenderable pipelindedMultiHopValue)
    renderableToPDF "app/Inserts/pipelined-hop-bounding" (toRenderable pipelindedMultiHopBounding)
    renderableToPDF "app/Inserts/compared-hop-blocktypes" (toRenderable comparedCDFNode10)
    putStrLn "All done"

renderableToPDF :: FilePath -> Renderable () -> IO ()
renderableToPDF filename renderable = do
    _ <- renderableToFile myOptions (filename <> ".svg") renderable
    callProcess "svg2pdf" [filename <> ".svg", filename <> ".pdf"]
  where
    myOptions = def { _fo_size = (400,400), _fo_format = SVG }
