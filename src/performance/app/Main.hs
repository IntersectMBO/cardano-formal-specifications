{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import PraosModel
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

main :: IO ()
main = do 
    let myOptions = FileOptions (400,400) PDF
    _ <- renderableToFile myOptions "app/Inserts/oneHopDelays.pdf" (toRenderable oneHopCDF)
    _ <- renderableToFile myOptions "app/Inserts/multi-hop64k-plots.pdf" (toRenderable multiHopCDF64k)
    _ <- renderableToFile myOptions "app/Inserts/multi-hop-1024k-plots.pdf" (toRenderable multiHopCDF1024k)
    _ <- renderableToFile myOptions "app/Inserts/blended-hop-blocksizes.pdf" (toRenderable blendedHopCDFNode10)
    _ <- renderableToFile myOptions "app/Inserts/verified-hop-blocksizes.pdf" (toRenderable blendedHopCDFNode10')
    _ <- renderableToFile myOptions "app/Inserts/pipelined-hop-script.pdf" (toRenderable pipelindedMultiHopScript)
    _ <- renderableToFile myOptions "app/Inserts/pipelined-hop-value.pdf" (toRenderable pipelindedMultiHopValue)
    _ <- renderableToFile myOptions "app/Inserts/pipelined-hop-bounding.pdf" (toRenderable pipelindedMultiHopBounding)
    _ <- renderableToFile myOptions "app/Inserts/compared-hop-blocktypes.pdf" (toRenderable comparedCDFNode10)

    putStrLn "All done"
