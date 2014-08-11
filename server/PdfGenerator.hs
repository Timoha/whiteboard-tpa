module PdfGenerator where


import qualified Drawing
import qualified Board
import DbConnect (dbConnectInfo)
import Database.PostgreSQL.Simple

import Graphics.PDF

import GHC.Float


toPdfPoint :: Drawing.Point -> Point
toPdfPoint (Drawing.Point x y) =  fromIntegral x :+ fromIntegral y



toPdfColor :: Drawing.Color -> Color
toPdfColor (Drawing.Color r g b _) = Rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)



renderStroke :: Drawing.Stroke -> Draw ()
renderStroke (Drawing.Stroke _ _ ps (Drawing.Brush c size)) = do
    setWidth $ fromIntegral size
    strokeColor $ toPdfColor c
    setStrokeAlpha $ (float2Double . Drawing.alpha) c
    beginPath initPoint
    sequence $ fmap (lineto . toPdfPoint) (tail ps)
    fillAndStrokePath
    where
        initPoint = toPdfPoint $ head ps -- I don't think it's poisible to have empty points


--getPdf :: Board.BoardSettings -> [Drawing] -> Draw ()
--getPdf b ds = 