module PdfGenerator where


import qualified Drawing
import qualified Board

import Graphics.PDF

import GHC.Float
import Data.Maybe
import Data.List
import qualified Data.ByteString.Lazy as BL



toPdfPoint :: Drawing.Point -> Point
toPdfPoint (Drawing.Point x y) =  fromIntegral x :+ fromIntegral y



toPdfColor :: Drawing.Color -> Color
toPdfColor (Drawing.Color r g b _) = Rgb (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255)



renderStroke :: [Drawing.Point] -> Drawing.Brush -> Draw ()
renderStroke ps (Drawing.Brush c size) = do
    setWidth $ fromIntegral size
    strokeColor $ toPdfColor c
    setLineCap RoundCap
    setLineJoin RoundJoin
    setStrokeAlpha $ (float2Double . Drawing.alpha) c
    beginPath initPoint
    sequence $ fmap (lineto . toPdfPoint) (tail ps)
    strokePath
    where
        initPoint = toPdfPoint $ head ps -- I don't think it's poisible to have empty points


renderDot :: Drawing.Point -> Drawing.Brush -> Draw ()
renderDot (Drawing.Point x y) (Drawing.Brush c size) = do
    fillColor $ toPdfColor c
    setFillAlpha $ (float2Double . Drawing.alpha) c
    fill $ Circle (fromIntegral x) (fromIntegral y) $ (fromIntegral size) / 2


renderStrokeOrDot :: Drawing.Stroke -> Draw ()
renderStrokeOrDot (Drawing.Stroke _ _ ps b) =
    if null $ tail ps
        then renderDot (head ps) b
        else renderStroke ps b


renderBoard :: [Drawing.Drawing] -> Draw ()
renderBoard ds = do
    setRGBColorSpace
    sequence_ $ fmap renderStrokeOrDot $ sort strokes
    where
        getStrokes d = fromMaybe [] (Drawing.strokes d)
        strokes = foldl' (\ss d -> (getStrokes d) ++ ss) [] ds


renderPage :: PDFReference PDFPage -> Board.Dimensions -> Drawing.Color -> [Drawing.Drawing] -> PDF ()
renderPage page (Board.Dimensions w h _) backColor ds = do
    drawWithPage page $ do
        applyMatrix $ Matrix 1 0 0 (-1) 0 (fromIntegral h) -- flip vertically
        fillColor $ toPdfColor backColor
        fill $ Rectangle (0 :+ 0) ( fromIntegral w :+ fromIntegral h)
        renderBoard ds


getPdfDims :: Board.BoardSettings -> Maybe Board.Dimensions
getPdfDims b = Board.getDimensions (Board.paperType b) Board.paperDims


getPdf :: Board.BoardSettings -> [Drawing.Drawing] -> IO BL.ByteString
getPdf b ds = do
    let dims = getPdfDims b
    let rect = dims >>= (\(Board.Dimensions w h _) -> Just $ PDFRect 0 0 w h)
    pdfByteString (standardDocInfo { author=toPDFString "Whiteboard Wix", compressed = True}) (PDFRect 0 0 1000 800) $ do
        board <- addPage rect
        renderPage board (fromJust dims) (Board.backgroundColor b) ds
