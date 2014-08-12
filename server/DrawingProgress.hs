{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}



module DrawingProgress where


import Drawing
import Data.List
import Data.Maybe
import Data.Acid
import Data.Typeable
import Data.SafeCopy
import Control.Monad.Reader (ask)
import Control.Applicative
import DbConnect


import qualified Control.Monad.State as S
import qualified Data.Map.Strict as Map



stepStroke :: Strokes -> BrushedReceivedPoint -> Strokes
stepStroke d (b, ReceivedPoint t0 sid p)  =
    Map.insert sid (Stroke sid t0 (p:ps) b) d
        where Stroke _ _ ps _ = Map.findWithDefault (Stroke sid t0 [] b) sid d


addNPoints :: [BrushedReceivedPoint] -> Strokes -> Strokes
addNPoints ps d = foldl' stepStroke d ps


applyBrush :: Brush -> ReceivedPoint -> BrushedReceivedPoint
applyBrush = (,)


addStrokes :: [Stroke] -> Strokes -> Strokes
addStrokes ss d = foldl' (\d s -> Map.insert (strokeId s) s d) d ss


removeStroke :: StrokeId -> Strokes -> Strokes
removeStroke = Map.delete


collectDrawings :: Drawings -> [DrawingInfo]
collectDrawings ds = fmap (\(did, ss) -> DrawingInfo did "" "" (Just (Map.elems ss))) $ Map.toList ds




type Drawings = Map.Map DrawingId Strokes
type Strokes = Map.Map StrokeId Stroke
type TouchTime = Int
type BrushedReceivedPoint = (Brush, ReceivedPoint)

data ReceivedPoint = ReceivedPoint TouchTime StrokeId Point deriving (Show)

instance SafeCopy ReceivedPoint where
     putCopy (ReceivedPoint t0 sid p) = contain $ do safePut t0; safePut sid; safePut p;
     getCopy = contain $ ReceivedPoint <$> safeGet <*> safeGet <*> safeGet

type Boards = Map.Map BoardId Drawings
data BoardsState = BoardsState !(Boards)
    deriving (Show, Ord, Eq, Typeable)

$(deriveSafeCopy 0 'base ''BoardsState)


emptyBoard :: Drawings
emptyBoard = Map.empty

emptyDrawing :: Strokes
emptyDrawing = Map.empty



updateDrawing :: (Strokes -> Strokes) -> DrawingInfo -> BoardId -> Boards -> Boards
updateDrawing f (DrawingInfo did _ _ _) = Map.alter updateBoard
    where
        updateBoard ds = Just $
            case ds of
                Just ds -> Map.insert did (f $ fromMaybe emptyDrawing (Map.lookup did ds)) ds
                Nothing -> Map.insert did (f emptyDrawing) emptyBoard

getDrawing' :: DrawingInfo -> BoardId -> Boards -> Maybe DrawingInfo
getDrawing' (DrawingInfo did _ _ _ ) bid bs = Map.lookup bid bs >>= Map.lookup did >>= \ss -> Just $ DrawingInfo did "" "" (Just $ Map.elems ss)

removeDrawing' :: DrawingInfo -> BoardId -> Boards -> Boards
removeDrawing' (DrawingInfo did _ _ _) bid bs = Map.adjust (Map.alter deleteD did) bid bs
    where deleteD ds = Nothing

addNewPoints :: BoardId -> DrawingInfo -> [BrushedReceivedPoint] -> Update BoardsState ()
addNewPoints bid d ps = do
    BoardsState bs <- S.get
    S.put (BoardsState (updateDrawing (addNPoints ps) d bid bs))

addNewStrokes :: BoardId -> DrawingInfo -> [Stroke] -> Update BoardsState ()
addNewStrokes bid d ss = do
    BoardsState bs <- S.get
    S.put (BoardsState (updateDrawing (addStrokes ss) d bid bs))

removeOldStroke :: BoardId -> DrawingInfo -> StrokeId -> Update BoardsState ()
removeOldStroke bid d sid = do
    BoardsState bs <- S.get
    S.put (BoardsState (updateDrawing (removeStroke sid) d bid bs))


getDrawings :: BoardId -> Query BoardsState [DrawingInfo]
getDrawings bid = do
    BoardsState bs <- ask
    return $ collectDrawings (Map.findWithDefault emptyBoard bid bs)

getDrawing :: BoardId -> DrawingInfo -> Query BoardsState (Maybe DrawingInfo)
getDrawing bid d = do
    BoardsState bs <- ask
    return $ getDrawing' d bid bs


removeDrawing :: BoardId -> DrawingInfo -> Update BoardsState ()
removeDrawing bid d = do
    BoardsState bs <- S.get
    S.put (BoardsState (removeDrawing' d bid bs))


$(makeAcidic ''BoardsState ['addNewPoints, 'addNewStrokes, 'removeOldStroke, 'getDrawings, 'removeDrawing, 'getDrawing])

fixtures :: Boards
fixtures = Map.empty