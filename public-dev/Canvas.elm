import Dict
import Touch
import Window
import Text
import Char
import String
import Graphics.Input as Input


data Tool
    = Share
    | Start
    | Done
    | Brush
    | Erase
    | Undo
    | Move
    | ZoomIn
    | ZoomOut
    | Help
    | Default

data Tab
    = None
    | Color
    | Info


data State
    = Drawing
    | Erasing
    | Moving
    | Viewing


commands : Input.Input Tool
commands = Input.input Default


main = lift2 scene Window.dimensions
                   (Dict.values <~ foldp addN Dict.empty Touch.touches)

addN : [Touch.Touch] -> Dict.Dict Int [(Int,Int)] -> Dict.Dict Int [(Int,Int)]
addN ts dict = foldl add1 dict ts

add1 : Touch.Touch -> Dict.Dict Int [(Int,Int)] -> Dict.Dict Int [(Int,Int)]
add1 t d = let vs = Dict.getOrElse [] t.id d
           in  Dict.insert t.id ((t.x,t.y) :: vs) d

scene : (Int,Int) -> [[(Int,Int)]] -> Element
scene (w,h) paths =
    let float (a,b) = (toFloat a, toFloat -b)
        pathForms = group (map (traced thickLine . path . map float) paths)
        picture = collage w h [ move (float (-w `div` 2, -h `div` 2)) pathForms ]
    in  layers [ picture, container w h (topLeftAt (absolute 0) (absolute 150)) buttons ]

thickLine : LineStyle
thickLine = {defaultLine | color <- rgba 123 123 123 0.3, width <- 8, join <- Smooth, cap <- Round }


type Size = { w : Int, h : Int}

buttonSize : Size
buttonSize = { w = 65, h = 50 }

toolTab : Color
toolTab = rgb 102 102 102

icon : Color
icon = rgb 204 204 204

secondary : Color
secondary = rgb 193 39 45

toolTabShade : Size -> Float -> Element
toolTabShade size alpha =
    color (rgba 0 0 0 alpha)
    <| spacer size.w size.h

toolTabBorder : Size -> Int -> Color -> Element
toolTabBorder parentSize width clr =
    container parentSize.w parentSize.h midRight
    . color clr
    <| spacer width parentSize.h





type Button = {size : Size, content : Element}

toolTabStyle : Color -> Color -> Size -> Int -> Bool -> Bool -> Element -> Element
toolTabStyle background border size borderWidth active hover element =
    layers [ color background . container size.w size.h middle <| spacer size.w size.h
           , if active
             then toolTabShade size 0.2
             else empty
           , if hover
             then toolTabBorder size borderWidth border
             else empty
           , container size.w size.h middle element
           ]


button : Size -> Int -> Tool -> Element -> Element
button size borderWidth tool element =
    let btn active hover = toolTabStyle toolTab secondary size borderWidth active hover element
    in  Input.customButton commands.handle tool (btn False False) (btn False True) (btn True False)


buttons : Element
buttons = flow down <| map ((button buttonSize 5 Default) . (txt 0.6 icon))
      [ "!"
      , "+"
      , "?"
      ]

txt : Float -> Color -> String -> Element
txt p clr string =
    leftAligned . Text.height (p * (toFloat buttonSize.h)) .
    typeface ["Helvetica Neue","Sans-serif"] . Text.color clr <| toText string


step : Tool -> State -> State
step tool state =
    case tool of
      Start -> Drawing
      Brush -> Drawing

      _ -> None

tab : Tab -> Tab -> Tab
tab currTab newTab =
    case currTab of
    None -> newTab
    newTab -> None
