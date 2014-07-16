Elm.Canvas = Elm.Canvas || {};
Elm.Canvas.make = function (_elm) {
   "use strict";
   _elm.Canvas = _elm.Canvas || {};
   if (_elm.Canvas.values)
   return _elm.Canvas.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Canvas";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Dict = Elm.Dict.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Set = Elm.Set.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var Touch = Elm.Touch.make(_elm);
   var Window = Elm.Window.make(_elm);
   var _op = {};
   var thickLine = function (brush) {
      return _U.replace([["color"
                         ,brush.color]
                        ,["width",brush.size]
                        ,["join"
                         ,Graphics.Collage.Smooth]
                        ,["cap"
                         ,Graphics.Collage.Round]],
      Graphics.Collage.defaultLine);
   };
   var applyBrush = F2(function (ts,
   b) {
      return A2(List.map,
      function (t) {
         return _U.insert("brush",
         b,
         t);
      },
      ts);
   });
   var portToBrush = function (p) {
      return {_: {}
             ,color: A4(Color.rgba,
             p.red,
             p.green,
             p.blue,
             p.alpha)
             ,size: p.size};
   };
   var modePort = Native.Ports.portIn("modePort",
   Native.Ports.incomingSignal(function (v) {
      return typeof v === "string" || typeof v === "object" && v instanceof String ? v : _E.raise("invalid input, expecting JSString but got " + v);
   }));
   var undoPort = Native.Ports.portIn("undoPort",
   Native.Ports.incomingSignal(function (v) {
      return _U.isJSArray(v) ? {ctor: "_Tuple0"} : _E.raise("invalid input, expecting JSArray but got " + v);
   }));
   var brushPort = Native.Ports.portIn("brushPort",
   Native.Ports.incomingSignal(function (v) {
      return typeof v === "object" && "size" in v && "red" in v && "green" in v && "blue" in v && "alpha" in v ? {_: {}
                                                                                                                 ,size: typeof v.size === "number" ? v.size : _E.raise("invalid input, expecting JSNumber but got " + v.size)
                                                                                                                 ,red: typeof v.red === "number" ? v.red : _E.raise("invalid input, expecting JSNumber but got " + v.red)
                                                                                                                 ,green: typeof v.green === "number" ? v.green : _E.raise("invalid input, expecting JSNumber but got " + v.green)
                                                                                                                 ,blue: typeof v.blue === "number" ? v.blue : _E.raise("invalid input, expecting JSNumber but got " + v.blue)
                                                                                                                 ,alpha: typeof v.alpha === "number" ? v.alpha : _E.raise("invalid input, expecting JSNumber but got " + v.alpha)} : _E.raise("invalid input, expecting JSObject [\"size\",\"red\",\"green\",\"blue\",\"alpha\"] but got " + v);
   }));
   var defaultCanvas = {_: {}
                       ,dimensions: {ctor: "_Tuple2"
                                    ,_0: 0
                                    ,_1: 0}
                       ,drawing: Dict.empty
                       ,history: Dict.empty
                       ,scale: 1
                       ,topLeft: {ctor: "_Tuple2"
                                 ,_0: 0
                                 ,_1: 0}};
   var line = F2(function (p1,p2) {
      return {_: {},p1: p1,p2: p2};
   });
   var toSegments = function (ps) {
      return function () {
         var startNew = F2(function (p,
         ls) {
            return {ctor: "::"
                   ,_0: A2(line,p,p)
                   ,_1: ls};
         });
         var connectPrev = F2(function (p,
         ls) {
            return function () {
               var p1 = List.head(ls).p1;
               return {ctor: "::"
                      ,_0: A2(line,p1,p)
                      ,_1: List.tail(ls)};
            }();
         });
         var connect = F2(function (p,
         ls) {
            return startNew(p)(A2(connectPrev,
            p,
            ls));
         });
         return List.tail(A3(List.foldl,
         connect,
         {ctor: "::"
         ,_0: A2(line,
         List.head(ps),
         List.head(ps))
         ,_1: _L.fromArray([])},
         ps));
      }();
   };
   var pointToTuple = function (p) {
      return {ctor: "_Tuple2"
             ,_0: p.x
             ,_1: p.y};
   };
   var dot = F2(function (pos,
   brush) {
      return Graphics.Collage.move(pointToTuple(pos))(A2(Graphics.Collage.filled,
      brush.color,
      Graphics.Collage.circle(brush.size / 2)));
   });
   var display = F2(function (_v0,
   paths) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return function () {
                 var strokeOrDot = function (path) {
                    return function () {
                       var _v4 = _U.cmp(List.length(path.points),
                       1) > 0;
                       switch (_v4)
                       {case false: return A2(dot,
                            List.head(path.points),
                            path.brush);
                          case true:
                          return Graphics.Collage.traced(thickLine(path.brush))(A2(List.map,
                            pointToTuple,
                            path.points));}
                       _E.Case($moduleName,
                       "between lines 280 and 283");
                    }();
                 };
                 var forms = A2(List.map,
                 strokeOrDot,
                 paths);
                 var $float = function (_v5) {
                    return function () {
                       switch (_v5.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: Basics.toFloat(_v5._0)
                                 ,_1: Basics.toFloat(0 - _v5._1)};}
                       _E.Case($moduleName,
                       "on line 278, column 20 to 41");
                    }();
                 };
                 return A3(Graphics.Collage.collage,
                 _v0._0,
                 _v0._1,
                 _L.fromArray([A2(Graphics.Collage.move,
                 $float({ctor: "_Tuple2"
                        ,_0: (0 - _v0._0) / 2 | 0
                        ,_1: (0 - _v0._1) / 2 | 0}),
                 Graphics.Collage.group(forms))]));
              }();}
         _E.Case($moduleName,
         "between lines 277 and 284");
      }();
   });
   var point = F2(function (x,y) {
      return {_: {},x: x,y: y};
   });
   var linesIntersection = F2(function (l1,
   l2) {
      return function () {
         var dy34 = l2.p1.y - l2.p2.y;
         var dy12 = l1.p1.y - l1.p2.y;
         var dx34 = l2.p1.x - l2.p2.x;
         var dx12 = l1.p1.x - l1.p2.x;
         var den = dx12 * dy34 - dy12 * dx34;
         return _U.eq(den,
         0) ? Maybe.Nothing : function () {
            var det34 = l2.p1.x * l2.p2.y - l2.p1.y * l2.p2.x;
            var det12 = l1.p1.x * l1.p2.y - l1.p1.y * l1.p2.x;
            var numx = det12 * dx34 - dx12 * det34;
            var numy = det12 * dy34 - dy12 * det34;
            return Maybe.Just(A2(point,
            numx / den,
            numy / den));
         }();
      }();
   });
   var isIntersect = F2(function (l1,
   l2) {
      return function () {
         var _v9 = A2(linesIntersection,
         l1,
         l2);
         switch (_v9.ctor)
         {case "Just": return true;
            case "Nothing": return false;}
         _E.Case($moduleName,
         "between lines 134 and 136");
      }();
   });
   var isStrokesIntersect = F2(function (s1,
   s2) {
      return function () {
         var _v11 = _U.cmp(List.length(s1.points),
         1) > 0 || _U.cmp(List.length(s2.points),
         1) > 0;
         switch (_v11)
         {case true: return function () {
                 var segs2 = toSegments(s2.points);
                 var segs1 = toSegments(s1.points);
                 return A2(List.any,
                 function (x) {
                    return A2(List.any,
                    isIntersect(x),
                    segs2);
                 },
                 segs1);
              }();}
         return false;
      }();
   });
   var strokesCrossed = F2(function (s,
   ss) {
      return A2(List.filter,
      isStrokesIntersect(s),
      ss);
   });
   var isLineStrokeIntersect = F2(function (l,
   s) {
      return function () {
         var _v12 = _U.cmp(List.length(s.points),
         1) > 0;
         switch (_v12)
         {case true:
            return List.any(isIntersect(l))(toSegments(s.points));}
         return false;
      }();
   });
   var Line = F2(function (a,b) {
      return {_: {},p1: a,p2: b};
   });
   var Point = F2(function (a,b) {
      return {_: {},x: a,y: b};
   });
   var Stroke = F3(function (a,
   b,
   c) {
      return {_: {}
             ,brush: c
             ,id: a
             ,points: b};
   });
   var Brushed = F2(function (a,
   b) {
      return _U.insert("brush",
      a,
      b);
   });
   var Brush = F2(function (a,b) {
      return {_: {}
             ,color: b
             ,size: a};
   });
   var Canvas = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,dimensions: c
             ,drawing: a
             ,history: b
             ,scale: d
             ,topLeft: e};
   });
   var Input = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,action: b
             ,brush: c
             ,canvasDims: d
             ,mode: a};
   });
   var Drew = function (a) {
      return {ctor: "Drew",_0: a};
   };
   var add1 = F2(function (t,
   _v13) {
      return function () {
         switch (_v13.ctor)
         {case "_Tuple2":
            return function () {
                 var id = Basics.abs(t.id);
                 var vs = A3(Dict.getOrElse,
                 {_: {}
                 ,brush: t.brush
                 ,id: id
                 ,points: _L.fromArray([])},
                 id,
                 _v13._0);
                 return {ctor: "_Tuple2"
                        ,_0: A3(Dict.insert,
                        id,
                        _U.replace([["points"
                                    ,{ctor: "::"
                                     ,_0: A2(point,
                                     Basics.toFloat(t.x),
                                     Basics.toFloat(0 - t.y))
                                     ,_1: vs.points}]],
                        vs),
                        _v13._0)
                        ,_1: A3(Dict.insert,
                        id,
                        Drew(id),
                        _v13._1)};
              }();}
         _E.Case($moduleName,
         "between lines 202 and 207");
      }();
   });
   var addN = F3(function (ts,
   d,
   h) {
      return A3(List.foldl,
      add1,
      {ctor: "_Tuple2",_0: d,_1: h},
      ts);
   });
   var Erased = function (a) {
      return {ctor: "Erased"
             ,_0: a};
   };
   var undo = function (_v17) {
      return function () {
         switch (_v17.ctor)
         {case "_Tuple2":
            return function () {
                 var ids = Dict.keys(_v17._1);
                 return function () {
                    switch (ids.ctor)
                    {case "[]":
                       return {ctor: "_Tuple2"
                              ,_0: _v17._0
                              ,_1: Dict.empty};}
                    return function () {
                       var _v22 = A2(Dict.get,
                       List.maximum(ids),
                       _v17._1);
                       switch (_v22.ctor)
                       {case "Just":
                          switch (_v22._0.ctor)
                            {case "Drew":
                               return {ctor: "_Tuple2"
                                      ,_0: A2(Dict.remove,
                                      _v22._0._0,
                                      _v17._0)
                                      ,_1: A2(Dict.remove,
                                      _v22._0._0,
                                      _v17._1)};
                               case "Erased":
                               return {ctor: "_Tuple2"
                                      ,_0: A3(List.foldl,
                                      F2(function (s,d) {
                                         return A3(Dict.insert,
                                         s.id,
                                         s,
                                         d);
                                      }),
                                      _v17._0,
                                      _v22._0._0)
                                      ,_1: A2(Dict.remove,
                                      List.maximum(ids),
                                      _v17._1)};}
                            break;
                          case "Nothing":
                          return {ctor: "_Tuple2"
                                 ,_0: Dict.empty
                                 ,_1: Dict.empty};}
                       _E.Case($moduleName,
                       "between lines 183 and 186");
                    }();
                 }();
              }();}
         _E.Case($moduleName,
         "between lines 178 and 186");
      }();
   };
   var removeEraser = F2(function (d,
   h) {
      return function () {
         var ids = Dict.keys(h);
         return function () {
            switch (ids.ctor)
            {case "[]":
               return {ctor: "_Tuple2"
                      ,_0: d
                      ,_1: Dict.empty};}
            return function () {
               var _v27 = A2(Dict.get,
               List.maximum(ids),
               h);
               switch (_v27.ctor)
               {case "Just":
                  switch (_v27._0.ctor)
                    {case "Erased":
                       return {ctor: "_Tuple2"
                              ,_0: A2(Dict.remove,
                              List.maximum(ids),
                              d)
                              ,_1: h};}
                    break;}
               return {ctor: "_Tuple2"
                      ,_0: d
                      ,_1: h};
            }();
         }();
      }();
   });
   var eraser = F3(function (ts,
   d,
   h) {
      return function () {
         switch (ts.ctor)
         {case "[]":
            return A2(removeEraser,d,h);}
         return function () {
            var firstT = List.head(ts);
            var id = Basics.abs(firstT.id);
            var $ = function () {
               var _v31 = A2(Dict.get,id,d);
               switch (_v31.ctor)
               {case "Just":
                  return function () {
                       var crossed = Dict.values(d);
                       var vs = crossed;
                       var eraserSeg = A2(line,
                       A2(point,firstT.x,firstT.y),
                       List.head(_v31._0.points));
                       return A3(addN,
                       _L.fromArray([firstT]),
                       d,
                       A3(Dict.insert,
                       id,
                       Erased(_L.append(crossed,vs)),
                       h));
                    }();
                  case "Nothing": return A3(addN,
                    _L.fromArray([firstT]),
                    d,
                    h);}
               _E.Case($moduleName,
               "between lines 229 and 236");
            }(),
            d$ = $._0,
            h$ = $._1;
            return {ctor: "_Tuple2"
                   ,_0: d$
                   ,_1: h$};
         }();
      }();
   });
   var Viewing = {ctor: "Viewing"};
   var Erasing = {ctor: "Erasing"};
   var Drawing = {ctor: "Drawing"};
   var portToMode = function (s) {
      return function () {
         switch (s)
         {case "Drawing": return Drawing;
            case "Erasing": return Erasing;}
         _E.Case($moduleName,
         "between lines 90 and 92");
      }();
   };
   var Touches = function (a) {
      return {ctor: "Touches"
             ,_0: a};
   };
   var ZoomOut = {ctor: "ZoomOut"};
   var ZoomIn = {ctor: "ZoomIn"};
   var Undo = {ctor: "Undo"};
   var actions = Signal.merges(_L.fromArray([A2(Signal._op["<~"],
                                            Touches,
                                            Touch.touches)
                                            ,A2(Signal._op["<~"],
                                            Basics.always(Undo),
                                            undoPort)]));
   var input = A2(Signal._op["~"],
   A2(Signal._op["~"],
   A2(Signal._op["~"],
   A2(Signal._op["<~"],
   Input,
   A2(Signal._op["<~"],
   portToMode,
   modePort)),
   actions),
   A2(Signal._op["<~"],
   portToBrush,
   brushPort)),
   Window.dimensions);
   var stepCanvas = F2(function (_v34,
   _v35) {
      return function () {
         return function () {
            return function () {
               var $ = function () {
                  var _v38 = _v34.action;
                  switch (_v38.ctor)
                  {case "Touches":
                     return function () {
                          var _v40 = _v34.mode;
                          switch (_v40.ctor)
                          {case "Drawing": return A3(addN,
                               A2(applyBrush,
                               _v38._0,
                               _v34.brush),
                               _v35.drawing,
                               _v35.history);
                             case "Erasing":
                             return A3(eraser,
                               A2(applyBrush,
                               _v38._0,
                               {_: {}
                               ,color: A4(Color.rgba,0,0,0,0.5)
                               ,size: 15}),
                               _v35.drawing,
                               _v35.history);}
                          return {ctor: "_Tuple2"
                                 ,_0: _v35.drawing
                                 ,_1: _v35.history};
                       }();
                     case "Undo":
                     return undo({ctor: "_Tuple2"
                                 ,_0: _v35.drawing
                                 ,_1: _v35.history});}
                  _E.Case($moduleName,
                  "between lines 244 and 250");
               }(),
               drawing$ = $._0,
               history$ = $._1;
               return _U.replace([["drawing"
                                  ,drawing$]
                                 ,["history",history$]],
               _v35);
            }();
         }();
      }();
   });
   var canvasState = A3(Signal.foldp,
   stepCanvas,
   defaultCanvas,
   input);
   var main = A2(Signal._op["~"],
   A2(Signal._op["<~"],
   display,
   Window.dimensions),
   A2(Signal._op["<~"],
   function ($) {
      return Dict.values(function (_) {
         return _.drawing;
      }($));
   },
   canvasState));
   _elm.Canvas.values = {_op: _op
                        ,point: point
                        ,pointToTuple: pointToTuple
                        ,line: line
                        ,defaultCanvas: defaultCanvas
                        ,portToBrush: portToBrush
                        ,portToMode: portToMode
                        ,actions: actions
                        ,input: input
                        ,linesIntersection: linesIntersection
                        ,isIntersect: isIntersect
                        ,toSegments: toSegments
                        ,isStrokesIntersect: isStrokesIntersect
                        ,isLineStrokeIntersect: isLineStrokeIntersect
                        ,strokesCrossed: strokesCrossed
                        ,undo: undo
                        ,applyBrush: applyBrush
                        ,addN: addN
                        ,add1: add1
                        ,removeEraser: removeEraser
                        ,eraser: eraser
                        ,stepCanvas: stepCanvas
                        ,canvasState: canvasState
                        ,thickLine: thickLine
                        ,dot: dot
                        ,display: display
                        ,main: main
                        ,Undo: Undo
                        ,ZoomIn: ZoomIn
                        ,ZoomOut: ZoomOut
                        ,Touches: Touches
                        ,Drawing: Drawing
                        ,Erasing: Erasing
                        ,Viewing: Viewing
                        ,Erased: Erased
                        ,Drew: Drew
                        ,Input: Input
                        ,Canvas: Canvas
                        ,Brush: Brush
                        ,Brushed: Brushed
                        ,Stroke: Stroke
                        ,Point: Point
                        ,Line: Line};
   return _elm.Canvas.values;
};