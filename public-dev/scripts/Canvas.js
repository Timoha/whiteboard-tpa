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
   var Debug = Elm.Debug.make(_elm);
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
   var addHistoryFirst = F3(function (ts,
   v,
   h) {
      return function () {
         switch (ts.ctor)
         {case "[]": return h;}
         return A3(Dict.insert,
         Basics.abs(List.head(ts).id),
         v,
         h);
      }();
   });
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
   var ccw = F3(function (a,b,c) {
      return _U.cmp((c.y - a.y) * (b.x - a.x),
      (b.y - a.y) * (c.x - a.x)) > 0;
   });
   var isIntersect = F2(function (l1,
   l2) {
      return Basics.not(_U.eq(A3(ccw,
      l1.p1,
      l2.p1,
      l2.p2),
      A3(ccw,
      l1.p2,
      l2.p1,
      l2.p2)) || _U.eq(A3(ccw,
      l1.p1,
      l1.p2,
      l2.p1),
      A3(ccw,l1.p1,l1.p2,l2.p2)));
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
   var actionPort = Native.Ports.portIn("actionPort",
   Native.Ports.incomingSignal(function (v) {
      return typeof v === "string" || typeof v === "object" && v instanceof String ? v : _E.raise("invalid input, expecting JSString but got " + v);
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
         var startNew = F2(function (p1,
         ls) {
            return {ctor: "::"
                   ,_0: A2(line,p1,p1)
                   ,_1: ls};
         });
         var connectPrev = F2(function (p2,
         ps) {
            return function () {
               switch (ps.ctor)
               {case "[]": return A2(startNew,
                    p2,
                    _L.fromArray([]));}
               return {ctor: "::"
                      ,_0: A2(line,
                      List.head(ps).p1,
                      p2)
                      ,_1: List.tail(ps)};
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
         _L.fromArray([]),
         ps));
      }();
   };
   var isStrokesIntersect = F2(function (s1,
   s2) {
      return _U.cmp(List.length(s1.points),
      1) > 0 || _U.cmp(List.length(s2.points),
      1) > 0 ? function () {
         var segs2 = toSegments(s2.points);
         var segs1 = toSegments(s1.points);
         return A2(List.any,
         function (x) {
            return A2(List.any,
            isIntersect(x),
            segs2);
         },
         segs1);
      }() : false;
   });
   var strokesCrossed = F2(function (s,
   ss) {
      return A2(List.filter,
      isStrokesIntersect(s),
      ss);
   });
   var isLineStrokeIntersect = F2(function (l,
   s) {
      return _U.cmp(List.length(s.points),
      1) > 0 ? List.any(isIntersect(l))(toSegments(s.points)) : false;
   });
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
   var display = F2(function (_v2,
   paths) {
      return function () {
         switch (_v2.ctor)
         {case "_Tuple2":
            return function () {
                 var strokeOrDot = function (path) {
                    return function () {
                       var _v6 = _U.cmp(List.length(path.points),
                       1) > 0;
                       switch (_v6)
                       {case false: return A2(dot,
                            List.head(path.points),
                            path.brush);
                          case true:
                          return Graphics.Collage.traced(thickLine(path.brush))(A2(List.map,
                            pointToTuple,
                            path.points));}
                       _E.Case($moduleName,
                       "between lines 297 and 300");
                    }();
                 };
                 var forms = A2(List.map,
                 strokeOrDot,
                 paths);
                 var $float = function (_v7) {
                    return function () {
                       switch (_v7.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: Basics.toFloat(_v7._0)
                                 ,_1: Basics.toFloat(0 - _v7._1)};}
                       _E.Case($moduleName,
                       "on line 295, column 20 to 41");
                    }();
                 };
                 return A3(Graphics.Collage.collage,
                 _v2._0,
                 _v2._1,
                 _L.fromArray([A2(Graphics.Collage.move,
                 $float({ctor: "_Tuple2"
                        ,_0: (0 - _v2._0) / 2 | 0
                        ,_1: (0 - _v2._1) / 2 | 0}),
                 Graphics.Collage.group(forms))]));
              }();}
         _E.Case($moduleName,
         "between lines 294 and 301");
      }();
   });
   var point = F2(function (x,y) {
      return {_: {},x: x,y: y};
   });
   var add1 = F2(function (t,d) {
      return function () {
         var id = Basics.abs(t.id);
         var vs = A3(Dict.getOrElse,
         {_: {}
         ,brush: t.brush
         ,id: id
         ,points: _L.fromArray([])},
         id,
         d);
         return A3(Dict.insert,
         id,
         _U.replace([["points"
                     ,{ctor: "::"
                      ,_0: A2(point,t.x,0 - t.y)
                      ,_1: vs.points}]],
         vs),
         d);
      }();
   });
   var addHead = F2(function (ts,
   d) {
      return function () {
         switch (ts.ctor)
         {case "[]": return d;}
         return A2(add1,List.head(ts),d);
      }();
   });
   var addN = F2(function (ts,d) {
      return A3(List.foldl,
      add1,
      d,
      ts);
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
   var recordDrew = F2(function (ts,
   h) {
      return A3(List.foldl,
      function (t) {
         return A2(Dict.insert,
         Basics.abs(t.id),
         Drew(Basics.abs(t.id)));
      },
      h,
      ts);
   });
   var Erased = function (a) {
      return {ctor: "Erased"
             ,_0: a};
   };
   var undo = F2(function (d,h) {
      return function () {
         var ids = Dict.keys(h);
         return function () {
            switch (ids.ctor)
            {case "[]":
               return {ctor: "_Tuple2"
                      ,_0: d
                      ,_1: h};}
            return function () {
               var lastId = List.maximum(ids);
               return function () {
                  var _v13 = A2(Dict.get,
                  lastId,
                  h);
                  switch (_v13.ctor)
                  {case "Just":
                     switch (_v13._0.ctor)
                       {case "Drew":
                          return {ctor: "_Tuple2"
                                 ,_0: A2(Dict.remove,
                                 _v13._0._0,
                                 d)
                                 ,_1: A2(Dict.remove,
                                 _v13._0._0,
                                 h)};
                          case "Erased":
                          return {ctor: "_Tuple2"
                                 ,_0: A3(List.foldl,
                                 F2(function (s,d) {
                                    return A3(Dict.insert,
                                    s.id,
                                    s,
                                    d);
                                 }),
                                 d,
                                 _v13._0._0)
                                 ,_1: A3(List.foldl,
                                 F2(function (s,h$) {
                                    return A3(Dict.insert,
                                    s.id,
                                    Drew(s.id),
                                    h$);
                                 }),
                                 A2(Dict.remove,lastId,h),
                                 _v13._0._0)};}
                       break;
                     case "Nothing":
                     return {ctor: "_Tuple2"
                            ,_0: Dict.empty
                            ,_1: Dict.empty};}
                  _E.Case($moduleName,
                  "between lines 177 and 181");
               }();
            }();
         }();
      }();
   });
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
               var lastId = List.maximum(ids);
               return function () {
                  var _v18 = A2(Dict.get,
                  lastId,
                  h);
                  switch (_v18.ctor)
                  {case "Just":
                     switch (_v18._0.ctor)
                       {case "Erased":
                          return function () {
                               switch (_v18._0._0.ctor)
                               {case "[]":
                                  return {ctor: "_Tuple2"
                                         ,_0: A2(Dict.remove,lastId,d)
                                         ,_1: A2(Dict.remove,lastId,h)};}
                               return {ctor: "_Tuple2"
                                      ,_0: A2(Dict.remove,lastId,d)
                                      ,_1: h};
                            }();}
                       break;}
                  return {ctor: "_Tuple2"
                         ,_0: d
                         ,_1: h};
               }();
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
            var t = List.head(ts);
            var id = Basics.abs(t.id);
            return function () {
               var _v23 = A2(Dict.get,id,d);
               switch (_v23.ctor)
               {case "Just":
                  return function () {
                       var _raw = function () {
                          var _v25 = A2(Dict.get,id,h);
                          switch (_v25.ctor)
                          {case "Just": return _v25._0;
                             case "Nothing":
                             return Erased(_L.fromArray([]));}
                          _E.Case($moduleName,
                          "between lines 248 and 251");
                       }(),
                       $ = _raw.ctor === "Erased" ? _raw : _E.Case($moduleName,
                       "between lines 248 and 251"),
                       vs = $._0;
                       var strokes = List.tail(List.reverse(Dict.values(d)));
                       var eraserSeg = A2(line,
                       A2(point,t.x,0 - t.y),
                       List.head(_v23._0.points));
                       var crossed = A2(List.filter,
                       isLineStrokeIntersect(eraserSeg),
                       strokes);
                       var erased = List.isEmpty(crossed) ? _L.fromArray([]) : _L.fromArray([List.head(crossed)]);
                       return {ctor: "_Tuple2"
                              ,_0: A3(List.foldl,
                              function (s) {
                                 return Dict.remove(s.id);
                              },
                              A2(add1,t,d),
                              crossed)
                              ,_1: A3(Dict.insert,
                              id,
                              Erased(_L.append(erased,vs)),
                              h)};
                    }();
                  case "Nothing":
                  return {ctor: "_Tuple2"
                         ,_0: A2(add1,t,d)
                         ,_1: A3(Dict.insert,
                         id,
                         Erased(_L.fromArray([])),
                         h)};}
               _E.Case($moduleName,
               "between lines 241 and 252");
            }();
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
         "between lines 92 and 94");
      }();
   };
   var Touches = function (a) {
      return {ctor: "Touches"
             ,_0: a};
   };
   var None = {ctor: "None"};
   var ZoomOut = {ctor: "ZoomOut"};
   var ZoomIn = {ctor: "ZoomIn"};
   var Undo = {ctor: "Undo"};
   var portToAction = function (s) {
      return function () {
         switch (s)
         {case "None": return None;
            case "Undo": return Undo;
            case "ZoomIn": return ZoomIn;
            case "ZoomOut": return ZoomOut;}
         _E.Case($moduleName,
         "between lines 99 and 103");
      }();
   };
   var actions = Signal.merges(_L.fromArray([A2(Signal._op["<~"],
                                            Touches,
                                            Touch.touches)
                                            ,A2(Signal._op["<~"],
                                            portToAction,
                                            actionPort)]));
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
   var stepCanvas = F2(function (_v29,
   _v30) {
      return function () {
         return function () {
            return function () {
               var $ = function () {
                  var _v33 = _v29.action;
                  switch (_v33.ctor)
                  {case "None":
                     return {ctor: "_Tuple2"
                            ,_0: _v30.drawing
                            ,_1: _v30.history};
                     case "Touches":
                     return function () {
                          var _v35 = _v29.mode;
                          switch (_v35.ctor)
                          {case "Drawing":
                             return {ctor: "_Tuple2"
                                    ,_0: A2(addN,
                                    A2(applyBrush,
                                    _v33._0,
                                    _v29.brush),
                                    _v30.drawing)
                                    ,_1: A2(recordDrew,
                                    _v33._0,
                                    _v30.history)};
                             case "Erasing":
                             return A3(eraser,
                               A2(applyBrush,
                               _v33._0,
                               {_: {}
                               ,color: A4(Color.rgba,0,0,0,0.1)
                               ,size: 15}),
                               _v30.drawing,
                               _v30.history);}
                          return {ctor: "_Tuple2"
                                 ,_0: _v30.drawing
                                 ,_1: _v30.history};
                       }();
                     case "Undo": return A2(undo,
                       _v30.drawing,
                       _v30.history);}
                  _E.Case($moduleName,
                  "between lines 260 and 267");
               }(),
               drawing$ = $._0,
               history$ = $._1;
               return _U.replace([["drawing"
                                  ,drawing$]
                                 ,["history",history$]],
               _v30);
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
                        ,portToAction: portToAction
                        ,actions: actions
                        ,input: input
                        ,ccw: ccw
                        ,isIntersect: isIntersect
                        ,toSegments: toSegments
                        ,isStrokesIntersect: isStrokesIntersect
                        ,isLineStrokeIntersect: isLineStrokeIntersect
                        ,strokesCrossed: strokesCrossed
                        ,undo: undo
                        ,applyBrush: applyBrush
                        ,recordDrew: recordDrew
                        ,addHistoryFirst: addHistoryFirst
                        ,addHead: addHead
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
                        ,None: None
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