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
   var minScale = F2(function (_v0,
   _v1) {
      return function () {
         switch (_v1.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v0.ctor)
                 {case "_Tuple2":
                    return A2(Basics.min,
                      _v0._0 / _v1._0,
                      _v0._1 / _v1._1);}
                 _E.Case($moduleName,
                 "on line 340, column 3 to 27");
              }();}
         _E.Case($moduleName,
         "on line 340, column 3 to 27");
      }();
   });
   var dot = F2(function (pos,
   brush) {
      return Graphics.Collage.move(pos)(A2(Graphics.Collage.filled,
      brush.color,
      Graphics.Collage.circle(brush.size / 2)));
   });
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
   var scaleTouches = F3(function (_v8,
   zoom,
   t) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2":
            return _U.replace([["x"
                               ,Basics.round(Basics.toFloat(_v8._1 + t.x) / zoom)]
                              ,["y"
                               ,Basics.round(Basics.toFloat(_v8._0 + t.y) / zoom)]],
              t);}
         _E.Case($moduleName,
         "between lines 270 and 271");
      }();
   });
   var stepMove = F5(function (ts,
   _v12,
   _v13,
   lastMove,
   _v14) {
      return function () {
         switch (_v14.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v13.ctor)
                 {case "_Tuple2":
                    return function () {
                         switch (_v12.ctor)
                         {case "_Tuple2":
                            return function () {
                                 switch (ts.ctor)
                                 {case "[]":
                                    return {ctor: "_Tuple2"
                                           ,_0: Maybe.Nothing
                                           ,_1: {ctor: "_Tuple2"
                                                ,_0: _v14._0
                                                ,_1: _v14._1}};}
                                 return function () {
                                    var t = List.head(ts);
                                    return function () {
                                       switch (lastMove.ctor)
                                       {case "Just":
                                          switch (lastMove._0.ctor)
                                            {case "_Tuple2":
                                               return function () {
                                                    var left$ = _v14._1 + (lastMove._0._0 - t.x);
                                                    var top$ = _v14._0 + (lastMove._0._1 - t.y);
                                                    return {ctor: "_Tuple2"
                                                           ,_0: Maybe.Just({ctor: "_Tuple2"
                                                                           ,_0: t.x
                                                                           ,_1: t.y})
                                                           ,_1: {ctor: "_Tuple2"
                                                                ,_0: top$
                                                                ,_1: left$}};
                                                 }();}
                                            break;
                                          case "Nothing":
                                          return {ctor: "_Tuple2"
                                                 ,_0: Maybe.Just({ctor: "_Tuple2"
                                                                 ,_0: t.x
                                                                 ,_1: t.y})
                                                 ,_1: {ctor: "_Tuple2"
                                                      ,_0: _v14._0
                                                      ,_1: _v14._1}};}
                                       _E.Case($moduleName,
                                       "between lines 259 and 265");
                                    }();
                                 }();
                              }();}
                         _E.Case($moduleName,
                         "between lines 255 and 265");
                      }();}
                 _E.Case($moduleName,
                 "between lines 255 and 265");
              }();}
         _E.Case($moduleName,
         "between lines 255 and 265");
      }();
   });
   var stepScale = F3(function (factor,
   s,
   topLeft) {
      return {ctor: "_Tuple2"
             ,_0: s
             ,_1: topLeft};
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
                                    ,_0: 10000
                                    ,_1: 7000}
                       ,drawing: Dict.empty
                       ,history: Dict.empty
                       ,lastMove: Maybe.Nothing
                       ,topLeft: {ctor: "_Tuple2"
                                 ,_0: 500
                                 ,_1: 500}
                       ,zoom: 1};
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
   var display = F2(function (_v30,
   _v31) {
      return function () {
         return function () {
            switch (_v30.ctor)
            {case "_Tuple2":
               return function () {
                    var flipVert = function (_v36) {
                       return function () {
                          switch (_v36.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: _v36._0
                                    ,_1: 0 - _v36._1};}
                          _E.Case($moduleName,
                          "on line 329, column 24 to 29");
                       }();
                    };
                    var strokeOrDot = function (p) {
                       return _U.cmp(List.length(p.points),
                       1) > 0 ? Graphics.Collage.traced(thickLine(p.brush))(A2(List.map,
                       function ($) {
                          return flipVert(pointToTuple($));
                       },
                       p.points)) : A2(dot,
                       flipVert(pointToTuple(List.head(p.points))),
                       p.brush);
                    };
                    var $float = function (_v40) {
                       return function () {
                          switch (_v40.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: Basics.toFloat(_v40._0)
                                    ,_1: Basics.toFloat(_v40._1)};}
                          _E.Case($moduleName,
                          "on line 326, column 21 to 41");
                       }();
                    };
                    var $ = $float(_v31.topLeft),
                    top = $._0,
                    left = $._1;
                    var $ = $float({ctor: "_Tuple2"
                                   ,_0: _v30._0
                                   ,_1: _v30._1}),
                    w$ = $._0,
                    h$ = $._1;
                    var paths = Dict.values(_v31.drawing);
                    var forms = A2(List.map,
                    strokeOrDot,
                    paths);
                    return A3(Graphics.Collage.collage,
                    _v30._0,
                    _v30._1,
                    _L.fromArray([A2(Graphics.Collage.move,
                    flipVert({ctor: "_Tuple2"
                             ,_0: (0 - w$) * _v31.zoom / 2 - left
                             ,_1: (0 - h$) * _v31.zoom / 2 - top}),
                    Graphics.Collage.scale(_v31.zoom)(Graphics.Collage.group(forms)))]));
                 }();}
            _E.Case($moduleName,
            "between lines 324 and 335");
         }();
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
                      ,_0: A2(point,t.x,t.y)
                      ,_1: vs.points}]],
         vs),
         d);
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
   var Canvas = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,dimensions: c
             ,drawing: a
             ,history: b
             ,lastMove: f
             ,topLeft: e
             ,zoom: d};
   });
   var Input = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,action: b
             ,brush: c
             ,canvasDims: d
             ,mode: a
             ,windowDims: e};
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
                  var _v45 = A2(Dict.get,
                  lastId,
                  h);
                  switch (_v45.ctor)
                  {case "Just":
                     switch (_v45._0.ctor)
                       {case "Drew":
                          return {ctor: "_Tuple2"
                                 ,_0: A2(Dict.remove,
                                 _v45._0._0,
                                 d)
                                 ,_1: A2(Dict.remove,
                                 _v45._0._0,
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
                                 _v45._0._0)
                                 ,_1: A3(List.foldl,
                                 F2(function (s,h$) {
                                    return A3(Dict.insert,
                                    s.id,
                                    Drew(s.id),
                                    h$);
                                 }),
                                 A2(Dict.remove,lastId,h),
                                 _v45._0._0)};}
                       break;
                     case "Nothing":
                     return {ctor: "_Tuple2"
                            ,_0: Dict.empty
                            ,_1: Dict.empty};}
                  _E.Case($moduleName,
                  "between lines 183 and 187");
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
                  var _v50 = A2(Dict.get,
                  lastId,
                  h);
                  switch (_v50.ctor)
                  {case "Just":
                     switch (_v50._0.ctor)
                       {case "Erased":
                          return function () {
                               switch (_v50._0._0.ctor)
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
               var _v55 = A2(Dict.get,id,d);
               switch (_v55.ctor)
               {case "Just":
                  return function () {
                       var _raw = A3(Dict.getOrElse,
                       Erased(_L.fromArray([])),
                       id,
                       h),
                       $ = _raw.ctor === "Erased" ? _raw : _E.Case($moduleName,
                       "on line 243, column 38 to 69"),
                       vs = $._0;
                       var strokes = List.tail(List.reverse(Dict.values(d)));
                       var eraserSeg = A2(line,
                       A2(point,t.x,t.y),
                       List.head(_v55._0.points));
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
                              erased)
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
               "between lines 236 and 245");
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
            case "Erasing": return Erasing;
            case "Viewing": return Viewing;}
         _E.Case($moduleName,
         "between lines 95 and 98");
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
         "between lines 103 and 107");
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
   Window.dimensions),
   Window.dimensions);
   var stepCanvas = F2(function (_v59,
   _v60) {
      return function () {
         return function () {
            return function () {
               var $ = function () {
                  var _v63 = _v59.action;
                  switch (_v63.ctor)
                  {case "Touches":
                     return function () {
                          var _v65 = _v59.mode;
                          switch (_v65.ctor)
                          {case "Viewing":
                             return A5(stepMove,
                               _v63._0,
                               _v59.canvasDims,
                               _v59.windowDims,
                               _v60.lastMove,
                               _v60.topLeft);}
                          return {ctor: "_Tuple2"
                                 ,_0: _v60.lastMove
                                 ,_1: _v60.topLeft};
                       }();}
                  return {ctor: "_Tuple2"
                         ,_0: _v60.lastMove
                         ,_1: _v60.topLeft};
               }(),
               lastMove$ = $._0,
               topLeft$ = $._1;
               var $ = function () {
                  var _v66 = _v59.action;
                  switch (_v66.ctor)
                  {case "Touches":
                     return function () {
                          var ts = A2(List.map,
                          A2(scaleTouches,
                          _v60.topLeft,
                          _v60.zoom),
                          _v66._0);
                          return function () {
                             var _v68 = _v59.mode;
                             switch (_v68.ctor)
                             {case "Drawing":
                                return {ctor: "_Tuple2"
                                       ,_0: A2(addN,
                                       A2(applyBrush,ts,_v59.brush),
                                       _v60.drawing)
                                       ,_1: A2(recordDrew,
                                       ts,
                                       _v60.history)};
                                case "Erasing":
                                return A3(eraser,
                                  A2(applyBrush,
                                  ts,
                                  {_: {}
                                  ,color: A4(Color.rgba,0,0,0,0.1)
                                  ,size: 15}),
                                  _v60.drawing,
                                  _v60.history);}
                             return {ctor: "_Tuple2"
                                    ,_0: _v60.drawing
                                    ,_1: _v60.history};
                          }();
                       }();
                     case "Undo": return A2(undo,
                       _v60.drawing,
                       _v60.history);}
                  return {ctor: "_Tuple2"
                         ,_0: _v60.drawing
                         ,_1: _v60.history};
               }(),
               drawing$ = $._0,
               history$ = $._1;
               return _U.replace([["drawing"
                                  ,drawing$]
                                 ,["history",history$]
                                 ,["topLeft",topLeft$]
                                 ,["lastMove",lastMove$]],
               _v60);
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
   canvasState);
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
                        ,addN: addN
                        ,add1: add1
                        ,removeEraser: removeEraser
                        ,eraser: eraser
                        ,stepScale: stepScale
                        ,stepMove: stepMove
                        ,scaleTouches: scaleTouches
                        ,stepCanvas: stepCanvas
                        ,canvasState: canvasState
                        ,thickLine: thickLine
                        ,dot: dot
                        ,display: display
                        ,minScale: minScale
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