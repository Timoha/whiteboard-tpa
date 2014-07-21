Elm.Editor = Elm.Editor || {};
Elm.Editor.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   if (_elm.Editor.values)
   return _elm.Editor.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Editor";
   var Basics = Elm.Basics.make(_elm);
   var Canvas = Elm.Canvas.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Debug = Elm.Debug.make(_elm);
   var Dict = Elm.Dict.make(_elm);
   var Eraser = Elm.Eraser.make(_elm);
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
   var Navigation = Elm.Navigation.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var Touch = Elm.Touch.make(_elm);
   var Window = Elm.Window.make(_elm);
   var _op = {};
   var display = F2(function (_v0,
   _v1) {
      return function () {
         return function () {
            switch (_v0.ctor)
            {case "_Tuple2":
               return function () {
                    var toAbsPos = F2(function (_v6,
                    _v7) {
                       return function () {
                          switch (_v7.ctor)
                          {case "_Tuple2":
                             return function () {
                                  switch (_v6.ctor)
                                  {case "_Tuple2":
                                     return {ctor: "_Tuple2"
                                            ,_0: _v7._0 - _v6._0 * _v1.zoom
                                            ,_1: _v7._1 + _v6._1 * _v1.zoom};}
                                  _E.Case($moduleName,
                                  "on line 173, column 33 to 61");
                               }();}
                          _E.Case($moduleName,
                          "on line 173, column 33 to 61");
                       }();
                    });
                    var toZero = F2(function (zoom,
                    _v14) {
                       return function () {
                          switch (_v14.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: (0 - _v14._0) * zoom / 2
                                    ,_1: _v14._1 * zoom / 2};}
                          _E.Case($moduleName,
                          "on line 172, column 27 to 54");
                       }();
                    });
                    var paths = Dict.values(_v1.drawing);
                    var flipVert = function (_v18) {
                       return function () {
                          switch (_v18.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: _v18._0
                                    ,_1: 0 - _v18._1};}
                          _E.Case($moduleName,
                          "on line 165, column 24 to 29");
                       }();
                    };
                    var strokeOrDot = function (p) {
                       return _U.cmp(List.length(p.points),
                       1) > 0 ? Graphics.Collage.traced(Canvas.thickLine(p.brush))(A2(List.map,
                       function ($) {
                          return flipVert(Canvas.pointToTuple($));
                       },
                       p.points)) : A2(Canvas.dot,
                       flipVert(Canvas.pointToTuple(List.head(p.points))),
                       p.brush);
                    };
                    var forms = A2(List.map,
                    strokeOrDot,
                    paths);
                    var $float = function (_v22) {
                       return function () {
                          switch (_v22.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: Basics.toFloat(_v22._0)
                                    ,_1: Basics.toFloat(_v22._1)};}
                          _E.Case($moduleName,
                          "on line 164, column 21 to 41");
                       }();
                    };
                    var pos = toAbsPos(_v1.absPos)(A2(toZero,
                    _v1.zoom,
                    $float({ctor: "_Tuple2"
                           ,_0: _v0._0
                           ,_1: _v0._1})));
                    return A3(Graphics.Collage.collage,
                    _v0._0,
                    _v0._1,
                    _L.fromArray([Graphics.Collage.scale(_v1.zoom)(A2(Graphics.Collage.move,
                    pos,
                    Graphics.Collage.group(forms)))]));
                 }();}
            _E.Case($moduleName,
            "between lines 163 and 175");
         }();
      }();
   });
   var eraserBrush = {_: {}
                     ,color: Color.toRgb(A4(Color.rgba,
                     0,
                     0,
                     0,
                     0.1))
                     ,size: 15};
   var withinWindowDims = F2(function (ts,
   _v26) {
      return function () {
         switch (_v26.ctor)
         {case "_Tuple2":
            return function () {
                 var within = function (t) {
                    return _U.cmp(t.x,
                    0) > 0 && (_U.cmp(t.x,
                    _v26._0) < 0 && (_U.cmp(t.y,
                    0) > 0 && _U.cmp(t.y,
                    _v26._1) < 0));
                 };
                 return A2(List.filter,
                 within,
                 ts);
              }();}
         _E.Case($moduleName,
         "between lines 97 and 98");
      }();
   });
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
      return typeof v === "object" && "size" in v && "color" in v ? {_: {}
                                                                    ,size: typeof v.size === "number" ? v.size : _E.raise("invalid input, expecting JSNumber but got " + v.size)
                                                                    ,color: typeof v.color === "object" && "red" in v.color && "green" in v.color && "blue" in v.color && "alpha" in v.color ? {_: {}
                                                                                                                                                                                               ,red: typeof v.color.red === "number" ? v.color.red : _E.raise("invalid input, expecting JSNumber but got " + v.color.red)
                                                                                                                                                                                               ,green: typeof v.color.green === "number" ? v.color.green : _E.raise("invalid input, expecting JSNumber but got " + v.color.green)
                                                                                                                                                                                               ,blue: typeof v.color.blue === "number" ? v.color.blue : _E.raise("invalid input, expecting JSNumber but got " + v.color.blue)
                                                                                                                                                                                               ,alpha: typeof v.color.alpha === "number" ? v.color.alpha : _E.raise("invalid input, expecting JSNumber but got " + v.color.alpha)} : _E.raise("invalid input, expecting JSObject [\"red\",\"green\",\"blue\",\"alpha\"] but got " + v.color)} : _E.raise("invalid input, expecting JSObject [\"size\",\"color\"] but got " + v);
   }));
   var defaultCanvas = {_: {}
                       ,absPos: {ctor: "_Tuple2"
                                ,_0: 0
                                ,_1: 0}
                       ,dimensions: {ctor: "_Tuple2"
                                    ,_0: 2000
                                    ,_1: 1300}
                       ,drawing: Dict.empty
                       ,history: Dict.empty
                       ,lastMove: Maybe.Nothing
                       ,maxZoom: Math.pow(2,4)
                       ,minZoom: 1
                       ,windowDims: {ctor: "_Tuple2"
                                    ,_0: 0
                                    ,_1: 0}
                       ,zoom: 1
                       ,zoomOffset: {ctor: "_Tuple2"
                                    ,_0: 0
                                    ,_1: 0}};
   var getCanvas = function (c) {
      return {_: {}
             ,dimensions: c.dimensions
             ,drawing: c.drawing
             ,history: c.history};
   };
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
   var Viewing = {ctor: "Viewing"};
   var getDrawing = F2(function (_v30,
   _v31) {
      return function () {
         return function () {
            return function () {
               var _v34 = _v30.mode;
               switch (_v34.ctor)
               {case "Viewing":
                  return Maybe.Just({_: {}
                                    ,absPos: _v31.absPos
                                    ,dimensions: _v31.dimensions
                                    ,drawing: Dict.values(_v31.drawing)
                                    ,windowDims: _v31.windowDims
                                    ,zoom: _v31.zoom
                                    ,zoomOffset: _v31.zoomOffset});}
               return Maybe.Nothing;
            }();
         }();
      }();
   });
   var Erasing = {ctor: "Erasing"};
   var Drawing = {ctor: "Drawing"};
   var portToMode = function (s) {
      return function () {
         switch (s)
         {case "Drawing": return Drawing;
            case "Erasing": return Erasing;
            case "Viewing": return Viewing;}
         _E.Case($moduleName,
         "between lines 66 and 69");
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
         "between lines 74 and 78");
      }();
   };
   var actions = Signal.merges(_L.fromArray([A2(Signal._op["<~"],
                                            Touches,
                                            A2(Signal._op["~"],
                                            A2(Signal._op["<~"],
                                            withinWindowDims,
                                            Touch.touches),
                                            Window.dimensions))
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
   brushPort),
   Signal.constant({ctor: "_Tuple2"
                   ,_0: 10000
                   ,_1: 7000})),
   Window.dimensions);
   var stepCanvas = F2(function (_v37,
   _v38) {
      return function () {
         return function () {
            return function () {
               var $float = function (_v41) {
                  return function () {
                     switch (_v41.ctor)
                     {case "_Tuple2":
                        return {ctor: "_Tuple2"
                               ,_0: Basics.toFloat(_v41._0)
                               ,_1: Basics.toFloat(_v41._1)};}
                     _E.Case($moduleName,
                     "on line 125, column 21 to 41");
                  }();
               };
               var zcanvas = _U.replace([["windowDims"
                                         ,_v37.windowDims]
                                        ,["minZoom"
                                         ,A2(Basics.max,
                                         1,
                                         A2(Navigation.minScale,
                                         $float(_v37.windowDims),
                                         $float(_v38.dimensions)))]],
               _v38);
               var c = getCanvas(zcanvas);
               var canvas$ = function () {
                  var _v45 = _v37.action;
                  switch (_v45.ctor)
                  {case "Touches":
                     return function () {
                          var ts$ = A2(List.map,
                          A3(Navigation.scaleTouches,
                          _v38.absPos,
                          _v38.zoomOffset,
                          _v38.zoom),
                          _v45._0);
                          return function () {
                             var _v47 = _v37.mode;
                             switch (_v47.ctor)
                             {case "Drawing":
                                return _U.replace([["drawing"
                                                   ,A2(Canvas.addN,
                                                   A2(Canvas.applyBrush,
                                                   ts$,
                                                   _v37.brush),
                                                   _v38.drawing)]
                                                  ,["history"
                                                   ,A2(Canvas.recordDrew,
                                                   ts$,
                                                   _v38.history)]],
                                  c);
                                case "Erasing":
                                return A2(Eraser.stepEraser,
                                  A2(Canvas.applyBrush,
                                  ts$,
                                  eraserBrush),
                                  c);}
                             return c;
                          }();
                       }();
                     case "Undo":
                     return Canvas.stepUndo(c);}
                  return c;
               }();
               var zcanvas$ = Navigation.withinBounds(_v38.dimensions)(function () {
                  var _v48 = _v37.action;
                  switch (_v48.ctor)
                  {case "Touches":
                     return function () {
                          var _v50 = _v37.mode;
                          switch (_v50.ctor)
                          {case "Viewing":
                             return A2(Navigation.stepMove,
                               _v48._0,
                               zcanvas);}
                          return zcanvas;
                       }();
                     case "ZoomIn":
                     return A2(Navigation.stepZoom,
                       2,
                       zcanvas);
                     case "ZoomOut":
                     return A2(Navigation.stepZoom,
                       1 / 2,
                       zcanvas);}
                  return zcanvas;
               }());
               return _U.replace([["drawing"
                                  ,canvas$.drawing]
                                 ,["history",canvas$.history]],
               zcanvas$);
            }();
         }();
      }();
   });
   var canvasState = A3(Signal.foldp,
   stepCanvas,
   defaultCanvas,
   input);
   var canvasOut = Native.Ports.portOut("canvasOut",
   Native.Ports.outgoingSignal(function (v) {
      return v.ctor === "Nothing" ? null : {absPos: [v._0.absPos._0
                                                    ,v._0.absPos._1]
                                           ,zoomOffset: [v._0.zoomOffset._0
                                                        ,v._0.zoomOffset._1]
                                           ,zoom: v._0.zoom
                                           ,dimensions: [v._0.dimensions._0
                                                        ,v._0.dimensions._1]
                                           ,windowDims: [v._0.windowDims._0
                                                        ,v._0.windowDims._1]
                                           ,drawing: _L.toArray(v._0.drawing).map(function (v) {
                                              return {points: _L.toArray(v.points).map(function (v) {
                                                        return {x: v.x,y: v.y};
                                                     })
                                                     ,brush: {size: v.brush.size
                                                             ,color: {red: v.brush.color.red
                                                                     ,green: v.brush.color.green
                                                                     ,blue: v.brush.color.blue
                                                                     ,alpha: v.brush.color.alpha}}};
                                           })};
   }),
   A2(Signal._op["~"],
   A2(Signal._op["<~"],
   getDrawing,
   input),
   canvasState));
   var main = A2(Signal._op["~"],
   A2(Signal._op["<~"],
   display,
   Window.dimensions),
   canvasState);
   _elm.Editor.values = {_op: _op
                        ,getCanvas: getCanvas
                        ,defaultCanvas: defaultCanvas
                        ,portToMode: portToMode
                        ,portToAction: portToAction
                        ,actions: actions
                        ,input: input
                        ,withinWindowDims: withinWindowDims
                        ,getDrawing: getDrawing
                        ,eraserBrush: eraserBrush
                        ,stepCanvas: stepCanvas
                        ,canvasState: canvasState
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
                        ,Input: Input};
   return _elm.Editor.values;
};Elm.Eraser = Elm.Eraser || {};
Elm.Eraser.make = function (_elm) {
   "use strict";
   _elm.Eraser = _elm.Eraser || {};
   if (_elm.Eraser.values)
   return _elm.Eraser.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Eraser";
   var Basics = Elm.Basics.make(_elm);
   var Canvas = Elm.Canvas.make(_elm);
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
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var Touch = Elm.Touch.make(_elm);
   var _op = {};
   var removeEraser = function (_v0) {
      return function () {
         return function () {
            var ids = Dict.keys(_v0.history);
            var $ = List.isEmpty(ids) ? {ctor: "_Tuple2"
                                        ,_0: Dict.empty
                                        ,_1: Dict.empty} : function () {
               var lastId = List.maximum(ids);
               var removeLast = Dict.remove(lastId);
               return function () {
                  var _v2 = A2(Dict.get,
                  lastId,
                  _v0.history);
                  switch (_v2.ctor)
                  {case "Just":
                     switch (_v2._0.ctor)
                       {case "Erased":
                          return List.isEmpty(_v2._0._0) ? {ctor: "_Tuple2"
                                                           ,_0: removeLast(_v0.drawing)
                                                           ,_1: removeLast(_v0.history)} : {ctor: "_Tuple2"
                                                                                           ,_0: removeLast(_v0.drawing)
                                                                                           ,_1: _v0.history};}
                       break;}
                  return {ctor: "_Tuple2"
                         ,_0: _v0.drawing
                         ,_1: _v0.history};
               }();
            }(),
            d = $._0,
            h = $._1;
            return _U.replace([["drawing",d]
                              ,["history",h]],
            _v0);
         }();
      }();
   };
   var toSegments = function (ps) {
      return function () {
         var startNew = F2(function (p1,
         ls) {
            return {ctor: "::"
                   ,_0: A2(Canvas.line,p1,p1)
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
                      ,_0: A2(Canvas.line,
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
   var isLineStrokeIntersect = F2(function (l,
   s) {
      return _U.cmp(List.length(s.points),
      1) > 0 ? List.any(isIntersect(l))(toSegments(s.points)) : false;
   });
   var stepEraser = F2(function (ts,
   _v6) {
      return function () {
         return List.isEmpty(ts) ? removeEraser(_v6) : function () {
            var t = List.head(ts);
            var id = Basics.abs(t.id);
            var drawing$ = A2(Canvas.stepStroke,
            t,
            _v6.drawing);
            return function () {
               var _v8 = A2(Dict.get,
               id,
               _v6.drawing);
               switch (_v8.ctor)
               {case "Just":
                  return function () {
                       var es = function () {
                          var _v10 = A2(Dict.get,
                          id,
                          _v6.history);
                          switch (_v10.ctor)
                          {case "Just":
                             switch (_v10._0.ctor)
                               {case "Erased":
                                  return _v10._0._0;}
                               break;}
                          return _L.fromArray([]);
                       }();
                       var strokes = List.tail(List.reverse(Dict.toList(_v6.drawing)));
                       var eraserSeg = A2(Canvas.line,
                       A2(Canvas.point,t.x,t.y),
                       List.head(_v8._0.points));
                       var crossed = A2(List.filter,
                       function (_v13) {
                          return function () {
                             switch (_v13.ctor)
                             {case "_Tuple2":
                                return A2(isLineStrokeIntersect,
                                  eraserSeg,
                                  _v13._1);}
                             _E.Case($moduleName,
                             "on line 74, column 50 to 83");
                          }();
                       },
                       strokes);
                       var erased = List.isEmpty(crossed) ? _L.fromArray([]) : _L.fromArray([List.head(crossed)]);
                       return _U.replace([["drawing"
                                          ,A3(List.foldl,
                                          function (_v17) {
                                             return function () {
                                                switch (_v17.ctor)
                                                {case "_Tuple2":
                                                   return Dict.remove(_v17._0);}
                                                _E.Case($moduleName,
                                                "on line 79, column 57 to 72");
                                             }();
                                          },
                                          drawing$,
                                          erased)]
                                         ,["history"
                                          ,A3(Dict.insert,
                                          id,
                                          Canvas.Erased(_L.append(erased,
                                          es)),
                                          _v6.history)]],
                       _v6);
                    }();
                  case "Nothing":
                  return _U.replace([["drawing"
                                     ,drawing$]
                                    ,["history"
                                     ,A3(Dict.insert,
                                     id,
                                     Canvas.Erased(_L.fromArray([])),
                                     _v6.history)]],
                    _v6);}
               _E.Case($moduleName,
               "between lines 70 and 82");
            }();
         }();
      }();
   });
   _elm.Eraser.values = {_op: _op
                        ,ccw: ccw
                        ,isIntersect: isIntersect
                        ,toSegments: toSegments
                        ,isLineStrokeIntersect: isLineStrokeIntersect
                        ,removeEraser: removeEraser
                        ,stepEraser: stepEraser};
   return _elm.Eraser.values;
};Elm.Canvas = Elm.Canvas || {};
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
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var Touch = Elm.Touch.make(_elm);
   var _op = {};
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
   var line = F2(function (p1,p2) {
      return {_: {},p1: p1,p2: p2};
   });
   var pointToTuple = function (p) {
      return {ctor: "_Tuple2"
             ,_0: Basics.toFloat(p.x)
             ,_1: Basics.toFloat(p.y)};
   };
   var point = F2(function (x,y) {
      return {_: {},x: x,y: y};
   });
   var stepStroke = F2(function (t,
   d) {
      return function () {
         var id = Basics.abs(t.id);
         var vs = A3(Dict.getOrElse,
         {_: {}
         ,brush: t.brush
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
      stepStroke,
      d,
      ts);
   });
   var toRgbaColor = function (_v0) {
      return function () {
         return A4(Color.rgba,
         _v0.red,
         _v0.green,
         _v0.blue,
         _v0.alpha);
      }();
   };
   var thickLine = function (brush) {
      return _U.replace([["color"
                         ,toRgbaColor(brush.color)]
                        ,["width",brush.size]
                        ,["join"
                         ,Graphics.Collage.Smooth]
                        ,["cap"
                         ,Graphics.Collage.Round]],
      Graphics.Collage.defaultLine);
   };
   var dot = F2(function (pos,
   brush) {
      return Graphics.Collage.move(pos)(A2(Graphics.Collage.filled,
      toRgbaColor(brush.color),
      Graphics.Collage.circle(brush.size / 2)));
   });
   var Line = F2(function (a,b) {
      return {_: {},p1: a,p2: b};
   });
   var Point = F2(function (a,b) {
      return {_: {},x: a,y: b};
   });
   var Stroke = F2(function (a,b) {
      return {_: {}
             ,brush: b
             ,points: a};
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
   var stepUndo = function (_v2) {
      return function () {
         return function () {
            var ids = Dict.keys(_v2.history);
            return List.isEmpty(ids) ? _v2 : function () {
               var lastId = List.maximum(ids);
               var $ = function () {
                  var _v4 = A2(Dict.get,
                  lastId,
                  _v2.history);
                  switch (_v4.ctor)
                  {case "Just":
                     switch (_v4._0.ctor)
                       {case "Drew":
                          return {ctor: "_Tuple2"
                                 ,_0: A2(Dict.remove,
                                 _v4._0._0,
                                 _v2.drawing)
                                 ,_1: A2(Dict.remove,
                                 _v4._0._0,
                                 _v2.history)};
                          case "Erased":
                          return {ctor: "_Tuple2"
                                 ,_0: A3(List.foldl,
                                 F2(function (_v8,d) {
                                    return function () {
                                       switch (_v8.ctor)
                                       {case "_Tuple2":
                                          return A3(Dict.insert,
                                            _v8._0,
                                            _v8._1,
                                            d);}
                                       _E.Case($moduleName,
                                       "on line 61, column 55 to 73");
                                    }();
                                 }),
                                 _v2.drawing,
                                 _v4._0._0)
                                 ,_1: A3(List.foldl,
                                 F2(function (_v12,h) {
                                    return function () {
                                       switch (_v12.ctor)
                                       {case "_Tuple2":
                                          return A3(Dict.insert,
                                            _v12._0,
                                            Drew(_v12._0),
                                            h);}
                                       _E.Case($moduleName,
                                       "on line 62, column 55 to 81");
                                    }();
                                 }),
                                 A2(Dict.remove,
                                 lastId,
                                 _v2.history),
                                 _v4._0._0)};}
                       break;
                     case "Nothing":
                     return {ctor: "_Tuple2"
                            ,_0: Dict.empty
                            ,_1: Dict.empty};}
                  _E.Case($moduleName,
                  "between lines 58 and 64");
               }(),
               d = $._0,
               h = $._1;
               return _U.replace([["drawing",d]
                                 ,["history",h]],
               _v2);
            }();
         }();
      }();
   };
   var Canvas = F3(function (a,
   b,
   c) {
      return {_: {}
             ,dimensions: c
             ,drawing: a
             ,history: b};
   });
   _elm.Canvas.values = {_op: _op
                        ,toRgbaColor: toRgbaColor
                        ,point: point
                        ,pointToTuple: pointToTuple
                        ,line: line
                        ,recordDrew: recordDrew
                        ,stepUndo: stepUndo
                        ,applyBrush: applyBrush
                        ,addN: addN
                        ,stepStroke: stepStroke
                        ,thickLine: thickLine
                        ,dot: dot
                        ,Erased: Erased
                        ,Drew: Drew
                        ,Canvas: Canvas
                        ,Brush: Brush
                        ,Brushed: Brushed
                        ,Stroke: Stroke
                        ,Point: Point
                        ,Line: Line};
   return _elm.Canvas.values;
};Elm.Navigation = Elm.Navigation || {};
Elm.Navigation.make = function (_elm) {
   "use strict";
   _elm.Navigation = _elm.Navigation || {};
   if (_elm.Navigation.values)
   return _elm.Navigation.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Navigation";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
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
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var Touch = Elm.Touch.make(_elm);
   var _op = {};
   var scaleTouches = F4(function (_v0,
   _v1,
   zoom,
   t) {
      return function () {
         switch (_v1.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v0.ctor)
                 {case "_Tuple2":
                    return function () {
                         var $float = function (_v8) {
                            return function () {
                               switch (_v8.ctor)
                               {case "_Tuple2":
                                  return {ctor: "_Tuple2"
                                         ,_0: Basics.toFloat(_v8._0)
                                         ,_1: Basics.toFloat(_v8._1)};}
                               _E.Case($moduleName,
                               "on line 81, column 21 to 41");
                            }();
                         };
                         var $ = $float({ctor: "_Tuple2"
                                        ,_0: t.x
                                        ,_1: t.y}),
                         tx = $._0,
                         ty = $._1;
                         return _U.replace([["x"
                                            ,Basics.round(tx / zoom + _v0._0 + _v1._0)]
                                           ,["y"
                                            ,Basics.round(ty / zoom + _v0._1 + _v1._1)]],
                         t);
                      }();}
                 _E.Case($moduleName,
                 "between lines 80 and 84");
              }();}
         _E.Case($moduleName,
         "between lines 80 and 84");
      }();
   });
   var stepZoom = F2(function (factor,
   _v12) {
      return function () {
         return function () {
            var $ = _v12.zoomOffset,
            x = $._0,
            y = $._1;
            var zoom$ = A2(Basics.min,
            A2(Basics.max,
            _v12.zoom * factor,
            _v12.minZoom),
            _v12.maxZoom);
            var delta = F2(function (_v14,
            _v15) {
               return function () {
                  switch (_v15.ctor)
                  {case "_Tuple2":
                     return function () {
                          switch (_v14.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: _v14._0 - _v15._0
                                    ,_1: _v14._1 - _v15._1};}
                          _E.Case($moduleName,
                          "on line 67, column 30 to 44");
                       }();}
                  _E.Case($moduleName,
                  "on line 67, column 30 to 44");
               }();
            });
            var $float = function (_v22) {
               return function () {
                  switch (_v22.ctor)
                  {case "_Tuple2":
                     return {ctor: "_Tuple2"
                            ,_0: Basics.toFloat(_v22._0)
                            ,_1: Basics.toFloat(_v22._1)};}
                  _E.Case($moduleName,
                  "on line 66, column 21 to 41");
               }();
            };
            var scaleF = F2(function (f,
            _v26) {
               return function () {
                  switch (_v26.ctor)
                  {case "_Tuple2":
                     return {ctor: "_Tuple2"
                            ,_0: _v26._0 / f
                            ,_1: _v26._1 / f};}
                  _E.Case($moduleName,
                  "on line 65, column 24 to 36");
               }();
            });
            var winD = scaleF(_v12.zoom)($float(_v12.windowDims));
            var winD$ = scaleF(zoom$)($float(_v12.windowDims));
            var $ = scaleF(2)(A2(delta,
            winD,
            winD$)),
            dx = $._0,
            dy = $._1;
            return _U.replace([["zoom"
                               ,zoom$]
                              ,["zoomOffset"
                               ,{ctor: "_Tuple2"
                                ,_0: x + dx
                                ,_1: y + dy}]],
            _v12);
         }();
      }();
   });
   var withinBounds = F2(function (dimensions,
   _v30) {
      return function () {
         return function () {
            var limitRightBottom = F2(function (_v32,
            _v33) {
               return function () {
                  switch (_v33.ctor)
                  {case "_Tuple2":
                     return function () {
                          switch (_v32.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: A2(Basics.min,
                                    _v32._0,
                                    _v33._0)
                                    ,_1: A2(Basics.min,
                                    _v32._1,
                                    _v33._1)};}
                          _E.Case($moduleName,
                          "on line 50, column 41 to 59");
                       }();}
                  _E.Case($moduleName,
                  "on line 50, column 41 to 59");
               }();
            });
            var limitLeftTop = F2(function (_v40,
            _v41) {
               return function () {
                  switch (_v41.ctor)
                  {case "_Tuple2":
                     return function () {
                          switch (_v40.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: A2(Basics.max,
                                    _v40._0,
                                    _v41._0)
                                    ,_1: A2(Basics.max,
                                    _v40._1,
                                    _v41._1)};}
                          _E.Case($moduleName,
                          "on line 49, column 37 to 55");
                       }();}
                  _E.Case($moduleName,
                  "on line 49, column 37 to 55");
               }();
            });
            var subT = F2(function (_v48,
            _v49) {
               return function () {
                  switch (_v49.ctor)
                  {case "_Tuple2":
                     return function () {
                          switch (_v48.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: _v48._0 - _v49._0
                                    ,_1: _v48._1 - _v49._1};}
                          _E.Case($moduleName,
                          "on line 48, column 29 to 43");
                       }();}
                  _E.Case($moduleName,
                  "on line 48, column 29 to 43");
               }();
            });
            var addT = F2(function (_v56,
            _v57) {
               return function () {
                  switch (_v57.ctor)
                  {case "_Tuple2":
                     return function () {
                          switch (_v56.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: _v56._0 + _v57._0
                                    ,_1: _v56._1 + _v57._1};}
                          _E.Case($moduleName,
                          "on line 47, column 29 to 43");
                       }();}
                  _E.Case($moduleName,
                  "on line 47, column 29 to 43");
               }();
            });
            var leftTop = A2(limitLeftTop,
            A2(addT,
            _v30.absPos,
            _v30.zoomOffset),
            {ctor: "_Tuple2",_0: 0,_1: 0});
            var $float = function (_v64) {
               return function () {
                  switch (_v64.ctor)
                  {case "_Tuple2":
                     return {ctor: "_Tuple2"
                            ,_0: Basics.toFloat(_v64._0)
                            ,_1: Basics.toFloat(_v64._1)};}
                  _E.Case($moduleName,
                  "on line 46, column 21 to 41");
               }();
            };
            var $ = $float(dimensions),
            w = $._0,
            h = $._1;
            var scaleF = F2(function (f,
            _v68) {
               return function () {
                  switch (_v68.ctor)
                  {case "_Tuple2":
                     return {ctor: "_Tuple2"
                            ,_0: _v68._0 / f
                            ,_1: _v68._1 / f};}
                  _E.Case($moduleName,
                  "on line 45, column 24 to 36");
               }();
            });
            var windowDims$ = scaleF(_v30.zoom)($float(_v30.windowDims));
            var $ = A2(limitRightBottom,
            A2(addT,leftTop,windowDims$),
            {ctor: "_Tuple2",_0: w,_1: h}),
            right = $._0,
            bottom = $._1;
            var absPos$ = _U.cmp(right,
            w) < 0 && _U.cmp(bottom,
            h) < 0 ? A2(subT,
            leftTop,
            _v30.zoomOffset) : A2(subT,
            A2(subT,
            {ctor: "_Tuple2"
            ,_0: right
            ,_1: bottom},
            windowDims$),
            _v30.zoomOffset);
            return _U.replace([["absPos"
                               ,absPos$]],
            _v30);
         }();
      }();
   });
   var minScale = F2(function (_v72,
   _v73) {
      return function () {
         switch (_v73.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v72.ctor)
                 {case "_Tuple2":
                    return A2(Basics.max,
                      _v72._0 / _v73._0,
                      _v72._1 / _v73._1);}
                 _E.Case($moduleName,
                 "on line 39, column 3 to 27");
              }();}
         _E.Case($moduleName,
         "on line 39, column 3 to 27");
      }();
   });
   var stepMove = F2(function (ts,
   _v80) {
      return function () {
         return function () {
            var $ = _v80.absPos,
            x = $._0,
            y = $._1;
            return List.isEmpty(ts) ? _U.replace([["lastMove"
                                                  ,Maybe.Nothing]],
            _v80) : function () {
               var $float = function (_v82) {
                  return function () {
                     switch (_v82.ctor)
                     {case "_Tuple2":
                        return {ctor: "_Tuple2"
                               ,_0: Basics.toFloat(_v82._0)
                               ,_1: Basics.toFloat(_v82._1)};}
                     _E.Case($moduleName,
                     "on line 24, column 23 to 43");
                  }();
               };
               var t = List.head(ts);
               return function () {
                  var _v86 = _v80.lastMove;
                  switch (_v86.ctor)
                  {case "Just":
                     switch (_v86._0.ctor)
                       {case "_Tuple2":
                          return function () {
                               var $ = $float({ctor: "_Tuple2"
                                              ,_0: _v86._0._0 - t.x
                                              ,_1: _v86._0._1 - t.y}),
                               dx = $._0,
                               dy = $._1;
                               var x$ = x + dx / _v80.zoom;
                               var y$ = y + dy / _v80.zoom;
                               return _U.replace([["lastMove"
                                                  ,Maybe.Just({ctor: "_Tuple2"
                                                              ,_0: t.x
                                                              ,_1: t.y})]
                                                 ,["absPos"
                                                  ,{ctor: "_Tuple2"
                                                   ,_0: x$
                                                   ,_1: y$}]],
                               _v80);
                            }();}
                       break;
                     case "Nothing":
                     return _U.replace([["lastMove"
                                        ,Maybe.Just({ctor: "_Tuple2"
                                                    ,_0: t.x
                                                    ,_1: t.y})]],
                       _v80);}
                  _E.Case($moduleName,
                  "between lines 25 and 33");
               }();
            }();
         }();
      }();
   });
   var Zoomable = F8(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h) {
      return _U.insert("lastMove",
      g,
      _U.insert("zoomOffset",
      f,
      _U.insert("absPos",
      e,
      _U.insert("maxZoom",
      d,
      _U.insert("minZoom",
      c,
      _U.insert("zoom",
      b,
      _U.insert("windowDims",
      a,
      h)))))));
   });
   _elm.Navigation.values = {_op: _op
                            ,stepMove: stepMove
                            ,minScale: minScale
                            ,withinBounds: withinBounds
                            ,stepZoom: stepZoom
                            ,scaleTouches: scaleTouches
                            ,Zoomable: Zoomable};
   return _elm.Navigation.values;
};