Elm.Minimap = Elm.Minimap || {};
Elm.Minimap.make = function (_elm) {
   "use strict";
   _elm.Minimap = _elm.Minimap || {};
   if (_elm.Minimap.values)
   return _elm.Minimap.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Minimap";
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
   var Navigation = Elm.Navigation.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var Window = Elm.Window.make(_elm);
   var _op = {};
   var scaleToMinimap = F2(function (_v0,
   _v1) {
      return function () {
         switch (_v1.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v0.ctor)
                 {case "_Tuple2":
                    return function () {
                         var factor = _v1._0 / _v0._0;
                         return {ctor: "_Tuple2"
                                ,_0: factor
                                ,_1: {ctor: "_Tuple2"
                                     ,_0: _v1._0
                                     ,_1: _v0._1 * factor}};
                      }();}
                 _E.Case($moduleName,
                 "between lines 19 and 21");
              }();}
         _E.Case($moduleName,
         "between lines 19 and 21");
      }();
   });
   var display = F2(function (thisWindowDims,
   c) {
      return function () {
         switch (c.ctor)
         {case "Just":
            return function () {
                 var borderLine = _U.replace([["width"
                                              ,3]
                                             ,["color"
                                              ,A3(Color.rgb,41,171,226)]
                                             ,["join"
                                              ,Graphics.Collage.Clipped]],
                 Graphics.Collage.defaultLine);
                 var scaleDim = F2(function (f,
                 _v10) {
                    return function () {
                       switch (_v10.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: _v10._0 * f
                                 ,_1: _v10._1 * f};}
                       _E.Case($moduleName,
                       "on line 33, column 30 to 42");
                    }();
                 });
                 var addT = F2(function (_v14,
                 _v15) {
                    return function () {
                       switch (_v15.ctor)
                       {case "_Tuple2":
                          return function () {
                               switch (_v14.ctor)
                               {case "_Tuple2":
                                  return {ctor: "_Tuple2"
                                         ,_0: _v14._0 + _v15._0
                                         ,_1: _v14._1 + _v15._1};}
                               _E.Case($moduleName,
                               "on line 32, column 33 to 47");
                            }();}
                       _E.Case($moduleName,
                       "on line 32, column 33 to 47");
                    }();
                 });
                 var flipVert = function (_v22) {
                    return function () {
                       switch (_v22.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: _v22._0
                                 ,_1: 0 - _v22._1};}
                       _E.Case($moduleName,
                       "on line 31, column 28 to 33");
                    }();
                 };
                 var $float = function (_v26) {
                    return function () {
                       switch (_v26.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: Basics.toFloat(_v26._0)
                                 ,_1: Basics.toFloat(_v26._1)};}
                       _E.Case($moduleName,
                       "on line 30, column 25 to 45");
                    }();
                 };
                 var _ = A2(scaleToMinimap,
                 $float(c._0.dimensions),
                 $float(thisWindowDims));
                 var h = function () {
                    switch (_.ctor)
                    {case "_Tuple2":
                       switch (_._1.ctor)
                         {case "_Tuple2":
                            return _._1._1;}
                         break;}
                    _E.Case($moduleName,
                    "on line 34, column 27 to 82");
                 }();
                 var w = function () {
                    switch (_.ctor)
                    {case "_Tuple2":
                       switch (_._1.ctor)
                         {case "_Tuple2":
                            return _._1._0;}
                         break;}
                    _E.Case($moduleName,
                    "on line 34, column 27 to 82");
                 }();
                 var zoom$ = function () {
                    switch (_.ctor)
                    {case "_Tuple2":
                       switch (_._1.ctor)
                         {case "_Tuple2": return _._0;}
                         break;}
                    _E.Case($moduleName,
                    "on line 34, column 27 to 82");
                 }();
                 var leftTop = flipVert(scaleDim(zoom$)(A2(addT,
                 c._0.absPos,
                 c._0.zoomOffset)));
                 var canvas = Graphics.Collage.scale(zoom$)(Graphics.Collage.move({ctor: "_Tuple2"
                                                                                  ,_0: (0 - w) / 2
                                                                                  ,_1: h / 2})(Canvas.renderStrokes(c._0.drawing)));
                 var $ = scaleDim(zoom$ / c._0.zoom)($float(c._0.windowDims)),
                 bw = $._0,
                 bh = $._1;
                 var zeroPos = A2(addT,
                 {ctor: "_Tuple2"
                 ,_0: (0 - (w - bw)) / 2
                 ,_1: (h - bh) / 2},
                 leftTop);
                 var border = Graphics.Collage.move(zeroPos)(A2(Graphics.Collage.outlined,
                 borderLine,
                 A2(Graphics.Collage.rect,
                 bw,
                 bh)));
                 return A3(Graphics.Collage.collage,
                 Basics.round(w),
                 Basics.round(h),
                 _L.fromArray([canvas,border]));
              }();
            case "Nothing":
            return A2(Graphics.Element.spacer,
              0,
              0);}
         _E.Case($moduleName,
         "between lines 26 and 41");
      }();
   });
   var canvasPort = Native.Ports.portIn("canvasPort",
   Native.Ports.incomingSignal(function (v) {
      return v === null ? Maybe.Nothing : Maybe.Just(typeof v === "object" && "absPos" in v && "zoomOffset" in v && "zoom" in v && "dimensions" in v && "windowDims" in v && "drawing" in v ? {_: {}
                                                                                                                                                                                              ,absPos: _U.isJSArray(v.absPos) ? {ctor: "_Tuple2"
                                                                                                                                                                                                                                ,_0: typeof v.absPos[0] === "number" ? v.absPos[0] : _E.raise("invalid input, expecting JSNumber but got " + v.absPos[0])
                                                                                                                                                                                                                                ,_1: typeof v.absPos[1] === "number" ? v.absPos[1] : _E.raise("invalid input, expecting JSNumber but got " + v.absPos[1])} : _E.raise("invalid input, expecting JSArray but got " + v.absPos)
                                                                                                                                                                                              ,zoomOffset: _U.isJSArray(v.zoomOffset) ? {ctor: "_Tuple2"
                                                                                                                                                                                                                                        ,_0: typeof v.zoomOffset[0] === "number" ? v.zoomOffset[0] : _E.raise("invalid input, expecting JSNumber but got " + v.zoomOffset[0])
                                                                                                                                                                                                                                        ,_1: typeof v.zoomOffset[1] === "number" ? v.zoomOffset[1] : _E.raise("invalid input, expecting JSNumber but got " + v.zoomOffset[1])} : _E.raise("invalid input, expecting JSArray but got " + v.zoomOffset)
                                                                                                                                                                                              ,zoom: typeof v.zoom === "number" ? v.zoom : _E.raise("invalid input, expecting JSNumber but got " + v.zoom)
                                                                                                                                                                                              ,dimensions: _U.isJSArray(v.dimensions) ? {ctor: "_Tuple2"
                                                                                                                                                                                                                                        ,_0: typeof v.dimensions[0] === "number" ? v.dimensions[0] : _E.raise("invalid input, expecting JSNumber but got " + v.dimensions[0])
                                                                                                                                                                                                                                        ,_1: typeof v.dimensions[1] === "number" ? v.dimensions[1] : _E.raise("invalid input, expecting JSNumber but got " + v.dimensions[1])} : _E.raise("invalid input, expecting JSArray but got " + v.dimensions)
                                                                                                                                                                                              ,windowDims: _U.isJSArray(v.windowDims) ? {ctor: "_Tuple2"
                                                                                                                                                                                                                                        ,_0: typeof v.windowDims[0] === "number" ? v.windowDims[0] : _E.raise("invalid input, expecting JSNumber but got " + v.windowDims[0])
                                                                                                                                                                                                                                        ,_1: typeof v.windowDims[1] === "number" ? v.windowDims[1] : _E.raise("invalid input, expecting JSNumber but got " + v.windowDims[1])} : _E.raise("invalid input, expecting JSArray but got " + v.windowDims)
                                                                                                                                                                                              ,drawing: _U.isJSArray(v.drawing) ? _L.fromArray(v.drawing.map(function (v) {
                                                                                                                                                                                                 return typeof v === "object" && "points" in v && "brush" in v ? {_: {}
                                                                                                                                                                                                                                                                 ,points: _U.isJSArray(v.points) ? _L.fromArray(v.points.map(function (v) {
                                                                                                                                                                                                                                                                    return typeof v === "object" && "x" in v && "y" in v ? {_: {}
                                                                                                                                                                                                                                                                                                                           ,x: typeof v.x === "number" ? v.x : _E.raise("invalid input, expecting JSNumber but got " + v.x)
                                                                                                                                                                                                                                                                                                                           ,y: typeof v.y === "number" ? v.y : _E.raise("invalid input, expecting JSNumber but got " + v.y)} : _E.raise("invalid input, expecting JSObject [\"x\",\"y\"] but got " + v);
                                                                                                                                                                                                                                                                 })) : _E.raise("invalid input, expecting JSArray but got " + v.points)
                                                                                                                                                                                                                                                                 ,brush: typeof v.brush === "object" && "size" in v.brush && "color" in v.brush ? {_: {}
                                                                                                                                                                                                                                                                                                                                                  ,size: typeof v.brush.size === "number" ? v.brush.size : _E.raise("invalid input, expecting JSNumber but got " + v.brush.size)
                                                                                                                                                                                                                                                                                                                                                  ,color: typeof v.brush.color === "object" && "red" in v.brush.color && "green" in v.brush.color && "blue" in v.brush.color && "alpha" in v.brush.color ? {_: {}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ,red: typeof v.brush.color.red === "number" ? v.brush.color.red : _E.raise("invalid input, expecting JSNumber but got " + v.brush.color.red)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ,green: typeof v.brush.color.green === "number" ? v.brush.color.green : _E.raise("invalid input, expecting JSNumber but got " + v.brush.color.green)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ,blue: typeof v.brush.color.blue === "number" ? v.brush.color.blue : _E.raise("invalid input, expecting JSNumber but got " + v.brush.color.blue)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ,alpha: typeof v.brush.color.alpha === "number" ? v.brush.color.alpha : _E.raise("invalid input, expecting JSNumber but got " + v.brush.color.alpha)} : _E.raise("invalid input, expecting JSObject [\"red\",\"green\",\"blue\",\"alpha\"] but got " + v.brush.color)} : _E.raise("invalid input, expecting JSObject [\"size\",\"color\"] but got " + v.brush)} : _E.raise("invalid input, expecting JSObject [\"points\",\"brush\"] but got " + v);
                                                                                                                                                                                              })) : _E.raise("invalid input, expecting JSArray but got " + v.drawing)} : _E.raise("invalid input, expecting JSObject [\"absPos\",\"zoomOffset\",\"zoom\",\"dimensions\",\"windowDims\",\"drawing\"] but got " + v));
   }));
   var main = A2(Signal._op["~"],
   A2(Signal._op["<~"],
   display,
   Window.dimensions),
   canvasPort);
   var PortCanvas = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,absPos: a
             ,dimensions: d
             ,drawing: f
             ,windowDims: e
             ,zoom: c
             ,zoomOffset: b};
   });
   _elm.Minimap.values = {_op: _op
                         ,scaleToMinimap: scaleToMinimap
                         ,display: display
                         ,main: main
                         ,PortCanvas: PortCanvas};
   return _elm.Minimap.values;
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
   var renderStrokes = function (ss) {
      return function () {
         var flipVert = function (_v2) {
            return function () {
               switch (_v2.ctor)
               {case "_Tuple2":
                  return {ctor: "_Tuple2"
                         ,_0: _v2._0
                         ,_1: 0 - _v2._1};}
               _E.Case($moduleName,
               "on line 107, column 24 to 29");
            }();
         };
         var strokeOrDot = function (s) {
            return _U.cmp(List.length(s.points),
            1) > 0 ? Graphics.Collage.traced(thickLine(s.brush))(A2(List.map,
            function ($) {
               return flipVert(pointToTuple($));
            },
            s.points)) : A2(dot,
            flipVert(pointToTuple(List.head(s.points))),
            s.brush);
         };
         var ss$ = A2(List.map,
         strokeOrDot,
         ss);
         return Graphics.Collage.group(ss$);
      }();
   };
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
   var stepUndo = function (_v6) {
      return function () {
         return function () {
            var ids = Dict.keys(_v6.history);
            return List.isEmpty(ids) ? _v6 : function () {
               var lastId = List.maximum(ids);
               var $ = function () {
                  var _v8 = A2(Dict.get,
                  lastId,
                  _v6.history);
                  switch (_v8.ctor)
                  {case "Just":
                     switch (_v8._0.ctor)
                       {case "Drew":
                          return {ctor: "_Tuple2"
                                 ,_0: A2(Dict.remove,
                                 _v8._0._0,
                                 _v6.drawing)
                                 ,_1: A2(Dict.remove,
                                 _v8._0._0,
                                 _v6.history)};
                          case "Erased":
                          return {ctor: "_Tuple2"
                                 ,_0: A3(List.foldl,
                                 F2(function (_v12,d) {
                                    return function () {
                                       switch (_v12.ctor)
                                       {case "_Tuple2":
                                          return A3(Dict.insert,
                                            _v12._0,
                                            _v12._1,
                                            d);}
                                       _E.Case($moduleName,
                                       "on line 61, column 55 to 73");
                                    }();
                                 }),
                                 _v6.drawing,
                                 _v8._0._0)
                                 ,_1: A3(List.foldl,
                                 F2(function (_v16,h) {
                                    return function () {
                                       switch (_v16.ctor)
                                       {case "_Tuple2":
                                          return A3(Dict.insert,
                                            _v16._0,
                                            Drew(_v16._0),
                                            h);}
                                       _E.Case($moduleName,
                                       "on line 62, column 55 to 81");
                                    }();
                                 }),
                                 A2(Dict.remove,
                                 lastId,
                                 _v6.history),
                                 _v8._0._0)};}
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
               _v6);
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
                        ,renderStrokes: renderStrokes
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