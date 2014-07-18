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
                 "on line 352, column 3 to 27");
              }();}
         _E.Case($moduleName,
         "on line 352, column 3 to 27");
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
            return function () {
                 var $float = function (_v12) {
                    return function () {
                       switch (_v12.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: Basics.toFloat(_v12._0)
                                 ,_1: Basics.toFloat(_v12._1)};}
                       _E.Case($moduleName,
                       "on line 276, column 21 to 41");
                    }();
                 };
                 var $ = $float({ctor: "_Tuple2"
                                ,_0: t.x
                                ,_1: t.y}),
                 x = $._0,
                 y = $._1;
                 return _U.replace([["x"
                                    ,Basics.round(x / zoom) + _v8._1]
                                   ,["y"
                                    ,Basics.round(y / zoom) + _v8._0]],
                 t);
              }();}
         _E.Case($moduleName,
         "between lines 275 and 279");
      }();
   });
   var stepMove = F2(function (ts,
   _v16) {
      return function () {
         return function () {
            var $ = _v16.topLeft,
            top = $._0,
            left = $._1;
            var $ = _v16.windowDims,
            winW = $._0,
            winH = $._1;
            var $ = _v16.dimensions,
            canW = $._0,
            canH = $._1;
            return List.isEmpty(ts) ? _U.replace([["lastMove"
                                                  ,Maybe.Nothing]],
            _v16) : function () {
               var roundT = function (_v18) {
                  return function () {
                     switch (_v18.ctor)
                     {case "_Tuple2":
                        return {ctor: "_Tuple2"
                               ,_0: Basics.round(_v18._0)
                               ,_1: Basics.round(_v18._1)};}
                     _E.Case($moduleName,
                     "on line 259, column 24 to 40");
                  }();
               };
               var $float = function (_v22) {
                  return function () {
                     switch (_v22.ctor)
                     {case "_Tuple2":
                        return {ctor: "_Tuple2"
                               ,_0: Basics.toFloat(_v22._0)
                               ,_1: Basics.toFloat(_v22._1)};}
                     _E.Case($moduleName,
                     "on line 258, column 23 to 43");
                  }();
               };
               var t = List.head(ts);
               var $ = $float({ctor: "_Tuple2"
                              ,_0: t.x
                              ,_1: t.y}),
               tx = $._0,
               ty = $._1;
               return function () {
                  var _v26 = _v16.lastMove;
                  switch (_v26.ctor)
                  {case "Just":
                     switch (_v26._0.ctor)
                       {case "_Tuple2":
                          return function () {
                               var $ = roundT({ctor: "_Tuple2"
                                              ,_0: tx
                                              ,_1: ty}),
                               tx$ = $._0,
                               ty$ = $._1;
                               var $ = $float({ctor: "_Tuple2"
                                              ,_0: _v26._0._0 - t.x
                                              ,_1: _v26._0._1 - t.y}),
                               dx = $._0,
                               dy = $._1;
                               var top$ = top + Basics.round(dy / _v16.zoom);
                               var left$ = left + Basics.round(dx / _v16.zoom);
                               return _U.replace([["lastMove"
                                                  ,Maybe.Just({ctor: "_Tuple2"
                                                              ,_0: t.x
                                                              ,_1: t.y})]
                                                 ,["topLeft"
                                                  ,{ctor: "_Tuple2"
                                                   ,_0: top$
                                                   ,_1: left$}]],
                               _v16);
                            }();}
                       break;
                     case "Nothing":
                     return _U.replace([["lastMove"
                                        ,Maybe.Just({ctor: "_Tuple2"
                                                    ,_0: t.x
                                                    ,_1: t.y})]],
                       _v16);}
                  _E.Case($moduleName,
                  "between lines 261 and 270");
               }();
            }();
         }();
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
                                    ,_0: 10000
                                    ,_1: 7000}
                       ,drawing: Dict.empty
                       ,history: Dict.empty
                       ,lastMove: Maybe.Nothing
                       ,topLeft: {ctor: "_Tuple2"
                                 ,_0: 0
                                 ,_1: 0}
                       ,windowDims: {ctor: "_Tuple2"
                                    ,_0: 0
                                    ,_1: 0}
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
   var display = F2(function (_v31,
   _v32) {
      return function () {
         return function () {
            switch (_v31.ctor)
            {case "_Tuple2":
               return function () {
                    var flipVert = function (_v37) {
                       return function () {
                          switch (_v37.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: _v37._0
                                    ,_1: 0 - _v37._1};}
                          _E.Case($moduleName,
                          "on line 341, column 24 to 29");
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
                    var $float = function (_v41) {
                       return function () {
                          switch (_v41.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: Basics.toFloat(_v41._0)
                                    ,_1: Basics.toFloat(_v41._1)};}
                          _E.Case($moduleName,
                          "on line 338, column 21 to 41");
                       }();
                    };
                    var $ = $float(_v32.topLeft),
                    top = $._0,
                    left = $._1;
                    var $ = $float({ctor: "_Tuple2"
                                   ,_0: _v31._0
                                   ,_1: _v31._1}),
                    w$ = $._0,
                    h$ = $._1;
                    var paths = Dict.values(_v32.drawing);
                    var forms = A2(List.map,
                    strokeOrDot,
                    paths);
                    return A3(Graphics.Collage.collage,
                    _v31._0,
                    _v31._1,
                    _L.fromArray([A2(Graphics.Collage.move,
                    flipVert({ctor: "_Tuple2"
                             ,_0: (0 - w$) * _v32.zoom / 2 - left
                             ,_1: (0 - h$) * _v32.zoom / 2 - top}),
                    Graphics.Collage.scale(_v32.zoom)(Graphics.Collage.group(forms)))]));
                 }();}
            _E.Case($moduleName,
            "between lines 336 and 347");
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
   var getCanvas = function (c) {
      return {_: {}
             ,dimensions: c.dimensions
             ,drawing: c.drawing
             ,history: c.history};
   };
   var Zoomable = F5(function (a,
   b,
   c,
   d,
   e) {
      return _U.insert("lastMove",
      d,
      _U.insert("topLeft",
      c,
      _U.insert("zoom",
      b,
      _U.insert("windowDims",a,e))));
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
   var Canvas = F3(function (a,
   b,
   c) {
      return {_: {}
             ,dimensions: c
             ,drawing: a
             ,history: b};
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
   var undo = function (_v45) {
      return function () {
         return function () {
            var ids = Dict.keys(_v45.history);
            return List.isEmpty(ids) ? _v45 : function () {
               var lastId = List.maximum(ids);
               return function () {
                  var _v47 = A2(Dict.get,
                  lastId,
                  _v45.history);
                  switch (_v47.ctor)
                  {case "Just":
                     switch (_v47._0.ctor)
                       {case "Drew":
                          return _U.replace([["drawing"
                                             ,A2(Dict.remove,
                                             _v47._0._0,
                                             _v45.drawing)]
                                            ,["history"
                                             ,A2(Dict.remove,
                                             _v47._0._0,
                                             _v45.history)]],
                            _v45);
                          case "Erased":
                          return _U.replace([["drawing"
                                             ,A3(List.foldl,
                                             F2(function (s,d) {
                                                return A3(Dict.insert,
                                                s.id,
                                                s,
                                                d);
                                             }),
                                             _v45.drawing,
                                             _v47._0._0)]
                                            ,["history"
                                             ,A3(List.foldl,
                                             F2(function (s,h) {
                                                return A3(Dict.insert,
                                                s.id,
                                                Drew(s.id),
                                                h);
                                             }),
                                             A2(Dict.remove,
                                             lastId,
                                             _v45.history),
                                             _v47._0._0)]],
                            _v45);}
                       break;
                     case "Nothing":
                     return _U.replace([["drawing"
                                        ,Dict.empty]
                                       ,["history",Dict.empty]],
                       _v45);}
                  _E.Case($moduleName,
                  "between lines 173 and 180");
               }();
            }();
         }();
      }();
   };
   var removeEraser = function (_v51) {
      return function () {
         return function () {
            var ids = Dict.keys(_v51.history);
            return function () {
               switch (ids.ctor)
               {case "[]":
                  return _U.replace([["drawing"
                                     ,Dict.empty]
                                    ,["history",Dict.empty]],
                    _v51);}
               return function () {
                  var lastId = List.maximum(ids);
                  return function () {
                     var _v54 = A2(Dict.get,
                     lastId,
                     _v51.history);
                     switch (_v54.ctor)
                     {case "Just":
                        switch (_v54._0.ctor)
                          {case "Erased":
                             return function () {
                                  switch (_v54._0._0.ctor)
                                  {case "[]":
                                     return _U.replace([["drawing"
                                                        ,A2(Dict.remove,
                                                        lastId,
                                                        _v51.drawing)]
                                                       ,["history"
                                                        ,A2(Dict.remove,
                                                        lastId,
                                                        _v51.history)]],
                                       _v51);}
                                  return _U.replace([["drawing"
                                                     ,A2(Dict.remove,
                                                     lastId,
                                                     _v51.drawing)]],
                                  _v51);
                               }();}
                          break;}
                     return _v51;
                  }();
               }();
            }();
         }();
      }();
   };
   var eraser = F2(function (ts,
   _v58) {
      return function () {
         return List.isEmpty(ts) ? removeEraser(_v58) : function () {
            var t = List.head(ts);
            var id = Basics.abs(t.id);
            return function () {
               var _v60 = A2(Dict.get,
               id,
               _v58.drawing);
               switch (_v60.ctor)
               {case "Just":
                  return function () {
                       var _raw = A3(Dict.getOrElse,
                       Erased(_L.fromArray([])),
                       id,
                       _v58.history),
                       $ = _raw.ctor === "Erased" ? _raw : _E.Case($moduleName,
                       "on line 238, column 34 to 71"),
                       vs = $._0;
                       var strokes = List.tail(List.reverse(Dict.values(_v58.drawing)));
                       var eraserSeg = A2(line,
                       A2(point,t.x,t.y),
                       List.head(_v60._0.points));
                       var crossed = A2(List.filter,
                       isLineStrokeIntersect(eraserSeg),
                       strokes);
                       var erased = List.isEmpty(crossed) ? _L.fromArray([]) : _L.fromArray([List.head(crossed)]);
                       return _U.replace([["drawing"
                                          ,A3(List.foldl,
                                          function (s) {
                                             return Dict.remove(s.id);
                                          },
                                          A2(add1,t,_v58.drawing),
                                          erased)]
                                         ,["history"
                                          ,A3(Dict.insert,
                                          id,
                                          Erased(_L.append(erased,vs)),
                                          _v58.history)]],
                       _v58);
                    }();
                  case "Nothing":
                  return _U.replace([["drawing"
                                     ,A2(add1,t,_v58.drawing)]
                                    ,["history"
                                     ,A3(Dict.insert,
                                     id,
                                     Erased(_L.fromArray([])),
                                     _v58.history)]],
                    _v58);}
               _E.Case($moduleName,
               "between lines 232 and 242");
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
         "between lines 103 and 106");
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
         "between lines 111 and 115");
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
   var stepCanvas = F2(function (_v64,
   _v65) {
      return function () {
         return function () {
            return function () {
               var zcanvas$ = function () {
                  var _v68 = _v64.action;
                  switch (_v68.ctor)
                  {case "Touches":
                     return function () {
                          var ts = A2(List.map,
                          A2(scaleTouches,
                          _v65.topLeft,
                          _v65.zoom),
                          _v68._0);
                          return function () {
                             var _v70 = _v64.mode;
                             switch (_v70.ctor)
                             {case "Viewing":
                                return A2(stepMove,
                                  _v68._0,
                                  _v65);}
                             return _v65;
                          }();
                       }();}
                  return _v65;
               }();
               var c = getCanvas(_v65);
               var canvas$ = function () {
                  var _v71 = _v64.action;
                  switch (_v71.ctor)
                  {case "Touches":
                     return function () {
                          var ts$ = A2(List.map,
                          A2(scaleTouches,
                          _v65.topLeft,
                          _v65.zoom),
                          _v71._0);
                          return function () {
                             var _v73 = _v64.mode;
                             switch (_v73.ctor)
                             {case "Drawing":
                                return _U.replace([["drawing"
                                                   ,A2(addN,
                                                   A2(applyBrush,
                                                   ts$,
                                                   _v64.brush),
                                                   _v65.drawing)]
                                                  ,["history"
                                                   ,A2(recordDrew,
                                                   ts$,
                                                   _v65.history)]],
                                  c);
                                case "Erasing":
                                return A2(eraser,
                                  A2(applyBrush,
                                  ts$,
                                  {_: {}
                                  ,color: A4(Color.rgba,0,0,0,0.1)
                                  ,size: 15}),
                                  c);}
                             return c;
                          }();
                       }();
                     case "Undo": return undo(c);}
                  return c;
               }();
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
   var main = A2(Signal._op["~"],
   A2(Signal._op["<~"],
   display,
   Window.dimensions),
   canvasState);
   _elm.Canvas.values = {_op: _op
                        ,getCanvas: getCanvas
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
                        ,isLineStrokeIntersect: isLineStrokeIntersect
                        ,undo: undo
                        ,applyBrush: applyBrush
                        ,recordDrew: recordDrew
                        ,addN: addN
                        ,add1: add1
                        ,removeEraser: removeEraser
                        ,eraser: eraser
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
                        ,Line: Line
                        ,Zoomable: Zoomable};
   return _elm.Canvas.values;
};