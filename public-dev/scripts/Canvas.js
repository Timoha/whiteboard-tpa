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
                 "on line 369, column 3 to 27");
              }();}
         _E.Case($moduleName,
         "on line 369, column 3 to 27");
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
   var stepZoom = F2(function (factor,
   _v8) {
      return function () {
         return function () {
            var $ = _v8.zoomOffset,
            x = $._0,
            y = $._1;
            var zoom$ = _v8.zoom * factor;
            var delta = F2(function (_v10,
            _v11) {
               return function () {
                  switch (_v11.ctor)
                  {case "_Tuple2":
                     return function () {
                          switch (_v10.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: _v10._0 - _v11._0
                                    ,_1: _v10._1 - _v11._1};}
                          _E.Case($moduleName,
                          "on line 286, column 30 to 44");
                       }();}
                  _E.Case($moduleName,
                  "on line 286, column 30 to 44");
               }();
            });
            var roundT = function (_v18) {
               return function () {
                  switch (_v18.ctor)
                  {case "_Tuple2":
                     return {ctor: "_Tuple2"
                            ,_0: Basics.round(_v18._0)
                            ,_1: Basics.round(_v18._1)};}
                  _E.Case($moduleName,
                  "on line 285, column 22 to 38");
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
                  "on line 284, column 21 to 41");
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
                  "on line 283, column 24 to 36");
               }();
            });
            var winD = scaleF(_v8.zoom)($float(_v8.windowDims));
            var winD$ = scaleF(zoom$)($float(_v8.windowDims));
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
            _v8);
         }();
      }();
   });
   var scaleTouches = F4(function (_v30,
   _v31,
   zoom,
   t) {
      return function () {
         switch (_v31.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v30.ctor)
                 {case "_Tuple2":
                    return function () {
                         var $float = function (_v38) {
                            return function () {
                               switch (_v38.ctor)
                               {case "_Tuple2":
                                  return {ctor: "_Tuple2"
                                         ,_0: Basics.toFloat(_v38._0)
                                         ,_1: Basics.toFloat(_v38._1)};}
                               _E.Case($moduleName,
                               "on line 274, column 21 to 41");
                            }();
                         };
                         var $ = $float({ctor: "_Tuple2"
                                        ,_0: t.x
                                        ,_1: t.y}),
                         tx = $._0,
                         ty = $._1;
                         return _U.replace([["x"
                                            ,Basics.round(tx / zoom + _v30._0 + _v31._0)]
                                           ,["y"
                                            ,Basics.round(ty / zoom + _v30._1 + _v31._1)]],
                         t);
                      }();}
                 _E.Case($moduleName,
                 "between lines 273 and 277");
              }();}
         _E.Case($moduleName,
         "between lines 273 and 277");
      }();
   });
   var stepMove = F2(function (ts,
   _v42) {
      return function () {
         return function () {
            var $ = _v42.absPos,
            x = $._0,
            y = $._1;
            return List.isEmpty(ts) ? _U.replace([["lastMove"
                                                  ,Maybe.Nothing]],
            _v42) : function () {
               var roundT = function (_v44) {
                  return function () {
                     switch (_v44.ctor)
                     {case "_Tuple2":
                        return {ctor: "_Tuple2"
                               ,_0: Basics.round(_v44._0)
                               ,_1: Basics.round(_v44._1)};}
                     _E.Case($moduleName,
                     "on line 259, column 24 to 40");
                  }();
               };
               var $float = function (_v48) {
                  return function () {
                     switch (_v48.ctor)
                     {case "_Tuple2":
                        return {ctor: "_Tuple2"
                               ,_0: Basics.toFloat(_v48._0)
                               ,_1: Basics.toFloat(_v48._1)};}
                     _E.Case($moduleName,
                     "on line 258, column 23 to 43");
                  }();
               };
               var t = List.head(ts);
               return function () {
                  var _v52 = _v42.lastMove;
                  switch (_v52.ctor)
                  {case "Just":
                     switch (_v52._0.ctor)
                       {case "_Tuple2":
                          return function () {
                               var $ = $float({ctor: "_Tuple2"
                                              ,_0: _v52._0._0 - t.x
                                              ,_1: _v52._0._1 - t.y}),
                               dx = $._0,
                               dy = $._1;
                               var x$ = x + dx / _v42.zoom;
                               var y$ = y + dy / _v42.zoom;
                               return _U.replace([["lastMove"
                                                  ,Maybe.Just({ctor: "_Tuple2"
                                                              ,_0: t.x
                                                              ,_1: t.y})]
                                                 ,["absPos"
                                                  ,{ctor: "_Tuple2"
                                                   ,_0: x$
                                                   ,_1: y$}]],
                               _v42);
                            }();}
                       break;
                     case "Nothing":
                     return _U.replace([["lastMove"
                                        ,Maybe.Just({ctor: "_Tuple2"
                                                    ,_0: t.x
                                                    ,_1: t.y})]],
                       _v42);}
                  _E.Case($moduleName,
                  "between lines 260 and 268");
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
                       ,absPos: {ctor: "_Tuple2"
                                ,_0: 0
                                ,_1: 0}
                       ,dimensions: {ctor: "_Tuple2"
                                    ,_0: 10000
                                    ,_1: 7000}
                       ,drawing: Dict.empty
                       ,history: Dict.empty
                       ,lastMove: Maybe.Nothing
                       ,windowDims: {ctor: "_Tuple2"
                                    ,_0: 0
                                    ,_1: 0}
                       ,zoom: 1
                       ,zoomOffset: {ctor: "_Tuple2"
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
   var display = F2(function (_v57,
   _v58) {
      return function () {
         return function () {
            switch (_v57.ctor)
            {case "_Tuple2":
               return function () {
                    var toAbsPos = F2(function (_v63,
                    _v64) {
                       return function () {
                          switch (_v64.ctor)
                          {case "_Tuple2":
                             return function () {
                                  switch (_v63.ctor)
                                  {case "_Tuple2":
                                     return {ctor: "_Tuple2"
                                            ,_0: _v64._0 - _v63._0 * _v58.zoom
                                            ,_1: _v64._1 + _v63._1 * _v58.zoom};}
                                  _E.Case($moduleName,
                                  "on line 362, column 33 to 61");
                               }();}
                          _E.Case($moduleName,
                          "on line 362, column 33 to 61");
                       }();
                    });
                    var toZero = F2(function (zoom,
                    _v71) {
                       return function () {
                          switch (_v71.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: (0 - _v71._0) * zoom / 2
                                    ,_1: _v71._1 * zoom / 2};}
                          _E.Case($moduleName,
                          "on line 361, column 27 to 54");
                       }();
                    });
                    var paths = Dict.values(_v58.drawing);
                    var flipVert = function (_v75) {
                       return function () {
                          switch (_v75.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: _v75._0
                                    ,_1: 0 - _v75._1};}
                          _E.Case($moduleName,
                          "on line 353, column 24 to 29");
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
                    var forms = A2(List.map,
                    strokeOrDot,
                    paths);
                    var $float = function (_v79) {
                       return function () {
                          switch (_v79.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: Basics.toFloat(_v79._0)
                                    ,_1: Basics.toFloat(_v79._1)};}
                          _E.Case($moduleName,
                          "on line 352, column 21 to 41");
                       }();
                    };
                    var pos = toAbsPos(_v58.absPos)(A2(toZero,
                    _v58.zoom,
                    $float({ctor: "_Tuple2"
                           ,_0: _v57._0
                           ,_1: _v57._1})));
                    return A3(Graphics.Collage.collage,
                    _v57._0,
                    _v57._1,
                    _L.fromArray([Graphics.Collage.scale(_v58.zoom)(A2(Graphics.Collage.move,
                    pos,
                    Graphics.Collage.group(forms)))]));
                 }();}
            _E.Case($moduleName,
            "between lines 351 and 364");
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
   var Zoomable = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return _U.insert("lastMove",
      e,
      _U.insert("zoomOffset",
      d,
      _U.insert("absPos",
      c,
      _U.insert("zoom",
      b,
      _U.insert("windowDims",a,f)))));
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
   var undo = function (_v83) {
      return function () {
         return function () {
            var ids = Dict.keys(_v83.history);
            return List.isEmpty(ids) ? _v83 : function () {
               var lastId = List.maximum(ids);
               return function () {
                  var _v85 = A2(Dict.get,
                  lastId,
                  _v83.history);
                  switch (_v85.ctor)
                  {case "Just":
                     switch (_v85._0.ctor)
                       {case "Drew":
                          return _U.replace([["drawing"
                                             ,A2(Dict.remove,
                                             _v85._0._0,
                                             _v83.drawing)]
                                            ,["history"
                                             ,A2(Dict.remove,
                                             _v85._0._0,
                                             _v83.history)]],
                            _v83);
                          case "Erased":
                          return _U.replace([["drawing"
                                             ,A3(List.foldl,
                                             F2(function (s,d) {
                                                return A3(Dict.insert,
                                                s.id,
                                                s,
                                                d);
                                             }),
                                             _v83.drawing,
                                             _v85._0._0)]
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
                                             _v83.history),
                                             _v85._0._0)]],
                            _v83);}
                       break;
                     case "Nothing":
                     return _U.replace([["drawing"
                                        ,Dict.empty]
                                       ,["history",Dict.empty]],
                       _v83);}
                  _E.Case($moduleName,
                  "between lines 175 and 182");
               }();
            }();
         }();
      }();
   };
   var removeEraser = function (_v89) {
      return function () {
         return function () {
            var ids = Dict.keys(_v89.history);
            return function () {
               switch (ids.ctor)
               {case "[]":
                  return _U.replace([["drawing"
                                     ,Dict.empty]
                                    ,["history",Dict.empty]],
                    _v89);}
               return function () {
                  var lastId = List.maximum(ids);
                  return function () {
                     var _v92 = A2(Dict.get,
                     lastId,
                     _v89.history);
                     switch (_v92.ctor)
                     {case "Just":
                        switch (_v92._0.ctor)
                          {case "Erased":
                             return function () {
                                  switch (_v92._0._0.ctor)
                                  {case "[]":
                                     return _U.replace([["drawing"
                                                        ,A2(Dict.remove,
                                                        lastId,
                                                        _v89.drawing)]
                                                       ,["history"
                                                        ,A2(Dict.remove,
                                                        lastId,
                                                        _v89.history)]],
                                       _v89);}
                                  return _U.replace([["drawing"
                                                     ,A2(Dict.remove,
                                                     lastId,
                                                     _v89.drawing)]],
                                  _v89);
                               }();}
                          break;}
                     return _v89;
                  }();
               }();
            }();
         }();
      }();
   };
   var eraser = F2(function (ts,
   _v96) {
      return function () {
         return List.isEmpty(ts) ? removeEraser(_v96) : function () {
            var t = List.head(ts);
            var id = Basics.abs(t.id);
            return function () {
               var _v98 = A2(Dict.get,
               id,
               _v96.drawing);
               switch (_v98.ctor)
               {case "Just":
                  return function () {
                       var _raw = A3(Dict.getOrElse,
                       Erased(_L.fromArray([])),
                       id,
                       _v96.history),
                       $ = _raw.ctor === "Erased" ? _raw : _E.Case($moduleName,
                       "on line 240, column 34 to 71"),
                       vs = $._0;
                       var strokes = List.tail(List.reverse(Dict.values(_v96.drawing)));
                       var eraserSeg = A2(line,
                       A2(point,t.x,t.y),
                       List.head(_v98._0.points));
                       var crossed = A2(List.filter,
                       isLineStrokeIntersect(eraserSeg),
                       strokes);
                       var erased = List.isEmpty(crossed) ? _L.fromArray([]) : _L.fromArray([List.head(crossed)]);
                       return _U.replace([["drawing"
                                          ,A3(List.foldl,
                                          function (s) {
                                             return Dict.remove(s.id);
                                          },
                                          A2(add1,t,_v96.drawing),
                                          erased)]
                                         ,["history"
                                          ,A3(Dict.insert,
                                          id,
                                          Erased(_L.append(erased,vs)),
                                          _v96.history)]],
                       _v96);
                    }();
                  case "Nothing":
                  return _U.replace([["drawing"
                                     ,A2(add1,t,_v96.drawing)]
                                    ,["history"
                                     ,A3(Dict.insert,
                                     id,
                                     Erased(_L.fromArray([])),
                                     _v96.history)]],
                    _v96);}
               _E.Case($moduleName,
               "between lines 234 and 244");
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
         "between lines 105 and 108");
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
         "between lines 113 and 117");
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
   Signal.constant({ctor: "_Tuple2"
                   ,_0: 10000
                   ,_1: 7000})),
   Window.dimensions);
   var stepCanvas = F2(function (_v102,
   _v103) {
      return function () {
         return function () {
            return function () {
               var zcanvas = _U.replace([["windowDims"
                                         ,_v102.windowDims]],
               _v103);
               var c = getCanvas(zcanvas);
               var canvas$ = function () {
                  var _v106 = _v102.action;
                  switch (_v106.ctor)
                  {case "Touches":
                     return function () {
                          var ts$ = A2(List.map,
                          A3(scaleTouches,
                          _v103.absPos,
                          _v103.zoomOffset,
                          _v103.zoom),
                          _v106._0);
                          return function () {
                             var _v108 = _v102.mode;
                             switch (_v108.ctor)
                             {case "Drawing":
                                return _U.replace([["drawing"
                                                   ,A2(addN,
                                                   A2(applyBrush,
                                                   ts$,
                                                   _v102.brush),
                                                   _v103.drawing)]
                                                  ,["history"
                                                   ,A2(recordDrew,
                                                   ts$,
                                                   _v103.history)]],
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
               var zcanvas$ = function () {
                  var _v109 = _v102.action;
                  switch (_v109.ctor)
                  {case "Touches":
                     return function () {
                          var _v111 = _v102.mode;
                          switch (_v111.ctor)
                          {case "Viewing":
                             return A2(stepMove,
                               _v109._0,
                               zcanvas);}
                          return zcanvas;
                       }();
                     case "ZoomIn":
                     return A2(stepZoom,2,zcanvas);
                     case "ZoomOut":
                     return A2(stepZoom,
                       1 / 2,
                       zcanvas);}
                  return zcanvas;
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
                        ,stepZoom: stepZoom
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