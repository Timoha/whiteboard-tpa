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
   var Array = Elm.Array.make(_elm);
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
   var minScale = F2(function (_v0,
   _v1) {
      return function () {
         switch (_v1.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v0.ctor)
                 {case "_Tuple2":
                    return A2(Basics.max,
                      _v0._0 / _v1._0,
                      _v0._1 / _v1._1);}
                 _E.Case($moduleName,
                 "on line 341, column 3 to 27");
              }();}
         _E.Case($moduleName,
         "on line 341, column 3 to 27");
      }();
   });
   var constructZooms = function (_v8) {
      return function () {
         return function () {
            var zoomStep = 2;
            var zoomScale = A3(List.foldl,
            F2(function (z,zs) {
               return {ctor: "::"
                      ,_0: List.head(zs) / zoomStep
                      ,_1: zs};
            }),
            _L.fromArray([16]),
            _L.range(1,9));
            var $float = function (_v10) {
               return function () {
                  switch (_v10.ctor)
                  {case "_Tuple2":
                     return {ctor: "_Tuple2"
                            ,_0: Basics.toFloat(_v10._0)
                            ,_1: Basics.toFloat(_v10._1)};}
                  _E.Case($moduleName,
                  "on line 346, column 21 to 41");
               }();
            };
            var minZoom = A2(minScale,
            $float(_v8.windowDims),
            $float(_v8.dimensions));
            var zooms$ = Array.fromList({ctor: "::"
                                        ,_0: minZoom
                                        ,_1: A2(List.filter,
                                        function (z) {
                                           return _U.cmp(z,minZoom) > 0;
                                        },
                                        zoomScale)});
            var zoomLevel = List.filter(function (_v14) {
               return function () {
                  switch (_v14.ctor)
                  {case "_Tuple2":
                     return _U.eq(_v14._1,1);}
                  _E.Case($moduleName,
                  "on line 351, column 36 to 42");
               }();
            })(Array.toIndexedList(zooms$));
            var zoomLevel$ = List.isEmpty(zoomLevel) ? 0 : Basics.fst(List.head(zoomLevel));
            return _U.replace([["zooms"
                               ,A2(Debug.log,"zooms",zooms$)]
                              ,["zoomLevel",zoomLevel$]],
            _v8);
         }();
      }();
   };
   var scaleTouches = F4(function (_v18,
   _v19,
   zoom,
   t) {
      return function () {
         switch (_v19.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v18.ctor)
                 {case "_Tuple2":
                    return function () {
                         var $float = function (_v26) {
                            return function () {
                               switch (_v26.ctor)
                               {case "_Tuple2":
                                  return {ctor: "_Tuple2"
                                         ,_0: Basics.toFloat(_v26._0)
                                         ,_1: Basics.toFloat(_v26._1)};}
                               _E.Case($moduleName,
                               "on line 332, column 21 to 41");
                            }();
                         };
                         var $ = $float({ctor: "_Tuple2"
                                        ,_0: t.x
                                        ,_1: t.y}),
                         tx = $._0,
                         ty = $._1;
                         return _U.replace([["x"
                                            ,Debug.log("x")(Basics.round(tx / zoom + _v18._0 + _v19._0))]
                                           ,["y"
                                            ,Debug.log("y")(Basics.round(ty / zoom + _v18._1 + _v19._1))]],
                         t);
                      }();}
                 _E.Case($moduleName,
                 "between lines 331 and 335");
              }();}
         _E.Case($moduleName,
         "between lines 331 and 335");
      }();
   });
   var stepZoom = F2(function (deltaLevel,
   _v30) {
      return function () {
         return function () {
            var $ = _v30.zoomOffset,
            x = $._0,
            y = $._1;
            var zoomLevel$$ = _v30.zoomLevel + deltaLevel;
            var zoom = A2(Array.getOrFail,
            A2(Debug.log,
            "oldzoomlevel",
            _v30.zoomLevel),
            _v30.zooms);
            var $ = Debug.log("zoom")(function () {
               var _v32 = A2(Array.get,
               zoomLevel$$,
               _v30.zooms);
               switch (_v32.ctor)
               {case "Just":
                  return {ctor: "_Tuple2"
                         ,_0: _v32._0
                         ,_1: zoomLevel$$};
                  case "Nothing":
                  return {ctor: "_Tuple2"
                         ,_0: zoom
                         ,_1: _v30.zoomLevel};}
               _E.Case($moduleName,
               "between lines 317 and 320");
            }()),
            zoom$ = $._0,
            zoomLevel$ = $._1;
            var delta = F2(function (_v34,
            _v35) {
               return function () {
                  switch (_v35.ctor)
                  {case "_Tuple2":
                     return function () {
                          switch (_v34.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: _v34._0 - _v35._0
                                    ,_1: _v34._1 - _v35._1};}
                          _E.Case($moduleName,
                          "on line 314, column 30 to 44");
                       }();}
                  _E.Case($moduleName,
                  "on line 314, column 30 to 44");
               }();
            });
            var $float = function (_v42) {
               return function () {
                  switch (_v42.ctor)
                  {case "_Tuple2":
                     return {ctor: "_Tuple2"
                            ,_0: Basics.toFloat(_v42._0)
                            ,_1: Basics.toFloat(_v42._1)};}
                  _E.Case($moduleName,
                  "on line 313, column 21 to 41");
               }();
            };
            var scaleF = F2(function (f,
            _v46) {
               return function () {
                  switch (_v46.ctor)
                  {case "_Tuple2":
                     return {ctor: "_Tuple2"
                            ,_0: _v46._0 / f
                            ,_1: _v46._1 / f};}
                  _E.Case($moduleName,
                  "on line 312, column 24 to 36");
               }();
            });
            var winD = scaleF(zoom)($float(_v30.windowDims));
            var winD$ = scaleF(zoom$)($float(_v30.windowDims));
            var $ = scaleF(2)(A2(delta,
            winD,
            winD$)),
            dx = $._0,
            dy = $._1;
            return _U.replace([["zoomLevel"
                               ,zoomLevel$]
                              ,["zoomOffset"
                               ,{ctor: "_Tuple2"
                                ,_0: x + dx
                                ,_1: y + dy}]],
            _v30);
         }();
      }();
   });
   var withinBounds = function (_v50) {
      return function () {
         return function () {
            var limitRightBottom = F2(function (_v52,
            _v53) {
               return function () {
                  switch (_v53.ctor)
                  {case "_Tuple2":
                     return function () {
                          switch (_v52.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: A2(Basics.min,
                                    _v52._0,
                                    _v53._0)
                                    ,_1: A2(Basics.min,
                                    _v52._1,
                                    _v53._1)};}
                          _E.Case($moduleName,
                          "on line 297, column 41 to 59");
                       }();}
                  _E.Case($moduleName,
                  "on line 297, column 41 to 59");
               }();
            });
            var limitLeftTop = F2(function (_v60,
            _v61) {
               return function () {
                  switch (_v61.ctor)
                  {case "_Tuple2":
                     return function () {
                          switch (_v60.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: A2(Basics.max,
                                    _v60._0,
                                    _v61._0)
                                    ,_1: A2(Basics.max,
                                    _v60._1,
                                    _v61._1)};}
                          _E.Case($moduleName,
                          "on line 296, column 37 to 55");
                       }();}
                  _E.Case($moduleName,
                  "on line 296, column 37 to 55");
               }();
            });
            var subT = F2(function (_v68,
            _v69) {
               return function () {
                  switch (_v69.ctor)
                  {case "_Tuple2":
                     return function () {
                          switch (_v68.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: _v68._0 - _v69._0
                                    ,_1: _v68._1 - _v69._1};}
                          _E.Case($moduleName,
                          "on line 295, column 29 to 43");
                       }();}
                  _E.Case($moduleName,
                  "on line 295, column 29 to 43");
               }();
            });
            var addT = F2(function (_v76,
            _v77) {
               return function () {
                  switch (_v77.ctor)
                  {case "_Tuple2":
                     return function () {
                          switch (_v76.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: _v76._0 + _v77._0
                                    ,_1: _v76._1 + _v77._1};}
                          _E.Case($moduleName,
                          "on line 294, column 29 to 43");
                       }();}
                  _E.Case($moduleName,
                  "on line 294, column 29 to 43");
               }();
            });
            var leftTop = A2(limitLeftTop,
            A2(addT,
            _v50.absPos,
            _v50.zoomOffset),
            {ctor: "_Tuple2",_0: 0,_1: 0});
            var $float = function (_v84) {
               return function () {
                  switch (_v84.ctor)
                  {case "_Tuple2":
                     return {ctor: "_Tuple2"
                            ,_0: Basics.toFloat(_v84._0)
                            ,_1: Basics.toFloat(_v84._1)};}
                  _E.Case($moduleName,
                  "on line 293, column 21 to 41");
               }();
            };
            var $ = $float(_v50.dimensions),
            w = $._0,
            h = $._1;
            var scaleF = F2(function (f,
            _v88) {
               return function () {
                  switch (_v88.ctor)
                  {case "_Tuple2":
                     return {ctor: "_Tuple2"
                            ,_0: _v88._0 / f
                            ,_1: _v88._1 / f};}
                  _E.Case($moduleName,
                  "on line 292, column 24 to 36");
               }();
            });
            var zoom = A2(Array.getOrFail,
            _v50.zoomLevel,
            _v50.zooms);
            var windowDims$ = scaleF(zoom)($float(_v50.windowDims));
            var $ = A2(limitRightBottom,
            A2(addT,leftTop,windowDims$),
            {ctor: "_Tuple2",_0: w,_1: h}),
            right = $._0,
            bottom = $._1;
            var absPos$ = _U.cmp(right,
            w) < 0 && _U.cmp(bottom,
            h) < 0 ? A2(subT,
            leftTop,
            _v50.zoomOffset) : A2(subT,
            A2(subT,
            {ctor: "_Tuple2"
            ,_0: right
            ,_1: bottom},
            windowDims$),
            _v50.zoomOffset);
            return _U.replace([["absPos"
                               ,absPos$]],
            _v50);
         }();
      }();
   };
   var stepMove = F2(function (ts,
   _v92) {
      return function () {
         return function () {
            var zoom = A2(Array.getOrFail,
            _v92.zoomLevel,
            _v92.zooms);
            var $ = _v92.absPos,
            x = $._0,
            y = $._1;
            return List.isEmpty(ts) ? _U.replace([["lastMove"
                                                  ,Maybe.Nothing]],
            _v92) : function () {
               var $float = function (_v94) {
                  return function () {
                     switch (_v94.ctor)
                     {case "_Tuple2":
                        return {ctor: "_Tuple2"
                               ,_0: Basics.toFloat(_v94._0)
                               ,_1: Basics.toFloat(_v94._1)};}
                     _E.Case($moduleName,
                     "on line 272, column 23 to 43");
                  }();
               };
               var t = List.head(ts);
               return function () {
                  var _v98 = _v92.lastMove;
                  switch (_v98.ctor)
                  {case "Just":
                     switch (_v98._0.ctor)
                       {case "_Tuple2":
                          return function () {
                               var $ = $float({ctor: "_Tuple2"
                                              ,_0: _v98._0._0 - t.x
                                              ,_1: _v98._0._1 - t.y}),
                               dx = $._0,
                               dy = $._1;
                               var x$ = x + dx / zoom;
                               var y$ = y + dy / zoom;
                               return _U.replace([["lastMove"
                                                  ,Maybe.Just({ctor: "_Tuple2"
                                                              ,_0: t.x
                                                              ,_1: t.y})]
                                                 ,["absPos"
                                                  ,{ctor: "_Tuple2"
                                                   ,_0: x$
                                                   ,_1: y$}]],
                               _v92);
                            }();}
                       break;
                     case "Nothing":
                     return _U.replace([["lastMove"
                                        ,Maybe.Just({ctor: "_Tuple2"
                                                    ,_0: t.x
                                                    ,_1: t.y})]],
                       _v92);}
                  _E.Case($moduleName,
                  "between lines 273 and 281");
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
   var withinWindowDims = F2(function (ts,
   _v102) {
      return function () {
         switch (_v102.ctor)
         {case "_Tuple2":
            return function () {
                 var within = function (t) {
                    return _U.cmp(t.x,
                    0) > 0 && (_U.cmp(t.x,
                    _v102._0) < 0 && (_U.cmp(t.y,
                    0) > 0 && _U.cmp(t.y,
                    _v102._1) < 0));
                 };
                 return A2(List.filter,
                 within,
                 ts);
              }();}
         _E.Case($moduleName,
         "between lines 139 and 140");
      }();
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
                                    ,_0: 2000
                                    ,_1: 1300}
                       ,drawing: Dict.empty
                       ,history: Dict.empty
                       ,lastMove: Maybe.Nothing
                       ,windowDims: {ctor: "_Tuple2"
                                    ,_0: 0
                                    ,_1: 0}
                       ,zoomLevel: 1
                       ,zoomOffset: {ctor: "_Tuple2"
                                    ,_0: 0
                                    ,_1: 0}
                       ,zooms: Array.fromList(_L.fromArray([0.5
                                                           ,1
                                                           ,2]))};
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
   var display = F2(function (_v107,
   _v108) {
      return function () {
         return function () {
            switch (_v107.ctor)
            {case "_Tuple2":
               return function () {
                    var toZero = F2(function (zoom,
                    _v113) {
                       return function () {
                          switch (_v113.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: (0 - _v113._0) * zoom / 2
                                    ,_1: _v113._1 * zoom / 2};}
                          _E.Case($moduleName,
                          "on line 421, column 27 to 54");
                       }();
                    });
                    var paths = Dict.values(_v108.drawing);
                    var flipVert = function (_v117) {
                       return function () {
                          switch (_v117.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: _v117._0
                                    ,_1: 0 - _v117._1};}
                          _E.Case($moduleName,
                          "on line 414, column 24 to 29");
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
                    var $float = function (_v121) {
                       return function () {
                          switch (_v121.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: Basics.toFloat(_v121._0)
                                    ,_1: Basics.toFloat(_v121._1)};}
                          _E.Case($moduleName,
                          "on line 413, column 21 to 41");
                       }();
                    };
                    var zoom = A2(Array.getOrFail,
                    _v108.zoomLevel,
                    _v108.zooms);
                    var toAbsPos = F2(function (_v125,
                    _v126) {
                       return function () {
                          switch (_v126.ctor)
                          {case "_Tuple2":
                             return function () {
                                  switch (_v125.ctor)
                                  {case "_Tuple2":
                                     return {ctor: "_Tuple2"
                                            ,_0: _v126._0 - _v125._0 * zoom
                                            ,_1: _v126._1 + _v125._1 * zoom};}
                                  _E.Case($moduleName,
                                  "on line 422, column 33 to 61");
                               }();}
                          _E.Case($moduleName,
                          "on line 422, column 33 to 61");
                       }();
                    });
                    var pos = toAbsPos(_v108.absPos)(A2(toZero,
                    zoom,
                    $float({ctor: "_Tuple2"
                           ,_0: _v107._0
                           ,_1: _v107._1})));
                    return A3(Graphics.Collage.collage,
                    _v107._0,
                    _v107._1,
                    _L.fromArray([Graphics.Collage.scale(zoom)(A2(Graphics.Collage.move,
                    pos,
                    Graphics.Collage.group(forms)))]));
                 }();}
            _E.Case($moduleName,
            "between lines 411 and 424");
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
   var Zoomable = F7(function (a,
   b,
   c,
   d,
   e,
   f,
   g) {
      return _U.insert("lastMove",
      f,
      _U.insert("zoomOffset",
      e,
      _U.insert("absPos",
      d,
      _U.insert("zooms",
      c,
      _U.insert("zoomLevel",
      b,
      _U.insert("windowDims",
      a,
      g))))));
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
   var stepUndo = function (_v133) {
      return function () {
         return function () {
            var ids = Dict.keys(_v133.history);
            return List.isEmpty(ids) ? _v133 : function () {
               var lastId = List.maximum(ids);
               var $ = function () {
                  var _v135 = A2(Dict.get,
                  lastId,
                  _v133.history);
                  switch (_v135.ctor)
                  {case "Just":
                     switch (_v135._0.ctor)
                       {case "Drew":
                          return {ctor: "_Tuple2"
                                 ,_0: A2(Dict.remove,
                                 _v135._0._0,
                                 _v133.drawing)
                                 ,_1: A2(Dict.remove,
                                 _v135._0._0,
                                 _v133.history)};
                          case "Erased":
                          return {ctor: "_Tuple2"
                                 ,_0: A3(List.foldl,
                                 F2(function (s,d) {
                                    return A3(Dict.insert,
                                    s.id,
                                    s,
                                    d);
                                 }),
                                 _v133.drawing,
                                 _v135._0._0)
                                 ,_1: A3(List.foldl,
                                 F2(function (s,h) {
                                    return A3(Dict.insert,
                                    s.id,
                                    Drew(s.id),
                                    h);
                                 }),
                                 A2(Dict.remove,
                                 lastId,
                                 _v133.history),
                                 _v135._0._0)};}
                       break;
                     case "Nothing":
                     return {ctor: "_Tuple2"
                            ,_0: Dict.empty
                            ,_1: Dict.empty};}
                  _E.Case($moduleName,
                  "between lines 184 and 190");
               }(),
               d = $._0,
               h = $._1;
               return _U.replace([["drawing",d]
                                 ,["history",h]],
               _v133);
            }();
         }();
      }();
   };
   var removeEraser = function (_v139) {
      return function () {
         return function () {
            var ids = Dict.keys(_v139.history);
            var $ = List.isEmpty(ids) ? {ctor: "_Tuple2"
                                        ,_0: Dict.empty
                                        ,_1: Dict.empty} : function () {
               var lastId = List.maximum(ids);
               var removeLast = Dict.remove(lastId);
               return function () {
                  var _v141 = A2(Dict.get,
                  lastId,
                  _v139.history);
                  switch (_v141.ctor)
                  {case "Just":
                     switch (_v141._0.ctor)
                       {case "Erased":
                          return List.isEmpty(_v141._0._0) ? {ctor: "_Tuple2"
                                                             ,_0: removeLast(_v139.drawing)
                                                             ,_1: removeLast(_v139.history)} : {ctor: "_Tuple2"
                                                                                               ,_0: removeLast(_v139.drawing)
                                                                                               ,_1: _v139.history};}
                       break;}
                  return {ctor: "_Tuple2"
                         ,_0: _v139.drawing
                         ,_1: _v139.history};
               }();
            }(),
            d = $._0,
            h = $._1;
            return _U.replace([["drawing",d]
                              ,["history",h]],
            _v139);
         }();
      }();
   };
   var stepEraser = F2(function (ts,
   _v144) {
      return function () {
         return List.isEmpty(ts) ? removeEraser(_v144) : function () {
            var t = List.head(ts);
            var id = Basics.abs(t.id);
            return function () {
               var _v146 = A2(Dict.get,
               id,
               _v144.drawing);
               switch (_v146.ctor)
               {case "Just":
                  return function () {
                       var _raw = A3(Dict.getOrElse,
                       Erased(_L.fromArray([])),
                       A2(Debug.log,"t.id",id),
                       _v144.history),
                       $ = _raw.ctor === "Erased" ? _raw : _E.Case($moduleName,
                       "on line 253, column 34 to 90"),
                       vs = $._0;
                       var strokes = List.tail(List.reverse(Dict.values(_v144.drawing)));
                       var eraserSeg = A2(line,
                       A2(point,t.x,t.y),
                       List.head(_v146._0.points));
                       var crossed = A2(List.filter,
                       isLineStrokeIntersect(eraserSeg),
                       strokes);
                       var erased = List.isEmpty(crossed) ? _L.fromArray([]) : _L.fromArray([List.head(crossed)]);
                       return _U.replace([["drawing"
                                          ,A3(List.foldl,
                                          function (s) {
                                             return Dict.remove(s.id);
                                          },
                                          A2(add1,t,_v144.drawing),
                                          erased)]
                                         ,["history"
                                          ,A3(Dict.insert,
                                          id,
                                          Erased(_L.append(erased,vs)),
                                          _v144.history)]],
                       _v144);
                    }();
                  case "Nothing":
                  return _U.replace([["drawing"
                                     ,A2(add1,t,_v144.drawing)]
                                    ,["history"
                                     ,A3(Dict.insert,
                                     id,
                                     Erased(_L.fromArray([])),
                                     _v144.history)]],
                    _v144);}
               _E.Case($moduleName,
               "between lines 247 and 257");
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
         "between lines 108 and 111");
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
         "between lines 116 and 120");
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
   A2(Signal._op["<~"],
   portToBrush,
   brushPort)),
   Signal.constant({ctor: "_Tuple2"
                   ,_0: 10000
                   ,_1: 7000})),
   Window.dimensions);
   var stepCanvas = F2(function (_v150,
   _v151) {
      return function () {
         return function () {
            return function () {
               var zc = _U.replace([["windowDims"
                                    ,_v150.windowDims]],
               _v151);
               var zcanvas = constructZooms(zc);
               var c = getCanvas(zcanvas);
               var canvas$ = function () {
                  var _v154 = _v150.action;
                  switch (_v154.ctor)
                  {case "Touches":
                     return function () {
                          var ts$ = A2(List.map,
                          A3(scaleTouches,
                          _v151.absPos,
                          _v151.zoomOffset,
                          A2(Array.getOrFail,
                          zcanvas.zoomLevel,
                          zcanvas.zooms)),
                          _v154._0);
                          return function () {
                             var _v156 = _v150.mode;
                             switch (_v156.ctor)
                             {case "Drawing":
                                return _U.replace([["drawing"
                                                   ,A2(addN,
                                                   A2(applyBrush,
                                                   ts$,
                                                   _v150.brush),
                                                   _v151.drawing)]
                                                  ,["history"
                                                   ,A2(recordDrew,
                                                   ts$,
                                                   _v151.history)]],
                                  c);
                                case "Erasing":
                                return A2(stepEraser,
                                  A2(applyBrush,
                                  ts$,
                                  {_: {}
                                  ,color: A4(Color.rgba,0,0,0,0.1)
                                  ,size: 15}),
                                  c);}
                             return c;
                          }();
                       }();
                     case "Undo":
                     return stepUndo(c);}
                  return c;
               }();
               var zcanvas$ = withinBounds(function () {
                  var _v157 = _v150.action;
                  switch (_v157.ctor)
                  {case "Touches":
                     return function () {
                          var _v159 = _v150.mode;
                          switch (_v159.ctor)
                          {case "Viewing":
                             return A2(stepMove,
                               _v157._0,
                               zcanvas);}
                          return zcanvas;
                       }();
                     case "ZoomIn":
                     return A2(stepZoom,1,zcanvas);
                     case "ZoomOut":
                     return A2(stepZoom,-1,zcanvas);}
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
                        ,withinWindowDims: withinWindowDims
                        ,ccw: ccw
                        ,isIntersect: isIntersect
                        ,toSegments: toSegments
                        ,isLineStrokeIntersect: isLineStrokeIntersect
                        ,stepUndo: stepUndo
                        ,applyBrush: applyBrush
                        ,recordDrew: recordDrew
                        ,addN: addN
                        ,add1: add1
                        ,removeEraser: removeEraser
                        ,stepEraser: stepEraser
                        ,stepMove: stepMove
                        ,withinBounds: withinBounds
                        ,stepZoom: stepZoom
                        ,scaleTouches: scaleTouches
                        ,minScale: minScale
                        ,constructZooms: constructZooms
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
                        ,Line: Line
                        ,Zoomable: Zoomable};
   return _elm.Canvas.values;
};