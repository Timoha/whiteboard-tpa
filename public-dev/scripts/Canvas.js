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
   var undo = function (d) {
      return function () {
         var ids = Dict.keys(d);
         return function () {
            switch (ids.ctor)
            {case "[]": return Dict.empty;}
            return A2(Dict.remove,
            List.maximum(ids),
            d);
         }();
      }();
   };
   var lineToTuple = function (l) {
      return {ctor: "_Tuple2"
             ,_0: l.p1
             ,_1: l.p2};
   };
   var line = F2(function (p1,p2) {
      return {_: {},p1: p1,p2: p2};
   });
   var connectPoints = function (ps) {
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
   var scene = F2(function (_v1,
   paths) {
      return function () {
         switch (_v1.ctor)
         {case "_Tuple2":
            return function () {
                 var strokeOrDot = function (path) {
                    return function () {
                       var _v5 = _U.cmp(List.length(path.points),
                       1) > 0;
                       switch (_v5)
                       {case false: return A2(dot,
                            List.head(path.points),
                            path.brush);
                          case true:
                          return Graphics.Collage.traced(thickLine(path.brush))(A2(List.map,
                            pointToTuple,
                            path.points));}
                       _E.Case($moduleName,
                       "between lines 158 and 161");
                    }();
                 };
                 var forms = A2(List.map,
                 strokeOrDot,
                 paths);
                 var $float = function (_v6) {
                    return function () {
                       switch (_v6.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: Basics.toFloat(_v6._0)
                                 ,_1: Basics.toFloat(0 - _v6._1)};}
                       _E.Case($moduleName,
                       "on line 156, column 24 to 45");
                    }();
                 };
                 return A3(Graphics.Collage.collage,
                 _v1._0,
                 _v1._1,
                 _L.fromArray([A2(Graphics.Collage.move,
                 $float({ctor: "_Tuple2"
                        ,_0: (0 - _v1._0) / 2 | 0
                        ,_1: (0 - _v1._1) / 2 | 0}),
                 Graphics.Collage.group(forms))]));
              }();}
         _E.Case($moduleName,
         "between lines 156 and 162");
      }();
   });
   var point = F2(function (x,y) {
      return {_: {},x: x,y: y};
   });
   var linesIntersection = F2(function (_v10,
   _v11) {
      return function () {
         switch (_v11.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v10.ctor)
                 {case "_Tuple2":
                    return function () {
                         var dy34 = _v11._0.y - _v11._1.y;
                         var dy12 = _v10._0.y - _v10._1.y;
                         var dx34 = _v11._0.x - _v11._1.x;
                         var dx12 = _v10._0.x - _v10._1.x;
                         var den = dx12 * dy34 - dy12 * dx34;
                         return _U.eq(den,
                         0) ? Maybe.Nothing : function () {
                            var det34 = _v11._0.x * _v11._1.y - _v11._0.y * _v11._1.x;
                            var det12 = _v10._0.x * _v10._1.y - _v10._0.y * _v10._1.x;
                            var numx = det12 * dx34 - dx12 * det34;
                            var numy = det12 * dy34 - dy12 * det34;
                            return Maybe.Just(A2(point,
                            numx / den,
                            numy / den));
                         }();
                      }();}
                 _E.Case($moduleName,
                 "between lines 49 and 61");
              }();}
         _E.Case($moduleName,
         "between lines 49 and 61");
      }();
   });
   var isIntersect = F2(function (l1,
   l2) {
      return function () {
         var _v18 = A2(linesIntersection,
         lineToTuple(l1),
         lineToTuple(l2));
         switch (_v18.ctor)
         {case "Just": return true;
            case "Nothing": return false;}
         _E.Case($moduleName,
         "between lines 67 and 69");
      }();
   });
   var isStrokesIntersect = F2(function (s1,
   s2) {
      return function () {
         var _v20 = _U.cmp(List.length(s1.points),
         1) > 0 || _U.cmp(List.length(s2.points),
         1) > 0;
         switch (_v20)
         {case true: return function () {
                 var segs2 = connectPoints(s2.points);
                 var segs1 = connectPoints(s1.points);
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
   var strokeIntersects = F2(function (s,
   ss) {
      return A2(List.filter,
      isStrokesIntersect(s),
      ss);
   });
   var add1 = F2(function (t,d) {
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
                      ,_0: A2(point,
                      Basics.toFloat(t.x),
                      Basics.toFloat(0 - t.y))
                      ,_1: vs.points}]],
         vs),
         d);
      }();
   });
   var addN = F2(function (ts,
   dict) {
      return A3(List.foldl,
      add1,
      dict,
      ts);
   });
   var undoAction = Native.Ports.portIn("undoAction",
   Native.Ports.incomingSignal(function (v) {
      return _U.isJSArray(v) ? {ctor: "_Tuple0"} : _E.raise("invalid input, expecting JSArray but got " + v);
   }));
   var newBrush = Native.Ports.portIn("newBrush",
   Native.Ports.incomingSignal(function (v) {
      return typeof v === "object" && "size" in v && "red" in v && "green" in v && "blue" in v && "alpha" in v ? {_: {}
                                                                                                                 ,size: typeof v.size === "number" ? v.size : _E.raise("invalid input, expecting JSNumber but got " + v.size)
                                                                                                                 ,red: typeof v.red === "number" ? v.red : _E.raise("invalid input, expecting JSNumber but got " + v.red)
                                                                                                                 ,green: typeof v.green === "number" ? v.green : _E.raise("invalid input, expecting JSNumber but got " + v.green)
                                                                                                                 ,blue: typeof v.blue === "number" ? v.blue : _E.raise("invalid input, expecting JSNumber but got " + v.blue)
                                                                                                                 ,alpha: typeof v.alpha === "number" ? v.alpha : _E.raise("invalid input, expecting JSNumber but got " + v.alpha)} : _E.raise("invalid input, expecting JSObject [\"size\",\"red\",\"green\",\"blue\",\"alpha\"] but got " + v);
   }));
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
   var Draw = function (a) {
      return {ctor: "Draw",_0: a};
   };
   var Erase = function (a) {
      return {ctor: "Erase",_0: a};
   };
   var Undo = {ctor: "Undo"};
   var DrawStroke = function (a) {
      return {ctor: "DrawStroke"
             ,_0: a};
   };
   var events = A2(Signal.merge,
   A2(Signal._op["<~"],
   DrawStroke,
   A2(Signal._op["~"],
   A2(Signal._op["<~"],
   applyBrush,
   Touch.touches),
   A2(Signal._op["<~"],
   portToBrush,
   newBrush))),
   A2(Signal._op["<~"],
   Basics.always(Undo),
   undoAction));
   var canvasState = function () {
      var update = F2(function (event,
      paths) {
         return function () {
            switch (event.ctor)
            {case "DrawStroke":
               return A2(addN,event._0,paths);
               case "Undo":
               return undo(paths);}
            _E.Case($moduleName,
            "between lines 106 and 109");
         }();
      });
      return A3(Signal.foldp,
      update,
      Dict.empty,
      events);
   }();
   var main = A2(Signal._op["~"],
   A2(Signal._op["<~"],
   scene,
   Window.dimensions),
   A2(Signal._op["<~"],
   Dict.values,
   canvasState));
   _elm.Canvas.values = {_op: _op
                        ,point: point
                        ,pointToTuple: pointToTuple
                        ,line: line
                        ,lineToTuple: lineToTuple
                        ,linesIntersection: linesIntersection
                        ,isIntersect: isIntersect
                        ,connectPoints: connectPoints
                        ,isStrokesIntersect: isStrokesIntersect
                        ,strokeIntersects: strokeIntersects
                        ,events: events
                        ,canvasState: canvasState
                        ,undo: undo
                        ,portToBrush: portToBrush
                        ,applyBrush: applyBrush
                        ,addN: addN
                        ,add1: add1
                        ,thickLine: thickLine
                        ,dot: dot
                        ,scene: scene
                        ,main: main
                        ,DrawStroke: DrawStroke
                        ,Undo: Undo
                        ,Erase: Erase
                        ,Draw: Draw
                        ,Brush: Brush
                        ,Brushed: Brushed
                        ,Stroke: Stroke
                        ,Point: Point
                        ,Line: Line};
   return _elm.Canvas.values;
};