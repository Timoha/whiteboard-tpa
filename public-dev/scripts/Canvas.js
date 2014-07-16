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
                       "between lines 204 and 207");
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
                       "on line 202, column 24 to 45");
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
         "between lines 202 and 208");
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
         var _v10 = A2(linesIntersection,
         l1,
         l2);
         switch (_v10.ctor)
         {case "Just": return true;
            case "Nothing": return false;}
         _E.Case($moduleName,
         "between lines 121 and 123");
      }();
   });
   var isStrokesIntersect = F2(function (s1,
   s2) {
      return function () {
         var _v12 = _U.cmp(List.length(s1.points),
         1) > 0 || _U.cmp(List.length(s2.points),
         1) > 0;
         switch (_v12)
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
   var isLineIntersectStroke = F2(function (l,
   s) {
      return function () {
         var _v13 = _U.cmp(List.length(s.points),
         1) > 0;
         switch (_v13)
         {case true:
            return List.any(isIntersect(l))(toSegments(s.points));}
         return false;
      }();
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
   var addHead = F2(function (ts,
   d) {
      return function () {
         switch (ts.ctor)
         {case "[]": return d;}
         return A2(add1,List.head(ts),d);
      }();
   });
   var addChange = F3(function (ts,
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
   var portToBrush = function (p) {
      return {_: {}
             ,color: A4(Color.rgba,
             p.red,
             p.green,
             p.blue,
             p.alpha)
             ,size: p.size};
   };
   var mode = Native.Ports.portIn("mode",
   Native.Ports.incomingSignal(function (v) {
      return typeof v === "string" || typeof v === "object" && v instanceof String ? v : _E.raise("invalid input, expecting JSString but got " + v);
   }));
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
   var Drawn = function (a) {
      return {ctor: "Drawn",_0: a};
   };
   var addDrawn = F2(function (ts,
   history) {
      return A3(List.foldl,
      F2(function (t,h) {
         return A3(Dict.insert,
         Basics.abs(t.id),
         Drawn(Basics.abs(t.id)),
         h);
      }),
      history,
      ts);
   });
   var Erased = function (a) {
      return {ctor: "Erased"
             ,_0: a};
   };
   var Undo = {ctor: "Undo"};
   var Erasing = function (a) {
      return {ctor: "Erasing"
             ,_0: a};
   };
   var Drawing = function (a) {
      return {ctor: "Drawing"
             ,_0: a};
   };
   var modeToAction = F3(function (mode,
   ts,
   brush) {
      return function () {
         switch (mode)
         {case "Drawing":
            return Drawing(A2(applyBrush,
              ts,
              brush));
            case "Erasing":
            return Erasing(A2(applyBrush,
              ts,
              {_: {}
              ,color: A4(Color.rgba,0,0,0,0.4)
              ,size: 8}));}
         _E.Case($moduleName,
         "between lines 40 and 42");
      }();
   });
   var events = Signal.merges(_L.fromArray([A2(Signal._op["~"],
                                           A2(Signal._op["~"],
                                           A2(Signal._op["<~"],
                                           modeToAction,
                                           mode),
                                           Touch.touches),
                                           A2(Signal._op["<~"],
                                           portToBrush,
                                           newBrush))
                                           ,A2(Signal._op["<~"],
                                           Basics.always(Undo),
                                           undoAction)]));
   var historyState = function () {
      var update = F2(function (event,
      history) {
         return function () {
            switch (event.ctor)
            {case "Drawing":
               return A2(addDrawn,
                 event._0,
                 history);
               case "Erasing":
               return A3(addChange,
                 event._0,
                 Erased(_L.fromArray([])),
                 history);}
            return history;
         }();
      });
      return A3(Signal.foldp,
      update,
      Dict.empty,
      events);
   }();
   var stepCanvas = F2(function (event,
   paths) {
      return function () {
         switch (event.ctor)
         {case "Drawing": return A2(addN,
              event._0,
              paths);
            case "Erasing":
            return A2(addHead,
              event._0,
              paths);
            case "Undo":
            return undo(paths);}
         _E.Case($moduleName,
         "between lines 80 and 83");
      }();
   });
   var canvasState = A3(Signal.foldp,
   stepCanvas,
   Dict.empty,
   events);
   var main = A2(Signal._op["~"],
   A2(Signal._op["<~"],
   scene,
   Window.dimensions),
   A2(Signal._op["<~"],
   Dict.values,
   canvasState));
   _elm.Canvas.values = {_op: _op
                        ,portToBrush: portToBrush
                        ,modeToAction: modeToAction
                        ,events: events
                        ,historyState: historyState
                        ,addChange: addChange
                        ,addDrawn: addDrawn
                        ,canvasState: canvasState
                        ,stepCanvas: stepCanvas
                        ,point: point
                        ,pointToTuple: pointToTuple
                        ,line: line
                        ,linesIntersection: linesIntersection
                        ,isIntersect: isIntersect
                        ,toSegments: toSegments
                        ,isStrokesIntersect: isStrokesIntersect
                        ,isLineIntersectStroke: isLineIntersectStroke
                        ,strokesCrossed: strokesCrossed
                        ,undo: undo
                        ,applyBrush: applyBrush
                        ,addN: addN
                        ,add1: add1
                        ,addHead: addHead
                        ,thickLine: thickLine
                        ,dot: dot
                        ,scene: scene
                        ,main: main
                        ,Drawing: Drawing
                        ,Erasing: Erasing
                        ,Undo: Undo
                        ,Erased: Erased
                        ,Drawn: Drawn
                        ,Brush: Brush
                        ,Brushed: Brushed
                        ,Stroke: Stroke
                        ,Point: Point
                        ,Line: Line};
   return _elm.Canvas.values;
};