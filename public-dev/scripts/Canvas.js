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
   var scene = F2(function (_v0,
   paths) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return function () {
                 var $float = function (_v4) {
                    return function () {
                       switch (_v4.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: Basics.toFloat(_v4._0)
                                 ,_1: Basics.toFloat(0 - _v4._1)};}
                       _E.Case($moduleName,
                       "on line 36, column 24 to 45");
                    }();
                 };
                 var strokeOrDot = function (path) {
                    return function () {
                       var _v8 = _U.cmp(List.length(path.points),
                       1) > 0;
                       switch (_v8)
                       {case false: return A2(dot,
                            $float(List.head(path.points)),
                            path.brush);
                          case true:
                          return A2(Graphics.Collage.traced,
                            thickLine(path.brush),
                            A2(List.map,
                            $float,
                            path.points));}
                       _E.Case($moduleName,
                       "between lines 38 and 41");
                    }();
                 };
                 var forms = A2(List.map,
                 strokeOrDot,
                 paths);
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
         "between lines 36 and 42");
      }();
   });
   var add1 = F2(function (t,d) {
      return function () {
         var vs = A3(Dict.getOrElse,
         {_: {}
         ,brush: t.brush
         ,points: _L.fromArray([])},
         t.id,
         d);
         return A3(Dict.insert,
         t.id,
         _U.replace([["points"
                     ,{ctor: "::"
                      ,_0: {ctor: "_Tuple2"
                           ,_0: t.x
                           ,_1: t.y}
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
   var newBrush = Native.Ports.portIn("newBrush",
   Native.Ports.incomingSignal(function (v) {
      return typeof v === "object" && "size" in v && "red" in v && "green" in v && "blue" in v && "alpha" in v ? {_: {}
                                                                                                                 ,size: typeof v.size === "number" ? v.size : _E.raise("invalid input, expecting JSNumber but got " + v.size)
                                                                                                                 ,red: typeof v.red === "number" ? v.red : _E.raise("invalid input, expecting JSNumber but got " + v.red)
                                                                                                                 ,green: typeof v.green === "number" ? v.green : _E.raise("invalid input, expecting JSNumber but got " + v.green)
                                                                                                                 ,blue: typeof v.blue === "number" ? v.blue : _E.raise("invalid input, expecting JSNumber but got " + v.blue)
                                                                                                                 ,alpha: typeof v.alpha === "number" ? v.alpha : _E.raise("invalid input, expecting JSNumber but got " + v.alpha)} : _E.raise("invalid input, expecting JSObject [\"size\",\"red\",\"green\",\"blue\",\"alpha\"] but got " + v);
   }));
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
   var main = A2(Signal._op["~"],
   A2(Signal._op["<~"],
   scene,
   Window.dimensions),
   A2(Signal._op["<~"],
   function ($) {
      return List.reverse(Dict.values($));
   },
   A3(Signal.foldp,
   addN,
   Dict.empty,
   A2(Signal._op["~"],
   A2(Signal._op["<~"],
   applyBrush,
   Touch.touches),
   A2(Signal._op["<~"],
   portToBrush,
   newBrush)))));
   _elm.Canvas.values = {_op: _op
                        ,main: main
                        ,portToBrush: portToBrush
                        ,applyBrush: applyBrush
                        ,addN: addN
                        ,add1: add1
                        ,scene: scene
                        ,thickLine: thickLine
                        ,dot: dot
                        ,Brush: Brush
                        ,Brushed: Brushed
                        ,Stroke: Stroke};
   return _elm.Canvas.values;
};