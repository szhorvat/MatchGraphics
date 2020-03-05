(* Mathematica Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: MatchGraphics *)
(* :Context: MatchGraphics` *)
(* :Author: szhorvat *)
(* :Date: 2020-03-05 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2020 szhorvat *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["MatchGraphics`", {"GraphicsInformation`"}];
(* Exported symbols added here with SymbolName::usage *)

GrMatchWidths::usage =
    "GrMatchWidths[graphicsList] makes the widths of all graphics the same and matches the horizontal image padding.\n" <>
    "GrMatchWidths[graphicsList, width] makes the frame widths of all graphics be width.";

GrMatchHeights::usage =
    "GrMatchHeights[graphicsList] makes the heights of all graphics the same and matches the vertical image padding.\n" <>
    "GrMatchHeights[graphicsList, height] makes the frame heights of all graphics be height.";

GrMatchSizes::usage =
    "GrMatchSizes[graphicsMatrix] matches the widths/heights of graphics in the same column/row.\n" <>
    "GrMatchSizes[graphicsMatrix, widths, heights] applies the specified widths and heights.";

Begin["`Private`"];

matchVertPad[paddings_] := ReplacePart[paddings, {_, 2} -> Max /@ Transpose[paddings[[All, 2]]]]

matchHorizPad[paddings_] := ReplacePart[paddings, {_, 1} -> Max /@ Transpose[paddings[[All, 1]]]]

getPadding[g_Graphics] := Lookup[GraphicsInformation[g], "ImagePadding"]

(* this sets not the size of the entire image, but only the size of the plot area *)
setSize[g_Graphics, size_] := Show[g, ImageSize -> Automatic -> size]

GrMatchWidths[glist : {__Graphics}, width : (_?NumericQ) | Automatic : Automatic] :=
    Module[{paddings, graphics, w = width},
      If[w === Automatic,
        w = Lookup[GraphicsInformation@First[glist], "ImageSize"][[1]]
      ];
      graphics = setSize[#, w] & /@ glist;
      paddings = getPadding /@ graphics;
      MapThread[
        Show[#1, ImagePadding -> #2] &,
        {graphics, matchHorizPad[paddings]}
      ]
    ]

GrMatchHeights[glist : {__Graphics}, height : (_?NumericQ) | Automatic : Automatic] :=
    Module[{paddings, graphics, h = height},
      If[h === Automatic,
        h = Lookup[GraphicsInformation@First[glist], "ImageSize"][[2]]
      ];
      graphics = setSize[#, {Automatic, h}] & /@ glist;
      paddings = getPadding /@ graphics;
      MapThread[
        Show[#1, ImagePadding -> #2] &,
        {graphics, matchVertPad[paddings]}
      ]
    ]

GrMatchSizes::badsz = "The graphics grid has size `1` but `2` width and height values were given.";
GrMatchSizes[
  gmatrix_ /; MatrixQ[gmatrix, MatchQ[Head[#], Graphics] &],
  widths : {__?NumericQ} | _?NumericQ | Automatic : Automatic,
  heights : {__?NumericQ} | _?NumericQ | Automatic : Automatic
] :=
    Catch@Module[
      {paddings, graphics,
        rows, cols,
        w = widths, h = heights
      },
      {rows, cols} = Dimensions[gmatrix];
      (* TODO: GraphicsInformation is also run in getPadding.
         Can we avoid running it twice?
         Probably not because the second time it is run after ImageSize was set. *)
      If[w === Automatic,
        (* take the widths from the first row *)
        w = Lookup[GraphicsInformation[#], "ImageSize"][[1]] & /@ gmatrix[[1]]
      ];
      If[h === Automatic,
        (* take the heights from the first column *)
        h = Lookup[GraphicsInformation[#], "ImageSize"][[2]] & /@ gmatrix[[All, 1]]
      ];
      If[Not@ListQ[w],
        w = ConstantArray[w, cols]
      ];
      If[Not@ListQ[h],
        h = ConstantArray[h, rows]
      ];
      If[{rows, cols} != {Length[h], Length[w]},
        Message[GrMatchSizes::badsz, {rows, cols}, {Length[h], Length[w]}];
        Throw[gmatrix]
      ];
      graphics =
          MapThread[
            setSize,
            {gmatrix, Transpose@Outer[List, w, h, 1]},
            2
          ];
      paddings = Map[getPadding, graphics, {2}];
      paddings = Transpose[matchHorizPad /@ Transpose[matchVertPad /@ paddings]];
      MapThread[
        Show[#1, ImagePadding -> #2] &,
        {graphics, paddings},
        2
      ]
    ]

End[]; (* `Private` *)

EndPackage[]