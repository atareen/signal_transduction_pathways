(* Content-type: application/vnd.wolfram.cdf.text *)

(*** Wolfram CDF File ***)
(* http://www.wolfram.com/cdf *)

(* CreatedBy='Mathematica 11.2' *)

(***************************************************************************)
(*                                                                         *)
(*                                                                         *)
(*  Under the Wolfram FreeCDF terms of use, this file and its content are  *)
(*  bound by the Creative Commons BY-SA Attribution-ShareAlike license.    *)
(*                                                                         *)
(*        For additional information concerning CDF licensing, see:        *)
(*                                                                         *)
(*         www.wolfram.com/cdf/adopting-cdf/licensing-options.html         *)
(*                                                                         *)
(*                                                                         *)
(***************************************************************************)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[      1088,         20]
NotebookDataLength[    327287,       6391]
NotebookOptionsPosition[    323154,       6309]
NotebookOutlinePosition[    323866,       6336]
CellTagsIndexPosition[    323823,       6333]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{

Cell[CellGroupData[{
Cell["Laplace Approximation", "Section",ExpressionUUID->"fc2fc132-6bf7-4de0-8e09-8fef5b298426"],

Cell["\<\
The following section sets up the equations required to compute the exact and \
the Laplace approximations of the conditional probabilities\
\>", "Text",ExpressionUUID->"711e4e16-d0ce-4c58-a71f-248af966f642"],

Cell[CellGroupData[{

Cell[BoxData[{
 RowBox[{
  RowBox[{"SetOptions", "[", 
   RowBox[{"$FrontEndSession", ",", 
    RowBox[{"EvaluationCompletionAction", "\[Rule]", "\"\<ShowTiming\>\""}]}],
    "]"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{"Exact", " ", "probability"}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    RowBox[{"p", "[", 
     RowBox[{"y_", ",", "Inp_", ",", "k_", ",", "V_"}], "]"}], ":=", 
    RowBox[{
     SuperscriptBox["\[ExponentialE]", 
      FractionBox[
       RowBox[{"2", " ", "V", " ", "y", " ", 
        RowBox[{"(", 
         RowBox[{
          RowBox[{
           SuperscriptBox["Inp", "2"], " ", 
           SuperscriptBox["k", "2"]}], "-", 
          SuperscriptBox["\[Alpha]", "2"]}], ")"}]}], 
       SuperscriptBox[
        RowBox[{"(", 
         RowBox[{
          RowBox[{
           RowBox[{"-", "Inp"}], " ", "k"}], "+", "\[Alpha]"}], ")"}], "2"]]], 
     SuperscriptBox["\[ExponentialE]", 
      RowBox[{"-", 
       RowBox[{"Log", "[", 
        RowBox[{"1", "+", 
         FractionBox[
          RowBox[{
           RowBox[{"(", 
            RowBox[{"\[Alpha]", "-", 
             RowBox[{"Inp", " ", "k"}]}], ")"}], "y"}], 
          RowBox[{"Inp", " ", "k", " ", 
           SubscriptBox["O", "tot"]}]]}], "]"}]}]], 
     SuperscriptBox["\[ExponentialE]", 
      RowBox[{
       FractionBox[
        RowBox[{"4", " ", "Inp", " ", "k", " ", 
         SubscriptBox["O", "tot"], " ", "\[Alpha]", " ", "V"}], 
        SuperscriptBox[
         RowBox[{"(", 
          RowBox[{"\[Alpha]", "-", 
           RowBox[{"Inp", " ", "k"}]}], ")"}], "2"]], 
       RowBox[{"Log", "[", 
        RowBox[{"1", "+", 
         FractionBox[
          RowBox[{
           RowBox[{"(", 
            RowBox[{"\[Alpha]", "-", 
             RowBox[{"Inp", " ", "k"}]}], ")"}], "y"}], 
          RowBox[{"Inp", " ", "k", " ", 
           SubscriptBox["O", "tot"]}]]}], "]"}]}]]}]}], ";"}], " ", 
  RowBox[{"(*", " ", 
   RowBox[{
   "I", " ", "checked", " ", "that", " ", "this", " ", "probability", " ", 
    "is", " ", "the", " ", "correct", " ", "solution", " ", "to", " ", "FP", 
    " ", "equation"}], " ", "*)"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{"define", " ", "f", 
     RowBox[{"(", "x", ")"}], " ", "and", " ", "find", " ", "max", " ", 
     RowBox[{"O_max", ".", " ", "f"}], 
     RowBox[{"(", "x", ")"}], " ", "is", " ", "the", " ", "exponent", " ", 
     "in", " ", "the", " ", "conditional", " ", "probability", " ", "as", " ",
      "V"}], " ", "\[Rule]", " ", "infinity"}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"f", "[", 
    RowBox[{"y_", ",", "Inp_", ",", "k_"}], "]"}], ":=", 
   RowBox[{
    RowBox[{
     FractionBox[
      RowBox[{"4", " ", "Inp", " ", "k", " ", 
       SubscriptBox["O", "tot"], " ", "\[Alpha]", " "}], 
      SuperscriptBox[
       RowBox[{"(", 
        RowBox[{"\[Alpha]", "-", 
         RowBox[{"Inp", " ", "k"}]}], ")"}], "2"]], 
     RowBox[{"Log", "[", 
      RowBox[{"1", "+", 
       FractionBox[
        RowBox[{
         RowBox[{"(", 
          RowBox[{"\[Alpha]", "-", 
           RowBox[{"Inp", " ", "k"}]}], ")"}], "y"}], 
        RowBox[{"Inp", " ", "k", " ", 
         SubscriptBox["O", "tot"]}]]}], "]"}]}], "-", 
    FractionBox[
     RowBox[{"2", " ", "y", " ", 
      RowBox[{"(", 
       RowBox[{
        RowBox[{"Inp", " ", "k"}], "+", "\[Alpha]"}], ")"}]}], 
     RowBox[{"\[Alpha]", "-", 
      RowBox[{"Inp", " ", "k"}]}]]}]}], "\n", 
  RowBox[{"(*", 
   RowBox[{
    RowBox[{"Solve", "[", 
     RowBox[{
      RowBox[{
       RowBox[{"D", "[", 
        RowBox[{
         RowBox[{"f", "[", 
          RowBox[{"y", ",", "Inp", ",", "k"}], "]"}], ",", "y"}], "]"}], 
       "\[Equal]", "0"}], ",", "y"}], "]"}], ";"}], " ", "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{
   "the", " ", "following", " ", "with", " ", "the", " ", "linear", " ", 
    "correction", " ", "works", " ", "really", " ", "well"}], " ", "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", 
   RowBox[{
    RowBox[{
     RowBox[{"fDoublePrimeyMax", "[", 
      RowBox[{"Inp_", ",", "k_"}], "]"}], ":=", 
     RowBox[{
      RowBox[{"-", 
       FractionBox[
        RowBox[{"2", " ", 
         RowBox[{"(", 
          RowBox[{"\[Alpha]", "+", 
           RowBox[{"Inp", " ", "k"}]}], ")"}]}], 
        RowBox[{"Inp", "  ", "*", "k", "*", "  ", 
         SubscriptBox["O", "tot"]}]]}], "-", " ", 
      RowBox[{"Inp", "*", "k"}]}]}], ";"}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    RowBox[{"fDoublePrimeyMax", "[", 
     RowBox[{"Inp_", ",", "k_"}], "]"}], ":=", 
    RowBox[{
     RowBox[{"-", 
      FractionBox["2", 
       SubscriptBox["O", "tot"]]}], "-", 
     FractionBox[
      RowBox[{"Inp", " ", "k"}], 
      RowBox[{"\[Alpha]", " ", 
       SubscriptBox["O", "tot"]}]], "-", 
     FractionBox["\[Alpha]", 
      RowBox[{"Inp", " ", "k", " ", 
       SubscriptBox["O", "tot"]}]]}]}], ";"}], "\[IndentingNewLine]", 
  RowBox[{"(*", 
   RowBox[{
    RowBox[{
     RowBox[{"fDoublePrimeyMax", "[", 
      RowBox[{"Inp_", ",", "k_"}], "]"}], ":=", 
     RowBox[{"-", 
      FractionBox[
       RowBox[{"2", " ", 
        RowBox[{"(", 
         RowBox[{"\[Alpha]", "+", 
          RowBox[{"Inp", " ", "k"}]}], ")"}]}], 
       RowBox[{"Inp", "  ", "*", "k", "*", "  ", 
        SubscriptBox["O", "tot"]}]]}]}], ";"}], 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"fMax", "[", 
    RowBox[{"Inp_", ",", "k_"}], "]"}], ":=", 
   RowBox[{
    FractionBox[
     RowBox[{
      RowBox[{"-", "2"}], "Inp", " ", "k"}], 
     RowBox[{"\[Alpha]", "-", 
      RowBox[{"Inp", " ", "k"}]}]], "+", 
    RowBox[{
     FractionBox[
      RowBox[{"(", 
       RowBox[{"4", " ", "Inp", " ", "k", " ", "\[Alpha]", " ", 
        SubscriptBox["O", "tot"]}], ")"}], 
      SuperscriptBox[
       RowBox[{"(", 
        RowBox[{"\[Alpha]", "-", 
         RowBox[{"Inp", " ", "k"}]}], ")"}], "2"]], 
     RowBox[{"Log", "[", 
      FractionBox[
       RowBox[{"2", "\[Alpha]"}], 
       RowBox[{"\[Alpha]", "-", 
        RowBox[{"Inp", " ", "k"}]}]], "]"}]}]}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    RowBox[{"Omax", "[", 
     RowBox[{"Inp_", ",", "k_"}], "]"}], ":=", 
    FractionBox[
     RowBox[{"Inp", " ", "k", " ", 
      SubscriptBox["O", "tot"]}], 
     RowBox[{
      RowBox[{"Inp", " ", "k"}], "+", "\[Alpha]"}]]}], ";"}], " ", 
  RowBox[{"(*", " ", 
   RowBox[{
   "location", " ", "of", " ", "the", " ", "mean", " ", "of", " ", "the", " ",
     "conditional", " ", "probability"}], " ", "*)"}], "\[IndentingNewLine]", 
  
  RowBox[{"(*", " ", 
   RowBox[{
   "Define", " ", "the", " ", "laplace", " ", "approximation", " ", "of", " ",
     "the", " ", "exact", " ", "probability"}], " ", "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{
   "works", " ", "okay", " ", "for", " ", "boundary", " ", "near", " ", "1"}],
    " ", "*)"}], "\[IndentingNewLine]", 
  RowBox[{"(*", 
   RowBox[{
    RowBox[{"pLaplace", "[", 
     RowBox[{"y_", ",", "Inp_", ",", "k_", ",", "V_"}], "]"}], ":=", 
    RowBox[{"Exp", "[", 
     RowBox[{"V", 
      RowBox[{"(", 
       RowBox[{
        FractionBox["1", 
         RowBox[{"2", "/", 
          RowBox[{"(", 
           RowBox[{"3", "*", "Inp"}], ")"}]}]], 
        RowBox[{"fDoublePrimeyMax", "[", 
         RowBox[{"Inp", ",", "k"}], "]"}], " ", 
        SuperscriptBox[
         RowBox[{"(", 
          RowBox[{"y", "-", 
           RowBox[{"Omax", "[", 
            RowBox[{"Inp", ",", "k"}], "]"}]}], ")"}], "2"]}], ")"}]}], 
     "]"}]}], "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"pLaplace", "[", 
   RowBox[{"y_", ",", "Inp_", ",", "k_", ",", "V_"}], "]"}], ":=", 
  RowBox[{"Exp", "[", 
   RowBox[{"V", 
    RowBox[{"(", 
     RowBox[{
      FractionBox["1", "2"], 
      RowBox[{"fDoublePrimeyMax", "[", 
       RowBox[{"Inp", ",", "k"}], "]"}], " ", 
      SuperscriptBox[
       RowBox[{"(", 
        RowBox[{"y", "-", 
         RowBox[{"Omax", "[", 
          RowBox[{"Inp", ",", "k"}], "]"}]}], ")"}], "2"]}], ")"}]}], 
   "]"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"pLaplace", "[", 
   RowBox[{"y", ",", "x", ",", "k", ",", "V"}], "]"}], "\[IndentingNewLine]", 
  
  RowBox[{"(*", " ", 
   RowBox[{"Some", " ", "sample", " ", "parameter", " ", "choices"}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    SubscriptBox["O", 
     RowBox[{"tot", " "}]], "=", " ", "1"}], ";", " ", 
   RowBox[{"\[Alpha]", " ", "=", " ", "1"}], ";"}], 
  " "}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"k", " ", "=", " ", "2.001"}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Inp", " ", "=", "0.9"}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"V", " ", "=", " ", "20"}], ";"}], "\[IndentingNewLine]", 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{
   "the", " ", "following", " ", "are", " ", "the", " ", "normalizations", 
    " ", "for", " ", "one", " ", "input"}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"NormPLap", " ", "=", " ", 
   RowBox[{"NIntegrate", "[", 
    RowBox[{
     RowBox[{"pLaplace", "[", 
      RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], ",", 
     RowBox[{"{", 
      RowBox[{"y", ",", "0", ",", "1"}], "}"}]}], "]"}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"NormPCond", " ", "=", " ", 
    RowBox[{"NIntegrate", "[", 
     RowBox[{
      RowBox[{"p", "[", 
       RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], ",", 
      RowBox[{"{", 
       RowBox[{"y", ",", "0", ",", "1"}], "}"}]}], "]"}]}], ";"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{"Recreate", " ", "probability", " ", "comparison"}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{"Plot", "[", 
  RowBox[{
   RowBox[{"{", 
    RowBox[{
     FractionBox[
      RowBox[{"p", "[", 
       RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "NormPCond"], 
     ",", 
     FractionBox[
      RowBox[{"pLaplace", "[", 
       RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "NormPLap"]}], 
    "}"}], ",", 
   RowBox[{"{", 
    RowBox[{"y", ",", "0.0", ",", "1"}], "}"}], ",", 
   RowBox[{"PlotRange", "\[Rule]", "All"}], ",", 
   RowBox[{"PlotLegends", "\[Rule]", 
    RowBox[{"{", 
     RowBox[{
     "\"\<P(\!\(\*SuperscriptBox[\(O\), \(*\)]\)|I,\!\(\*SubscriptBox[\(k\), \
\(11\)]\))\>\"", ",", "\"\<\!\(\*SubscriptBox[\(P\), \(Laplace\)]\)\>\""}], 
     " ", "}"}]}], ",", 
   RowBox[{"PlotStyle", "\[Rule]", 
    RowBox[{"{", 
     RowBox[{
      RowBox[{"{", 
       RowBox[{
        RowBox[{"Thickness", "[", "0.005", "]"}], ",", "Blue"}], "}"}], ",", 
      RowBox[{"{", 
       RowBox[{"Red", ",", "Dashed", ",", 
        RowBox[{"Thickness", "[", "0.004", "]"}]}], "}"}], ",", 
      RowBox[{"{", 
       RowBox[{
        RowBox[{"Thickness", "[", "0.005", "]"}], ",", "Green"}], "}"}]}], 
     "}"}]}], ",", 
   RowBox[{"TicksStyle", "\[Rule]", 
    RowBox[{"Directive", "[", 
     RowBox[{"\"\<Label\>\"", ",", "30"}], "]"}]}], ",", 
   RowBox[{"Ticks", "\[Rule]", 
    RowBox[{"{", "Automatic", "}"}]}], ",", 
   RowBox[{"LabelStyle", "\[Rule]", 
    RowBox[{"{", 
     RowBox[{
      RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
      RowBox[{"FontSize", "\[Rule]", "24"}]}], "}"}]}], ",", 
   RowBox[{"FrameLabel", "\[Rule]", 
    RowBox[{"{", 
     RowBox[{
     "\"\<\!\(\*SuperscriptBox[\(O\), \(*\)]\)\>\"", ",", 
      "\"\<Probability \>\""}], "}"}]}], ",", 
   RowBox[{"Frame", "\[Rule]", "True"}], ",", 
   RowBox[{"FrameStyle", "\[Rule]", 
    RowBox[{"Directive", "[", "Black", "]"}]}]}], 
  "]"}], "\[IndentingNewLine]"}], "Input",ExpressionUUID->"59b50278-88fb-423d-\
ac0f-05dbe0426058"],

Cell[BoxData[
 SuperscriptBox["\[ExponentialE]", 
  RowBox[{
   FractionBox["1", "2"], " ", "V", " ", 
   RowBox[{"(", 
    RowBox[{
     RowBox[{"-", 
      FractionBox["2", 
       SubscriptBox["O", "tot"]]}], "-", 
     FractionBox[
      RowBox[{"k", " ", "x"}], 
      RowBox[{"\[Alpha]", " ", 
       SubscriptBox["O", "tot"]}]], "-", 
     FractionBox["\[Alpha]", 
      RowBox[{"k", " ", "x", " ", 
       SubscriptBox["O", "tot"]}]]}], ")"}], " ", 
   SuperscriptBox[
    RowBox[{"(", 
     RowBox[{"y", "-", 
      FractionBox[
       RowBox[{"k", " ", "x", " ", 
        SubscriptBox["O", "tot"]}], 
       RowBox[{
        RowBox[{"k", " ", "x"}], "+", "\[Alpha]"}]]}], ")"}], 
    "2"]}]]], "Output",
 FontSize->18,ExpressionUUID->"1a9d04e4-f7d1-471b-9d40-5b4f532c7aa0"],

Cell[BoxData[
 TemplateBox[{GraphicsBox[{{{{}, {}, 
       TagBox[{
         Directive[
          Opacity[1.], 
          AbsoluteThickness[1.6], 
          Thickness[0.005], 
          RGBColor[0, 0, 1]], 
         LineBox[CompressedData["
1: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
VPlTFH5GOCBoO8vS+vocln1zzgjKOeB/z5YtiA==
          "]]}, 
        Annotation[#, "Charting`Private`Tag$1275#1"]& ], 
       TagBox[{
         Directive[
          Opacity[1.], 
          AbsoluteThickness[1.6], 
          RGBColor[1, 0, 0], 
          Dashing[{Small, Small}], 
          Thickness[0.004]], 
         LineBox[CompressedData["
1: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
OAd+KPnXQRh50poDZw4yepZ9fC51EhemxhVdj+Vgf0tXhQJzF7Er8txLbr8c
OBq2mDo96SaaN/LwSJK5yBiM05VO7iMqj5sZt2rlQqWo65CTQD+RZ1KTY22S
i5uhJ4uevOwnYmofXS4MyIUHW7tWsOkA8chx8O2Z+Vy83rXt3/YzQ4RZsuzS
LH0ehg4HcwZXDhGG/T4nfPjyQHbv3v2NHCbOyylQOxXzMOFqbFi04zMhshi/
50ZIHoo466/ISowQW4ToHjOm5WF+1+iPKasRgl35cmNidR6O7G01eJ8/QnyP
Z7NYWsrDvfiYOxbnRolGPcsse6V8WOz0L7hpO0Y8bD6CoJV8iFIaPTo3TRAc
ktnjH5kLYKh364L7jQki1kvK85dgAWZYL5ffzp0gWlX+69U7VYDTotcKMwwn
iX0NPPd3BxSA5/fw4YXaKWK4djE+Q64QN49pjXxsmSVs/jM5T1UuhIj1jt+8
h+cI9hcTXzcbFEK5WlWmI3KOOHJ6kLR1KwRlVTs6w2ae8Kuq70NvIU5Np5Oe
0ouEUnkcR5NdEfLDXvW8c1gmhnYIZ//1LkLJZEsqY+kyYf00TFcqpggGPurh
Z+i+EFHwS/BrKALCqzOdXL4Qv4qfKlwRLEZxLZm9LfQrkVqgazNZXAyJqwlr
7aurxGmBbkH+tmJcTh7+NHzqGzFge7Hy3EQxCn4XS6wP/EawHDm7KYWjBLla
QRsjjn0nbuTKJlnrl6A023zZ780asSWLc2jdulLkn9d+N+j0i7jq+UuTylsK
lV6yz2f+FxFtNN5QI14KnU29cZ1XfhPim/PzPXRKkSqS/uag/B9Cwcbgzea0
UsQp7l9oZ/9HyIZpx/vvL4NRhZrC8+frcNmtccRFvgyGdTWT+rXr8MT6hNDD
U2WQjmykP8xOj3KVff6XdcuwO8vTbiqIHip//jpte1GG2ohDIikl63FdL/5a
wlAZctzdrTnBCOdzW4ODp8uwYe2Xn54/I+JlfTvcV8qwXY0iRZ1hxByHnbIZ
azk4B3zHE0OZYFN6/shB2XJMts14BrOzwHPHKk+RVzniXwQPsEmxI4X9rlpy
UDnmj2r4/wlhR+vakMfbmHJIh8X4WDFRwNtaR++UX47/Yq1Ff1EpiHIKWzo1
Xo7HIuF6gpkbkTOq2PjpWAUYlcMfKLRwouuM07KXUgX8eQLejXNy4VtKCa/K
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
v3+LuL5jUi3Lzwv/A86WTtk=
          "]]}, 
        Annotation[#, "Charting`Private`Tag$1275#2"]& ]}}, {}, {}}, {
    DisplayFunction -> Identity, Ticks -> {Automatic, Automatic}, 
     AxesOrigin -> {0, 0}, FrameTicks -> {{Automatic, 
        Charting`ScaledFrameTicks[{Identity, Identity}]}, {Automatic, 
        Charting`ScaledFrameTicks[{Identity, Identity}]}}, 
     GridLines -> {None, None}, DisplayFunction -> Identity, 
     PlotRangePadding -> {{
        Scaled[0.02], 
        Scaled[0.02]}, {
        Scaled[0.05], 
        Scaled[0.05]}}, PlotRangeClipping -> True, ImagePadding -> All, 
     DisplayFunction -> Identity, AspectRatio -> 
     NCache[GoldenRatio^(-1), 0.6180339887498948], Axes -> {True, True}, 
     AxesLabel -> {None, None}, AxesOrigin -> {0, 0}, DisplayFunction :> 
     Identity, Frame -> {{True, True}, {True, True}}, FrameLabel -> {{
        FormBox["\"Probability \"", TraditionalForm], None}, {
        FormBox[
        "\"\\!\\(\\*SuperscriptBox[\\(O\\), \\(*\\)]\\)\"", TraditionalForm], 
        None}}, FrameStyle -> Directive[
       GrayLevel[0]], 
     FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
     GridLines -> {None, None}, GridLinesStyle -> Directive[
       GrayLevel[0.5, 0.4]], 
     LabelStyle -> {FontFamily -> "Helvetica", FontSize -> 24}, 
     Method -> {
      "DefaultBoundaryStyle" -> Automatic, "DefaultMeshStyle" -> 
       AbsolutePointSize[6], "ScalingFunctions" -> None, 
       "CoordinatesToolOptions" -> {"DisplayFunction" -> ({
           (Identity[#]& )[
            Part[#, 1]], 
           (Identity[#]& )[
            Part[#, 2]]}& ), "CopiedValueFunction" -> ({
           (Identity[#]& )[
            Part[#, 1]], 
           (Identity[#]& )[
            Part[#, 2]]}& )}}, PlotRange -> {All, All}, PlotRangeClipping -> 
     True, PlotRangePadding -> {{Automatic, Automatic}, {
       Automatic, Automatic}}, Ticks -> {Automatic, Automatic}, TicksStyle -> 
     Directive["Label", 30]}],FormBox[
    FormBox[
     TemplateBox[{
      "\"P(\\!\\(\\*SuperscriptBox[\\(O\\), \
\\(*\\)]\\)|I,\\!\\(\\*SubscriptBox[\\(k\\), \\(11\\)]\\))\"", 
       "\"\\!\\(\\*SubscriptBox[\\(P\\), \\(Laplace\\)]\\)\""}, "LineLegend", 
      DisplayFunction -> (FormBox[
        StyleBox[
         StyleBox[
          PaneBox[
           TagBox[
            GridBox[{{
               TagBox[
                GridBox[{{
                   GraphicsBox[{{
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.5], 
                    Opacity[1.], 
                    AbsoluteThickness[1.6], 
                    Thickness[0.045], 
                    RGBColor[0, 0, 1]], {
                    LineBox[{{0, 10}, {40, 10}}]}}, {
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.5], 
                    Opacity[1.], 
                    AbsoluteThickness[1.6], 
                    Thickness[0.045], 
                    RGBColor[0, 0, 1]], {}}}, AspectRatio -> Full, 
                    ImageSize -> {40, 10}, PlotRangePadding -> None, 
                    ImagePadding -> Automatic, 
                    BaselinePosition -> (Scaled[-0.33399999999999996`] -> 
                    Baseline)], #}, {
                   GraphicsBox[{{
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.5], 
                    Opacity[1.], 
                    AbsoluteThickness[1.6], 
                    RGBColor[1, 0, 0], 
                    Dashing[{Small, Small}], 
                    Thickness[0.036000000000000004`]], {
                    LineBox[{{0, 10}, {40, 10}}]}}, {
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.5], 
                    Opacity[1.], 
                    AbsoluteThickness[1.6], 
                    RGBColor[1, 0, 0], 
                    Dashing[{Small, Small}], 
                    Thickness[0.036000000000000004`]], {}}}, AspectRatio -> 
                    Full, ImageSize -> {40, 10}, PlotRangePadding -> None, 
                    ImagePadding -> Automatic, 
                    BaselinePosition -> (Scaled[-0.33399999999999996`] -> 
                    Baseline)], #2}}, 
                 GridBoxAlignment -> {
                  "Columns" -> {Center, Left}, "Rows" -> {{Baseline}}}, 
                 AutoDelete -> False, 
                 GridBoxDividers -> {
                  "Columns" -> {{False}}, "Rows" -> {{False}}}, 
                 GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}},
                  GridBoxSpacings -> {
                  "Columns" -> {{0.5}}, "Rows" -> {{0.8}}}], "Grid"]}}, 
             GridBoxAlignment -> {"Columns" -> {{Left}}, "Rows" -> {{Top}}}, 
             AutoDelete -> False, 
             GridBoxItemSize -> {
              "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}}, 
             GridBoxSpacings -> {"Columns" -> {{1}}, "Rows" -> {{0}}}], 
            "Grid"], Alignment -> Left, AppearanceElements -> None, 
           ImageMargins -> {{5, 5}, {5, 5}}, ImageSizeAction -> 
           "ResizeToFit"], LineIndent -> 0, StripOnInput -> False], {
         FontFamily -> "Helvetica", FontSize -> 24, FontFamily -> "Arial"}, 
         Background -> Automatic, StripOnInput -> False], TraditionalForm]& ),
       InterpretationFunction :> (RowBox[{"LineLegend", "[", 
         RowBox[{
           RowBox[{"{", 
             RowBox[{
               RowBox[{"Directive", "[", 
                 RowBox[{
                   RowBox[{"Opacity", "[", "1.`", "]"}], ",", 
                   RowBox[{"AbsoluteThickness", "[", "1.6`", "]"}], ",", 
                   RowBox[{"Thickness", "[", "0.005`", "]"}], ",", 
                   InterpretationBox[
                    ButtonBox[
                    TooltipBox[
                    GraphicsBox[{{
                    GrayLevel[0], 
                    RectangleBox[{0, 0}]}, {
                    GrayLevel[0], 
                    RectangleBox[{1, -1}]}, {
                    RGBColor[0, 0, 1], 
                    RectangleBox[{0, -1}, {2, 1}]}}, DefaultBaseStyle -> 
                    "ColorSwatchGraphics", AspectRatio -> 1, Frame -> True, 
                    FrameStyle -> RGBColor[0., 0., 0.6666666666666666], 
                    FrameTicks -> None, PlotRangePadding -> None, ImageSize -> 
                    Dynamic[{
                    Automatic, 
                    1.35 (CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[
                    Magnification])}]], "RGBColor[0, 0, 1]"], Appearance -> 
                    None, BaseStyle -> {}, BaselinePosition -> Baseline, 
                    DefaultBaseStyle -> {}, ButtonFunction :> 
                    With[{Typeset`box$ = EvaluationBox[]}, 
                    If[
                    Not[
                    AbsoluteCurrentValue["Deployed"]], 
                    SelectionMove[Typeset`box$, All, Expression]; 
                    FrontEnd`Private`$ColorSelectorInitialAlpha = 1; 
                    FrontEnd`Private`$ColorSelectorInitialColor = 
                    RGBColor[0, 0, 1]; 
                    FrontEnd`Private`$ColorSelectorUseMakeBoxes = True; 
                    MathLink`CallFrontEnd[
                    FrontEnd`AttachCell[Typeset`box$, 
                    FrontEndResource["RGBColorValueSelector"], {
                    0, {Left, Bottom}}, {Left, Top}, 
                    "ClosingActions" -> {
                    "SelectionDeparture", "ParentChanged", 
                    "EvaluatorQuit"}]]]], BaseStyle -> Inherited, Evaluator -> 
                    Automatic, Method -> "Preemptive"], 
                    RGBColor[0, 0, 1], Editable -> False, Selectable -> 
                    False]}], "]"}], ",", 
               RowBox[{"Directive", "[", 
                 RowBox[{
                   RowBox[{"Opacity", "[", "1.`", "]"}], ",", 
                   RowBox[{"AbsoluteThickness", "[", "1.6`", "]"}], ",", 
                   InterpretationBox[
                    ButtonBox[
                    TooltipBox[
                    GraphicsBox[{{
                    GrayLevel[0], 
                    RectangleBox[{0, 0}]}, {
                    GrayLevel[0], 
                    RectangleBox[{1, -1}]}, {
                    RGBColor[1, 0, 0], 
                    RectangleBox[{0, -1}, {2, 1}]}}, DefaultBaseStyle -> 
                    "ColorSwatchGraphics", AspectRatio -> 1, Frame -> True, 
                    FrameStyle -> RGBColor[0.6666666666666666, 0., 0.], 
                    FrameTicks -> None, PlotRangePadding -> None, ImageSize -> 
                    Dynamic[{
                    Automatic, 
                    1.35 (CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[
                    Magnification])}]], "RGBColor[1, 0, 0]"], Appearance -> 
                    None, BaseStyle -> {}, BaselinePosition -> Baseline, 
                    DefaultBaseStyle -> {}, ButtonFunction :> 
                    With[{Typeset`box$ = EvaluationBox[]}, 
                    If[
                    Not[
                    AbsoluteCurrentValue["Deployed"]], 
                    SelectionMove[Typeset`box$, All, Expression]; 
                    FrontEnd`Private`$ColorSelectorInitialAlpha = 1; 
                    FrontEnd`Private`$ColorSelectorInitialColor = 
                    RGBColor[1, 0, 0]; 
                    FrontEnd`Private`$ColorSelectorUseMakeBoxes = True; 
                    MathLink`CallFrontEnd[
                    FrontEnd`AttachCell[Typeset`box$, 
                    FrontEndResource["RGBColorValueSelector"], {
                    0, {Left, Bottom}}, {Left, Top}, 
                    "ClosingActions" -> {
                    "SelectionDeparture", "ParentChanged", 
                    "EvaluatorQuit"}]]]], BaseStyle -> Inherited, Evaluator -> 
                    Automatic, Method -> "Preemptive"], 
                    RGBColor[1, 0, 0], Editable -> False, Selectable -> 
                    False], ",", 
                   RowBox[{"Dashing", "[", 
                    RowBox[{"{", 
                    RowBox[{"Small", ",", "Small"}], "}"}], "]"}], ",", 
                   RowBox[{"Thickness", "[", "0.004`", "]"}]}], "]"}]}], 
             "}"}], ",", 
           RowBox[{"{", 
             RowBox[{#, ",", #2}], "}"}], ",", 
           RowBox[{"LegendMarkers", "\[Rule]", "None"}], ",", 
           RowBox[{"LabelStyle", "\[Rule]", 
             RowBox[{"{", 
               RowBox[{
                 RowBox[{"FontFamily", "\[Rule]", "\"Helvetica\""}], ",", 
                 RowBox[{"FontSize", "\[Rule]", "24"}]}], "}"}]}], ",", 
           RowBox[{"LegendLayout", "\[Rule]", "\"Column\""}]}], "]"}]& ), 
      Editable -> True], TraditionalForm], TraditionalForm]},
  "Legended",
  DisplayFunction->(GridBox[{{
      TagBox[
       ItemBox[
        PaneBox[
         TagBox[#, "SkipImageSizeLevel"], Alignment -> {Center, Baseline}, 
         BaselinePosition -> Baseline], DefaultBaseStyle -> "Labeled"], 
       "SkipImageSizeLevel"], 
      ItemBox[#2, DefaultBaseStyle -> "LabeledLabel"]}}, 
    GridBoxAlignment -> {"Columns" -> {{Center}}, "Rows" -> {{Center}}}, 
    AutoDelete -> False, GridBoxItemSize -> Automatic, 
    BaselinePosition -> {1, 1}]& ),
  Editable->True,
  InterpretationFunction->(RowBox[{"Legended", "[", 
     RowBox[{#, ",", 
       RowBox[{"Placed", "[", 
         RowBox[{#2, ",", "After"}], "]"}]}], "]"}]& )]], "Output",
 FontSize->18,ExpressionUUID->"d428bd41-3b99-4772-ab4d-b95c393edefe"]
}, Open  ]],

Cell[CellGroupData[{

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{
   "Reproduce", " ", "corrected", " ", "conditional", " ", "probability", " ",
     "in", " ", 
    RowBox[{"java", "."}]}], " ", "*)"}], "\[IndentingNewLine]", " ", 
  RowBox[{"NIntegrate", "[", 
   RowBox[{
    RowBox[{"pLaplace", "[", 
     RowBox[{"y", ",", "0.505", ",", "k", ",", "V"}], "]"}], ",", 
    RowBox[{"{", 
     RowBox[{"y", ",", "0", ",", "1"}], "}"}]}], "]"}]}]], "Input",ExpressionU\
UID->"e7e6f8c3-6906-4f8c-90c6-6075b72629d4"],

Cell[BoxData["0.27427265066546813`"], "Output",ExpressionUUID->"2431798e-7361-4a51-b59b-68847881a265"]
}, Open  ]],

Cell[BoxData[""], "Input",ExpressionUUID->"04447605-31e0-4f7e-9e6e-644f292ca94f"],

Cell[BoxData[
 RowBox[{"pLaplace", "[", 
  RowBox[{"0.505", ",", "0.505", ",", "3", ",", "20"}], "]"}]], "Input",Expres\
sionUUID->"74942d00-9ac9-474b-bee5-c97abe8052d3"],

Cell[CellGroupData[{

Cell[BoxData[
 RowBox[{"\[IndentingNewLine]", 
  FractionBox[
   RowBox[{"pLaplace", "[", 
    RowBox[{"0.055", ",", "0.055", ",", "3", ",", "20"}], "]"}], 
   RowBox[{"NIntegrate", "[", 
    RowBox[{
     RowBox[{"pLaplace", "[", 
      RowBox[{"y", ",", "0.055", ",", "k", ",", "V"}], "]"}], ",", 
     RowBox[{"{", 
      RowBox[{"y", ",", "0", ",", "1"}], "}"}]}], "]"}]]}]], "Input",Expressio\
nUUID->"0cabf08d-530f-4c5e-978f-9f5215542f98"],

Cell[BoxData["2.1692284303726828`"], "Output",ExpressionUUID->"b638d9e7-0a8d-493b-bb9c-3b3701726254"]
}, Open  ]]
}, Open  ]],

Cell[CellGroupData[{

Cell["Plot Probability Maxima", "Section",ExpressionUUID->"b23bf8b3-9b43-4405-a8a2-3e35551c44b4"],

Cell[BoxData[
 RowBox[{"(*", " ", 
  RowBox[{
   RowBox[{
   "This", " ", "shows", " ", "that", " ", "the", " ", "maxima", " ", 
    "disagrees", " ", "a", " ", "lot", " ", "near", " ", "the", " ", 
    "boundary", " ", "y"}], " ", "=", " ", 
   RowBox[{
    RowBox[{
    "0", " ", "unless", " ", "include", " ", "the", " ", "linear", " ", 
     "correction", " ", "in", " ", "the", " ", "variance", " ", "or", " ", 
     "fDoublePrimeyMax"}], " ", "=", " ", 
    RowBox[{
     RowBox[{"-", "k"}], "*", "input"}]}]}], " ", "*)"}]], "Input",ExpressionU\
UID->"3b69e0d5-5fdb-4654-aaed-7a547a4c0b5e"],

Cell[CellGroupData[{

Cell[BoxData[{
 RowBox[{
  RowBox[{
   RowBox[{"maxPy", "[", 
    RowBox[{"k_", ",", "Inp_"}], "]"}], ":=", " ", 
   RowBox[{"Max", "[", 
    RowBox[{
     RowBox[{"Function", "[", 
      RowBox[{"y", ",", 
       FractionBox[
        RowBox[{"p", "[", 
         RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], 
        RowBox[{"NIntegrate", "[", 
         RowBox[{
          RowBox[{"p", "[", 
           RowBox[{"yp", ",", "Inp", ",", "k", ",", "V"}], "]"}], ",", 
          RowBox[{"{", 
           RowBox[{"yp", ",", "0", ",", "1"}], "}"}]}], "]"}]]}], "]"}], "/@", 
     RowBox[{"Range", "[", 
      RowBox[{"0.005", ",", "0.995", ",", "0.01"}], "]"}]}], "]"}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"maxPyLap", "[", 
    RowBox[{"k_", ",", "Inp_"}], "]"}], ":=", " ", 
   RowBox[{"Max", "[", 
    RowBox[{
     RowBox[{"Function", "[", 
      RowBox[{"y", ",", 
       FractionBox[
        RowBox[{"pLaplace", "[", 
         RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], 
        RowBox[{"NIntegrate", "[", 
         RowBox[{
          RowBox[{"pLaplace", "[", 
           RowBox[{"yp", ",", "Inp", ",", "k", ",", "V"}], "]"}], ",", 
          RowBox[{"{", 
           RowBox[{"yp", ",", "0", ",", "1"}], "}"}]}], "]"}]]}], "]"}], "/@", 
     RowBox[{"Range", "[", 
      RowBox[{"0.005", ",", "0.995", ",", "0.01"}], "]"}]}], "]"}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"maxPyList", "[", "k_", "]"}], ":=", " ", 
   RowBox[{
    RowBox[{"Function", "[", 
     RowBox[{"Inp", ",", 
      RowBox[{"maxPy", "[", 
       RowBox[{"k", ",", "Inp"}], "]"}]}], "]"}], "/@", 
    RowBox[{"Range", "[", 
     RowBox[{"0.005", ",", "0.995", ",", "0.025"}], "]"}]}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{
    RowBox[{"maxPyLapList", "[", "k_", "]"}], " ", ":=", " ", 
    RowBox[{
     RowBox[{"Function", "[", 
      RowBox[{"Inp", ",", 
       RowBox[{"maxPyLap", "[", 
        RowBox[{"k", ",", "Inp"}], "]"}]}], "]"}], "/@", 
     RowBox[{"Range", "[", 
      RowBox[{"0.005", ",", "0.995", ",", "0.025"}], "]"}]}]}], ";"}], 
  "\[IndentingNewLine]"}], "\[IndentingNewLine]", 
 RowBox[{"ListLinePlot", "[", 
  RowBox[{
   RowBox[{"{", 
    RowBox[{
     RowBox[{"maxPyList", "[", "3", "]"}], ",", 
     RowBox[{"maxPyLapList", "[", "3", "]"}]}], "}"}], ",", 
   RowBox[{"PlotLegends", "\[Rule]", 
    RowBox[{"{", 
     RowBox[{"\"\<Max Exact\>\"", ",", "\"\<Max Lap\>\""}], " ", "}"}]}], ",", 
   RowBox[{"PlotStyle", "\[Rule]", 
    RowBox[{"{", 
     RowBox[{
      RowBox[{"{", 
       RowBox[{
        RowBox[{"Thickness", "[", "0.005", "]"}], ",", "Blue"}], "}"}], ",", 
      RowBox[{"{", 
       RowBox[{"Red", ",", "Dashed", ",", 
        RowBox[{"Thickness", "[", "0.004", "]"}]}], "}"}]}], "}"}]}], ",", 
   RowBox[{"TicksStyle", "\[Rule]", 
    RowBox[{"Directive", "[", 
     RowBox[{"\"\<Label\>\"", ",", "30"}], "]"}]}], ",", 
   RowBox[{"Ticks", "\[Rule]", 
    RowBox[{"{", "Automatic", "}"}]}], ",", 
   RowBox[{"LabelStyle", "\[Rule]", 
    RowBox[{"{", 
     RowBox[{
      RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
      RowBox[{"FontSize", "\[Rule]", "24"}]}], "}"}]}], ",", 
   RowBox[{"FrameLabel", "\[Rule]", 
    RowBox[{"{", 
     RowBox[{"\"\<I\>\"", ",", "\"\<Max\>\""}], "}"}]}], ",", 
   RowBox[{"Frame", "\[Rule]", "True"}], ",", 
   RowBox[{"FrameStyle", "\[Rule]", 
    RowBox[{"Directive", "[", "Black", "]"}]}], ",", 
   RowBox[{"PlotRange", "\[Rule]", "All"}]}], "]"}]}], "Input",ExpressionUUID-\
>"e12af993-1271-45e7-b7d8-09cdf98a46e7"],

Cell[BoxData[
 TemplateBox[{GraphicsBox[{{}, {{{}, {}, {
        Hue[0.67, 0.6, 0.6], 
        Directive[
         PointSize[0.016666666666666666`], 
         AbsoluteThickness[1.6], 
         Thickness[0.005], 
         RGBColor[0, 0, 1]], 
        LineBox[CompressedData["
1:eJxTTMoPSmViYGDQAGIQDQEf7CVZd5RUPnVwgAo41NuXRn5kV4XyORxCfnrm
rg9RgPIFHNqN/A7uXysD5Ys4OG80tjjZIgXlSzgEpbsuNNSUhPJlHCzezbY7
7iIB5Ss4fNO65PhkqTiUr+QQt9PGKc0CxldxSMmZ5VX0VQzKV3MojXzbK3wQ
xtdwuCrlFZS8HMbXcmgQfpN5ZzKMr+PAqXb02sdeGF/PYYaGpO8kuLyBw6Fb
DkrRc2B8Q4eO67JTfRfC+EYOnYvP1z88AOMbO+Qainj4X4DxTRy+eHazL30L
45s61K4qDgsRgLnfzOHy+6PRyvowvrmDftEGG2E/GN/CwWrpovzofBjf0uHG
i+mLD02G8a0cckJsHj7cBeNbO1x6kHcz9DGMb+OgvJpX6jEXLDxtHQ4WWT6a
qAfj2zkUf8xkOeYF49s7zE/f7spZD+M7OLxbu1VIbS2U3+DgcOTlo87EMzB5
R4dyHbOJN3/B5B0d2NLD3l+Bx6eTw6YTGefbnaD8BieHB/eNeDLrYfLODj8r
oxJnzIbJOzt0JX4/uPcyTN7FoXda/QbZT5IOAO/JnY4=
         "]]}, {
        Hue[0.9060679774997897, 0.6, 0.6], 
        Directive[
         PointSize[0.016666666666666666`], 
         AbsoluteThickness[1.6], 
         RGBColor[1, 0, 0], 
         Dashing[{Small, Small}], 
         Thickness[0.004]], 
        LineBox[CompressedData["
1:eJxTTMoPSmViYGDQAGIQDQEf7J1dtbOO/rRxgAo4uGxKeXr8vAqUz+EwL5hl
g5quApQv4KA1XUacr18GyhdxWNV1ap9OrBSUL+GQaHh05XdeSShfxqG+9v7q
45YSUL6CQ2K9+C6pmeJQvpKDe0Giwg5DGF/FQbNhK2v5OzEoX83BzZvt9e7D
ML6Gw6mlucGvlsL4Wg5buPcLFkyC8XUcDvPVfTvTC+PrOYjKzZ15czKMb+DA
9GYy34pZML6hw18Zbq50uHlGDu88/M+17IPxjR2kWoxde67A+CYOT/OfN2c8
hvFNHeTXNcprccHcb+bgUMrV36UD45s78NxbuuyvL4xv4fCyv3+CawGMb+kw
8Z6k7pHJML6Vg/3ONQk7t8P41g4vna+sKLoF49s4BNWt3VnwE8a3dWDnDVES
VICFr53D4VfpU2QDYXx7B4fjrM8mV8L4Dg72nkoL5k+B8hscHPJmdG9wOAGT
d3To5v3VwvQZJu/oIJSizf0VHp9ODu9viKUGu0P5DU4Os2cw3M9Pgsk7O8Sb
GshEzYTJOzto+IlvU98Bk3dx6Gx/1D75jaQDAIjHlb0=
         
         "]]}}}, {}, {}, {}, {}}, {
    DisplayFunction -> Identity, PlotRangePadding -> {{
        Scaled[0.02], 
        Scaled[0.02]}, {
        Scaled[0.02], 
        Scaled[0.05]}}, AxesOrigin -> {0.390625, 0}, 
     PlotRange -> {{1., 40.}, {0, 33.79276904092166}}, PlotRangeClipping -> 
     True, ImagePadding -> All, DisplayFunction -> Identity, AspectRatio -> 
     NCache[GoldenRatio^(-1), 0.6180339887498948], Axes -> {True, True}, 
     AxesLabel -> {None, None}, AxesOrigin -> {0.390625, 0}, DisplayFunction :> 
     Identity, Frame -> {{True, True}, {True, True}}, FrameLabel -> {{
        FormBox["\"Max\"", TraditionalForm], None}, {
        FormBox["\"I\"", TraditionalForm], None}}, FrameStyle -> Directive[
       GrayLevel[0]], 
     FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
     GridLines -> {None, None}, GridLinesStyle -> Directive[
       GrayLevel[0.5, 0.4]], 
     LabelStyle -> {FontFamily -> "Helvetica", FontSize -> 24}, 
     Method -> {"CoordinatesToolOptions" -> {"DisplayFunction" -> ({
           (Identity[#]& )[
            Part[#, 1]], 
           (Identity[#]& )[
            Part[#, 2]]}& ), "CopiedValueFunction" -> ({
           (Identity[#]& )[
            Part[#, 1]], 
           (Identity[#]& )[
            Part[#, 2]]}& )}}, 
     PlotRange -> {{1., 40.}, {0, 33.79276904092166}}, PlotRangeClipping -> 
     True, PlotRangePadding -> {{
        Scaled[0.02], 
        Scaled[0.02]}, {
        Scaled[0.02], 
        Scaled[0.05]}}, Ticks -> {Automatic, Automatic}, TicksStyle -> 
     Directive["Label", 30]}],FormBox[
    FormBox[
     TemplateBox[{"\"Max Exact\"", "\"Max Lap\""}, "LineLegend", 
      DisplayFunction -> (FormBox[
        StyleBox[
         StyleBox[
          PaneBox[
           TagBox[
            GridBox[{{
               TagBox[
                GridBox[{{
                   GraphicsBox[{{
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.15], 
                    AbsoluteThickness[1.6], 
                    Thickness[0.045], 
                    RGBColor[0, 0, 1]], {
                    LineBox[{{0, 10}, {40, 10}}]}}, {
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.15], 
                    AbsoluteThickness[1.6], 
                    Thickness[0.045], 
                    RGBColor[0, 0, 1]], {}}}, AspectRatio -> Full, 
                    ImageSize -> {40, 10}, PlotRangePadding -> None, 
                    ImagePadding -> Automatic, 
                    BaselinePosition -> (Scaled[-0.33399999999999996`] -> 
                    Baseline)], #}, {
                   GraphicsBox[{{
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.15], 
                    AbsoluteThickness[1.6], 
                    RGBColor[1, 0, 0], 
                    Dashing[{Small, Small}], 
                    Thickness[0.036000000000000004`]], {
                    LineBox[{{0, 10}, {40, 10}}]}}, {
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.15], 
                    AbsoluteThickness[1.6], 
                    RGBColor[1, 0, 0], 
                    Dashing[{Small, Small}], 
                    Thickness[0.036000000000000004`]], {}}}, AspectRatio -> 
                    Full, ImageSize -> {40, 10}, PlotRangePadding -> None, 
                    ImagePadding -> Automatic, 
                    BaselinePosition -> (Scaled[-0.33399999999999996`] -> 
                    Baseline)], #2}}, 
                 GridBoxAlignment -> {
                  "Columns" -> {Center, Left}, "Rows" -> {{Baseline}}}, 
                 AutoDelete -> False, 
                 GridBoxDividers -> {
                  "Columns" -> {{False}}, "Rows" -> {{False}}}, 
                 GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}},
                  GridBoxSpacings -> {
                  "Columns" -> {{0.5}}, "Rows" -> {{0.8}}}], "Grid"]}}, 
             GridBoxAlignment -> {"Columns" -> {{Left}}, "Rows" -> {{Top}}}, 
             AutoDelete -> False, 
             GridBoxItemSize -> {
              "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}}, 
             GridBoxSpacings -> {"Columns" -> {{1}}, "Rows" -> {{0}}}], 
            "Grid"], Alignment -> Left, AppearanceElements -> None, 
           ImageMargins -> {{5, 5}, {5, 5}}, ImageSizeAction -> 
           "ResizeToFit"], LineIndent -> 0, StripOnInput -> False], {
         FontFamily -> "Helvetica", FontSize -> 24, FontFamily -> "Arial"}, 
         Background -> Automatic, StripOnInput -> False], TraditionalForm]& ),
       InterpretationFunction :> (RowBox[{"LineLegend", "[", 
         RowBox[{
           RowBox[{"{", 
             RowBox[{
               RowBox[{"Directive", "[", 
                 RowBox[{
                   RowBox[{"PointSize", "[", "0.016666666666666666`", "]"}], 
                   ",", 
                   RowBox[{"AbsoluteThickness", "[", "1.6`", "]"}], ",", 
                   RowBox[{"Thickness", "[", "0.005`", "]"}], ",", 
                   InterpretationBox[
                    ButtonBox[
                    TooltipBox[
                    GraphicsBox[{{
                    GrayLevel[0], 
                    RectangleBox[{0, 0}]}, {
                    GrayLevel[0], 
                    RectangleBox[{1, -1}]}, {
                    RGBColor[0, 0, 1], 
                    RectangleBox[{0, -1}, {2, 1}]}}, DefaultBaseStyle -> 
                    "ColorSwatchGraphics", AspectRatio -> 1, Frame -> True, 
                    FrameStyle -> RGBColor[0., 0., 0.6666666666666666], 
                    FrameTicks -> None, PlotRangePadding -> None, ImageSize -> 
                    Dynamic[{
                    Automatic, 
                    1.35 (CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[
                    Magnification])}]], "RGBColor[0, 0, 1]"], Appearance -> 
                    None, BaseStyle -> {}, BaselinePosition -> Baseline, 
                    DefaultBaseStyle -> {}, ButtonFunction :> 
                    With[{Typeset`box$ = EvaluationBox[]}, 
                    If[
                    Not[
                    AbsoluteCurrentValue["Deployed"]], 
                    SelectionMove[Typeset`box$, All, Expression]; 
                    FrontEnd`Private`$ColorSelectorInitialAlpha = 1; 
                    FrontEnd`Private`$ColorSelectorInitialColor = 
                    RGBColor[0, 0, 1]; 
                    FrontEnd`Private`$ColorSelectorUseMakeBoxes = True; 
                    MathLink`CallFrontEnd[
                    FrontEnd`AttachCell[Typeset`box$, 
                    FrontEndResource["RGBColorValueSelector"], {
                    0, {Left, Bottom}}, {Left, Top}, 
                    "ClosingActions" -> {
                    "SelectionDeparture", "ParentChanged", 
                    "EvaluatorQuit"}]]]], BaseStyle -> Inherited, Evaluator -> 
                    Automatic, Method -> "Preemptive"], 
                    RGBColor[0, 0, 1], Editable -> False, Selectable -> 
                    False]}], "]"}], ",", 
               RowBox[{"Directive", "[", 
                 RowBox[{
                   RowBox[{"PointSize", "[", "0.016666666666666666`", "]"}], 
                   ",", 
                   RowBox[{"AbsoluteThickness", "[", "1.6`", "]"}], ",", 
                   InterpretationBox[
                    ButtonBox[
                    TooltipBox[
                    GraphicsBox[{{
                    GrayLevel[0], 
                    RectangleBox[{0, 0}]}, {
                    GrayLevel[0], 
                    RectangleBox[{1, -1}]}, {
                    RGBColor[1, 0, 0], 
                    RectangleBox[{0, -1}, {2, 1}]}}, DefaultBaseStyle -> 
                    "ColorSwatchGraphics", AspectRatio -> 1, Frame -> True, 
                    FrameStyle -> RGBColor[0.6666666666666666, 0., 0.], 
                    FrameTicks -> None, PlotRangePadding -> None, ImageSize -> 
                    Dynamic[{
                    Automatic, 
                    1.35 (CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[
                    Magnification])}]], "RGBColor[1, 0, 0]"], Appearance -> 
                    None, BaseStyle -> {}, BaselinePosition -> Baseline, 
                    DefaultBaseStyle -> {}, ButtonFunction :> 
                    With[{Typeset`box$ = EvaluationBox[]}, 
                    If[
                    Not[
                    AbsoluteCurrentValue["Deployed"]], 
                    SelectionMove[Typeset`box$, All, Expression]; 
                    FrontEnd`Private`$ColorSelectorInitialAlpha = 1; 
                    FrontEnd`Private`$ColorSelectorInitialColor = 
                    RGBColor[1, 0, 0]; 
                    FrontEnd`Private`$ColorSelectorUseMakeBoxes = True; 
                    MathLink`CallFrontEnd[
                    FrontEnd`AttachCell[Typeset`box$, 
                    FrontEndResource["RGBColorValueSelector"], {
                    0, {Left, Bottom}}, {Left, Top}, 
                    "ClosingActions" -> {
                    "SelectionDeparture", "ParentChanged", 
                    "EvaluatorQuit"}]]]], BaseStyle -> Inherited, Evaluator -> 
                    Automatic, Method -> "Preemptive"], 
                    RGBColor[1, 0, 0], Editable -> False, Selectable -> 
                    False], ",", 
                   RowBox[{"Dashing", "[", 
                    RowBox[{"{", 
                    RowBox[{"Small", ",", "Small"}], "}"}], "]"}], ",", 
                   RowBox[{"Thickness", "[", "0.004`", "]"}]}], "]"}]}], 
             "}"}], ",", 
           RowBox[{"{", 
             RowBox[{#, ",", #2}], "}"}], ",", 
           RowBox[{"LegendMarkers", "\[Rule]", 
             RowBox[{"{", 
               RowBox[{
                 RowBox[{"{", 
                   RowBox[{"False", ",", "Automatic"}], "}"}], ",", 
                 RowBox[{"{", 
                   RowBox[{"False", ",", "Automatic"}], "}"}]}], "}"}]}], ",", 
           RowBox[{"Joined", "\[Rule]", 
             RowBox[{"{", 
               RowBox[{"True", ",", "True"}], "}"}]}], ",", 
           RowBox[{"LabelStyle", "\[Rule]", 
             RowBox[{"{", 
               RowBox[{
                 RowBox[{"FontFamily", "\[Rule]", "\"Helvetica\""}], ",", 
                 RowBox[{"FontSize", "\[Rule]", "24"}]}], "}"}]}], ",", 
           RowBox[{"LegendLayout", "\[Rule]", "\"Column\""}]}], "]"}]& ), 
      Editable -> True], TraditionalForm], TraditionalForm]},
  "Legended",
  DisplayFunction->(GridBox[{{
      TagBox[
       ItemBox[
        PaneBox[
         TagBox[#, "SkipImageSizeLevel"], Alignment -> {Center, Baseline}, 
         BaselinePosition -> Baseline], DefaultBaseStyle -> "Labeled"], 
       "SkipImageSizeLevel"], 
      ItemBox[#2, DefaultBaseStyle -> "LabeledLabel"]}}, 
    GridBoxAlignment -> {"Columns" -> {{Center}}, "Rows" -> {{Center}}}, 
    AutoDelete -> False, GridBoxItemSize -> Automatic, 
    BaselinePosition -> {1, 1}]& ),
  Editable->True,
  InterpretationFunction->(RowBox[{"Legended", "[", 
     RowBox[{#, ",", 
       RowBox[{"Placed", "[", 
         RowBox[{#2, ",", "After"}], "]"}]}], "]"}]& )]], "Output",ExpressionU\
UID->"ff475cd1-8e70-49a0-8ea4-1d422628dd10"]
}, Open  ]],

Cell[CellGroupData[{

Cell["Get Output Probability", "Subsection",ExpressionUUID->"f157a6a8-39bb-4a60-b985-6875ca133d59"],

Cell[CellGroupData[{

Cell[BoxData[{
 RowBox[{
  RowBox[{
   RowBox[{"pInput", "[", 
    RowBox[{"Inp_", ",", "mu_", ",", "sigma_"}], "]"}], ":=", " ", 
   RowBox[{
    RowBox[{"Exp", "[", 
     FractionBox[
      RowBox[{"-", 
       SuperscriptBox[
        RowBox[{"(", 
         RowBox[{"Inp", "-", "mu"}], ")"}], "2"]}], 
      RowBox[{"2", " ", 
       SuperscriptBox["sigma", "2"]}]], "]"}], "*", 
    FractionBox["1", 
     SuperscriptBox[
      RowBox[{"(", 
       RowBox[{"2", " ", "Pi", " ", 
        SuperscriptBox["sigma", 
         RowBox[{" ", "2"}]]}], ")"}], 
      FractionBox["1", "2"]]]}]}], "      ", 
  RowBox[{"(*", " ", 
   RowBox[{"This", " ", "probability", " ", "is", " ", "normalized"}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"dx", "=", "0.1"}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"pLaplaceNormalized", "[", 
   RowBox[{"y_", ",", "Inp_", ",", "k_", ",", "V_"}], "]"}], ":=", 
  RowBox[{
   RowBox[{"Exp", "[", 
    RowBox[{"V", 
     RowBox[{"(", 
      RowBox[{
       FractionBox["1", "2"], 
       RowBox[{"fDoublePrimeyMax", "[", 
        RowBox[{"Inp", ",", "k"}], "]"}], " ", 
       SuperscriptBox[
        RowBox[{"(", 
         RowBox[{"y", "-", 
          RowBox[{"Omax", "[", 
           RowBox[{"Inp", ",", "k"}], "]"}]}], ")"}], "2"]}], ")"}]}], "]"}], 
   
   SuperscriptBox[
    RowBox[{"(", 
     RowBox[{"NIntegrate", "[", 
      RowBox[{
       RowBox[{"pLaplace", "[", 
        RowBox[{"yp", ",", "Inp", ",", "k", ",", "V"}], "]"}], ",", 
       RowBox[{"{", 
        RowBox[{"yp", ",", "0.0001", ",", "1"}], "}"}]}], "]"}], ")"}], 
    RowBox[{"-", "1"}]]}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"pOutputLap", "[", 
   RowBox[{"y_", ",", "k_", ",", "V_"}], "]"}], ":=", 
  RowBox[{"Sum", "[", 
   RowBox[{
    RowBox[{
     RowBox[{"pLaplaceNormalized", "[", 
      RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", 
     RowBox[{"pInput", "[", 
      RowBox[{"Inp", ",", "0.5", ",", "0.1"}], "]"}], "*", "dx"}], ",", 
    RowBox[{"{", 
     RowBox[{"Inp", ",", "0.0001", ",", "1", ",", "dx"}], "}"}]}], 
   "]"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"pOutputLap1", "[", 
    RowBox[{"y_", ",", "k_", ",", "V_"}], "]"}], ":=", 
   RowBox[{"Sum", "[", 
    RowBox[{
     RowBox[{
      RowBox[{"pLaplace", "[", 
       RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", 
      RowBox[{"pInput", "[", 
       RowBox[{"Inp", ",", "0.5", ",", "0.1"}], "]"}]}], ",", 
     RowBox[{"{", 
      RowBox[{"Inp", ",", "0.0001", ",", "1", ",", "dx"}], "}"}]}], "]"}]}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{"Sum", "[", 
    RowBox[{
     RowBox[{
      RowBox[{"pOutputLap", "[", 
       RowBox[{"y", ",", "k", ",", "V"}], "]"}], "*", "0.1"}], ",", 
     RowBox[{"{", 
      RowBox[{"y", ",", "0", ",", "1", ",", "0.1"}], "}"}]}], "]"}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{" ", 
  RowBox[{
   RowBox[{"pNormalized", "[", 
    RowBox[{"y_", ",", "Inp_", ",", "k_", ",", "V_"}], "]"}], ":=", 
   RowBox[{
    RowBox[{"p", "[", 
     RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], 
    SuperscriptBox[
     RowBox[{"(", 
      RowBox[{"NIntegrate", "[", 
       RowBox[{
        RowBox[{"p", "[", 
         RowBox[{"yp", ",", "Inp", ",", "k", ",", "V"}], "]"}], ",", 
        RowBox[{"{", 
         RowBox[{"yp", ",", "0.0001", ",", "1"}], "}"}]}], "]"}], ")"}], 
     RowBox[{"-", "1"}]]}]}], " "}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"pOutput", "[", 
   RowBox[{"y_", ",", "k_", ",", "V_"}], "]"}], ":=", 
  RowBox[{"Sum", "[", 
   RowBox[{
    RowBox[{
     RowBox[{"pNormalized", "[", 
      RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", 
     RowBox[{"pInput", "[", 
      RowBox[{"Inp", ",", "0.5", ",", "0.1"}], "]"}], "*", "dx"}], ",", 
    RowBox[{"{", 
     RowBox[{"Inp", ",", "0.0001", ",", "1", ",", "dx"}], "}"}]}], 
   "]"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"pOutput1", "[", 
   RowBox[{"y_", ",", "k_", ",", "V_"}], "]"}], ":=", 
  RowBox[{"Sum", "[", 
   RowBox[{
    RowBox[{
     RowBox[{"p", "[", 
      RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", 
     RowBox[{"pInput", "[", 
      RowBox[{"Inp", ",", "0.5", ",", "0.1"}], "]"}], "*", "dx"}], ",", 
    RowBox[{"{", 
     RowBox[{"Inp", ",", "0.0001", ",", "1", ",", "dx"}], "}"}]}], 
   "]"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Sum", "[", 
   RowBox[{
    RowBox[{
     RowBox[{"pNormalized", "[", 
      RowBox[{"y", ",", "0.5", ",", "1.5", ",", "100"}], "]"}], "*", "0.1"}], 
    ",", 
    RowBox[{"{", 
     RowBox[{"y", ",", "0", ",", "1", ",", "0.1"}], "}"}]}], "]"}], 
  "\n"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"pJoint", "[", 
   RowBox[{"y_", ",", "Inp_", ",", "k_", ",", "V_"}], "]"}], ":=", 
  RowBox[{
   RowBox[{"pNormalized", "[", 
    RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", 
   RowBox[{"pInput", "[", 
    RowBox[{"Inp", ",", "0.5", ",", "0.1"}], "]"}]}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"pJointLaplace", "[", 
    RowBox[{"y_", ",", "Inp_", ",", "k_", ",", "V_"}], "]"}], ":=", 
   RowBox[{
    RowBox[{"pLaplaceNormalized", "[", 
     RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", 
    RowBox[{"pInput", "[", 
     RowBox[{"Inp", ",", "0.5", ",", "0.1"}], "]"}]}]}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{
   "check", " ", "to", " ", "see", " ", "if", " ", "joint", " ", 
    "probabilities", " ", "look", " ", "similar"}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{"DensityPlot", "[", 
  RowBox[{
   RowBox[{"{", 
    RowBox[{"pJoint", "[", 
     RowBox[{"y", ",", "x", ",", "k", ",", "V"}], "]"}], "}"}], ",", 
   RowBox[{"{", 
    RowBox[{"x", ",", "0", ",", "1"}], "}"}], ",", 
   RowBox[{"{", 
    RowBox[{"y", ",", "0", ",", "1"}], "}"}], ",", 
   RowBox[{"PlotRange", "\[Rule]", "All"}], ",", 
   RowBox[{"PlotLegends", "->", "Automatic"}]}], "]"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"DensityPlot", "[", 
   RowBox[{
    RowBox[{"{", 
     RowBox[{"pJointLaplace", "[", 
      RowBox[{"y", ",", "x", ",", "k", ",", "V"}], "]"}], "}"}], ",", 
    RowBox[{"{", 
     RowBox[{"x", ",", "0", ",", "1"}], "}"}], ",", 
    RowBox[{"{", 
     RowBox[{"y", ",", "0", ",", "1"}], "}"}], ",", 
    RowBox[{"PlotRange", "\[Rule]", "All"}], ",", 
    RowBox[{"PlotLegends", "->", "Automatic"}]}], "]"}], 
  "\[IndentingNewLine]"}], "\[IndentingNewLine]"}], "Input",ExpressionUUID->\
"16685f8c-70f9-4421-8cbf-d88eef8211f6"],

Cell[BoxData["0.9987235121102236`"], "Output",ExpressionUUID->"d173d3c4-a28d-47b7-9037-16c094d22c98"],

Cell[BoxData[
 TemplateBox[{GraphicsBox[
    GraphicsComplexBox[CompressedData["
1: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
JRddXjbj5Q/6/uJ1d/7En+X6rLpzf97vfCi52/+NdKx0fntQfupB55eV5LGS
P3lQ/PQgeTV6qc+874PWfVW/78Tfc8U/g+LUZedH4BP+rXc+N6WdW2pfX3b2
Mu36ReHMv7a8rvNNYOeh853LspNz8n1fmHl9T+K8cN6rfBCf7TuPlMNa+YVZ
xS3Y021xC36e9uxP36vSPt89HHR5gm16MLZv8VPmU4cOj+1fbo0zwNBjP5Dv
aJ8r/zB08U5+V3iueORMceqB3l8+Vz5i0L49Vvz+RvHCVafPOd5MfuRSdrW9
3/M7vtTzG/mpZWF/R5d29oPeo7yv+jwXtu9VM38x23JOudP59U7vTe5k1+9l
x++7feDvSPL90pHioBdVz77IPHOLOzIuuVP+v8/7Oo6xn07cztmZ72p+0fvS
72TsB8H0Tz9/VnroMt/rNX+e70scF7bvKjIP0O7JnB/P9zcnJVf4zDzfq5oH
7O8JmQc+/X0hegKf6EvmDR+KT3DS3fSLfY3e5z11/10R7X1v6zy/711ph32j
P5h2Pj/5XWbGtfedHU3/eqG44aD4wL6lHz5UvHtUfME3GH4yrjgqOmkPznVt
74/QH/hPu3Ra+pJ+oX0PhHyZDz6Zb5ufYt0+5Vfs79JevOnyAWDWHYxc2Bd5
zmNf7Befaff6vAfzpz7NlR+4kr3ov7vzd/iOs/IcdtfFCXl/OAjPSk62y143
n3vz3qGds1lX9AM+WNc8L76Sv7yUnzzvzj/5zu1IeboXirOPO3uY+fxLnSM4
X7LfrgvDn8/hafevOz+Q7wLf1P5iHbG/9Pe7hXzf/Vrv/c67+BV7jH4knpd9
zXjxQXmddr4Bp1xXnV/CPqB3YPSOfUA9mHrozXixx35HAp0+d+b71xu9Z72V
frX3rr738vty7Ff6uwPZm+dbv8tMPfe5tr1D8j0UmHWzHcEv+/0S/GSc1Pw/
/cDYb+axHct3zefdfs3vTt+WfqWf8n3ToPj8sOyt88N+F5Z570ZH6sNFd+5j
vIwzDmofQlfmrw5rH+R6tDjQ74yc78h3W2e1Hmm31zrfXnTnqJFf4tI3XdwB
zvwL9uNd6enH7fuh8oWnNa71z+cz1gd77PNI3vu+LvrB43j3NV7qyTP5mX2d
/w4KW655fzkUnfCb+dI+roKPfAfYfweOX81z08uyk/4uJb/TWdX+zXx7Ox84
/zbOtyf8TPZ8X+u4160r60Z7yvQDbZz/A1KKiDI=
     "], {{{
        EdgeForm[], 
        GrayLevel[0.8], 
        GraphicsGroupBox[{
          PolygonBox[CompressedData["
1:eJxN3Xn8fVP1BvBz7z1fmaKUucxjlGhAESKZ50yZx4gMP1GmTJmjMlSUvmaV
ZAhlljFjhjLPU1SkJJX4rfdnP/elP/brrLvPOvvsM+31rGetve/c2+y23leG
XdedPVXXjWr7dBW/31tlyklVajtzlRlST+d92fZV3p9tqXYzZjtFlWlKqCa7
WarM2jV56iqzZTtNlblzHm1/sMq7q0xXZfbsn7bKTGnvXVU+kDp6TvqZQTv/
M2lnrir/qk4uXfVLVflo6SxUdUtW+b+Slxm0vv+l5MWr/LHkRXI+bU9dfb6u
6lcqvWv71vcFq/y7b9egrYVzPa5j0Ryn7x/Odo4qz6Yvizlv3ag3q0/rVJur
1/b9VZ6v+lXr9yqDdg9G1f7yg3b/Vqvtc7Wdx3VX/WcH7R7sUe38p45dq37/
rLYzVlmh5I371qelqvy35A/VdukqB5a8Xx3z5dJZJn3Vx091Tcd1fzpb+14u
vRfT501K/lNtl3Cvq52/1+//lLxhnXPmKn8ued+q36jkzWvfamlXezdV/ZJV
/lryfrXdpHS2LJ2vlzx1lQ1L/kj165U8l7dr/2ZVt/agvTzaWbXKJ/t2HatU
Wa90Nqr9L5W8es7jmv5ada/W9jNVPlZtvl7bz1dZvOR/1HblKjd77lX+WfL2
pf/32q5QZaeSv1RlxZIvr/ZfLXmLKluVPFuV13KfZsv78em+9WvjKgdV++tU
2a30f1262zquyjeq7o3av6bnVb//Vdt13M9ck/s0qXT+Vvu+UmWzOna72v6t
6jfNftf3ZNVPU+U3tW/J0v931a1b5ciqm7rKHFW/U9p13Aals3fVfcGz9755
VlVu8V5V+W/Jy/Wt7ztWeavKZlW+7Lup+v3r2P2qHFrH/rO2b1f9Q1V/RJVv
Vnm+ylSurfYdV9uNqxzg/uV+6McmBgT3wTmqj4eWPKxycJVtq25P32bJB1XZ
puRXq83Hqzxa5Zg6dtMq+9S+aarsW/uP0seq+1b93q3kr3XtfK77n3VMP2j9
f73kmascVb93qfMeXdtJVW6tumXr+HeVPJt7UNspqxxf8ilV/+2Sj6jj90nb
BsGTq26qKt+t/SfU9v+q/uHSHw3afftO1W9R5ZD6/Zu+9cfx3x+07eFVpnMf
PCfH1/aHtZ22yldL/lFt311llirfL7Vzq1zsumvf6c5bbR5X2wOrfquqP947
VfKZJS9XZfr6/bXSPae276lylPdc276vqp9U5b0ljya1azu5yi9LZ7LnVvJM
JZ9vDKwyw6DdY/2Yq857Yv0+pOQdSt6+ytElnzdo13RSlW3q2K2rzKh/dZ6f
G3urnF11Z1U5qeRZa3tRbc9w/0t+X5WLvd/V3k7uV5Xzcs4fVHm82jmsyuWl
c0mVc6ucUPW/Lb1Vqvyyfn++b9dxtu+w2puy9H+ir1X/mntcZbuq37bKbCVf
U+UDVU43hho/q37m+v2hkq+s7RWuqdq4rrYfrHJD7f9NlQtLvqrK7FUm17FX
V93ZnknJd5Y8VR3z6/p9bLUzhe+36m8YtH79MmPLa/neL8+zda27lP6Xq/zE
va12Vqoyt3Go6n7rPlX5R8lnVHmj9i1Q55mr6n5a+q+UvHCVh+v3bVXmqfKL
qp+79O4qeT7vc+2frsr8Jb9Q2+mr/K7kM6u9d5d8S8kLVLnM2Gw87Jqsj7dk
+6sqd5TOvFUuLPneQbumG6rcXu18pcpqVc71HOvcv6/9T/gGav89VR4s+Q9V
rvau1DkXqvKh+v1Q6T5Y5ZGSF6rtY7VdtMq9XTtW+x8ZtH7dX+X3/9PHC+pc
T9Qxz9X+O0vevcqaVf6Q/fq+cu1f0DMt+YHU3VrlTd9v9eE93tM65se1fbb0
dq76Z2q7WJW9S56p6td3zaUzmV0u+eNVfldtPGEc7prsGt9X+/+YvsNBMAvM
McY9cMgYG9l3Til9rsqSvofavlDbjw0alpkpx8BIsBWMBR/BFTDHGCf5PcZY
2oV94A32CAb6YH7DOmTYCWbTPrz0kzrv56t8etD6OnfXcAkcNHf66RzwA2xz
Ud9OCIs4/4ey75K+YRRYBD6CjRboGi5aMH3eu294BXY6pc45Q5UPD1pfF0lb
5wf/wC2wz2Lpzxg/jfHUojnmotJd1TWU7l59w3Aw3xrDhpPgqNWGDUvBTHMO
2nfn/b4gGGnFqlsr+Aveu4xc5QuDhp2WzLXDTkvlevUVDlm6ewcn+Q0jeV/g
jfWHDcvAJLAJvLR817AN7AN7wDV/6RrGgKngphVyfcukXXgK9vls116mVXLe
/as/a4yCnWq7+qhhrUOCH2APOAtOgnlgjwlM0TX8AtvADFeV/MUq2wwaXoJT
4JPNg1tgDxgELlq7a7gIDlqv5AP6hot2NuZX+2sFT16nvSq7Dhq2go026hru
gsG+kPsFz8A9sBn8tWGuefXsU+CXMT6yhSfWDb6Cn+Ao+GfLKtPX/ZluUrP/
h2X8Z/sOHzWcBKP+PTZh667ZRrgI/oCDYKAduob9+QafzPnYcfb9xrqm7avs
612p/S9U+VMwEbyza9dwEtzhPn9h1LAXvLXDsOGot3MdO6Xdu6uNu/qGjzYO
RmI7Nho1rAabwUVw0h5dw1uwkj7vGjzD3nx/1DDOpNgguOgA5wjWglGcD5aB
RX5b9TtXOWLQMAy8snfV71ntrBk/YrPgI3hjy2Ab+OfIYD947JRgoYNLZ/dg
JNjmh6OGr2CqE0YNM8FFW9VxXwye+/Go4S5Y61fGluE7WAk+gS1+V3V7Vjl1
0PAgrAR77B3sBC/dEewEG8FIcNG3Sueekn/XN6x0Cewxajjq18FLMA1sA5t8
p2s4BW5i0/cNjoKX/j5q2Ane+cawYSTY5g+eaZWfDRomgltgJhgJhoJhDgoW
govgrpNyXb8IXoK9vhmMA5/cFIw0U5XTYID6fcuo4RR4Y3Ide82o4Q946L5h
w0Ywzz+Df+Ai+Ag2Oqv0H/UOVLls0PoIdxgHx3hLP+GvWQcNh9w1algI1tk2
2OdnXcNBMM75XcM7sNLPSz65bzgSroOR4Cs4BNaCoWCGo4Kp+CZPxU/hv/Bj
4KRL0qfLMz7DOXDQr7uGjy5Pv5+t+mOq3FT7HjHexGbDTTcPmq0/K9gGdrpv
1HAUbAQjwTDXdA13wVGwzbeHDS/BSOdU/X9GDTPBSrAOLHJV1T9Xv6fl31b5
bpX7B62v8MZN3TuYye+Tg3MWGTR8BIfAHn/1jg8b5oFPFqrtb92r0vlLMAm8
BB/d4Zph72HDVDAObPJg1Z86bHiJDYV74Jtb0w84CV7Sb3gEPmFvYTJ45R++
lypPDxpu0+dfpe/aGWOoP6Qt2A+WvLbkx+G0UcNLuwyaP2us23XUsOgSVfec
/aOGl+AamOnxrp37ya5hJ1jpyfRtpTruE4PGLX0yW3hk5WH7/Qx7ke1cVVYZ
tt/P0skWVti9zvk55yz5Q6Wz+qDZ/d1874OGB9bIFnexaOmsG/1lozu3a657
cn/f8MC+deyHq7xc8trDdj48yAbZsvXr5lxs/YdL3jjyBsN2Prb/IyVvGvkL
Ja85aPb+i9my73uNWn9ggq+V/ErkjUt/vUHrw1JsUzDERviSvnEln/BujJq8
KXvSN50l+nasa9l02PqNE9k6W7Z+NGp9ZutnHbYtW79F+sz2bzlsfeXDLF1t
LjFqHMqOqYMlPlo6O0T/F6XzVPD52qPWZ7Z/M+N/+JZla/uJ2P6th61PE/zI
qF2L9jfvW5ts/ezDtmXrtx22c7P9nyqdj6f9pWq7Z+rXL3m92P1DcRUl78VW
lTxlybt7hrX9lGPZodoeX+XAkjcctb6xv9sN27uOF/l6tmz8UdXOClW+VnU7
Dds+vMOWfesDW/6I8w6a/f1WtXfsqOkfWdsj0p8PDJsuHuOlqvt0lUH93iLX
BT9sU/LW4RpOrLrF65jDBo0fYXNwIofn/OQflM4Sxkp2pOSPlfwd423J3xg0
zHAcnFDlWON8387L3n/GOQdNPrnkk0btXOfV9pwq3yv5e7Xdbdiu7fba7jVs
7ZwyaByNPt1ZdR+v8oP6vXnpLxM8sMew9ZXednXeH41a3z5R9R8cNhv/RN/6
Tl655BVHzU+/oLbnVznNWF/bswatfr9hOzfe4vKqX77KmSX/tLY/SZ+vrG0/
bDb+ZyW/f9T69tMcxzbu2Lc+TNhvNj22/ObSXWHU7PIssMug2fR7S/eAYevP
hVU/LHk6Y7Sxq8oFJX+n2pgBBmJvanvwsPXtupKvrfKLkg8Ztn7gfi7Nlj2c
p/bPN2y26fqS54j+qdXmzn1rf+naP++w2e/ba/9tVX5V8pzDZosnexaDZovY
vk9X/bWDtm+Zkq8fNDt9W7W3euzi0cPWDz7TEWzxsNnu7UftWcAGyw5b39TP
NWxtnM4W9K0/2rkxbbCTn6q6qwft2Dt8C8PWzzVK/3OjZu+/M2zH4CT+WPIK
Ve4p+b7UsW/fSv/Z8uPSf/Zp+ZLvjrzcsF0zndeq7b9VmaLqdsu52OP5a/tA
bRcetH7BCBcY/6v+87HNtw+aLcZd3FHHrhVb/gdtDNuxnxk2Pcd/qep3zLHv
HbZj7zS+l/xonsFNJX952MaZPfp2LnZ9xap7fNA4kBd9N7n2JwfNzrOfPxy2
e4EXOb+O/VkwwWnDdi0fNSZUe6uMmnxf7b8XR1O/Hx61PrDRD5T8UG0Xr/JU
2oMZ9izdJ2vfLqNms9lu9vr09A1H8UztW23U2jlz2PpHj91mxw1nw77FM9jd
c4eNZxjbcDadfX82dp1NP3vYcAA+hL+Ps5gj26UjvzRov/EY+BE8BO7jvGHD
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
acf78FDqvSMwI2zJxsEXsCmMcUdkmPSu6MCbV6fPcCxcuULGGWMQWSwMZoKd
8OTwEhn2+OewxRGqeiLHkvx0yW8MG7f/nq7x/CtHFn8Qm3ima/mQKw/fiVF8
LscatzbM+ymWgUufs+T/Dhsn/5Gu8e7kZ7vGqeOpcdT/Hrb258x4IyZiPBRb
ID+fb2SNfBcXpf1n872slm9njejQx1mvmW+Ezro5F44b1y3WL16wdvp2WWR4
ZzBqXPeyXeOv1x2+k6O4XvqweuSX8n3hqf+cPmwQHXw4jttYvVb6g3/2ba6f
Y9dJf3y/a6adJXKuDfLNvqdv+f2vZxzYMOdyjbh9eA2Hv3HaketIfjnXu0nk
4ajx0mt0jc/fNNe4ZXRw2punTe3grem/mj7j6PHTuOvNM1bMGln9VWmTPt56
i4whOPUtM+ZsH/m16Ghz+dgl/Ptno79VdHDpW6fPcgtxnrjnftT44S+W/K5R
40t37hpfrB7vjhvGEeODr8mx9HdMvTFNHGOdPHd86/ZpHwe7U9qUk4CrNe7J
f8ClylHE95J3ydhIBz95XfqsD7hMnKacBFwo2Zj527SPD5561HjOr3eN19w5
9fi3r0QfX4m3NH7iKnG4sK446S6px2fiKI3DJ+dYuYj6sFvaOTmy8Rnfhn/D
p+Ja90gfcID4O9yd/Adt4oblKuyeY435uFHj6vSj1s6RXctF3CNt4uj+L/3B
le4VGf+Ej8L5yW3AZ8oHwOPhVWFsvNbX0ge5E+rlMcpt2DvyHDmWLcATqmc7
8H8HRJbboB04d8ZRO+/3uhbvhq3lLspz+HpkWJfMHl0VnRNiR/bKeeU07pf+
3xXZtYvF81OMveenD+yXfDxcB15KzsM3Ui8v4sDIODT1eDOcG+6MvcN/4cHk
VByYd8D7htM7JNfywVHjss7pmt2Bmc7oWv4Dzou9w3PRwf/JqdC+3IyFo+Nc
j6ZNOnIkcInyLXFiZLbysvByx8f+OtcHY3OPyHlxP8fkeucaNfnSrvGTx6Yd
+jDcHLG5R0V+OPdqfC3q2Vbco3uF+10g7ciFgOdhuIujf0TaYZdxOmy09o/P
c2G7cUC/iG2lw57yNfQHZrgtOvPGhmtXH/Bd38m1vNK3fGg52/gIvIo8h/lG
jZ+5sWscDRkfBiPDiPDhgpHpw4Z4rmuCDchj/IgDuTP2HU8CG8iNxKew6fgR
OvAD/kcfcBqwBB34gf+Be4IN8CPfz7GPRIZJcBK4iY8HMzgXP+bJ1MMMuKPT
0r76U6ODj/phrvcfkfFYi4waV3Bf13JQHSvXAueBw7k99+3Hqdc3fI7cCXzO
6ekbLmRy5BdzLR/OtauXX4EjOjX9fCHtkOVYnp424RSYBp5ZOTL8Arfwu8SK
4SB4aMnI8i1KnDhW/dNdwynwzZyR4SFYSAyXb7Zc2lcv1wKe4ePB8PA8n23F
1Iv3s/XqcTj8uFXSPsyF0/lZjsUpaQcuwvP8PPrwwPnRcd7zco3axKfAYziV
C3JeuABWGOMgMlvPBsIEy6YPF+YayfCWvAjtXJw2V009DIYP+lGe+6Zph13F
IeHdcE704S1YC86Ct+QM8GVxVZ9J/aXReTXHrpR6x8KP6+RY9to4gUc2VuC3
tMP38Y36Vo1DsAm8wjdikyfwRPp5Za4XxoFd4JZNI4vRwxFwA8ywXeRd0o54
NkzChsMKsAMshLOCixwLB8AAi6UP6rdOvTg4rAHTwBvbpx14A+elnd1in9mY
I9OHG9Ifdpg95j/pw605FhaACdgK/hnMMY6rkuEU+IK9Z+vJsEifdshikXw1
eIK9/njOxe6zw+wxe75rdPh22+Va9o2dpPPu2Cv9gU/EQMXw8Gdic2Tj+X65
RngEdrg7feOfsT3sBR+OjWfT3RM4Y5rok8Xg9o4OW41/wBecked7RZ4Xu6N+
Yv7jsNlReIR/yTazxfrDxv8s8u9z/9nze6Ozf3RmiKwdtphdZb/H/iuZbT0w
x8Im38i5+LW4EJwYboQd5qOy19/MtYtDLZ16sneejRzbczZ7llwL+bLIj6Q/
vi/j0lZp84ncTzwHvgNnNmX6IEaGO3ky9ey274i9OzbHTsQWcyybe2j64xuc
JsfiXvbIO3Z42oGB5kz7OD12Fm+Hg7gyNpGM24OhcL78Xb4sXMV24+fwODic
5aPDJr4dnnz6rh1Ln81kK16MzA6zVezUDyOzz/Onzd9Fx7yFBTKGkO+P7RbX
YLN+kDY/HJ0/5f6IedARP2Hr2MsHYmPZM/YLX4g3fDj1bPmisdVkNpmPzp6x
X6fn2MczvuLf70v/X8u1sE38djYL9/B6ngt99tiYfGL6c1uOVS/esXD6A2Ow
d3gBuYlPpR18huciRuUZiUO4z/IL+bN4fvlsf47MvvBn5c8tEBvB/2c75F6J
NfB/rcPwVvQ/F/0XYk9xAZ+OraHPLvNH8VxjO8U/Zzt813xaY7VYxltp31gv
n+pvGYf5isZbMQVcp3F40bTJh8Z9qZc7ZDvF6J0xeIroy5eQH8Lnkmsjr0m+
zdGp5x/tn3q+odx1+UODjLd8POOqGIT6rbuGWTxrmAunhduS18Fn4jsZx/Bp
8mZw/vw5PiEfbp/oH9Q1f079V2NH+MP7xnbwzYyNctG1M3XGEN/jTRmrncs4
L09CHsVxGdP4S8ZA88LkRZg7L4Yvj0IcX542mW/CDslD4K/JH8DxGatxemT5
4bb28Y3EJsT4+TfGSnkF/BFj7Mw59py0yS/7cXTYC3EEsQdjplwLbcm7mC0y
3wQvLV7oG+SvzJZ6dfb5lqfOWGSsM/59IG0a0/hUxlJ5AfIKjOGXR54tYxrf
xj0UCxZT5yPwO8h8ijdzroUzzog7+Db5I/NE//VcC3tkvFNvrFOnP8Zm4xK/
wlh0TuIXxivYeYFcCx4RzwgXm+NhTsQX4guISRtnJkfHWCT2qt74aeu39xB2
xhUafybnuR+U8QSmN24s1rdYxlNdm39iHoq5uubXfCSyubZyB1fKmLlwzvXx
vq2pYp9YiHbENs3T1Y4cwk3TjnnFK6Y/xkBzdT6a9l+PbN6uuTHmoZgb+2au
fbx2h3o+mnm38vbkb5gL87HobBZZO3I8lsh9+0++BVyEcUHunbw7uYTmz2zV
tTk+i0ffPBrzZ8VWzJUxL1Uu32f6tj7IJ7Pfsfgo830Wz7UslX3a3DDH6put
tlwTDvBTafOvfVubw1wUc2vl5u3etTU6yGKa5qeYWyqeuHlkOrb2jeOWn4mO
uSDL5bsWxyQf3LXt8vn25VAtHx3zLc1x+DEb1Le1IeS/mIdiHubkrsV3yGKO
uPUVU2/+Jl3xPvMazEs0J1Ge6hp5B/j25iCIvywYmY75BeYEmg9oHsHKqTeX
4POpX6dv6xT4be6jPP+HujbXSp6DuLm5ePLzxWJwnqvnXfVekvGlclrXyjvj
3dS3VyKvmX4eGNl38GSuRV6Q92it1HtP186zXjyy91euqznaYmrmcJHF3cTZ
xNvkusrRXT/v1USubmS66+ZYc2TltIkFLxl987nMjdog7+fEdtTi5i9G9l5Z
m0ib6+V98/79N/JGed/M4doo9Q+mTe2Is5uPLeZoPhd5nHNrrrbYqzlTm0WW
D2tOs3i693TzvJPHpt65zNuSG4prFcN1rNj6pql3rHj31rneZdOOGJN5QurF
oM3DkrOI/zQ3a4scKy+VfGjXtltGNs9sk3yD5lptle9CnJ183HiMybfJHrnn
bJYc4APynshl2zZ9sN0u8vWRZ8t3sH2+HfleZN+Hd2eHvD93RPYdyJfaMe+5
tUPI8Jjtl1K/cmTfgTyWL+edp7NT6uW1+C7gQN8G2byYVfKd4GHkD+2U9s1h
Ncfl4a7hJXFH+M534/vBvZjDat7MY12bV2L+wzxdi9eQxWLM0TD/AVY0d54M
78GMsCAc+FbkcZxkz+jQ3TP1b6YP6q2DZR6DmI55CvL7YT84kywmYq4GHTkt
sK4++96to2WewjiuTsbb2O4TWXyGLP5jna6vpU1jg/jKy7Fl8uqNA3gV9cYS
vAd944l5ZtvmmRpP9ou+dSG8N/JhrEG0f94fa455l+TX7Jf2X479MtYYT9aM
rD38gnUrNsvYtV+OlQ+vfs2MFeL6xhYcBRm3gJc5LN8+TGs8Mp5YF+iw0Tu5
OvLLYV05NYdGx/xQMqwrb/+QtCn36qhci5weOUDGIpgXJpZPrxyRb814dXjk
vaIPG5uLeniO1f+D0/4RaQeuti7EEZHvij00zriX+iDnSG6PnPVtuxafEcuR
Z288EVMz5lyW+Jq1Guw7NvXWYSAPM87Jpdgy4zN5nGd0fO6D3J6JNRy6FhcS
K8LDHBidXTPWfSttqvt2jp1YLyvHyqWXezQeq8jGLnNU5YEb6+TaO+b/Mgaq
N17xBWB9ON94aTwVqz8s4+BeGffI47Hx5Mj8A8eKF4k3yYOXA2+eLBmHY+0v
OeryzOWnW9sCTsAX/SA6cpdOSP8dp168CB914uidNc20s0fu2ylpU677j3It
90QWGzI/VSxHHEdulbz3g7rmN52We+IekPlQZ0Tn22n79MjWCrswbcqFPz39
p+uY76b/crDkTCnWvJAfBQud+T/6k9MHc1fVT5/2zsi5HKedQ7vW/7Miz5Bj
nYsvd07u//ci86H4PeJSfB/xK9duv9iU+h9E97z/OfYnad95fpJ7tWV07J9Y
ny3vA19PbjyODm8mrnZa2jw3fcDP4el+FHsrx4stOy3H8sVsL0g7OEnHnpz9
P4+OXDDHstvy0dx/vpj1Z3yb28SmWx9Dzpi5yRflfvJzL87zchxZ7r39l0RH
rr1j2WjrbFyadqw5RoYz+an8VXn+MOzlOVY7l6U/M6RN7fMZL4u+fH05/Cem
Xjsz5djLo+PdoAMby/m/IrI8t9+kP3Khrs67xLe9Ms9InZyn07uWf6dezFHs
UQyS7tnRo2P/VdHR9g1pXzxOO85zYc4rrx4nKebHl1WuzXP0TMkzpu66yNZT
0X9zxmGW60fv+MFkGEY+AI5luTxffXBOa6bJDx/nw9+Ue+U53JhnIc//psjW
uLs5OnDRLTmveRYX5bl4drdGx/7fRgcnCSfBSHhJtpadvSI6uAq+yUReefDY
bem/nPNbo4MXFe8ct3F7dOSk35JjYbE7oiNX8c70H98nv1petDVVyGJ849x0
uYdydeT9w3jWp7s7z8W78bvI7t3d6T9OxL06I23fm/atHSfHe6HcT33QhrVY
5FVf3zXfhw6MJ0/+vhwrF5HOb7o210B/5MmLOdKR12QdFVwfvkIutxxl3IL1
TMjmQeMo5UCLLd4ZeZFsH4qMiyDTlcst1+3OYDAy3AhvigvCnPhSMnxqrblH
U79eZPhTPvZjkXGtT6Q/6h6P/EBkMcbnoiNX0zp4T0Znz7RDhm3V8wf56XKS
4Rw+Il8RT4uzfTrtWK/j6dRb7+6ZXMsXIsPAcsKfjXxfZPFH2+ci43XJYpv4
VHneuGucoz7gG/hn/LTXU/fH9O2QyHQ/FR1+nOt9KtdirVH5znDRXyPzs+RB
/yky/4tsTh8sxDeDf+Q7wxbj/GdYRB2sof6Q6NCHJazJI0fWd8AWs81sonHe
uGxMNjb/Ne+z7auR/5L2tSm3WL4xezdFZFycPOS/R2ZztS+vUNxEvflKdF/L
scZMsjbk8b6R70s8yNwhPNtTqb85da+nXm6zXNIzuxanIIuJzB4dbfoe/5tv
3HqT/0o7eD58n1iBfOD/5Hv0ff87feCL0edbWStJ/S25Xv0wnssLeDPf4DNp
B9c3a+7bMJyZfOKnuhbvkw+EhzYf39qD83Ztvj3ZvHxb+8zPH0amY+v3OM9u
mHpz9q1VOF/X5uKTzce3tU/bkyLPF32/xavl/5vbz9eznSKyufLkibWoo6Nt
W8eIIYh1T0qbfMMpcyyOwjwB/oSYAK6fr2eOOT+QD/hAZD6drX3j3L1poiMO
Jk4/jg+Q2S9tT/AgXeNCyPwOPtB0qXdusrkO5jXj9HyP06VvdHyPeDrf7DGR
+RFvpc9iGnhAx5rHa6utcdxghrTpHObebt2175MMI1lLz/wFdY43z9U8Yr6V
uMIb0X1/jjW2iI1ZG8L6vvTlDpkn6/eRqTcndpxfNkt0tDFj2hHf8I7xX8xl
wDPukTGEDHvPkjYdC9fPHh3fKhkeput4dWw6PhFGElOW+wVnmrtqvTmcIf5V
/QHBZvK9jC3L51i4Cz5UD7PBzPLGDusabtcOfwp+Vg9HwXLmYMIP5mPOnXPN
FRnGw1nOk/rZ02fXom7e1M8dWXvW5TN3Us6Y8YhsDqztAqk3vpHth+HlmakT
F5bbJAYCX8nHEmsQ61A/ke/UN33t4BKtb4WvNq8Q7wqf2Po95pMWSb2cJrlN
Yh3iy9oXxxCjELcYxxzI43x4OVUTecSj1q75GeIk2jE2ij/QEct407fSt7Ug
8aM4XPmk1tSyRuQno0O2zqmtffJv14seHTwHPmXMYZDxJOIA4goTa5337drF
AayPZd1Oa30uFh38qrFRfMJcqbdr+1b0NkwfJ9ZL79NW19pbMvfT2qjWRcFf
4Wnx+/gW8QExg3FchYwDHE6qMXRSW7vTOl3W8RRrwBMuG5nOMunny33TtV7m
EmkfN8u+wx/GBnll9KyzuXW21tW0hpZ8M/mly+R84h7q7MPN4i5WiA6bz96z
9cYZ7RtbrMmp3lhhzpN5UMYT3/KKec8nVZ/7SW1tROsrwsFw8lldqzMfFKaG
rb0jvpeV802tGHly3snP5T3cN++GZ2pOrrUXtWdtZOsp4fltrbVnnT376bG/
+uP3SbHLq+ZYfVgp7ZujsXr/zhp92rHWH05VjGEcTyDjS61RtGZ08KBwM8ws
1rBW6q2ZTBZTgKnXjo54+sR8y0HDp7ApXPpY5GdTD79OrDGU9897DifCiPAh
PGxOCDz5+/QBLwt/qsdVOm6j/zkW1sS/Ppt2YMhXoufd3j/66vA/OCx8i2/g
i3nPrXu3cd7/m1Pv/VS3Sdqx3TSyb3DTfHc3Rcexx6b9CW6+b7E07U/EAfpu
Ig7ge9ki77m5JOIca+eb2izvMH5PvXjHwumH9YQn1vDoG+e2QdqBdeEY36q1
hsbfyQ555l/Js8MFij3gA+3fJjryn8jysTeLjliYOUvWJ7X+gPUdvRvWkPx+
6q3/ad0z65bSta6Y9UDl61r/7EvREe/eMTq4K/nNuAvHqRcH3z7t0P9S2jFH
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
rHp9e3ds7uT0x3P2e9q0/0beD+f5f1hjPTY=
           "]]}]}, {}, {}, {}, {}}}, 
     VertexColors -> CompressedData["
1:eJysvHk0Ve//Ni4iSSEqzcYypCSVkFdESgpRFKJChqJIIiVDSDImKkoDMhRS
Emmwj3nIdI55OMeQmSQS8nvtw+/7rN+z1ufzrGetX/+81/taZ+1939d+3dfr
uu59b8JnHY5ZsrKwsPCws7Cw4X9jXRvG09Up0KqzXL7LnQqNPukXlkXS4T6b
n7LoaB5o5+qyNG6ggpnToEuJJh06syxFtiBu8GiZazPil9n33itD3FchdLMS
4k49EnHtiK8U8OCrRPyYv8pVecR5HisXt5L4dZNbFYiX3JFK4kXcuPZVXS3i
fC/LNxQgXuAm8oX+Mw/4F+//Xoj4NeO3IjmIT//h0/yCuMsCEeXPiK//8pk3
A/GpwzvvpCKuzZXd/BHxqg6VM6mIl4pf+pWE+LWfrx9kIa610+HoG8T5qLsF
SfzGRNFJEg8ZXLuWxB1KWf4lIi5ecusYiZ9P/yPwhnlflmkS740Z1SPx8OZb
TFxVrmeKxLk+9TPxMJ7rTFzv0g8mLu07h0sx3Ji4jpFMYwritzwY68lxpsVZ
fCXxrZ/N+oPH8iCtNt/GdyMV9FI9io0P0uG48O+/NeN5EJJ/N2CzEBWWWR+L
6kCcbT2v+fM/efCgenwqAXGjv68adQ7RYXuxhz3bZB7YeH39mYe490N6uBXi
w+zLB50n8mBsb5qvAeJFfUVeixG3q958fBTva/dsyddneF+RSa5aR7w+Zf2D
z2P4XMLUk+QEEFdIWHt7Csd5YTnbHlbEv61Mqa3E8fsI78jNQ/wndfezjzgv
rSbPpE+If7JT+pyO+HW+92XJiI/IcdaQPHCFHvEk58vPX7CB5F9C/LExiQc0
vFxH4n+W3Z8heSuNOMvkX0S5jcnb/RtjTD61Nw0z+XQcucHE61PmeJ56MMfz
iZE5nrXMXfnI6x+w7DUk8e8f/FaTuPCfrpYNyENXcoWhMvLgH/9N6DfOd1pZ
gMd9MQEqR8oVT4hTIcWltFz1CB32lMs9MRAm4Fto4chvaSoYHPuypO4YHZZV
bLx/TpYAwluXEJOlgsDV2CjTE3RQLsnpSpMiQM2qLSx4KxVazU8LcB6nw65P
dz/qriDAVfKh71MJKiQ9d+ri0KXDR9V9Qd9YCAg+WurfK0KFnQaucbsO08Ep
4Up7/+88cF9hzfcD+R/qqt/3Gse5eBlt3An5b+0buzCC89KudTndgvNqjNut
mYfzHSr35PuC+PaKBxRyXVhd8mkm+f/jcqGO5EFXasFtkocg9xB+kp+wxTeN
SNyZhX0Nicsufsmsc9/ejUz+2bkSmHx2rO9n8vzh7gCT55jPc3Xub5rL/H3O
6Cbm77f+SWHiHkKcd9Wxbi9zh76dxfGfPijHlofjv84e7rgQ+dFozE82Q340
q/TftRkgD3/FFFreEJD9S+/eAX0qxPsZ8f91osO/cRPuCA0KiPBoHBS7QYVf
enqR+1GXqsd4OyXsKdByTrvUxp8Kd+r0k4hYOmxYPJqawU4BC5GHbyztqDCg
PlFz1J8OYlHT5QZXCAi1+rlr4X4qtO1vqtKwQn6eH6pvW02AbW/5xw5JKth+
v7dJUI8Ot3k19ibgOnJ6qe04gnVSrvNzMBDXS3LWK7s3yL/6msZEFpyX/zLR
Zf0437CAiZWkLoUb5G8ldWnt4+vsJP9cy6p4Sf7vrKh7QvIjv0z3OMlPq2nt
QpL/4T1WBiROqQ9dSeKZPiuYfI4e+8PkmT3lNpPPqPcjTP5tVw8x8cqlc/V/
WujMPeqvPHi2ZvblMRwP3+4jm2WR5yPFa5Q6lQlg6d5HjdmB9Tl9d0zrJB0+
Jfl/tcykAO83+Qy3aCrI3G4wfJ1Oh55Fuk3ewYUweuovD9t5GsycWr72rAkD
6qhmsqxxxVD5+aa5/BQNauSe1h1qYMAHz1hlodoiWF9teGvrNxp8OZAkfvYV
Ax69eBa1X7oAVsDGV0sX0mD4yXDiJCcD8t84nx1eRoHxjNz94xep8DoxW+la
AB2ujb3kGNxGgL7mj1o2XEehT/7RVHEd7b3fEKX0Nw8WF299tVoY9erDdGIq
8q+49+W388i/f1M95yDysOV+gVcj8tAWYa3+DvnJrJOWyUFcn7XaPQ3xfazD
BiTPXD7xtiRvhxboS5K8JbYWMHVmScYlZt0+EO5n8nmicU5Pnr6Y05OZv3N6
IjSbdO4Y3vfXmPXqH4hbXl3rRUX8gHoJZ+1WAkYmDxS+34Z9M/LEUhYcf9xq
Lz3riHxoM/sue7ER+0L2ouKOLjrcXCCS+sioHH6WXP99gbUeZDnE7MQ/dECd
uZhOnHgtJAh86/fyaYTGndNGLYc6ofiJKl3UhgoXhbVW7BhvBAplf2phXwcE
Haztu1VUCR+ln0Xf2doAjpXHZwXfdcJrQXMVVUoRPDI725n/iQbyP7k6k14y
oEz8iZuCGQXKD2xw3u+D+lZur64eQweZkSd2QpKoe0u9DIRwPQ6ObdFLwvVY
vGvqcDuuX9/Ss+NSWP/DL/zcurGuhC3bYiaQH15vF0Y52b8kvFW/Ig+ff7YU
kH2txOC2INnXfPa35ZC88c4WLiH5Nw81OEHyGeQpJ0jiVZfneH4w3x995vtj
28jpFRWI7/L4Z5SHeLGwhH8m4mrNttDJR8BmMUOelM1U2CzplrZHhw4Gh17Y
XHbLByc17w/D1VTYsGEiNryNDn+rf1AnRKvAO/1E98kzDbBVW15osLUT4p/3
t/981A4Dj0oXVaxtgzHpCqNOx3YI4+05cLevC6TXvff8YEeHWLUsbs2ERhg+
xBZtoc4AgaGJb3ai7aAbez0y/GcrVOjl6zNQrxd/kDro1dIIuvkHttjPdoB+
+5NnsK4YtpyuU9hNpYGOy7JB9Y8MeObJaRPzj4DW/UYfLuLzDP10Z1m8Lx2a
1Voq7i8lYJfY7UDXTVQQAwmj3UfpsHSFprIDruv9d0T27MJ1rSZVIieI/Gs7
6oTmID9ZvjryuchP35Uzcm+Rn7uHv1qRPMsEPHQg+WwauClD8llxYY5nudtz
PBvR53j+RrGikX7J+4T0KPm8LkboOZF+SSXgyjeVmTwYHBFqHcZ1Jx8fsMtC
iw4vtk0z4u5RQFAm6bdEKBV+lu8fF3lFh5BNS9Pej1SC5fTRqjb1BvD4TMQO
FHRCQ56yaf2uLijO1T4ZsoUOAV8iA+65NYGxdMVjidFBUOQdy3N41w1yTtsn
tyY1wBZZjTMOfYOgHh2TJvSsG15JtW/WdmuAxgNnpVaWdoF54cSJRHOss8xo
lf1ljRCYJJ0iY1wDCUIXDIz3NsLAgjh589udEJhwOCcivgCemviJ6myigd6N
xysaxBnQNCLZLmZOQMLa/NfWe6mgAntb756hg+DWUj0b1HmFkHXQhnW+UTbV
/ibqzEx2AO8/5AcuTad8R346vP4S30j/k0GIkXX+gOtmP8ln1vn+CGY/nedZ
aJ7nQ/M8X9+4MJd8Lh+Gg/1I/KxRuiqJlxWslYtF3+X7S244AJ+v6Y4lD83w
+SrvfHRc+AUBF2M/ii7UpcLeZZNFipdRzx1i731/UQaXe5/4y1fWwVa57kNV
gR0Qci2H+9ydDkiq9Kh5HtEOFrdmY1ydWmByq8SNzb1DoL76+PjA+l54csfM
PiiuDViY/0bg+91DFbx7B0Gsu9jLo7oLZlISX+hyDIFubRStprsbhEp6JDdv
boS3O49zEU7tMBbU/HA/SxsM7K/jG7zTDjv8DIMNxMpgxaVp1dKUOujfZxr0
x74D9hZpJKhLUsCMt4l2wBl9L1/d3echdHgdzXtMlIsA7vHxNB/0V4m/2sat
0V9tithF/Y666l1H+7kYeWDb8619FPlJe3oS3iJvs3vvxmQjb+0GBBup53fn
ed44z/PBeZ57wh14SZ6XhTxl+r0i90NMv7dndQjbHrz+qvfD3GQemX28PfQ7
4ruedMRkbyLgeynUOsugzy8bfLwGdU9K/sxDQZlCSGVTOJNymAZjh/V2G2ow
YA3HB6fTtqgHq7Jyr2U1g1BK1HFZRwbcUtkjMRwxCKu+zgq3l3eBtm6xmYV9
3f/w/LzV22OqewAuqa4KXrW9C4zCBIQsBIfhHd/Lewvv90LrH8dzuyzb4VNO
ntXf6S4ITRGZ3u5IB/Xv8amPQhuBz7r6ZOC2KnAq1WosOt8AcVORGic7O4G2
6OrQl0P5EMmuvcImH/32kqCNMrXIT5hS+4LNBHSvD316B+clJz2mJY3zSjly
nvsq1huFi0fRA3n2GKdnH8d6W1MrtB+TAnQIb/IifYvfnrzDGf+F5w+secy+
6XnSgdk3jf71MnF1Xctbn8k8tWR0IXmd6k5FBqlLtOPxMb3TeVD579PuetST
fVuDNHVRTzLe/zhpKkeBWXexjHIXKpy51PiNCKPD26mlRzQmKuCe4d5T0c71
MB0kI6Ak2AlX+R8VhvxjAM/tRycaLNpB6274s7SVrWDLZbvUBflvz51szSjr
AsIp9ON1uzpgPewQWZo8CCZtHAp+C7uBu+qm+tDvOhDRT4vf8aATohpYw9eO
tYOx8KGXF3Y3wzfuP4bvGFUgXzeRwf2xAYIcv5pyjnXCy9frXfnb8iE+Q7W+
Y4gKQqt6FEQn6HDl5axA8F4C2kWStcrRX62JoYw5or9qXqngKY598+51S/4J
5FmQvfVzNvJM5BiWVSE/JyU3nyGQnwO8FVYf/gvPfxO4mT7wc/NvJt4Q5c3E
T0osPEjqzyfqwd8kLnZgyxMStz12Uewc9gt/CZkWOfL5pk2YrsD7lrboyIbJ
EVAulzH0Bf2ViiDlTDv6k/XRRQ2JvAUQHHuPJ2maCofFwvS+sDIgiOe3UfCl
KtB8pDrefbcB+r6U2JWwdsG+Me63zYZtIHVvpqrCqRWGPd3YrGbb4dKZU6IX
DBmg8amndK1UO7wX3rJEvqkVzLWVBfYEN8K10Y8XLuQ3w96NkbuNzzNgi38Q
e/Pvcgi/eePe2q31wD5p9mW8qQMUFzY/j5PGel7I78idSwXO2o1tQuXo59cU
VDnvIKCoKl9yFsd/5sfm1ysM6eBM12Rdijybr7qmMYLz3dWezPqezEfbdkRS
kZ/KLqo9BflhbVqrlfVfeNZm7WPiP5Xncs1u97lcU33aVZbUkzjabXMSj/pS
LETilHa/jCbEn+0wEy5AXOCsSdZHxKN3ixd9QL+6Unj1ZmWsc9Nf23LLsI/E
zuQHnXUlYJWRZKSdOhUi6ruyrpynwxvDHzZnthcA46zY46/sNCjdGbrfmosB
adEGIYM7y2DpeGXcm/Q6OHMmRHuVUwd477A8c2F9JbgpU3fc/F4PsUXmgjPn
OuGrv13budky0GIo1r/srYN7YQHKas87gPWzvUHwqkJoYXj1pWnQwGFS3kZ7
HwP22E13/esiwP9q1sLIs9iPcqZcGLcw79i8dfNFf2VRasIWhv5qD5f+J1H0
Vy+oMpNLUDfkm0byziDPt5+v61JGni+DyRkK8vDuy0w8mTeVdkeovfsvPFdF
zPu9kDl8tG8OLyjPniXxntAppp50yJivIPGcyHNb3iBuuJF6nfQn6h3hPKQ/
8blWcicA9Tz4u8LbccSPyrklMBD/pLO//w3maMMlt4070ZdNjV85KIc5OjSh
7rTEKQIOL4qd+qxEBbN9Snzi6CNiFlYu3qhGAdWe9dJb3TGPX53cdOUBHbiP
emYZ8+SD4EO7ctn3VLhqz2LkWYB5X/bJ+YwcCqy2sqz5FYPr/dh3+f63WG9t
3WHNBAGbMr6+W3sSc42wfmTnNewvgnxf96AOP4hvFHuIOuzM2WqmgDqc5pgl
chr9RhZUijaj3/C4b1J5HetEEni5tHFeh20bj3fhvAbPe7HX4rycn5onppM5
5TbDieyDrjfj21P/C88M+zm8YB6vncfbnUSYevKhb4qJq2b4M/HWzx77yDpf
c27SgsRf7AkQI3GbeJGd5H7O1z/XA8n9nNN6gy/J/Rz/Jom/uWQOEl0zvRDr
QV0p8O0QiT/26IzCefGWrrg6gPO6sYc77A7O65qu7f4EbgLWODiF7EZ/m34y
KHsW+35XWmuA30YC7snLtjhLY47ruyFod4wOuwMrjILXEMBJHXUVkMLrGPvv
VcU8fu57c5QDOwEuJxuP7RJD37uqS1RHG3U1kP/eBOpAwyz7he1436TTOgP9
WJ+SoqXHXHCcuZeoxj/JvpZcsbkVxzk6GZpJ+meZDYkqpH9embpGiexTD3kT
Wkge+r7GhpA8XLh6/BDJA91iqzkDr/OP9bIgD873WY+4wgTiKxMfTvOi3o5E
sCxZh7izQ4MwK943bq+IA+Dv45uuU+l4nWnPWb4q/P3iwdrx3bsIuF7jpUts
p4LGxwULclDHRuhhMoPOBBxaXBvvvB/zfrmBfZwV5s0mhrKjIgHZJttHh+Wo
oPzlvUWWER1+PYq+y458Gp33fMmFfKq8/dzSgHweXWPJUo7j17Rfxk3mJmdJ
HU0yN62b7Frc9oCAj8GLLKK1qeAY5s5hYk+HVCnfwppmCkgeVHPfnECFc78a
0lKy6XDrwyPvhOUF4DGj+Xz3DK6X42xxu9gYwCp71UZ3kAIrrIQZOcmoAzK1
jf6fcf3eUI8u/UJAz+K0wL+G+Bybo9tGXHD9JkTMKGH9F4tKUcj6l0gT6ybr
P3Bs2ZYA1Mk30bnXt6FOWi6/xZOHdXLpinw8mUd0FxV1kut9poHTmqzzlAPq
Q8vbCajxD+/7aU4FLV6ZAA8POhw+MqyrdrEQCqrkBTxMafDcx6HfxIABHqeU
r5psKId769iUYLQOfNzc/b0SO6CsNDpG7TAFKl7zLM26SQUG1/LeyCisk6jk
O+myBPpdfjtF7C/CAooNDtgfL172n6n7kwenIao9E+uqIEvS2gTHyfJrSTqz
TgqUfcg68RrpUCHrpMWkI1Y7qRgot7ZHp0zTYCvr7C/fRgawqdjmHVyH68d3
iadsUyMYXfIsPr2gE5qkDpinyrWCq9Y61QLuVnhiwmkbdpwOfx5/u6KJunQ4
TN1yD+qSiqhrzA3UJbFrlqen1hIQunI00hXXhcRQtssDXBfe8lHLyPHsiV3H
9L2nhJqY+VrK8YTpfkcCTqa3iV9Wo4JJDV9LsSXW/yWhUNku9JXffVKfdzSD
b/bCkJVHGfDp5PXqs5a9sEntVXXJSwa4WiXXdwXTwKIyJdohmYDnYWn5x45R
IW4i9tEWJzqYWh7ddGcBXj9U7K+QKBV+OF4yt0S95c8IZfpD7qEzTD3/8qZ1
bj9w1b+LCeIESEWdOyeJ9fCOsWVroz72l11nn00pF4JarsG6lbo0YIj8seU5
xIDLW7qeV+wehNGw17VBjE74dODXz3Ks90x/9xNrbg9DzGTCirIXfVCWsJZb
IZMOXzceXJCxNx82anyR4M2jglUEtelUFfbrrWv7JyUJeLaB84vJVvQDbSp7
W7AO9youZurhYM/YXC4u9WKOMzY/jc8O+8gOpxdeVdhHVjnlW4rgvIIfhrBv
olEglu/mPe04Kqz9PHpu3Uf6//h8gS3sO4L3DEDc20SPgf6O/8GLJwPuuRYO
gNF9i1628U7I219YmTxTCMoPzp6Nvk2Dp7zCbluuMoDjw9q22RsEiA+/zcnQ
QL3qztK+Y4111dzL1PO7cnP+xMlpzp/8PsTXnYp9+fy7xsPBqD/VnnsOnCN1
T/x1F/gQML72yLZLmlSQVUjpmbVBH+50d+/zghKo3G4+mnC4DrpMNk0Iru8A
4cltX5MthyEprCdm9/E+XP/3YwuRn2vyp8NeHhoE1s0myeI8XXDz2pvPZyOo
sP3BXH9Jn+/jnP1z/WXRvlsqQbwE5P1ZwXME/UPzyr8xvUfpYHFPMH2feD4Y
U0VzNXKoQGmyePKsFHWj1W1Jqn83dLueX3T4BfKroX375GADqP5d6GOgUQ/c
7P+KfSaawPHytE4ofwf8HqqQ/5RaDG9u0iS2/aPB0mCdvLImBrR3OdzJ+EHA
8MXQo7znqHA9LfTtQU86iDvMjbN+vg92zvfB8GWHKWrYL1JX7JMn96Vd67Kj
viFv0dx2l4uMCdjXvCBYThn1bULqui/6BKqXeHDBAxqMGpi9kDdvAj1xzuoN
YR0w2h274XPIdzB6X9y7N7YeVhdR4iWVO6FW5c7fyfsFsKNPftpWmAZ2Zc/f
fhRmwPreGc/ddwiQpvy0rjqIPNqHxUraYd6Z7+NS8+PUmx/nPxe6I/9sHjxv
seFxxDp8tuz0aBXmJqUM9pqV6CfjqlLunUA/yfUlY+Ys+sn2fasGD2IepKen
cxzEPKguVqLGh3nw76VlS567F0GZSE9h+ysa8IZtFuoOZwBx0agsUqEAeoQf
3vi2iAYPX1jfE+RmwLKySy9TWClgH6plEWSLfdbf7fRDP9RbFuFv3qIEHPgd
qRq6hQrJ4/ZjJ3H93vsP+x7bXTanH/uH47dT91HG8ev67qh6ieMPveMms34b
AT5n7wuWbKPCo8WHZfhQbw97p0ZneBJQ0H7/X/YB1P9/f2XPY91O+y9sEDxD
gPHZLseUvVTYOrF8hnKGDk02KyxZVhNg7sPxKk4S/ertXGqHLvbTBG3WS+hD
Dn2tWEMn/UB57KwH6va9TSI5itiXhZZx+ZH7AMZDa/+Q+wAjsh8WKmAfL2aN
ZGzCesgf1tBYgvVQJWjUU4DzWun+/tRX/L2QFE8j6W9nu+bm2xw/N9+smbn5
WrrP4b535vB/nXP4A5cDcQl43+diAkX/EO8WX36tF/FerokIpm7/yXQnf6+d
F6hA/n586qoWgbqa++SZ0HbU1eBhhRlH1B9TpzJ/1xUEhPVFNydJYH50d3Di
xfkmMvIFHy8ioLqV+1kl+qKwuEdyVeiLkru9ByelCFDfKd78AnWvUyVIZjX2
l6QN21Qt8Peb10hZfsPfJ2cGGVPw9wvDT5Up3kff9aT6bvthKkTemH2bexH7
L31lJxuOh57x5/cUPkcHuzMyB3A8ds1nw4irBHxbFC2fjr4lu9Qotg59C1sQ
a5888q+j6ZlWifzrj6dKOiL/Fipjoku4sI8cuTF5Q5wK9vqPjpxD3yLWk3Zj
HHlQemgbRu4n3wjm+veFfD+ocG4ByU/g1cv6zP3MuwQzFxT9sb9VyEaAdvaY
z1fkxy5CsKEfxxPfte23EtZDgGD0jkysB+6onMKfyI8Mx8PvPIYEsH+m1jAU
qWDz/a+nnRniZo8MvM4RMNFZ+MdQhQq0O9Sg42eRf88ctpUsFMjdKWhAs6HC
DnOG4j9frLezMgYVFylw95DBC11/1AePU1ffxNJh6E5WyJ6TBIx9GIhzx3zh
cWOFxCBeX/PF8ttb0UcFPlfhWIk+KpWfS28N+igenR8bv6PvrX7v+uUO+t7s
S+2N19H3XlZQbWXLxvGnhnmZnUD9LFh08cFVrFu9g2dlVQg478Cx+u8OzE0N
L7KiTtLBYCLB0pqDgJTkww7X8Tk+zxK0uIPPcVvFwpgs8j11ieJZ0rdvn9Lx
JH37qQ1z/v939Zz/V3o67/8FJMf27ibgmC59JTf6zBb+L4Z/0JfSRldezpOg
QNsBk2QRZ6zbu9Y8PuQ+nn690gMK1sMrZU0dzDsCbCI8vK50+KZ5cWp3cD4I
Le4WfF+P833o2HCpkw5lwyrhctwFYPL49/ULU1QIfREbl7GAAZkv45cyGEWw
zHZ5l2Y+DfzkS5/TkhlgZSOVQg3PB1i422imgQob8uu8b3dhXwu+rZ+gif23
cOD+hxuYxzP1P3+JxHpwv0v8LCuDxlUPI5811UFl+K90xwcdAK1XVcvMCeBy
O7UlEnUjM9V2WQbqBpffuchA1FWz/Nn34VjPtBtKOkOoS9Wu6Vzkvgrnqn2d
JD9/iyuDSX5MBeb2B8ZV5/qv4625/nuRUAzw34P9+sTrRanIm1prymtP9OcN
dDtDBaV82H9nQ4vZN6xbQk+9oJIOxxT+rbZSLYEn41lX4zbVwY5WG4MfMwxY
sqj4tVJuEah9vhdanU2Db4KHODleMCCtLnlDrX4VnFKz/zhyvQEmM1yOnvjd
Cc/WPCscpX8HtQSvgLLMetB5fJW2RacTijKM3jLGKiCccM8dvVIPlypTN6Ss
6gT7Hlq+WmghrFryRt7jPA0CKhJzS0wYcEK25ubSqHzIKX5yWLIJn0t/goFX
Nx1OthzdTa67jd/tzpLzlT51ToS57gLn9I0/aD5X9szpG6vMzqdPJ/Lg7YLs
tea43hvtOzT4cb2bch3UGiomYBvA4dJTVPBvSDDWdUOdf5N5YukzAmp3DJhc
0kGdpLAqfb5EB47N4j/uC1WCXkL6GaiqB1nT82kulp2g8HBgZ+nDeqjW+lC1
Y0UzTJvfzcv9y4BXOe8y17c3w/fQp83FES1wS2fafCX6MTEb18ffdndC+Ws/
j77qdlB5tnePS3wzPI4IflFqUQMWS3KDPu1rhKpez+BAr07Qs18ZFmVSDDwi
zYtX/aDBg0bv1ONFDFhNWX1Ce2URKPC0vHAKRJ/Wv/LOyHUGVP5dml17hAC5
kMO2xxSoYPthQkXNFOuhL4ypV7qZN5l6FZtZwdSrjfN5PHi+Pyb8v/tOnw29
LmDfoT7e77IT+47o2bPBq7DvvBvcbdyGOTcocMvp47iu5ftPHrXBdd2nKZvJ
5V8MHy8k3Zb4RYP90xWvPasZoAbKH7Q0CZgR8Lbx3IX1Ro/UzTbGvtMxkT3i
XwTaF/P/HE6hQeJ6w+LCBwygRB38U98zAF+FNyVJrO8Ew+6gXap7a4Czra/h
6PIuEPCbkP64kQ6ZKvdfesU1wazAmvP7n1SCAE/wmuWrGiAnPMdXL7YTphe/
T78qTYDAFzufAuwvRypjzu3C/nJAjf9WrhYFuMUE+MMxZzU7P/x8DXMWyxl/
2xr0h4uKpzTM0R+Wp/z4Pon+8MPiM0xd2nNkKTMvWJ2KZPLzdN5XiM/zdmSe
N1aDnDQu7Kd+3FVbaxCve5OVRUE8wOHc2zzyfMvtnw02WIdC0hLua7EOJ5Ze
zeBfSoFt+04JO16kwq+bfc2cAXQ4P9C2O3gZAS9euaz9grmCP7P1z2Ucj9m1
GHad4XwYt/slafUL8zhjRnzkLx1sGjbJHWltgNLv6pvArxn0hPrrjR8zgEtZ
lJBYWQOLXs1WrBZvhGcp958mPuwEidWyJVsU+qFSqa+XdrADFJKOU1fY1UK4
wqms36eGQGP7sYBL2T8gUHr66q8dzf+TF1iecduzXx6A9teGW/v0O0H/QZxQ
WeQwxD9eFuxf3QcVGyr+SY/g82FbwGK1rBNi3ERizlDaQT22R2NVXTOUZr1M
XMUxCDWXAy01rDphIOXc+7DRGrgWffdv/6V64OcTjqNxNIPF+fFd3zg6YO/w
7MIPGejb/63zeGpAhcAk78p1zrgeF7OwWfsUQIbLnsTKdTQIdfh1p349A8rO
bkvowtzxgvec4hOs2zba8mYHrFvGL7cZ6lECuG/db3+I66LwnYPbTVwXPQ/n
zi185Js7t3B80dy5hcD/sH8lMPHeYTX2a8lgnVtd2K/VxLryLpL9NML0L2dK
MTBSznBJzNDg385v00OYr7eU3hHK5qRAY05bufIF9MMbtokvvEOHrdtdHkfG
fYdGVpdAzvh6SI0XPbJEtRNaa965dlPbYc8kTUVFow2CNvuzK+u2/w//Vsx/
Q/P/3wPuyv5bqxcMwVKl/PTIpm54xJuhr7Wk8X9+n/tG/l9a+AA8L3ze3H+/
E1jClXRyrQbBKPy2d4dhFyyQVOkvOkQDm6drj/BRmuHYhx8WQ3dboD30ZJ/B
N+Snsm86NIsCSaXfdq6IQV29nhq27S0drI++o8keKYFmwpVlQLIOJFf+FUxi
6YD9+bz73/zNg5lkYa895LmIzodeJVjnzb3JnMlN6Cvaw6OzzJC3zX9dXG7S
QYVn7nya3ts5ns0XzfWv/7R/yH42dJvoEswXT2qIUvRpIUfvtr5En+b48Mb4
BhPsa8orrnuXUCGjq1bRrR7Xe8KZ/AUKBOT3JV5Xwz5o63dSdhP2Qb/6jjAb
nUJw+uMbL3+cBj672ReaH2GAul3FX9qaKuhy8YzTNGmA7V9VGPGNnVB69N3E
us3DUG2d8ao6qxf2jb0pawhrh/2XIqTvJg3BTWUrKSvPHtjdv7DptHArrBH6
1CGelQ+FEgGx/J1UkPR8xsoxRIddNy7se8lKwGRkXYwD+kP3PxGvEtAfcrrM
zXex/9x8czvc/isPHVuMmxj+mEeKjnV9xLz2Y0dnxQrMa0JJkg5fRgkI7r9B
FbZCPU9pLmr1pkNaR7aE2H0GrHTeds8R2mHZfom4yvBW+LUtXbHkax/ES/bw
MzZ0gFc09bvjRC1U+a9forirD57scKvcNoT9TEVI2OQRFe6r7dC3UKMDD6dA
uUdoGxgdXfDjCm87XPsms2zQtR1c+3O+RC1sA9fl2+yO+LYDl4tbnU5mJVy6
OWNkL9YAIs9eHLmf3AmZZ41XuJbVwCqvAoUtlo3wrrB8L79FJ/iGNNGEUvLB
qXN556d2KrzaNPVgZz8d9OUu0KPVioAmw/ojL4oGTeZXHur6MuBwhHdFL/Yj
g2IdDSPsR7ol77/aYT/KuBcZ9hx1+PLx42YzyBvtMdvBHvIc48U5PrXn+bSn
/3eeJYJazrl6FkBpR+pmFtQZGNYpk0CdETmQ6DisUgkS0a0n1zHqIbuXV/ON
E+qh5jGlFvRPuhK2C+rDmqDgoVIO5XwHBMpYt08J18PsUMg3wf4mOKP5c3Js
fQdsiso9+FG5BiTs7p1NkGuEXaafSvWDOsFa5cbLYfvv4MdxtLPhQT3sZH9+
HuQ74aEwX/VXxRKw3f2ZUSZWB79bwthOTjOgWYixRbG7GFaPPN90bGkdiLzL
rQ7qYcCm3H+F5T4U+JQYzfUuCOtqdXobbzwdVPnVwqYF8iFBtWfxt0wqPq+g
1OpCOpwzVGgvQP/Zm5jS/QL95yjbo7Ep9J+PVrPlHtXFuo0P9mPdQ76fPZ9K
I/XTVOTyBeT51nquE0Pk/sDU48dN5HnIeT5F5vnU+j/wvP0oe72SNAWMNx+K
qUY/v2e9nrFgKB1a0rI99CYKweGi/LN6bxqY7bbITL3CgKm/Z8N/fcbc5KUQ
8NkQ8+lFOPzZBf1/59q+xYlFcDO+5fau9zRQf5cYf/gJA4qXDySFfKNA04PW
uvtPqeAsqJ6u9Y4OLyVpF6efEKBrEXq55Sjii9is1NDv6Zjeu/ZsjALJo/Yy
lNdUEP5z6Rx8pYNGXFnkkn0UoBxxvPvoOhV6Kf8+TkTQ4fQxx8U/7dFP6guN
+qhSYTzIspDVkg4Hne/5fsIc9+/FalFrzHG7o+u3nsMcp/D+SkvdeB74J5Q+
kUA/IC0l6dmJfereIcvaq5h/z19fFlSA9Zx4g/6uSPs/96P/xCf/V+XXpuhn
/hVYhQL6mc9v2KAV/cN1lUFfMEJc5OL23ZgHJ8cSY95i/zp4weFKTzQBH7Zp
pRxCHtRfHm/Id6BD7c1Ot0HUT92q7b+9UT8HpwfMTFE/r1YtT3RcSIB5qgrv
COrY4OTXfn4c50WZc6fIfMcZkqVD5rs7BpmXyXwnbdCwm3xfFs690IPcP5f8
prac3D93vzM3fsl7c+Pnmfft+ZveG2rJEmC97tvX9bJU0IznSzMkz62Fc42N
CRHQdvC3aRPm0+U1I3FfMZ9mb9xU5ov5S5GbcvcN5i8DviShLMxf0zfWOS/6
nQdHzFSfJKAfeDQmW+CCPAuZrjH3DC4Ey04/M1bMHS/XnvI9g7mDO9RsST/W
89Aexi0+/P1R/yyYxPHIJR7k2biuBP6ssH2ita4O5GK+L5H5zQBb6VNK5jge
V55dy97ieAZKBGtjcDwVfrv1e5e3gU5Q5NVw41b4ER19IImfDlNPeIfvedRD
25TN+qklzWCzuEzqCmsHNPzeFmufUwFVQQpSMxb1EFB0IESBqxOMubjXLk/I
h1hjIZXjbVSwWy7tzNGHeXBiTEOWLx8yLDLH3r6nQnLG+SXDBXSsp6VmRU0D
kFSwXNpjugPSluVk7BuohuxfC6kTJn2gXB2lXT7FgEs+kg+/ox/Z/fH3coHe
UngpsabkeHQdrLXTkl99pgPuizYNvaynwaeDhzJPuTTBq5u5Lnk3O2Cxl/jB
3hWDMGRdcbnqXifEBvYYxRvUwg6XhYa3zIaBVj61slSjD3wub76SoEoHlzUW
g05RvWByyFE+N5MBos9bApv0aWC70UA607IWDOK2lHM8boSNtXlxnJiL+q8O
qi5hIaBo8nRXJurPxn8SE0uwP0pmh68qUeyG9YFfOPr96MB7xs916GAjxDmN
Bc7oDcMYpWb9QrE+6C77LZ+/Cv2e5MZ8t3tN8OLTacvCDS0QWXHowtN1DFh4
eT1Xh3kx9NvskD7YQ4PWhT9aThQzwCTG+c+zVQS8CRvXVZekwsVGv5JQXToo
221d0JpGwJ2rS//V6lMBRn8IO17BemjxGb4oXg/NOW8/Hx1ogmb67cyf6zog
8tHOcK6dBJSXWEQ5bKfC9KbSS1cM6XA2xUXaovoHEOtZtkitYgD7Jraadzb1
sP5d0Inde7FfT6/uf7cD+6vBO5vTJ1Gfiyw7zqM+vLdk8VmM+kDNSTxUgnXL
v0vLWuN+CbyKTX+UtLcOtPLzzh/l6wBd0U22/Qtq4dpWp/djLo2wwIv9oalB
JwS6VW1RECVAeRXvfvctVNhUck3toD76sR+3N0vjung/neGfjnU+fjlX+BZ5
bv+QVum7NwVgmvcnIUSCBts2n2r4spkBypNxqerc+XBBcfny4++oIOi2Sawo
nw4+SUmpnehngl82enuiDvzaE/o5k9zvmrj3i9yvCFg/LEau98A3xwvIdb3g
5x7jdYGYsy577Xh6iApm+Y4CdehbEqViIgwQNz/xq+oN4n8EQ1Q6ETcct0t+
y0YA34mQnGy8vtP1m3+68fqfNv6IMlArANZ/R4eduWjQIS+wzXwZ+pRuP4bS
VB5UCwo2mKP/vEifSupH/9nUa8H+gk4A1/DlpWfOUEHj1fZdLR7oYxPTlHh3
fYeISd/opX71UAxuCUObOkE8rFmUd6wYSmJSF0nx1cEau9xVZf0MEOrj2+5r
0gRfxrImw5a2gE/dtw1s0gzgtLrb9PhPHuQ9dd8Sj8/L8zubwVG87zvzil/v
/mF/TEn+boj1LOwbUZ2D/XTsZ/DP1PtNMN7K1awj3AIHBY2We65hQGztS61s
hxJQTWMb995RB9+J4g1fOTugfOxqVv8IAUdvNfY+tcQ8/u+T5mX0dZ8S3145
i/5HqcUkRQ77xYbKfbzaqMNNQpJPVuN9V196lbgF7/suyuxKJN73blVr645v
BDzdK/v0tBEV/M1OKmhcw/wYM3a1AXk2zT3mlS9K7jPnLB1BnrePXlPlRr/N
Ea1imY9+O+Og3K2n6Lfjr9j63Bgtgk2uvz7HFNPAaWxU4GYq+o2CRRtnAwqB
7YnSl2+WNNg/fvagvjEDQDjrYYofARdvxmWeQ786xpCXLrelw5WZ5cNHLAiI
ylHxC8H+OHJ5neJt7I+XzT1qng0SIP+kYmmmBRVOrPKTTvGig1X9QUWKAeZi
DtFowPw1FK41Qj+N6yupaV0cOwVsTLc9P2NHhbf9traH/elgW3tUyuFXPviz
u925O4b1MORv92OKDvtdDH+U8hAw81WPfxX2x9DjHes/YX88Nmx1WbexGNbX
FrKwcdWB8gqHMkoXA4JoVX+SvQkIe75FUFMT14vsvQMtNtj3N/umJeF4wqNX
+SjgeGa4Yn83nf5f793eyLq+2Cw6AJpK3sf6vnXA05xdRwNRZ3YwTmiroM6w
qy3ICkSdqbqrmJyEeZ8z2iunAPM+d76k1TUcDwtjz9XnlTjf1g2sr0yoEDw0
I8G4TodDqi/pX9ILwcDP2/v2JRpc6VvLYmXBgJYZXZONbgQMHxJfHKtOBUrs
isHY83SYdYS+7o0EQLOycQj2qQ7rkgue2KeMD1tL/ELfxMvoqhD70AY2609m
/aS3gcone9HXt6pBo9R5Y+hIA2zhdIp5mN8JXpOVOV/7C0Bzy4avt/fQgCXN
NZ9rFwOSwk/H610hYHXdoxNs+6lQ4ZkvQH6/8ORyTe5+ryrIqzz/+1NEA/zd
5Z8jsKQLBvckhG6wLIFs4fHIUNk6cItqOqyOef9G77W3O1so4FGtnaqWQIWP
WhkxRDYdVil9j9bdQ8CBtJibT9GHyL2/H3EFfYh5991fRi8pcDtgSMwlkgpG
sbI3nF6jfmZpvZ1AP1ZFSS/1RT9myBl4YqElWc/nql9jf78/e8CD/D5ib/K+
vj7UpQYXQcNwxM2+i4pNkueybBZ/6CR9VB9f4g7E1ziohJDfhXVx6+uS34Wt
+XVGo488z7DuskwJ4u6vnwjnIk598odGIJ6XG3iLPHc0Hr9VhHwvM54ya5xB
nq8biB4gz8noyVgfIM+L2q+5ckgU+0UeV6rzVewX5iIje9ywX7wZ+8NdiH77
5rK1v0TRb6tzejWOot9+IcHjmRiF66Vw4ee1R6jgZnjUtNAeffilxY3jWJ/r
/ox1GmN9aglHVAxhfVb1ntMQ2EqA6kyB161tVHjv82FzzXHsj1KnRV7jePR5
RraT+pzgebiK1Oez2/2myH3CcqXBKNKPeQsa6JK4RWtnfM8xAmy+23rm4HiC
RH9GOmKdG21us9NMJaD7sstSC+yPCb8/jizH/hipKla86QIBAnJDAZ/3USH8
dco9Yws6LJX8oLdZnAJRLKtA5AoVdt1KGkoOxvzL3U+9siAfdG3F9xikU8FK
oWndPoIOUfeD1rXK5oNJ3bE0j89UiIovKThZgf7tw5mjEVME8EpkCT+wpsIT
29CTI7fpUNwYf/Ua8tPU92bDMuRnC8sUPQf5edQ9lGWvQ0AEV8ARigJe3zxF
+TnyKfD4wCFyvjXeNufJ+UYeiNpEztct9OpyEufO3sb8/mj/bB7zvC41ZO/P
hccpsKqofpG9JxWEWLhp7Y/o4NUUJnrNhQBN1xGTDqz/4nW93Qtw3X31u7CD
tSQflEdSMhf3UuFpHIu3/Cj2TdN+8dz2YuA5GNlSuqQO6kX0WjV+YE6x5+sb
fFAKss/5BXxu14HQ1THXliMdYP1Pk5UczyRrOnP/tnLtJHP/1nJlIvM8w/aL
1sz9yQCJTqZ/LllQPDW1rgr4U7njr5k2wC2lmbGEpk64HWzO3M88KD23n1my
d24/88qlRUycsJk7byBlPnfewGpnEsvXdQSIHi7blSaF9f/7gCdFjw57u1Id
OZQJECQ4FM3Rt3woSli0Cn2Le8GtLwP4e0bw25ef8Pf6vyWnKvD3dZ+HmT6/
rXaQef0lte7M6+8Imjs/kL5u7v3F4jNz+z8+5Tt+y2DfofHRvlVg31E9tNUi
AfuO4tAmTUooAW43q622H6YCR/13finyPeCwmXdaBQUCE3eLKL5A/b+Ycsz0
Ax2StbY+8Jwk4MbFe/asWCdP+zY3O2Gd/Hn26t5qNQL4mw7v4N1JhTbHvRu9
T9EhbfuJwCbUz0tPzXRqUT+tl1pMl+Fz9ByfyymOyXM5JWLB3Dg5gufwjPn3
Dsa9c/mF5Z1J0FNJAvym1eP2b0UfO6myosgA+/7oUd6YDRQQovZv0XOkgqLZ
i5+iQei7ot4YFfsVwtexXtcWCxpclORnhJ1iQGTyysBPmE8FdjS/LcVcpnBd
4vKOS/+rv1wL14/e1zIInu/WiTf864aaXscVYj754LZk4clnVNTP24866ulY
z7EwbYk6sFslbGck6gAP9eyhbtQBea+Tp2dxPP3T7tcv4HgoLGm/FXE8y53n
5nXDb25emvP7Uc/n3xe8m8+b+fN581lBcW+UNgEZnpfT/u5GvR2TP/rHhA4f
QsT9lkRTICb5/ReJCMyJ8jmtnsl0+Bzx+rb1egJYX9twtGGdVL34+rsf6yTg
TWSSyoEC2KgWdChwCQ0Gph+WavEwwEqv2ld0dhhOONxc1lHbDyem7+b4yXbA
0vn9nP3z47GaH0/SfC6WmMd15vGyC/Swn8bYT581Te1VpoLPFqt1IeZ0uDQm
4fvoE/p8KuX+KkPUzyM6YduxL2b4jV7tCS+EII3VV5SsaWC3WtzaxpQBxm7b
JWiJFGj4luQ2E4X9Ql3H/dMbXEecgirct3BdGB+10DhAhfODaXwLbchzMul3
Aq9R4K7c1wjtu6hjnzRN5F7QIWx+nML/2z7Jvf+Q62+tluJQ1SSAaqafdXMX
FfZJvF+SZUwHusingq3LayFVdLo1+UYjXO140xyh0wkOwgzZl07F8I7btvrG
IA3aTm0ezS5nQN+0Q87sSQI6DUvSM5TQt7h731uLPHCeq77EmkPAwAavPfEn
kJ/p3Yuqr/7nfYb/hIepbMu7U1MI4Rwdqj2uNKhOvCOtc4EBrT+qxAomCFhy
rG6f33mswz+5i7lxPbqtfZhdl1cCEsf2mDzSqoPpPQKfd2Ke4mvhd6hxy4cz
L3t/ctVgvakvvJ/URof3rSJDyevz4db3iy+kPmL/PTWg1FRMh/vuUkWvBwjo
OR243xF94wzdlNMJfeOumKidH88RMHlec/d59JnuQiusz6LPnE5PXWO7FvvF
Ow4VPazDMKcVzlewDoU7POXrNxNQt+qbRL4M+qVRQVdTg/97HtrECla9Uifg
1VKDhQWoM3Br6el1+Lwup5pyjKMfXjw4vPYa+uFt+1hNWtEP0zSSf0+jvpm5
3RZXRX3jD15/cjfq26jz2vXS6AfGVqg+/4P9qzhT1qiCfK9nHilUIUxAXPbl
9FWY77wYkTd/oa/j3zdp082B902/eekJ5gJPj7Vd8ZgLbhensFmgn4kp/vyv
n+wj5288byDXS+PQPUHEh9PfcdYhXmqTOlFIniPd+iz+LfkdveFr2gL0S+5+
aUsGEP+SLnmmBufrbPaoivxe4F9oRD75vcD+rJZL5Pek1eq3P5I89A2U2THf
p6j67ybfi/223bSUfC/mbm0UTb4Xmww5t7XXEP1z54GdG7EOiU2WVk/M6PBA
+HHzpeP4XPRPhDigr55uMzPmQpzrnYkdrRv9UsArRvVZzLOKwYMCnnQYaHy2
hl+LAMWvStqaqD+Xg1+tOYP6Y/i9w7O5kYArDaDiYYa+WvjWes2bOK/m5Kvk
ec7Z5vA60v90Nv4wIfejTO01OMk+az1x6jg5/nQh81Xk+Ad3+X0kzxunhKml
k/tax68PEOS+lvyttmuiwvngStyWEc6mgrL7Ss7kEtTDJSEr2XFed0C/oQ3H
b/VPa9Yax1/Lqn6HvH7ej22u5PWrHoXvJK9fyWHN7L9yKjzMvnxgJopZPz8V
2ygkn6HsTZUkvn7UxYPEc39ZxHEWF0F3VsfH1Z9pkDTsdaw4jgEy7/NcfCby
wKadEmKEOfT0hGkfN+bQuvuH9Z+vJGBK8euRGQkqLN23dOQ45o4E6sgicjwJ
LvuZ89U4ps2cb92mQWY9X1w3168jFOb6tQFHszj5+y3s4abM78XuHtlA4kah
Cr+75Akw4vG6YIk+Vp99Vasd+tgvNlXMeRn5bGHOKz8xnXkdi/S59QJRc+vl
/M/571hL1Jjn7YOVGpm/TxBQZ/qcO7I5zgbnsV8YxaecB6yrzzdDFM/RQaZw
Yu69P+8I8zru72/OvR+ZP1+3dP77Tb757zeDaOLM8Tg3Tc/5nJt3mPioZOsz
MfThnz6eopI+vHdjeADpwxd8e6tqhT6h7/JXT3Fcv/dXhgpEoE847jH33upK
1dz1nwjP+YHMeR2QnteBY/M6EPhy7vv0jelzPkesde730jNtUndrCbC+YjOV
a4rr7kKpU5k7HbbVj4ZMn8yHtPKd7/qLMaff+Hjkbx3mTRX1VlWLMti2XUXm
3uc6AP/S9SM3OiBSpFl2UJEAkVqf1cvRj3m9XbGVivko88bceDrnz30d6fr/
9pf//Vzc4/nncnT+uTjNPxfNZ1xFVCUC/j3Ten8Mr7+b/+vzJej3wkK7hPd+
Rh+l6Np8F/um2FWl++HYN08sp7nFxVFAJsPKuwZz2dk1xYbfMJeFz/dr6/n7
+v4f9DN//u9UaM8/R4355xi/3Vvp9DsKCJY8ONf3mAqSN1+935GOuq0lRj9p
REDIlJm3CurJiK9IcRauO4lf+5WceAk437EtSIM8/7lwWKsD8/Wj+fuKzt9X
+/8wntJ5fPs8bjiP2zw/0dKNeiXXFG8Yguv9fMXSaXG8r5X1gF6IAgG2QtcF
bDG38vDuDdHE56K9JPrvMswLSeWdmwowL1z3n9bsxbxsUCfGGzGVB4qQ8MRV
mAqLpNgCWbTo4BAXevLTKgJOp9e8PCxJhRsJMRORuv/37wWC/8Nz73F3DLvY
UwyO18/HDy2tg7E1BdnLexngq9Hq2radAjwX2D8luVAhWO7EofgwrFubU4/p
k3lwuLT9FQu5zxZ9KOoR+T3FjdK6TjsCJNqS9qRg7rv+7vB6bcx97k7a8l27
Cai9ti9QUY487ypxa6PR/30//U842+B6gcXP86Hi1V+Z4RYqxPGdS6zvoUNq
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
      "]], {
    DisplayFunction -> Identity, Ticks -> {Automatic, Automatic}, 
     AxesOrigin -> {8.719308035714285*^-6, 0.}, 
     FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
     GridLines -> {None, None}, AxesLabel -> {None, None}, 
     FrameLabel -> {{None, None}, {None, None}}, DisplayFunction -> Identity, 
     AspectRatio -> 1, AxesLabel -> {None, None}, DisplayFunction :> Identity,
      Frame -> True, FrameLabel -> {{None, None}, {None, None}}, 
     FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
     GridLinesStyle -> Directive[
       GrayLevel[0.5, 0.4]], 
     Method -> {
      "DefaultBoundaryStyle" -> Automatic, "DefaultPlotStyle" -> Automatic, 
       "GridLinesInFront" -> True}, PlotRange -> {All, All}, 
     PlotRangeClipping -> True, PlotRangePadding -> {{
        Scaled[0.02], 
        Scaled[0.02]}, {
        Scaled[0.02], 
        Scaled[0.02]}}, Ticks -> {Automatic, Automatic}}],FormBox[
    FormBox[
     TemplateBox[{
       FormBox[
        StyleBox[
         StyleBox[
          PaneBox[
           GraphicsBox[{{}, {}, 
             RasterBox[CompressedData["
1:eJxFlwmcjvUWxyV3YmZsZbtJ9i1q7q2spR9ZrrJUuLYUQiEx1iRRdmNwiySk
kEtlS8qSUYYwxjIz774/27svM7JV0r3P/zznmff9fHx4/6/3Of/fOd/zO+dt
/tqMIZPurVKlSpH+5x79j/j3mmnhO19dK0TT5fGXjj1sQ395Qb0D/5LxUFzp
/ODPZ/Br2aDX6oy0YeIr3n73zpexf4p/Zt66s/il2aFZkz+04X1Xj3dG7pXx
TMS1748xv2Dz9PuX/nzRhm3//vyb/U4ZV1+3had2OIc3T87Z0KiaHUdLq/ir
1lAwLljSwvP7OTyT6diV28MOiwjTTcG1CZdeGXDhPOqO7Hbkwjw7yovOPLt/
ioIlyvnNP266AG331rPNDtmR1a/13KpbFDww/oylw6QiHL1+1zo/akfbwhV7
RlxU8GXgVK1tT1xEXi8RyYHedEEFnV898Vx21WKMWV94s90YB+htBxXnvN8v
W1hSjH/4WmV88LEDCzp/88yIl1WMfPnbnxLbL6Fqh5UNXFcc2HQ4e+a+NSpI
7luXYZsfbfPP6k4cfmz6rntOqlgwcm+ny09dwd5zA7qs7uXEla+v2oYnVJDM
zKt4V2R3gRMx/Vv7HtI4T1cxWMg/4kTGro+63zNIQ4Opg66vG1qC5iLNSSda
NL0xbfh7Gsa2L65z/kQJbgi5bVzosVU8QMPeSP/H/te8FOdF2HEujGooMq2h
Yo+4SCm2fCpeLszVqzC8VhDtFuUff7NGGR49/Fn5nnMu2LaEmuyJaBg/bEjb
L1aX4fTFHX1/uO7CkzuPdPmtUMOWRxp9bK1hwTBVFMCNDV8teem5zzSUiXLm
WRDRKSob5Mb1Qy++ueVtjfVaWa8bQ449rBOmoTcBZEVtCuzGYT27T3fUsHBp
zsD/ZtqwU4961+pG3fMnjq3N0HBk1M3j7jwb182D3CuryvySikTOj21rZ9lR
NO/U5w/meFCiZznnRxWt9er1XmPn+nqQQ/VU8arAN8vB+fBgvfZr07IZKtfT
AR1aXbAHqfjP3Vo+r+KqCJPlREPXldtDVQ8G69WY00pF9bGbB9bPd+Lra6Ix
vNivU/DLXwp6dhIXdDHPXmQTMArmZ7XSU+1CqU77oqleTKvx+xdvfKfgkBz9
+HC2G5OeCryw9hMvLtURghXEjh68N5zvxm9DRaK96CDSPllBi3VzZzau6UE+
9asXeU0npEY/q2D0RPEAD/evj7lSsEGnaFlNL6i8A3x4Tqfj7i0ZxRTHi34i
zfN9+EqndXCpjGrh/HbJmj6Q3N0+VCewZDxdMGRT83U+vCXClvkwuc+UGRXL
ZebIjyr08jN3Mg6QP/ixUXz8qB9thv5NJ1pGiDoygLbieqP9WDHaclKtJ+Nh
Aj0AasOVfgTH77A/WS6h461PfBvuk7gv/OgrwhZJ7E8SFLIpP77UKXPskjBR
2MZKie8VQDVKvIT3j/bUrUOCiKqnChMWC8OSsG1ny+N/3idj21lhZAGQbTwu
4fjajIoG/WX2gQDnXYJd9PkqGWd0qtYXBtgPAviV/EqGLlZPdQCndXreXRpA
rRcObpxUXeH76Pfo/9N7TboG8Ej3j4oX91cgqPW1ktBzhDAYP8jOVil8Twl/
Tbqtjd/hB8m/oHD/SCgQNjrcj0V3uudeqa7y/SXuHz+2ijbur4LsVddP4X7y
Gb67WuV+kPCHTmFstg9WvQpNilS8I2xsn4RjAr92PqNPamisV8LbdGGda2p8
jfmW0Fng86HX8JPVGudBwg2P44PZ/bwgXIo0fCHkZsg4Epv9ff07HsNnagSZ
Vxmzfq8dPXrQg/cgGimIJ+gl43FKiMfwn9VB/lyGXqQX/2zkwQ/17xSnioKV
/B1srS3dftlt+FJmCJS+eTKmPylIcHN/h7hvZHTsLSad2/CrvBDzJEN00/K4
i+dJCKmFIrCMr8eJi7vQR/hYZhhdV4sBI4NkDnNhnLCH58MQLjQtLLPPuoz6
5IVRLGz1rgxqt1NOnpdh1Bc2WU8BjbdZTsP3MiM8nxRMpMBOlPZxVtx4PsLz
RYFO8Uo910gKP1wT4XmtgMblfxyo3nj7q+2LIyCbma5gp95dXfs6ICjonRUF
2cZyBeMJGDt60QCO4qqwga0Kmomxc8AOGv9roqC2PqwgoLt+0wl2vHO+vY5Y
tJLT7XV3F5xuaDd8NSsGo+0UjGkmnmxjP4rhlsDqpoLGBKQNZOtrYuyjKtxi
rehkQ1zgUhwDpbmFik9FemNW3Cd8ODvOe4M5p60Q3ZczMI4mVGCV56jVeG5+
HK/TS+W5beV7xUFjdqGKjSuX685kMXw7O4E/Fk3VNxcVQ3UXnDvTgo23MvTI
CfShxUnF/UJuGwu+FX6en4CeZL2gKkq+EwUsw+VL4pUAjUOHChpL68v4fQIz
BW4VKvtJGYz1K8HzUwO1ey0LEsRjwuCzpQay40EWzmOC547GXFvYl5NGnBEa
38PCz00acXM1UDtlW5m/JPeBxnWy8lxJcl9oPF+taE8DNmnELdCYYyv3Lcd1
mHuAje/HcSs07mNz3+S4mUGezzb2O47bMogJxJeN5zrH7RHkOtrxBtXV1Bvk
59pZZxJ0/dwgjtOcsfNeYuoNoiMJtnM/mHqDIJvOcHDdkqDHFARxPy0MDt6r
TL1BLKPvO/j7HLciCGqnsw5Qmg+YekOYSnuxEyT3mKk3BC81qJP9l+P2COEF
AszJ9TT1hkDXK3TyPcz6hozyVnUxV8lKnzP8w8VcmXpDILkfuJgrs74h5trF
XKU4Tojfs39mpri+IVA7wc1cpfjzMM8lN3OVYr1hkE2fcjNXKa5vGLSG3nUz
Vxx3RJg/9/DfHDc3zH3s4fpzXN13W4r166SHueK4u8LYJGxan0cGVxy3IMxz
2MtcpeAkvWHmxsvxUlzfMBI0H7z8uak3grEk2MtcmXojIJvu5mOuUlzfCPrw
XmZwZeqNMB8+vjfHzY3gEWEbt33Mlak3AmrfLn7+nqk3wvPQz1yZeiNYQvuv
n7ky6xsB/bebfn5v6o2AytspwFyZeqNwizTPCTBXpt4oSO6RAHNl1jcKCns9
wByVs94oSP7fJX5uOec7yn4vMVflrDdq7COjJOaqnPVGec+VmKty1hvFYtF4
myXmiuM6opwvie/HcSuixlx1mvsnx82MgWz7tsRccdyWMZANN5SZK47bI4bt
VBeZuSo3uBoRY90y6zT1xir3F4MrU28MZGObZObK1BvjOsrMVTn7VQzGz2+Z
uTL1xri+Mn+/nOsbw2PCBuorzJWpN851VzhPpt648bttmMJcmXrjzIPCXJn1
jYPkbFT4Hhw3N877vsJcmXrjWCHSbFGYK1NvnPlRmCuzvnFjbj+gMlcVHCfO
9U6fG/WLc77T58RHowTrT58b/CX4Hulz43dNgjnTKs+NuiW43ulzY79KVM5V
89zwqQT/vkmfG/t3gvkOVp4b+2QCr1DB0+fm3DbqnT6fTQVMsp+lz2kcjEsa
dakXqjw35kESJPel9DmN+x1J5ix9Xsjzisp9KX3egH5YJivngHluzL8U91X6
3Ph9lGK+0+fG3E1xnPT56+zTxvsI/g9eITvj
              "], {{
                Rational[-15, 2], 
                Rational[-225, 2]}, {
                Rational[15, 2], 
                Rational[225, 2]}}], {Antialiasing -> False, 
              AbsoluteThickness[0.1], 
              Directive[
               Opacity[0.3], 
               GrayLevel[0]], 
              LineBox[
               NCache[{{
                  Rational[15, 2], 
                  Rational[-225, 2]}, {
                  Rational[-15, 2], 
                  Rational[-225, 2]}, {
                  Rational[-15, 2], 
                  Rational[225, 2]}, {
                  Rational[15, 2], 
                  Rational[225, 2]}, {
                  Rational[15, 2], 
                  Rational[-225, 2]}}, {{7.5, -112.5}, {-7.5, -112.5}, {-7.5, 
                112.5}, {7.5, 112.5}, {7.5, -112.5}}]]}, {
              CapForm[None], {}}, {Antialiasing -> False, 
              StyleBox[
               LineBox[{{7.5, -112.5}, {7.5, 112.5}}], 
               Directive[
                AbsoluteThickness[0.2], 
                Opacity[0.3], 
                GrayLevel[0]], StripOnInput -> False], 
              StyleBox[
               StyleBox[{{
                  StyleBox[
                   LineBox[{{{7.5, -112.5}, 
                    Offset[{4., 0}, {7.5, -112.5}]}, {{
                    7.5, -73.70689655172414}, 
                    Offset[{4., 0}, {7.5, -73.70689655172414}]}, {{
                    7.5, -34.91379310344828}, 
                    Offset[{4., 0}, {7.5, -34.91379310344828}]}, {{7.5, 
                    3.8793103448275863`}, 
                    Offset[{4., 0}, {7.5, 3.8793103448275863`}]}, {{7.5, 
                    42.672413793103445`}, 
                    Offset[{4., 0}, {7.5, 42.672413793103445`}]}, {{7.5, 
                    81.4655172413793}, 
                    Offset[{4., 0}, {7.5, 81.4655172413793}]}}], 
                   Directive[
                    AbsoluteThickness[0.2], 
                    GrayLevel[0.4]], StripOnInput -> False], 
                  StyleBox[
                   LineBox[{{{7.5, -104.74137931034483`}, 
                    Offset[{2.5, 0.}, {7.5, -104.74137931034483`}]}, {{
                    7.5, -96.98275862068965}, 
                    Offset[{2.5, 0.}, {7.5, -96.98275862068965}]}, {{
                    7.5, -89.22413793103448}, 
                    Offset[{2.5, 0.}, {7.5, -89.22413793103448}]}, {{
                    7.5, -81.4655172413793}, 
                    Offset[{2.5, 0.}, {7.5, -81.4655172413793}]}, {{
                    7.5, -65.94827586206897}, 
                    Offset[{2.5, 0.}, {7.5, -65.94827586206897}]}, {{
                    7.5, -58.189655172413794`}, 
                    Offset[{2.5, 0.}, {7.5, -58.189655172413794`}]}, {{
                    7.5, -50.43103448275862}, 
                    Offset[{2.5, 0.}, {7.5, -50.43103448275862}]}, {{
                    7.5, -42.672413793103445`}, 
                    Offset[{2.5, 0.}, {7.5, -42.672413793103445`}]}, {{
                    7.5, -27.155172413793103`}, 
                    Offset[{2.5, 0.}, {7.5, -27.155172413793103`}]}, {{
                    7.5, -19.396551724137932`}, 
                    Offset[{2.5, 0.}, {7.5, -19.396551724137932`}]}, {{
                    7.5, -11.637931034482758`}, 
                    Offset[{2.5, 0.}, {7.5, -11.637931034482758`}]}, {{
                    7.5, -3.8793103448275863`}, 
                    Offset[{2.5, 0.}, {7.5, -3.8793103448275863`}]}, {{7.5, 
                    11.637931034482758`}, 
                    Offset[{2.5, 0.}, {7.5, 11.637931034482758`}]}, {{7.5, 
                    19.396551724137932`}, 
                    Offset[{2.5, 0.}, {7.5, 19.396551724137932`}]}, {{7.5, 
                    27.155172413793103`}, 
                    Offset[{2.5, 0.}, {7.5, 27.155172413793103`}]}, {{7.5, 
                    34.91379310344828}, 
                    Offset[{2.5, 0.}, {7.5, 34.91379310344828}]}, {{7.5, 
                    50.43103448275862}, 
                    Offset[{2.5, 0.}, {7.5, 50.43103448275862}]}, {{7.5, 
                    58.189655172413794`}, 
                    Offset[{2.5, 0.}, {7.5, 58.189655172413794`}]}, {{7.5, 
                    65.94827586206897}, 
                    Offset[{2.5, 0.}, {7.5, 65.94827586206897}]}, {{7.5, 
                    73.70689655172414}, 
                    Offset[{2.5, 0.}, {7.5, 73.70689655172414}]}, {{7.5, 
                    89.22413793103448}, 
                    Offset[{2.5, 0.}, {7.5, 89.22413793103448}]}, {{7.5, 
                    96.98275862068965}, 
                    Offset[{2.5, 0.}, {7.5, 96.98275862068965}]}, {{7.5, 
                    104.74137931034483`}, 
                    Offset[{2.5, 0.}, {7.5, 104.74137931034483`}]}, {{7.5, 
                    112.5}, 
                    Offset[{2.5, 0.}, {7.5, 112.5}]}}], 
                   Directive[
                    AbsoluteThickness[0.2], 
                    GrayLevel[0.4], 
                    Opacity[0.3]], StripOnInput -> False]}, 
                 StyleBox[
                  StyleBox[{{
                    StyleBox[{
                    InsetBox[
                    FormBox["0", TraditionalForm], 
                    Offset[{7., 0.}, {7.5, -112.5}], {-1, 0.}, Automatic, {1, 
                    0}], 
                    InsetBox[
                    FormBox[
                    TagBox[
                    InterpretationBox["\"2.5\"", 2.5, AutoDelete -> True], 
                    NumberForm[#, {
                    DirectedInfinity[1], 1}]& ], TraditionalForm], 
                    Offset[{7., 0.}, {7.5, -73.70689655172414}], {-1, 0.}, 
                    Automatic, {1, 0}], 
                    InsetBox[
                    FormBox[
                    TagBox[
                    InterpretationBox["\"5.0\"", 5., AutoDelete -> True], 
                    NumberForm[#, {
                    DirectedInfinity[1], 1}]& ], TraditionalForm], 
                    Offset[{7., 0.}, {7.5, -34.91379310344828}], {-1, 0.}, 
                    Automatic, {1, 0}], 
                    InsetBox[
                    FormBox[
                    TagBox[
                    InterpretationBox["\"7.5\"", 7.5, AutoDelete -> True], 
                    NumberForm[#, {
                    DirectedInfinity[1], 1}]& ], TraditionalForm], 
                    Offset[{7., 0.}, {7.5, 3.8793103448275863`}], {-1, 0.}, 
                    Automatic, {1, 0}], 
                    InsetBox[
                    FormBox[
                    TagBox[
                    InterpretationBox["\"10.0\"", 10., AutoDelete -> True], 
                    NumberForm[#, {
                    DirectedInfinity[1], 1}]& ], TraditionalForm], 
                    Offset[{7., 0.}, {7.5, 42.672413793103445`}], {-1, 0.}, 
                    Automatic, {1, 0}], 
                    InsetBox[
                    FormBox[
                    TagBox[
                    InterpretationBox["\"12.5\"", 12.5, AutoDelete -> True], 
                    NumberForm[#, {
                    DirectedInfinity[1], 1}]& ], TraditionalForm], 
                    Offset[{7., 0.}, {7.5, 81.4655172413793}], {-1, 0.}, 
                    Automatic, {1, 0}]}, 
                    Directive[
                    AbsoluteThickness[0.2], 
                    GrayLevel[0.4]], {
                    Directive[
                    Opacity[1]], 
                    Directive[
                    Opacity[1]]}, StripOnInput -> False], 
                    
                    StyleBox[{{}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, \
{}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}}, 
                    Directive[
                    AbsoluteThickness[0.2], 
                    GrayLevel[0.4], 
                    Opacity[0.3]], {
                    Directive[
                    Opacity[1]], 
                    Directive[
                    Opacity[1]]}, StripOnInput -> False]}, {}}, {
                    Directive[
                    Opacity[1]], 
                    Directive[
                    Opacity[1]]}, StripOnInput -> False], "GraphicsLabel", 
                  StripOnInput -> False]}, "GraphicsTicks", StripOnInput -> 
                False], 
               Directive[
                AbsoluteThickness[0.2], 
                Opacity[0.3], 
                GrayLevel[0]], StripOnInput -> False]}}, PlotRangePadding -> 
            Scaled[0.02], PlotRange -> All, Frame -> True, 
            FrameTicks -> {{False, False}, {True, False}}, FrameStyle -> 
            Opacity[0], FrameTicksStyle -> Opacity[0], 
            ImageSize -> {Automatic, 225}, BaseStyle -> {}], Alignment -> 
           Left, AppearanceElements -> None, ImageMargins -> {{5, 5}, {5, 5}},
            ImageSizeAction -> "ResizeToFit"], LineIndent -> 0, StripOnInput -> 
          False], {FontFamily -> "Arial"}, Background -> Automatic, 
         StripOnInput -> False], TraditionalForm]}, "BarLegend", 
      DisplayFunction -> (#& ), 
      InterpretationFunction :> (RowBox[{"BarLegend", "[", 
         RowBox[{
           RowBox[{"{", 
             RowBox[{
               RowBox[{
                 RowBox[{"Blend", "[", 
                   RowBox[{"\"M10DefaultDensityGradient\"", ",", "#1"}], 
                   "]"}], "&"}], ",", 
               RowBox[{"{", 
                 
                 RowBox[{
                  "2.2154323259325888`*^-23", ",", "14.539453191876296`"}], 
                 "}"}]}], "}"}], ",", 
           RowBox[{"LabelStyle", "\[Rule]", 
             RowBox[{"{", "}"}]}], ",", 
           RowBox[{"LegendLayout", "\[Rule]", "\"Column\""}], ",", 
           RowBox[{"Charting`TickAnnotations", "\[Rule]", "None"}], ",", 
           RowBox[{"ScalingFunctions", "\[Rule]", 
             RowBox[{"{", 
               RowBox[{"Identity", ",", "Identity"}], "}"}]}], ",", 
           RowBox[{"Charting`TickSide", "\[Rule]", "Right"}], ",", 
           RowBox[{"ColorFunctionScaling", "\[Rule]", "True"}]}], "]"}]& )], 
     TraditionalForm], TraditionalForm]},
  "Legended",
  DisplayFunction->(GridBox[{{
      TagBox[
       ItemBox[
        PaneBox[
         TagBox[#, "SkipImageSizeLevel"], Alignment -> {Center, Baseline}, 
         BaselinePosition -> Baseline], DefaultBaseStyle -> "Labeled"], 
       "SkipImageSizeLevel"], 
      ItemBox[#2, DefaultBaseStyle -> "LabeledLabel"]}}, 
    GridBoxAlignment -> {"Columns" -> {{Center}}, "Rows" -> {{Center}}}, 
    AutoDelete -> False, GridBoxItemSize -> Automatic, 
    BaselinePosition -> {1, 1}]& ),
  Editable->True,
  InterpretationFunction->(RowBox[{"Legended", "[", 
     RowBox[{#, ",", 
       RowBox[{"Placed", "[", 
         RowBox[{#2, ",", "After"}], "]"}]}], "]"}]& )]], "Output",ExpressionU\
UID->"e487eb29-ab9e-4031-9b62-49bb348de559"],

Cell[BoxData[
 TemplateBox[{GraphicsBox[
    GraphicsComplexBox[CompressedData["
1:eJyFm89uW8cVh4UCfZAyeQI+QNlFkTTbPkKBLoqk7as0fgRtuxMSbbQJLgwB
hlFApV22ki1bFEvZshKKL9CkEW++M/h9IyKCgfGPc+/MOXP+zpm5v/rDX3//
x18cHBxMfnlw8NA++fizH/9d//rgp78RH80SD4VfHf/5x3/P1T8vvOt+tSh8
Mf5Q+PsfHv7eFv5ynL/wRw/wyX8Lf717/abwp7sJbwuf78a/K/z5jsDvCo9/
W9E7GfFvjmbwm+1QGH6zf14YfsHwC4ZfMPyC4RcMv2D4BcMvGH5TblvRO5nt
pvntsIfP1ia/g/gdxO8gfgfxO4jfQfwO4ncQv4P4HcTvIH6H4nfX/qnJzXyB
3cIvGH7B8AuGXzD8guEXDL9g+AXDLxh+0+62oncy+8vDD5/MJd95J0fz6Tbl
O5d855LvXPKdS75zyXcu+c4l37nkO5d85ynf82Z31kvLzXyB3cIvGH7B8AuG
XzD8guEXDL9g+E2/uRW9kxF/3vyK7c56abmZL7Bb+AXDLxh+wfALhl8w/ILh
N+PCVvRORvy/5jftV2x31kvLzXyB3cIvGH7B8AuGXzD8guE3495W9E5G/GmL
C/ab9iu2O+ul5Wa+wG7hFwy/YPgFwy8YfjOub0XvZPa3h/bLFvccF+w37Vds
d9ZLy818gd3CLxh+wfALht/MW7aidzKbPDQXLa477jku2G/ar9jurJeWm/kC
u4VfMPyC4Tfzsq3oncy+esAftbzFcd1xz3HBftN+xXZnvbTczBfYLfyC4Tfz
zq3oncw+efjPFy0vc97iuO6457hgv2m/YruzXlpu5gvsFn4zr96K3snsPw/4
65Z3Oi9z3uK47rjnuGC/ab9iu7NeWm7mC+wWfpPen/Kr7+/35NlDl5c5b3Fc
d9xzXLDftF+x3VkvLTfzlfuireg97NqRr5PCI9mnXTuOc1b4d7sXzgt/sRvo
Uv3LwozvccBud48/eVmY8cGMDx7Xe1WY+RiH+Twu2C3jgxkfPNKzLsx8rEvS
ea5xz7uW8cCjvrwrzPheZ3DSdalxL2s88C5Of/ahcNK/7OTodU/6l5p3KfqX
NR+Y+VhX5rMcwYwPZhww84GZD5zrt9b6raVn607OyedadK1rfOhgPDDjmc6U
y7qwW/gBMx+/Mx+Y+bwOKfd1Ny7YLfPt8p0n39b4YPQBjLzAzJ/968LMD2Ze
cPqnjfzPRnaxkR5vJPdNzQdmPnD6K+pLhxUnxnba0ZXv7fO7Q72P/0UfjTMO
nVV/+rmh5EG//R7rRX/6lUHx4HnR93N0Zf/zLm5k3Dzr9u32y/Tnvva086vI
m+eRN/jxdTqsuA1/XkfT6XiV78+7uMXvGRfmygfOlA+cdX4884Mz+c15J+fU
w0Xxl/3Wk+cdneDkeyF9WnRxDf7Sz73t+DG9WR+Y7tXTx/X4edfv+sg+Oh0n
ael3nMu4cy4//qr4yf3/VHp6ong7dHmN47v1BMxzjrv5+7rDPEc+6fyA8TMu
vC3+cr8/ld2dSE+Hzi6dX1jPrSf78rDH5bLs1oHx+D3rJ30e4bgOZr2c3zBe
xsGW7+R6Hap+MK3x0++t5DdaPsT7rAfYeWTWxVZ79SDrK6vCqT9r7dP7vMVx
3nkG/Gc9YVr98J/Pz7v8jPfh3/URnmfdTb/pNX25/58q35rPnM/ts6N9egLO
Ota7Ts+Sr3cdH1knaXkivzuvQh5Zf9nMnE9mPWCq/G/e5Zc8j/55X+G833YB
Zj2cr7reYb1y/cP5LP3OK2mzPrPp8t2sF0yl5686u3f+6/qGn3e9w3YEXfBv
vXU9xHKnn9+9Dsmf89ZpzZ/52KbsDZx16o3qmptO7/a1zJ/44AB9y7hyrnz1
qIuj6C39+8bJuH9e/IEzjpzLX112ecXjcbr5ZejwuK7nOz7Zr7lO6Lwl/fll
59/th1xntD93v/my3VkPnTc5z7BdWk/9vO3F9UD6rVfoBXkLmLwk9ft0z3OL
bl/G77yXeNHl1/vyZt63X7D8HCfsJ+03sq541vkN19dZr1GfnlU7jvOm+333
2sf/7NqR3mXhcf5vZrthjv9Vv4NHvb6odqTvfTcv4/I7eHdO8epDYZ4f12lV
843rdFPjg3kfzPM8xzh+D+yW5z0umOfAbnl/dx7x8Xf1Ppj3waOePunaUU//
XniU73Fh5AIem6eFWUd+B+/q6Mf/qN9ZLzD9PD/q1+vCjM9z4J8bZ9+8/E47
8v2iMHoGRn/A0JfzX1U/epXzX9Xzud4vCjMv9NMP/aaLfrc8B4Yez7dTmx/+
XRh7Ynz6wfQzjt9nXcCsG/SD0UeeH+m7rf7Ur6vC8MF49KfeXVW/5QBmfp5n
fjB25X7eT/u4LowdQg/9zA9GD9zP+6wD/dBhv0I/9IKhl+fTPteF8RNpN+ui
D0w/zyN/cMqhjT/Gr3eFoZvx6Wc8+0X64QcMvx6f58A8l3ZwW5h+6PY8Kfe+
5f2U/11h/C16DmY+nmc+cMrvTn6+9dufj/LYyL/c1/z0o19g5AiGHnDGsU3N
7/nG1vVvx5lD4VbvdrwZ6T/p/CDY8WH8vdWneT7t9bTza0lPq/+63/Ve+/+s
Q7zp/G++f1z9xFX75/G5Vq91/9i0/Qp+P+2v1U/tr7P++bT6WVc/7/ql/b3r
k6xL1i3eVP5JP++7fuh+1wtZV+Z7fJ2va93wh1knaP7c9Tf7d9fjGC/r8Oui
O/Px27IzxnO9y/7T9S7eZz4wfsHvu15lf+n6lf0z/awf4+F37Ld4n/FdL7J/
dt0p898PHbZ/df3F/XkeedX549x3rsqfmc+0/1b/sP8b9WWj5zkfnFZ+jV9O
vzMtP8D6G9vOsVvysvR307Jj+o2d39p/Mi/9xuhN2lHzn6Yz/de06IDP9DeD
8LTLJ5Ej46Q/mu7NwxnH98bS30xLL1gX+xPnZ5l/Lbu8Iv3JtOw398mt3p34
ReHc57b6Cph5wM5zsh60LnqwX+azfaf/mNa46EXa97ww44KzLn6r/fhtl+cw
H/YI/8ZZB7pRffN9jZv18ea37E+wU7c+h/G5S9ZnF7pfsKi8xvezicfYDXpN
nPA5zr66pOuMPrdkXPTU5178nvuZZdHhOhPv+b4qfht+cp+y6O6HsP55nr/V
Oei25sv+tep419rHLGvefXYMnXluvCg/ROtzNeyCdU39v5QeXNY6ZX34nfZ9
i6Ibv5b+66b6c7+y7PQu+Wr+gffox97AmVecl99gHeiH77TbS91Xvah4CT/Y
U94j28rv3eve1LbmJw4kfy1vNR75Pi16wNDjOOG6An4Z+wXvi9Ng5kHfmM/7
BXDO96JwxtOr8jfInX7kS/9Iz5HuMzzr8gbnCVmfuezsgefTLl7XPPn+UnJZ
ll9CD3k+zxtanc11wbSXU+0vbro8PM8/1vJzzf6gh/fzXPequzeQ57lvO77z
nGdZ4xjn+c1SecSy+x0553cBq+Kf/oy/vt/R+AfDP+/nfbh2Lms/m/Wmebc/
cn0LnOdDK8lhVeuT9zBelX+zX015+P7HWnytCud5SqvLZ76wkv9eq15yU3aY
eviy8D49BSMHy9H9mVe0c7/0C7edXlsvfM7gvC33y7c1fuZtlzofeq28c6lz
wWVXt3RdD4yf8f4t482y+hnP9TZj17u8r83z+YuKR8w/zrPR/ule/vte8t1I
vg3zPPQbu15Hf9ZR7yXnTf3OfpS48njdqe27iG8Zr9t7me8Me/LKi6LfcYN1
Y3zbhfc56TfX8s9vO71K/3JXOOsJq9Lr1Iu2X+J96Mrz4/fFD/PuO9dgnDwH
aefPKaet6gpt34B/8Tmp5YD8eC7rmUOHke++uA7G/4IZP/ffQ5c/5Hd6r6v1
7/CZ9xu/0f586PIycNYJrro271lcSV+vFQdX2j+07y8zjryVH27353yPLvVy
pTi1Luzv2NJvf6v7IHfVn/eWGr2Mn/lLqy96H5H7mrZfyPsf7+TXbzs99ncc
eR/oVHnOs7Jv8ob8/qnlEc47sAPfw/O9QMdp9Cvvndxo3/Kh8tq873ne+YH8
PvNN1+b9thbP876G87j2nQJxCexzf/jkPbC/t+M96HY/csw6Xftejf7cv2yL
/uxv8Srr2NuyL+ze94TTfp92dXj0PM8xl9rXrjs7wN+B8WeZ7y5KzzMuvpD/
6r+fyjz1tvOj6b9OROegOPdU+etp0ck6gFkH6PK9dPgCs06ZN5wWH4wPzvP0
27LL/B5nXpj1cn0g/dai9JX1Id9hnehPfbkv+lhH6NsX9xy3wPSzHqxDxqUW
T+Eb/fP+nXXAPhg341Pbv6fdHhdd6Tf7ugXj8XzeF7zQ/vKi7CfjcttX5b3k
Vp/Puv+g/K3dI3SdOu1u2d37Yt28v4XurBOslH9f17zgscVfELeWhdOuHTfa
d5wZh1/LH59qv/lMefnzwnnO0s7vss7wsnDedyQesJ9dFx9g6M97LS8LP/5d
x1Jx67rb74GhlziP/PL+w7y7b8HzuZ53XRzNfUPLl/fdy4HezPP7/WLei3zd
5U3eD3q/h7/JOhPne9QH71WP2UgPN10czzx5rvznvN73/sn1f+8rGMf77Lxv
u9L92T7PSX1v31nmOHfd/h//m/nDSedH0m77vDv1fVn2O/Lv70Tbd6RZR7rr
8nviKOuw714Teo8/tR+13mB//A4mfjGO/XTS/77iWcb1+3oOv5vnAU8rXuS9
wJPuXlnW6ZtfS325LLlmXeNEeddJyQ26sl73tOSc/qzZzzj/RvZ/2e0bx/fJ
y5u+5/fN33R10bSja9HT4kGuS7vXj79GXvhn63HWo1bKC9t9QO+v8G/408yH
Lopf9CrrdN8Whp7Mm4+0bz3u8k3bh78nsNyRM3wj56xDtu/miYO+R5jnOe07
ePyh7wXu+26dOM46Zr600b5zU3kKeghdeZ7Q9mPEJ+Tj75lGeg+Fj+S3j2W/
h8JHymeOO79nPUZvaXOeJm/L03UK5J/xZTn7PxrLTFw=
     "], {{{
        EdgeForm[], 
        GrayLevel[0.8], 
        GraphicsGroupBox[{
          PolygonBox[CompressedData["
1:eJxN3XncR8X0B/D7XZ52bVIqrZaULZEI7SuJFi0qtCNkSVRatCFKiSilRahE
qGylzV4piV+ktFoqUSrJ+jvv53yeV/6Y1z3fuWfOnTv33pnP+cyZ+a60275b
v308DMM18w/DpI53VvJ7sUpLVJpWmqm0ePLpPDFH556U8/NUenKl+SqVqeGZ
dfKZdVyr0tLJW6DSspUWrLRQpbeXzstGbe+xMv6Skl9c6Sk5/wT2y/i8dVyy
0nLJW7jSMrHH1kqprzreFXlF9Znp+iylzmV/nVHf07+q8uuNuu7Pid3lKz17
6Gv7fXdsPK/SK6Z9b6tUWjX34/qr5eieJjOd757nmWm9F1fatK6zyajr+KSq
wx/r+Ax1Kp0fld0N6ty76/jvOvfKkn9W8qqV7imdzer37+q4cqV3Vp3/Uzqv
qrwfTvue2Nm88pao9PuSn5W6uIeXpK7q+PPSX73SoyW/NHru9WU5aoP3TPuB
blv2tyl7S1W6v7JeXcfXVt59JT9Qx3vTJt+p/N3q9xsqjapur6vjlpW+Vfnb
V9ql5BvK3rMrPeCa07RHpe3r3J/quIb6p65ra+cc1fGVOarjgWV/27K5U5X7
a6UHK2+dSjtV3u7ySl6hjl+s4zcqHVDX+made7P61XGZSg9X/h71+6E6rl/p
TSXvXWlD91P6m6lXpRfVtf5Wx01ct/LfWGXfMup6rZa2W2im672D5146D9f5
f5b8vdLdt+S3Vzqs7Py98l5V6XW5N/e0QOU/VOffVmnH3P/sfVf+P+q4lWdd
5x6r42t8NzPdPup3SF3rEe9LpdeW/v6O7iV1Ye/G0lnT+1TyC6edv3elN+e8
uhxRZR+tsv/VVlXn00p+QqWPVv72ld7vOVfZhd1DyXeWPBm1nTtKPrrSByo9
VnmHVtqt8v9TaedK+1Q6rPJ2r+O71EtHUb/fqA51rSNKHrv3yv9AHff0vZSt
g0o+sNJPp30/7630vtybuh9f+ie695LH1SYfKb1PVt7HKu1S6XDtXvaPqeNM
pZ0q79g67lv6vyjd9er3vPX7QyXPQ9e7VXn/quN22rzKnlT581d6pHR+X+nu
Sv+tNB31fX3wf+r13tI/tfIXUicdY6UFSl7QfdTPD2uTKrtQpQ9X3qdHXfZo
307lfch7VnknlHxcHQ+p/LvKxocrfaN+n17nP1vpk/rH0jmt5A0qPbnkT1Te
4aV/cOl+oeRFKy0y0/U7qdJHS+djnk3Ji4+6Lp/WV1X589mrtEzpbFy/lyr5
y3U8o47HeTfK5pf1k+pU8kylxTy/0nlDpSdp2yr7ukrHlP532aj0tcpfro5f
reNZlc4Z9b1+stLFlf/5+v3xkr/uO610YslvdF91PL/SbiXvWmmZOndF6c9X
1/1WyS+b9j19vtI5uY+TK/298l9faRd986jzff9fqrIHVdkzK++pde47dfy2
ftTzqXRlySt6F0o+t+SP1PFyda/0vVFf56JKl5S8bKUzSj62dOavtHz9/mXZ
fHmlC0v+rrGq0plD95U7jrpv+Wbqor7PqGv90DUrfcn7UHZ+4pup/E0rrVTy
VZV/XqUF69xjdbym8q6u9Mw6/7M6Pq3SC40NZe82/VDlP6F0n155vxh1fb9X
6YIqu1edu6nyfpR6fEsdRt0v/qDStSWvXOmCkn+Y/pLewmXv55V/Ycm7l43N
K61Sv/9RNvdmt9J12rTSV0vnMX1DpUWq3M2V9yvtUfn/F7uu/8tc0++bkvfj
SqtXudtL/9n6FvdfeTdU+vnQsns51XtX6Tl1/vah753OTVWPt1Rat9K3Kp1V
OveUzi2VVqt0mXuq/F/XNf5Yv8+u8/fW8QXuTd9S+b83Zmi/SreW/NtKz6p0
uWdfOmdW/h/q93NHXf9f+I4q775R19H4atx8Vn/2s7gD5pjDRn7DRPAFvAGD
wA/GbHhGPrw0zXll7pg0RoFLYKWlUwa+MabDGQvl2n7DQcvlN5wDH8FJXyob
G1V66ajrtNLQOAbOWil1WyD2XGPfaeMw2Oid08ZGMMkeJe9e6dzowRKwDSz0
jNwL/AKLPH1o/LNK6qxez0kbLZt2msNKq8XWOVXHDSqt5X2q4yaVNh419oEd
X1Q676i67DtpXPSKceMhOGfTceMq2OjLwT/rjxpnPS/3++5J4y046pXBTbAX
bLZW7kU9jK0vTr3Wzu85bAR/XKRspW2q/GvGjY9gIfgI3tlgaHwEz6w3NEba
atT4BC4wVu9a8gFVn9eOGyftX/f36kljJPgINoI3jPWbpj4wkj5l/eFxbLT5
8Dg28vu77FXa1fddxx3+ByPBZbDHzsFpMBKsBP+8emg8CPcY++AjWAj2gG3g
IHgDRtq50tYlH1r13XrSWAhGgiVhj4PhhUljrZekjpvlCG/AH0dNGt/ASIcH
/8BIV6pbpbeO+n5elzKwE3wET8BssBssBBfBSG+QH6z1WN6vl6adDgtGes+o
MZLxfY/UA5aAYbab9NgPI11a+pdMeyyGm2CmveiOG5PASD/UPpUOGHX93hxb
8ApcoJ7wFNz0tpLfEpwDM8FJcBA89KeS76t0b13713X81bTxERwFM71zaPwF
L8En2xlnJ42VPjiHu+rcIvBdHecbNTaDmfYv/Wu1ZaUPVv7Jk9Y5vtK+48Yg
cNHK9Xx/Euz0juAleGin4CM45FOTxkvsw0tw1MG551nsU+mESWMyuAtWgoOO
yTk4xNi/XzAVXPT64DT46jId37gx0QpVn+VnGmfBW7AK/PSLOr9/pc+UzoHB
WjDqz4JXYaYTgnOOHRoXwUonlHy+dzIY4GvBTTATPAUrwSHw0idTz3tK92OV
flDnflPHD1T60qhxDqwEb5xW8mVV/tLgIHjp1Mo/ctyYCl46NHgMXjoqOApe
gpXgE/f2nWCkJSt9YtrYC156YNJYC27aOHjpjNL/xqTxE8wEH8FMX6r8Y4IT
4SU4CYaCpWATmOnsoa8Jg8AicBLZvdxRZT9U6eJRj6u3D40rYC5Y6csl/z6Y
B1761bhxG7wE88BKMAA8BTPBDFdOGmvBTN+bNKaClx4O1oKXYCp4CfaA3ZYe
9fiifjAILPK30j290u/YKZ0fTRoXnTBuHAUvwU0/ruN3POtgKnjpmkljFHjJ
2PXN3P8nx42j4CU4B2aCM2CcZ5b8k1wfPoFV/lI6n6r0f3Xu6knjMZjpGzkP
E9xf9n8wbRwGH8FKsA2cxA7cBWvBSzDJV70D48ZmD4wbN8FM7MFMc1jJ8Rep
y0059/NJYxR4CSa6M8/NPcAjcNFp48ZU8JKxG95UBziJHXXbZ9KYZ406d/2k
cRvMdHHpvJVe5d3mWpPGVHDW8zx/7VPyb4KpfjZpvAUz/VLfH0wFZ8Gpvx0a
K92euq2vHx51//KiUXNMsMmG4/6Ns9m45LVHzbs8o+TNRz2+b5Q84/ra0V3R
+zNuvG1s2KzkdUeNDd5W19qw5D+UDJD9JfKtVf9bpo0BXh7bK1XaovJeOW2c
sFodXzNqeYtxXxvf8YrYxldsnTzj+5apJzxwccnPrLRd/d5u3Hb+UvmrGpdH
jQ9WK/n1yd+65C1GPca/q+r56pL/XPK05Acj75jzMMYq49ZR/y2rnq+aNq/y
7Mrfc9Tj+vv02ZOuw47jriuu5Dk4gUnrvzF5sMFW4743ddsl9WRn6XHjBThh
/ZLXzDPbK+0NJ7y6bB5SNl9TaSvtNm1MsNWkr4VDOWjS9/tIyc+t82slf/fU
2di967jrhCt5fum8ONhin+QZW5cdt65x/wjYsNJ76/zW0TfGb1vHbYIxXlrH
d40aKxxaOs+p8vvV7z3HfQ/yt638baaNAV7Ap4udneu4bsa+vcZdD9zGwb51
Y6bxo47PrXNHlvzaaV/LmL1DydtPe9x/yrjz8A/3lP46lUb1+yN1PGbS9f9g
HY+edN3el+vAE3eVjaNGbWfHaZf17VxXNp9X6SMlHz1qXsJYuc+4y/v9o5Lf
Vungko+rcsdOWj7G2FrpoyUvWnmHjHrcx7fC6zjVT1f+6vraynvHtH0D39Wu
lb/JpPv2G+r88yudXPJZxsFKn9KHVt67xl3v94y7friQ69Vt3Nc9DpaYtv3P
V7knT9rOhXWcjntsXquOl2YcucC3XOlzJa9R+cuNG2ecW3njkhcu+WznU4dz
6viFSp8t+WN1nSUqLVm/T0ldjL9/mHb7sfOg/qHSPGVrhTqePWo+5JKSN5r0
GP3Sad+7/K96XuOuz//V8fBxX+vWOr6g0ldKPgkOCW5YYdzj3Rl13dMr77PG
pPq987Tvhc5h464fnua8HI3bp5TOU+t6F9Tvk0t+w7TtXzRqvsOY+aIq+9Rx
j6MfHHd5555S5U4fNfZ4ceVfNup6vBlWrHN7V/po5a807jF6xXGP6WeWzZeU
fMWof3+u9K8q3at8O9O+lnHlvjq+vNKNniOskfHmB6W7WaUflfwVOKR0bvC9
pA7GQs/WWPqVutZx474f3O3aJf805y6HLcfdViemPsb0l5Z8fex8P+WMyd+H
78Zdz5PGfQ7n8cscjat7ls4eGe/X8a6Ne+w7Bycw7TpfW+dfER11cX6W06jz
62WMfw7sXsdVK61bNm4b9Tj42XFfD2fwKH9i2jzcn2DYtNXdOW98PyVtY0y/
a9TjtjH95Nwjm+uVfEd0vlXX/Wbeh32mXU9j8y/q+MrIN0y6Psb3X5X8mzo+
v9LbSv/m+v2WSrdW2nLSOp8f97WN2fdmrDXOGr/lwYRfGLefbpw+d9zcgnH4
/vQd+g1jsvPG9M+lzviWL457/ODjGz+MI7pF5cz1mHNZLr/Jv1FPWGTSY77r
GOvPGzdWwGngTnAks/M4457/0Ef9u8r8edTncB3G+JWG1sNt4Fj+GV2/fxcc
sLLnW2W30PfV7/PHbQefYL4FlsBvKM8u/uTH087Hk3xl3NyFtnv7pMd0Zd81
bT5lw9TlZbnWkjNdzn1cMG7cg/d4z6T5i9Ur/8Jx449tcg+bpB3gIhzI84ce
s43da+T8pim7WXRc6xUZ3+m7301jb9PgltWDl/ArTyv5a+PGS3iNM8b9nmsT
OAp/8sJgEec3HJp72SJ1gK9elbZ6e2y+YOi5pC0if6LyT5z0uPnq1EHdxpOe
34FvRtPGVXiW91b+/pOWYSs8y3qlf1vp/Hbabfb1cfMzc1jHETZTbslxczH7
TZsDYn+r2Jlrwx1ic79Jc0ZwG8yGf1l/aDy0Q+TXBN+tG/y2fe59++A9nNVO
0WETRvY9+5Z96775y4P3Xpf2vDt4D88C++FvNhoaQ5LXhOuq/kdOWue90+Z5
4KrfVd5LJs2VzDPpuSr5z6rjk8fNAc3xPrDcpePGhfiiS8bNHcFecCkuauOh
MZc8+PDb425DdsbTxpa4pMvGfQ0yLLpL7h2G3D33Dpfibl6Te3xT3hk8E3nr
3BfeaJuh8d2e0f9HsCacOVfGe/dw8ulcNW6cikeCM9+cdrti3JgSp/T61MGz
u3zcXBM8d9C020e7wbA4rW2H4K9RY86HUh/ttlV0tA+MuXfq/358TDAhPIX/
wdvAt/um7OE5v0vuEYcFx8KGcOMbh8al78x9HRH91xvXxs07zWK7vAP4xKOC
GZVdOPKuQ2Px96Q9teG7cq0/Thp7my+8vewcPm15u+BQc3DzTdv2bkPP08nH
g8GUrg+X/nrc3AiccZT6TJsj8w4emHb7wbhxM67M+QWnfQ6fBoe+dWg8//7c
47zB1exoy32T//bo49O+Wzr31bl/1O8PT5orU8/5p11vc5Dz5Blo/xdPG8Mf
mmdySOzA+PizPUo+KXZwc3+YtF1td38d/5Rr/XjcOB+Xh9ObxbZDc3L6sP2G
njc8Mvnvjg4u7ppx43Jc3JnBybiwkYmBmW6XP0+blztoaC4ORn6/ZzptjI6b
e8K08Ty9q8fN78HWPx23L0CHnd9V+tCo5yU/Ejv7RmYf/lWOv3BAroXT46cc
m3ZbbNJzZ/A5DtDRHOIpk+YTP16/T6zrfHza+HI0bv7NvOGi025TfCNfXz4u
kU90RN6l40tefNpzmf8dNV93uHF51HWg/5c6/59R83WTcevg61aqNltxpueA
l542VqfzpEmXxwcqd2Jsso+bPKLkxSftm2j/Zac996k8fpL/cWTpXF95102b
k1wuR9zgIdE5quQbx+3b4CRfH5+D/V+O2z/BUX4xPshpaR88IX7SvCpf4aMl
P2na9abz83HznHyAA6PzkdJZYtJ+E5tvDJ5nB/95euzwFZTj1zw07WvhAM+b
tI+E/+SD4BLPHLo9yTjSiybtd+Erv13Hr0yah1xq2vO5fJyvT5o/1VY3jZtT
lW/e+VNpt6dM249j56E6/jX+1JLxvdRh+Wm3KW5zzXH7HbhEPt8X83yXH/d5
nCzf7qy8Vy8ct39CD+4wbhoT+Dn6Ib7Sp6btd+FhV5y2/4N7XXbSPpr8W8bt
o8n/9LR5W/6UeWeYGpc7y9WNmtv8zLTbBVd7askvmbZfeNu4uVN+i3vFi5rL
XmnaPqR58A/H9mdSfzpnucf4j2yuU/orj5sX/e2475+MN74gZfk3rsPXu33c
Ph7O9qwqe+a0+VbHM6bN2z5t2r66/LvHzRHzoTzbb+Xd+GFkHK85aOfxyfw+
nO3XhvbBcbV4Wn7gd/P+3DlunxPvzE+7NDpPSbvNycqeMbTv+d2058rxZ5XF
FSvLB+Rnq/tnh/ZJL09b8b34YLhT8+BXpG5Pm3S74aXxzFclHweNizYXvlry
v04/us7ze829X6rPHzfnz1f847h9Ufz/F6bt0+KaV5m0H0r+fOyfPfT8OH8R
/3zutLlobfi3SfuYfvM72eafPjrpdndtz+rqtP/N4/b/zTXwf6/N/X65dH4y
ad7Z8ceTLqPNr0m78YVx3fAkjlpb8VU/Hjtzz+6KtP+J0Sf/u+z/q9L3p82Z
3xA7947bZ8eVw6zXJ3/VSfvX8p+afPV81qTbV9zA6rFzxdB8+42RT4185dDP
5hd5FvePm2Of85kd+enrT9sPx5NfN2kfFk/+0+RdUjp/Hrefj5/n1+Kkf1P5
D46bY+e3fG3a5ec47pvTPrh3Nq8Zul1/lfcBJwCv/3RozH5L7vGWSXMF4gSe
O+nYAPY35e9MOh8PgJ+/bmiM/9u0m/a9M/f+vEnHKvDP+fnyceMPjdvPx93f
OGmun/+8xqRjF8jPnzSHQOfXaZNrh+b8+e/Xl3xPXeuP0/b7N5w2L4CTf3jc
8ym4Bz79Xbn+I+PmLMyzfH3anL66bTxtf1j8Ax+QjauHnmvw7L6XNvlj6sDf
vyf3bi7ivtwXP958AC5hs2nzDvRwDvemzndOOu6Bv63cn0Y9Z/DCScuzcwiT
jg3FN/xn0nGS9Pkm5rn5et+e9tw6PwqP8UDy/zvp2EQ+LX9RPn7jrknHFcjn
M7LD35mZdNwgO6dq82Ce30/a1zJvjUs2r7/r0NjSvC+8Cjubd4ZL/1nXWmva
vO97kg+Tw+HmmfkaeFll4dI/JH93OG3Svo1rLTBtjCsGD/8LV8FIHws+hT9h
2MdyXbiXbB7ZfDEd8XIz0/Yf1BtO+mfsHBsZDvxY7MN7f540D2z+eLFpYzvy
6cFtMA/sBW/BWickn3z5tPnhIdgHBjKOX5/vyDuDGxsnH76DS2AS86ewg/HU
eD+N/PnowAlwGtsw9qLRkS/2jM2PGTenjTnMp14xbY53JlhVfeCoFYNTPlHy
ldPGJeYp9dXmG89Pf07W5+i7zTPqK4xX5kqNWU+f9pg0N1dqTlT/9sikudP5
0z8+LflXx47+H5doHlBftNKkcYA66JvNU/rm9E+L5Lo4V/ONV+ZbXDg2v5X6
sPmMaffL9L6X+hizxDtpf/2bGDZ4A9bCl4k7wpP9Y9LfnlgnXJn8pw499yXG
BtdhvkxcjRggnKX5SP0JngVX4LszZyU+ZoOhfXbxJXx5sa/87i2H5pro4IVw
bkvkWrgGOr5HMcZs4lvE1vL3+dQ7R2bHvJj64En45cvkuuJD+Jm+TZgEDj4z
7xL5jKHn1ldMvm+BL2S+Rpwkfh4ONy+/fN4Tcwbyj0n/vViehRhCds4amovH
k5vXFk+rDnzwh4NbPjd0TCCd84aeW18pZcUorpznNaQ+/LJjo6OseXnvEmxj
DDKfe8vQ3BuOC6eE/8MV4vw2jzzHs62StsJd4blwYPgw/Jp5TG1JxzwjjAYL
whI7RH/9obkmnBE+BAeEC5p7bs/Kc9kuNtcbmkfBB+E3Hk4fap5uv8j6UnHj
bJob5S/yu8UJHxmZf83X5fPO5DmskWekb9JfmbOD7+F88RiLpR87bGgfTr/E
j+NTKet56kvki1HZI/XE/XwlduDwS9IO5qQ8hzXzLDzzF+bZwfhk/da80ZmN
L02d+eOe+Yvy3O+K7H3B2z893ya8DDd7Z2FhuByOg+9gRJjkiuTDvZdFH667
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
2qvZ55B2M59u/bM5L/vqiLXdOu+P98ackfjV1+bd2Cb53iXz+fYLMOZak8WO
eTdrp8Vrmiv3Xm+d+ogn3TH5H48OPnN2HVfesWOio27mFrfJtazZ2iHfwtap
j/fcWq2dUlYMxpvTVtY9ixc8YuiYxF0ii4XfJXU4Kzq4TeuLxfyZp/bO7pz3
1pgiX58vxkusl3fHnh/ijsxTe26759l5Z3bL+2PtCLvi3C5P/kp573bNe+h9
2yNlrYthx5qXM1In9XF+z+hsHtkzt3bU+pKbh35H3DveAw7cJ++Vtbx7p57i
Sry3sKL3heyd/Xt0Ls037t7N63k32cGN4BasMeDv35T7Z9Pzf1veAdhs3+hY
z2pNya1D77Wl7Pq5171Sf+s1lIXl/pKy7Lwt7xV79hwS4ypmGB6EC+E+6wvE
9MOE5k+tI4C7cAXmG8xjSPKtLbUXBH2YTQyImG/cvnUc1jSIUbEflzUMc/Pq
ZNjM8b2R2dsv14LXzE/AbOYoyDCdeY8Dci0bjrBvXsn15cOzsOKBk8fXn5Lx
G/a/Eh9vzsI+DwdF9q0fkm+ZX+/73yb9wMHROSiy89aKvn+u7Ljrv236g0Pz
PYqlFyuPK8AhfCAy3sF+EMaaraKvLxHDL1+cgDURrgc/i9VhByZ8R+xoW3j8
nXmm1nJqc1jUnlpHpv7mo4/Od63POSoyTofO7D4Y087XF4nZ+VDaQTlYFo61
t8PRkRdOPtvajb6YIPMv4gP0efoZcyr6n/fmtz5HbI+49N2H3gvimOjY40L+
bkP3c+zo68zJsMOGb0cM96F5x47N/ToeF1nMjtiduT5JXJG+CMfy8Vzr/uTT
ERdvzwcxRY6zZYeOuT4+8sG5FtyuT2XHHL578e0Yl/kIn0i+6+oH9Xt+n5g6
OH4i+ead1JltceCfzDe4bMlXTztOC4djbglX8+ncu37DnkjX5hsXE21+BR4W
3yWmXbw6XWX4FH9O/mFDH0+J7BqnRv/BaWMCcez8nDNST+0pRkpME47FtcQ/
W59q74kjys41046BZ+/aaa9RPS3nT4/OybmW2DO2nBPjZE3rKcm3B9pn5uqf
OvAX+Cbiz/FO5kPM0/AnPpt8/gjfQv5iKfu5XNd1yOZfjEXGJOf5VvbVMpfE
vzg3bXhGdObP9c9MHXBTZ6ZtlDs3ZV3/nMnj+3jgpNQXZyV/ro5fTD3V5ezU
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
xpVbJvUfoqO+yq2cOvw/uz9adw==
           "]]}]}, {}, {}, {}, {}}}, 
     VertexColors -> CompressedData["
1:eJysu3k4Ve/3Pp6ECkWkUlGUFElIKqxICJkqykwZI0UiMqQBKRGijBkSFUll
irAPmSPOMQ/nOOYppaRUv3X0+r0/f9X3er2vt39cln09+3nuZ6173ffez95o
5axvvXDBggVCLAsWMOPvOv/qn/VcBCRbxLKbbyHDosWTFV+1qWCzX3un3scy
eLNThzQgQAbN7ZZ7yWpUyL6wRzEH476nqxYsFCTDpWiJHWMY71u7r64G47ln
tjctxrhNvfDkFMYLAqw8zmLc1cf3/CSOIxPMeasT4106GVt/TJWBRXC74zuM
n70zdqsU4+6rfNa/wbiKeaRkMca5Chay5WD87C2rPU8xbk4+KpeP8aQVWZRM
jJM3+apnYLx7rZRtHsZvhNuLMOKWx0QXMOLPfq45wojnvr+4khGXI7HPpWPc
M+6zHiN+/eQVXkace8vQd0Z8Mm90Pj7N4TUfrz4zOB9/c+13/B3Vcz5+2/F3
fMM/8UP/xIP/iQv+E1f/f8RjlnMsOThdBrORmbHnELdD8ruUNNRx/KVktdGZ
Mhi0HxB020CG8s0xc5sOUYH1zKazu7+XQSBLb6z5RsS/9XziCMZvXuuZTfxW
Bhr8xo27MJ6vpWHzlnF9WdP58i9l8O1k4WVhHIfvUf3OXhz/lfj3ktOfyuCU
6y2lXXjf4S/rrFZh/GTeSVE23K8OlVjp9zjPq2E/6ATOM7pHM+olzj85UDu4
EONHia21zzAeKqTXzsB50a2c24x1tQXeUmes68CiQRZG/N7V50cZ8SMi7/kY
8Xh/8Xn8R1/+nMeBe1nwPA7rgn/jn9/8Gx+LTb/xP+b8G7f+f3Cb/C/x5xn6
Gt+AOFAPpMeIIA719Nl6Gq7X577TQfpiAmhhC7nFN5MhNlRMReAwFVzcdA5J
iBDQuFK6TXs77vsv5tLZI1RYPSzC5iBLgO+9e3KNO/G+Mmn5FYZUMNrQ84p5
CwEa5VpCN/D6zPhN+WJHqXBF/lC96VIC6IojkxE4/voY1UcXcPx3HdZHL38t
A7aUZfXxOB9JKVYBddyv/Y9a7GsRf4v9mT8ZdVR0q57nIwMfkk5LBa5rvfq3
YyW4LgHNFZQXGH+1pCvnMSN+fWMtY72Lr1zyYqy3+VcsOwP/hgR1A0bc/azo
akb8cKDCPP4nzzLrM+L6S2/P45MQ8Bv/0qbfuN0W/o1//j/5X/APnjX/D5wz
yhQev0ScvfO5BQVxXRzrQn07EWehY5G0PMRzumntCTfEpzRaWIYf8RFzvbT9
7VMCijw89Gb1yRCo4/ctz5UKUx6vU4x1SHDcM9qlw5cMbPeFrt66RwUdey8J
5lMk2HP18K7oa1i/vTkeJ+OpEO72+NjqFgJmOtsdXM2QN0KMLKy9qdDtLTuV
e5AAjkZKALcsrku6lNnEmArqeU+EZpkJsHrkrl0vTAY3na/uM5rIA9ceNi/F
emRvyjxnifif9ElIlMf5HyvT2NeG6/VfUHKuHNdrIG2wKR/Xe1Hg8PYnGOfn
7xpn4MC5YjSKgUOUqt88/pC6fx7/nlcb5/Hv69w6j3/67I953Kydb/zOf77f
+O958RvP74u9/ooz9Uzil3U4zyPDAl42OM80A+ee/TjPu0qKrbvlCbiunfnR
SZoMdCX9aKETVHB2OSH47BEJnHYG1z2OJkNY52CKeyYVfm6tX2Zz6y1cF1se
6mhDgQStG0p9xjSQli/QFY6vApOkJXWasxToXk5dmtlCg9vFiRSNc5WwIaPq
WEEqBfzcpOwUw2iwpoV9MC+iHJZ9rb4g304Gid27cwv6qTAeIGEAzwiIP/hr
5NERMgxMD3zXOo/58Ml/Mogb41ObZu9g3yl87RW8SYcKb2ROBJBxXa3MpDf3
cF31+UkrHXFdE30Rq9sRh0MiclkM/C2ubz7OwD+huDeLkf9ja1lrGPjIJt3y
ZODzaWLDPP9k+njN889wefI8/8zpsszjX/v+0zye472X/4pztvVU1X6sx1ut
jbtoGD++VOpkI6O+XPPH+aUIaNj5vS5ekgwxGoVuJAMqONjZjcvfKge+mscg
2kqG/euMhb71UYFiaadkr1QH464GT75/b4GLmrbaudl9EH4oJI68pxmaJaSV
Y4LbwdojfO/EATp4+vPuXGKC9fwtQCB6qh3KdbcUPJjog4awupLiUw1Qmi5W
vH6mFXKywh9aXaPDLZfLHpJTb+G4Q1fVJn8K8L3WM9nvSoPwua7vrEwkuLDw
2YE1DtjX3hSlygZQQZtdii2Lh4AFwvm+YqLYL1rE+FMR/48x2xZIIf5XEvmP
OCL+TZXHag8i/icWLMpk8I9E5mUNBv+4idrWMvhntqm7iIEzz7PaAAZudTfp
SvN9Db78ZOCZ/JU0zzMcW6VW/g3ntJo9nwsxfiOBS7cI45QlFYeeY3xqMN9k
HOep0WvWII/zlPdXfp6D83zylD2bI6AcjpgNSpylkKHEp0YugEYFxzHNzSmC
jXDjq9zVj2Zt4DLB4zLQSYeKjO9P+3N64c7Rhg7D7T2QFVq+e8+pXuAjbQwT
PDQAO7ZB/PRNKvAH7pHhlWuHM5euhsX50+ADybTm0J5esPJXizv9qBsycoyv
L/nSDH48JT5v6tpB43J9XiwHHRxsOaS3Pa2Ed9Vuk8avKBC1P3taOYEGbnfZ
ucvIBKwZqDbkRV6K11qguBJ56dn3fM1oVgLSBZyKAjeRwcjm2/0ILSqUtB8+
X4f59jr5ufoSxP+KwU59Bv+b1KXZ5SA+Jz5v+FCA+Ei+NTvE6L9xdPNtDPz3
r242Y+C5apuJ4N9wPnv8ETujXpzk7OIYcd8W/qOMePiW3Q3dc2VQ3Hd7Nxn1
w7MXq0oOa+D4H1LORMWSYJOvW6xOJBnOLV+bmPQY+35p64lPCxpBneTwoP9w
G3hvtPEIqKfD6xmuF+bq/cBc5WaWL0UFDpYd2VPWHWAaqM4Rt3gCir9u7rwz
NgAK/KJatrvaYVbcW7N25QSoRmz9Fsg5CBS+xc2bA9pBcjrh8+OAAaDe/DU7
mUSFRyIdG29MtEHVkJgC+U0TRPlvYS20aAfB69uNqm3psNEq8t7mpxUgUNhM
2ihKAQNFjpNWW2iw4F1x/IgxASPyFpzy8ti/ViZ4hVigPtk/m5CE/VclMpfz
EUMP3E4Y1sH+m9BrYTDJ0IdjuZE1iE8Tj7peMeLT7fgqkoHbjwulBAM3Qwun
83/DeadtBzNjXzq4N87zT1mI4Tz/xDJ/2TaKuktbgf/hcdzfjPJO2V1YX2PR
7XxyqQQYaBRlRuuSgUVZdy73HBXOcNfLqrythZxNy5s521tAS0jJUiCyD5Le
6jp6Xe8DfdPOS3ERvTA3Ujjcd6ELmCS02JhHJqCVL+sd94ZhkF2fGt6T0QML
5n8+QNap8Jjc6HGoNHlx4LjPAPDs5XxyXXoC2F6GjbYbDgKXXd62C9ABSdce
hBzhx7z4BXcM3HqA+jTl7ELRXlBdY/DW4FUt8L98uNqI3AL5QSr1W8L6YPJK
3dkaeRKIO4udyvEkw5aSZ/0ykbguttzZU+wEiNtLL+1G/ZNfVXzwBeofbp/J
vhbMc369RgkOxGF7dDYxzdD/l1dsf464vbrkHs3Ic+p7r19Zf8GZ69OieT5/
lfebzyerf/O5TmWSwxTG/QzDttZivLmgv46xj2e49lZ0CRGQ+jxU5KQ4GfY8
2bZaGnVdR4umXrP0W7h3uEQy+DAF7rF+rxxRpcFPPa8zB8zbQWXsZXTYi06I
7Q747upGA+80X46sq+NwuyKr801SPzguy/Y7y9zyH5ynjMdaPUPHwFq80GAg
hA4jCmm2BjyT8KD4zn6rgGEIahtrDz7WC/JciV+llAYgUvVcttgNKpRuFzpg
rdQOiypnaUfdGiF1j0r0rZA2GFM0t2dl6QeOzkIjqdPlcKPkRWBOPRleLD/U
yNRJhYAWibDhbQQcgjzdBAkymMhPH195DPVtirJbDvJ5X9IX9VDE+Ybc7bWn
MN+mol/olCE+VTZdPG8QH+lb4RU5f8G5M+l3fFfE7/iv8X/6431uxyyMqw4+
aGb4sn5zRyPGfi35pdDogn7E6/jnn6LIJ4Yvnke9xvpiNvkwVLaNBDouN1iy
3VD/dH5eOheK/sJ1a7410zvo+Kri4ejRChWtI4vv89Oh9ZF+jEAjDfbpvH7Y
e7QXmriqNwtqdUPhtVGVCtdx2OZSNHzRox8EFV5w379PgSWBx1Re+o3DuLuq
3bvofoj1OahUPUKBa61264+z04F12rVZm+iFkmWtNzPbO+HNyJIt1mmN8Glj
e6lMShu8ygZRd8SZV9O6s7qtHD4o+wk4jpOhQXLZjYHPVGAL6hRxUSRg7GC7
KYcMGQruBQk8Qn21+ctlew3UvdVvR48wIZ8onpuxIhBn3gWsQ02Iz243rhkC
8TmWItqf+xeci/6Jb/8nfvSf+PTFNQoMPtFIZTvFiBd5FAvP91nZhogsrKPI
FdZzC3B/17u8PDKKcees21La2wnwWnLT+9AO1EXK1LcpmA+Zya/DZjZUQLm9
ntNOJgp0tGb5hrLS4Nh+y9B16Y3gON4q8Q5xoC3UWPBlUT8skN0j/MGsByy8
Bp00L3RDnvTzcL0fvdCZxZwg+4MK/cJSXbZLe8FTsPv6gYM9oL+P9sDgdRss
qTh6S8erE5Yf4j9rmEqDSrXDbXPGdeBudvq2KXMr+N1ik/2Q2wcudas/KS4t
h/Hj+5XKc8jwJr15TLsc82HjueaiHQQcPvs67wvOvzd6T7Us6quXHxocyJ+x
rpvVm3txvbcjc36m/Rc4/yl+dHflD0a8asW5ed2i7jI6Hz/UceddEcbPsjU3
MfSJe01zHkOfGC5yZ5aeLYOEq0ZbG3Dfj4tU5J7DPH/GNdKe70dAzChfvJsq
GZKUHXPF7akwcs040z+8Ak7xW75euJECCqKxzZYbaaBpvqt1/546oMWtv507
2wLjh48b1Wf1we5kw0CTuAZIVcpuCeNrA5HgB6f7E+gQulcVrE3r4ERx8eNS
xHPfL/Yolbw+2Nad4vlx9VuoGQ0L41GlAPDE3U7YT4OrKZtLpXsIYBo5MJZj
QQYpq/JDmr6IZ+CWjaPLCTiV/v6NIOpwz5DzToQ2FTweHuTpwz7lLJxkY4A4
M82+15P+L3jjT/G4qcn5+CxtfD4+0XNpPk4/evMLI89/8QdHMeJv47bqMOJK
cn0PNDHPtxiNyPVjPMJdYLwJ43769anHFxJQIXs7zgh93Hti8+dI9HH7vBtX
ankS8H1VqNsTFRynOs/6sS32haXXdfiKSeAXEXUtLh511CILec8c3McwA+e5
hnLYp070zY6Q4enapFzKJyoIP3RakqdZDqHZVes+l6MeM+venNxMhaG75uSI
b6ifj6f3aduR4Wec9+EX16hwm3bEVQ59wYkyXaY09AVXk8XMazBvS9ecPX0L
+XDDxFMrSeTDAU6R92WYJxfsL3uY4rpeJZ7ePoLrcr13iL/1v+iDf4rrXf3H
vwf/jrcN/I4/o87M65Nf30Tn9Un6HrN5ffLYmD7DuK/+nvoGxn2tN36VYOjM
8sei49M4zyy++8DL0KXszKHfMW7ebLp9/GcZ2BVTnWyFyEBSuTxXiboxm3PF
gB76fTm2pKLj6Pe/ED6rFqLf50pfZs+qR8B77aybu/agXzjI/YzJjAqCU3Wz
X9VQJ8gZNGajTyfFblzfhz79m7Dc0AA/ATx5rrXi28jwaDCy6Jge+o4xNhNn
1KtGN6eVXiGe22U+pEnjfUt2VH2Wx7zNDP2lJIrzXEl2c+LAvD2XR/OowXX1
kA4nlOK61lxIHX35F10Xxadn8BjXO7wqqv4Xxn2Sjh4bwfian7sdduD4fQur
fDbh+GIzMUpLcHyaa7qxJV5/ULtdaRSvT6tVY25j4El3opzbRYD3Q3+1mzvJ
IDj6I+6mIRXUtC0OUb0JyJ0rl398EOuR/46wvx0VRgXXlCtZE/BJ9rlysyIZ
HCOqdYutqNC2VLTszEYCeoQeSf8Qw/wZvrKtS58KBTLuAWd+lAHXHSPhH4jD
Z2LE2hlxEO80byfhuoykYioZdcpkOWLG8Gu6xyTjU28T4HI2I/2+BvYLpwzW
x45U+PklcoCrgwQRjysUMh6SIUSubMOBArz+9c3uktUVQPk5Zzj8E/0CZad0
yiIadDVua2+/TYJXCj/p5WFk0K87Ip/2CP2volXDYXcCSni3xbQcIENtxpqf
MzZU+F6RkwWoA4VaAjKbUAeuEiTdeIw6ULI0YBcD/+vkJekM/F2Dh0wZ+NuX
Xzmc3ElAfZxS+A9zzJOTJ6Kzfahwlz9dlGT4FupuZOVcN6TA82mD3KM6NLhk
Hfq0YGMdND8VfS3xqQXWFr+00crog57WKEHLmgrg75xKCpCkgHD7stMXJGhQ
kD4TEt9HQNX+n4snLDFPQmSXHvXDOuq/SK3jJaDVSUjCC/2mnaTAgSH0mxx3
jOafJ6/+tGX+efKbyGvzfhY6YjVa46pg10WmL7KzFMiP3eUZ20KDJ8dv6Qav
I4PG1T66Wkc7fEj33qPJRIdLDZEHj3h3wxe3/d5MW7qhYnzHkiSgwnBfm9C5
OeRhpxVSTcgn2y7aNQpfp8Ir99Aak2UEhJg2rckWQZ5Jz9pmg/zMdX5svq5J
I2PzdZ2/7Dd/snyN9Cv0IIBvb8YOBeS964PbYw8j70k1hzSxTrTDs+rogqKB
TlBOYo331qSBvNKFl/SsYdhQw7X2WzHiuPeSnpYcBd6xeupP3CNgNkvyq9th
MgTXE/Y8zlRQXTgrXob17pP408sY6z0mctNoMeab7j/PCeP+4Z/n//CP6wdD
567NBBDpxEk55IHXm7iaBlBvR/1hH3d+spIc2zcO75ntXrWO0aGqr+ar3n4y
tB6UsLyYPAku+qee5U+OgE6q8lMzXhronEw+/+ZoOSxIOC7uXol9QV5Bt4RC
BbdytuAk1MPs4oJ3g1EPK/ux5C069meelHRofBaE64Iv6jWauK4OuwfZ2biu
X3+oi/9f5x8Ib/8xuHoMBJpnvz/P7PtPvNfd82jMlzEgJVorntTuB/6FmSf2
6lZCRmYSX3AsBSL5DFP8gmhwdtMpwwuBBJTFZJTEqJMhal27zZzDn+fpv3tD
6jPkn3xNr1PqyD/89F1XtiD/qP+BT9K5f9nN1FaDfmDIQYfDLUBfZHs9QqAP
pjcW2zyRn4T8r5qK+RPDoGR/mP0ZuRc0fuyOEZAdB664S6UG3XSYK5w2NsQ8
/tN81F6vL9zMgffNVYrkwfy0byTbULGulbmZa5OFymFXoPpkcAHywCJxZpYa
KtiuElVsLuqHzLETdxeYUOFG0+bgNqwPUrW9X6BvC7zi9LZiet4B8iZNRS8O
98F5Ly0OHZsquBTFVlA1TIFq60OUmGoaTKSdzglHPSN+USC8HvVMpPqFEkvf
P+OWabl+6xf0QTfjlRqSGO+ttD3zXRE3/WJL+6cnCChopjXF7iPDiR3+aost
kM9ZuAYWXKHAufSUt0onOuBHGylqIqoPUm6tCfm69R18lzudk32lFSZphj1r
NtFhW6c2/0LrCrhw8e7QKl4K1DaVpvrw0VCnDvl5+BKwbvi00TrUgbd3FXpS
7f48T3n9tasuYR4q39+0SB3zsLva3T8T85D/5UszyRAC1O7d/S6FvH3Q5VLy
EeRt8TsRAZpV5RC+PO8rZYjx/OdI8ZKP6HMlgr58sKqC1H4Z5fYhCmwZH/3V
WEWDJrUPG5vuVUCO49ISQogCVge5D3UK0eCxdWncWj4SHNGQPTrtTIZY46Wb
RW5S4bhusRgf1u/rNb1LmLB+N33tFss78hf9XDaU5Yt6zH9Bjps16rGdDkvW
JKAeWz2z7E6FLgFTfl8ahLHvP/lZUPTRFOtop6cpE/J8duGzqAbkeSFV88xg
5PnpoE6t7UkEXInZYVSiQ4alG04/ZDqHPBx38uUD9DUHFNY9s0BfsD5Y4Vge
1vV0N0VmIfZB5jEbjh7sg+GdTksMEDfrkScsO7Evf7VIau5ivL9Tlnpeh/Ok
iDlprcY6Ut47vUIA88F36+pWZsyH3fuXzT9P2KT9ZX5dTl+v/Ffvm37J35Dq
wfua8J6RXYbjS6xOu/jlL9fXT0eEyi0iwEHt11gP4rabyNFj1UJdtNwgdPsq
AtSfi3wX3or+IuBq0AXdP9fdGgEezs6dqIeDnxncQv35UXdF0UvUn2zOTCzm
i9G3ahlwsmH/fcQbyP0Lx3cPqz6UE0WAivqxEy1aZFDdP5d76wzqlhFXV1PE
k4M6eOor4rk1UsfmNOLpXOQmZrmagIatRk4XcT6fhI6HFON8trNdlfmO69JS
TKXW47p4TketLv3Leq+YXfO8inmyvsCx2gbX660gJ5eIeZLkmNIxw0eAci03
10ocf01z+Bd7HP/8nsaw60aoG7P2irRhnQYkSQnuwzpNl43kEjRHfVWzdlpQ
gQxE+6472pZUiNXPC0tkIcH+I6vMLU5j37m02kgjEH2BzMpD0ddw/EGhtndq
ZPCf4L+hi3ybs6UhbMCFBCFkg/GCIMzD2z8aex7gPnasSfi0n4DkgIu3itHv
Pwj+cVjRiAr5adHJdQPlUJlMPrZkCuu064Dpx69UGJ9QvtyP4/dzaGn14/ij
183irXB8q5+GpKtrCeCtXvrwOOpYt2tbcjxRx7aqsWUSrASYR/lXR2wiQ57R
ASIe92VpnGPCM8TNS3PhNoYOz5G0Svqb/nd8T9/tK0NA6YVImjrqzKt6Cg1H
UGce9u1Y1r6BBOf0dhh+cMG6po6szgihAndPhc+FIgJe3d2addSQDDVDsSus
3anQqEuJroosh0CSjWxgO/Lq4Tu3v/VTQdRSdCj/VzkkPiE9+DxDhgyqvGjl
T6z3X6rhF4cqQeC15A1yBQVW9crVbH5KA8+Ml+471Muh/HlXyyyJDFZ8Iqbv
mjA/17sv7xkjcH9qP/meIsNk98cwH3/UdYtbt97fVgusEfLKQZkt4Dv1wOPs
2T6oj6Ed55IkYP+K5BROzGfvz6zrNDCfOxyc7hecJQGhOe29Afcrk0cnzRT3
6w3X+DBlAQGfS6LcJ5E/dZ/eT92HeXXKfuMWhn6rsx03ZeA2lX9P4G/1+0Ou
2lpvH/LSjmOT26XRNxVtSBk7jnXxyiO6QbwcRGXtvc2LyDCt+rLLpo4KJt9o
13Wkq8HWemdyoVALfHswNnH/Gw0CvpxKfPOwEvrV1DLCX1Bg6FisVVkcDQjF
3mapo40gbh5suPlSG+yyakvz/0wHf8Egla6qd5DvvHpg7bNW4GeVPhJ0iA6K
vlW50vfr4WHPLaEy41bY8Xa86+xCOnxMf55ZrVsF0wFsie/6KOC3mU/iTjkN
DnTcS9L+ToLe5AUf47PIwCX19ua7UiqQmWCe35Tbfr//fWV1+688ZucquWRy
pgz2R05tu7CBDJ6BWV2b0bcW1NlRnjYQ4Oq2XOyxCfIGW8N7uhcVZDYVxjzL
LId6WhCHKxV9Ss4lwXejVEi94rAc0lEH3tvLlatHhtD4dabpLlSYUmpRcVzZ
AAdfdZhw1LTCww0rlita0MGMdsWfEtsK9o+Ksw/ydcKC5FM5rbM0MGRfFW3x
vRMeetD9zBK6oEBgLS9PNvbHxu/T29zpUGxs4i893Au6NF2zYeNOaN9jdi9v
9j20yg2pr13bDjwdlZ4sSXSIWCjiGJZRCbWZZ1STX1LA/qq6blo8DTglX587
h/ve2yJDkcV95wujb5rGfSeSf+Nz4Z/nn7ITf+d/Lr7IERHsL6RV54SEGH55
dJ8KG/aXpkN55y5hvSs5dS+4jPUelvNwZQjW+/4/+Ige+VY/Lw0CHtlLaJ3a
TYZTtnPBF1E3Bf4hr1abJLK2sIzDihtM9cnWdPi2l/aZaaoJVp4T3Nke0Q+j
/CNz97BPFFsN0Fk5OmCu2ta4ZPwtPJteoCt5mQIVyy0Pu7jQQGPj7Ofy7w0Q
XnLpMItmG6xl6mvjr6UDWe9LW5UwAS9+qBSkiJOBLabO1AH1wM2EG07rVUnQ
+sV8m4Y38rCO87WTUVRQKlJVKkD/cuZox/Ia7FMBGoYcl9C/3P0HN6F/cNP8
f/TTKA3F0S7G+/Qe7jcVGD+8Sk2hAONGnStNmDA/JVKub5bB/HTzkpGbQJxH
xw68J1aSgGk3y1QH6hnZmdG73KhnJmgfX63kJCB0CUnRAOdjzZdwdw3OZ+sf
+G2J3Zs19h/bQGW7nJ5uSCd8C32Y7xJJg/6dm56oizTBPvHFV0jb2iHFrEl7
LIIOfMe4nd4sGwVyOVO12t4+UE+3K6uOaQb+KQHvqFsTEP1T4cuiPUOwR0Hk
TLdT13/8gkNWX41K9BgMDmn89IujQ4Myae2D0kno1uhYuH7fKIzE8FDazqFf
cFvPWttGh1rFF9872angonUl9NNgB9zwTsnfIjwOz7qCb41nYh2cjVxNPGwG
l/Sfge7ybcCtfUbGXLsTrs85K42jfs6vq9+1OJ8Aw0z6/TUGZCDZBFw2voB6
WEtdKq+4Aj45sVoyb6dA10bZB75iNCAt1uWtRf189dk7wSjM5zPbVHvsEeeK
vFqaxlECtPhPDa7fSwaHcu+AcrN//5zq5/2dD5twnGz5t/QDOM4Jj/s3+3Gc
9mcsd6JLq6BnSaHyZuYWaD7advB7Dw3i/tDf9946uM7+5TtwXCdk75TRChbX
dtu8UaFD0/p05qz8XnjWbcNpv7MH6Cyvbd5b9P4Hf5v5n4l//h6CKa4wfpn+
cZDS7vDliBgAxdhXKZ0n2v5z/Q5pHa2M42Pw1ZdXB0TooNCweWuN4zicFdfZ
xGrbD1kp63/pOVMg0Dw18EhuJ5R1sPOdD+gCjTj9VJ5y7MuLna5faiUBWauw
7mMqGbiNRK4+ysc+1S+pQjhWA+ulPoFsqRZ4QrjbL1vcB2pWevnm38ugyTq1
zgH12ItwjuXTyMMmJs+f+9EJsHVhaxe3IkNC6t65QL9/j/9KcfGZaDYCaHE+
SxqQlz4pdz1vYOjPhb47Mo6Xg2ti5MtbVWTINTPjfdzyZ50p3vr9vo/OWxjq
WOuz9xgFgn9GfDh1mAbHv6wgZfyagBz2xC27DIaBy6rZPGx1L3Av27uUHoS4
v2UyJO0YgtoPtjBm0AXkuLo+l5pyiFp0e8PZYfQ1xRPNN9DXpN3x9E1iJmCp
dYTCK9SNd8vkdvRp/vv1LkvK47xyGXlJMujyU/RlOTYh743tqfBsfKtw9yQB
R396sx2zJoPH686tUleocEdiZLubMw3qVy/dFSKF/eUhe+1UcTckcwucX7xi
BDzbHILcqTQYskrrGEa9w01WCDhiOQx843bhpsk0eBIZSDyOoEB3xt2DMg97
oVjM5meucA8UHUsdPOjYC83y1cs5l/SCfKKSu/LLbpAWOCv9qLIX9nQxfYyw
aoA5UesVI59bYe2JmSPmV+nQfq/GPWtHE+Rwvj6xQqId3BfzUEXvID8f1AzR
CCsHM89IT/U2MqTmSKwYp1Ph8+5mCffpt/B5RQLBib72TmFt/qLzNHiuzUe+
zkJA3i7ht/K4736DgRFHcd9/lHwTWP+SAOOQtV8XHiNDCbUr7awbFWZPC3Mm
oa/h0Tqw9wfiubKLJDr0XzyXbmWXbvmG+fbiut+eUbxvILNo3CDed8D8w8wq
9BexTBOrrFH/xzwbcXiK+j+HW2P9vVcVEHJ3Z0fpNgqcoGp2Rm+lQXHSdOet
+AawUvL02LeqDVI9rp06n0gHvwFfTUpoC7wZN5bgy+sArdmw7Hj1PgiP6T+4
QLsV7lft6w/91gE3Q9d1LuLug90Vn1i6FzZBnbeqiL5AO5hmzRw4lkCHfWnW
ybFcDSB7L1bQorIVHFKrDX+Y0mEg9J3KqdEqWMAkblO6rAV8+OxMSMM0OLpx
LjiHtRqYrPJOG/K1wJVrUru2fqBBdahfzwYnEnz1e35+QyAZ3A1Y2q8lUmEo
RT/lEGc50DvGQuJeoK+3HJdnqaDCZO0+k4qfZSAgEZZqgjq2S266/w36r1O5
3uUBWgTUvfcY/4x6gLmkSmMa9cCWNhFDM8a5EVA5zXiPUCzvoN36X+xL42l9
CS57EiyuHTP/dR39+0CY1J0EKgx/PXrQ91sl0ONkBFtqKKCqwTJnmk2DC2t4
rvSzkuCenlBSAfLw3i2Lz9xFHr6ZrFw88hJ16UxNycAvCmwQ51CFLhoEsW06
n+1YDtuf6X1a/44MHVEj/sadVHCQTJ2ZlqiElIUrRA6HU4BXU8Tn1WUa+EQW
0c+zkyBPYGLwpyPq+YC64rQgKoSK9WTc2IT92kByO08hGfpZfRa411Bhp9It
x6OJJDjMJnvuxF0yiES3Z/o/oQL4RtWouxGgd+PkD7UDyCeldVM+NlSYTtC8
fsAH+TM88nrFQTLc940VvmdHhR3Mn5aRv5RBUtuuli2oK9hOfmjtw37nJq3q
bMpOwETCzFQH+uj9wxWXnx/+9zhbjOzS8BUgYKaX7iYuRoZ6//V7t+tTYXmn
0bbuSAJ+mHgvOYZ+XGy7LU0Q/fiP0n3+J5eQIGlZdftpxOHgiyN1YohDxLXV
JVcsCEiadFh8Bf1v04oc3iT0v5kWS1mzUefk7v1sdRl1TruM1icF1Dk85/y3
Mt5jWrLmpDLeY5Z7fwpjvMe8dQ6ashnvl79e8WX4zS3Lz4/8zW/m6+v8VJMi
wPr+Xf3HyP8hk6UL3yH/3zivK7t6KwkGRu767HUjg8rIjk3RoVS435Tq5ve5
DBy02vZWoX5gs0zij0A8TV51hFcwvwVbdqOlUvspIEhJicvYR4PZ4GVZs5jP
WtuGwlYyzmFGb3k6h/fVhk3O0ag/F6zcsiAK9Wd+9GCNGepPibCsb15mPcC0
yTRG6EI3tA5uznP90QvLV4Xdvq7bCmc3XWI9/70DjjwM6lfl6oNPWwRG1Sfq
4HVszvAC8VaYK9UskWzvg94qZQNVoXLIrVKYPVeAdcrhv3GymgrypOntfVNj
cCbkqGSZNB3SSzboHnZrguVKfu+DUO95SVp+kkG9N8emUbkkthkSpdKLnqyr
hRxBnq6MjBYw3Jf2crtTH1i9rKcPjFHg7ElupYs+HdB9d1C03qMPXjkFBoTw
jIPAq4OhEwF0sO04rM6p2gwXH9E5/G9OwqJDT0x1Xo1AZ3JYqTj6TSX95y/M
gkbAZRUPhx17H2gzvzkRI0eGRysNli9APj2Q+O35QFc7SEUtluT/1QcXr50O
mviF+uEH02gm8skXkyIFFuybWuuDDh1ZNgARNd+eanhQQcps2nWXdzuw1aTY
T+2eBLuEm+X9tGH4vuz5N5a3vSCamb2gkaMTmNdTUgI0u8BT6QJ55wwVRFao
Dis9qYKnd2fad/+gwDYdIb25dhoIXHP6eZOfgIIf8e95tpGht9P32X49Kshd
KjyjUURAzqrGc+qGZFjn/LDUyJ0K5g7X5DXlCLh6tbvPQAr9fn3Myd3ovyrI
xgsb7QfhkWFHhT3qgEzzbIlH9FaYUZBzkEW/tmJn6FkR9Gtn+an6A3i9VaVJ
mhHW74wrlcKC9Ut9fUbyLeabs4P8yuXLakD3Qiu3r3kLPLhb9qFxex8cTfrY
6RbYDHeU7VcqpLVDUcKRD43idIgXVvvULEgA369i02Cs05pdHHLeWKex8oth
BPWwacfCywmYn8VM5pNncfzPPbdCfDzfgqYjSUnckgKPj5RIXTCkgVRjQtbF
snLgfdDC/HEA9VtWyM+zH9AXey6WE0I9Ez3mH3MH9YwKV4JIOe7L8lG5Tsb3
DlssQ60Z52qaZuvnGHVHW1e5pRb55yz7uZxvyD/yL3841SL/HBTcwEmLIEC2
yYxpOfLGIpXbHXQnKry7c3bo01ICThe0PijG6zu5e2sZ/k/oc29iyyICCsmS
6zgwX6iijrfFsP/Si1en1chWwNo+4SdKbBQwHqQ3PmGnQdsry/0Cc2XQY6Fa
kID6UzI8JmMD9qOpzMtOdX0EML8Y5fxuib5glTWzOerPj3xdz+sV3sFxp3ZN
maBWmC4IqisWpcN5RZe7YrNVcObN612RK1rg0KFI6Z9jNHDgXDm7yrEDkp4/
L2Dn6YIvlAcRziI0uJ8kN8aF/i57Zq+HHO7j0ONfnVMMHj4VXViC+bwmjLwz
CfPZ1sLu6jecz9yV4BiTRR2gdHKveNREJygcaPoxpoLji9wy9tKvhpRz75VO
ibUA0/11eweZ+mC/wuyVM1ME7GJtViSh3tsbtXHwNuo97w2j1xtQF/1yTgU9
xIdjOpBsjfhcGmje6486nGZuc/I84rA0WFZ4DnV4UPlaod6FBNhc5tvvh/u4
t+jp05e4j6Mrb+eO0ggQk7E0fo34+I/zim1DfBS5H9FsTxLw/aJCoq4iGbju
1pvqWCFPKhjwv0NdndwxXhSEvDp7ecHL58irVy9pNfp+J+D2kpStoXZk0M6N
0xu9RoXr6yIOa02VgyG774NPn5AP265Z2X6nwqu1HrNXwgmgX2IhnmqSYR+r
RP8dzAcf+ZvMk9wEnEua4MjaQoa0H2W75HWoEBn1bOvxsiq4tn2llxH6rKew
b6NoLw19nmC5TigNru/1ZuJX7IW7a+K09aK7ofSscCS3JwESb8x3xaqQ4diA
ak6cLRVGjM+2JR8jIO6tYIE3+rgja6w1+cz/731Zqehuy6DrY2DvqHZX6RId
eAalv9Wi3jt45rF5FOq9BNvpmhbUe7kOu+/2Y57na/YTTNi/eJyVrd5j3nIW
9y4/WIV9/JfioVgjMsSrvHot6kmFQ9phq5Xz3gLne+qJoy4U0OyVXDtmTYM5
pVOKsd4ESEotWx6N/X0aLGfPYX9/9aXHkYS8FOdiLyeMvOT0oivjMPLSpcrd
CrNdvRByCMZGNHpAJnnNMm/NXlgy6aSt86ERXpwxeNNc1AbsW3LjF39AHc4b
lGaTWwG7gsw/fUNdWlmf+r0SdWnZKE/GzYRGuDfAN+DyoA2o0Z/9y9n60Wct
XOSkVA1HC05ZpYm0QIj9+49DP2jQz12gx1tJguZNTrynHpDh1mlPocaX6Hfi
9qbNyhDQ28bV77CTDOHmtz3PGlJB9ovjFr2pCqhX2+fIvQ99FqGkcnw36nlL
N7LvFAkqxDf69T8hQ/XG9S+uvEF+uDl4fgZ1jkpfyeWviEP9mXUqBOLg2pda
zfg+q1H3RDDj+6x7a3UTGd9nnd8tmsPQ+Y0rLO0YOl/XJVmHofMVhblOMc45
9OTuUGGcc5A+BiyMcw5Zz468mEW+OigyfoHx/N/aK6W4BONhHkV2xRiv6TL/
ydAbxLLsTobeCLQJ42bw20Lbg9sZ/JYTYVLH4LeTirWsXzD/7d+Qb0Vg/m/w
+9RYiPmvLux3TVSdgN2cpqxvZMmw9U674oQxFeL37uwaP07A4uG8eNt9qPf2
NJzpwHwzH0o7LxlHgF3viSem2phXEQ0vKM5UOHG8y97UnIAE1dqDO1EveR/f
LmaKeimtQUqZ8XzYQsHFmqFzJPg+bGLMh0iP+sXQP4veTc0/L336yXD+vf8X
2+USU4cJ+KLxQM8I++6C8BND6qZUUD7ncE4/nYC54dsur/XQB4a0DTx1QV16
4vGpMFsCHLY9u+cBZKhNSGxWP4n609W1660wCTS0a+JLXMmwWrQw9cRtKiyd
vB1kvbQcXvxUmX2XQwYTy9TPhuVUcMzJEC/eUQ6bz9S+2FFMhmCTNhO+eipE
y+Z/5Ue/BgrK5gy/Juq2ZAvDr0nQ53YmcFcAS6Oq7MQcGRLvZJR9XUgDY+nU
OQ5TAuTdMl/elEed47HT/b0F3veFZduFzQSstqigr91OhicdTvX1qK++Emn1
wuIk4FG/wMFzgQxmXacytMOoUJf4+/xt54Pf52+f5v4+f9sy/Vs3fs/4rRt3
Mv3+niXOa/pSlAYJUsx+XAn0wfpdrd7qHI37+9OD1+siAR/8Z2xckE9kdz+9
4Il8Ut03ggOXQ4TXWJcgnQxaBxKPcE1QwWxgd/JQTxXYEQXkUPYWyJbdvp53
kAY7Hgw6n4ytgaTai2qLA1rAyjzqySadPpBwdCg5kEsCFpYW07xYMgwvK9Cj
ZlNh4T/nFgL+0beJ/+jbW39437f61FKnDrG3ICeYqXpUkwJDB7k+cxykgSuX
pk+N9VsgZj00HY0p4Op5MjhOnwaFOk6/mngb4dKDFY8+HG+DCfwfpYX+r/3C
L9PGVgP0C9fjPu3ZiDok4fDaMmHUIYtUb11I30GA1NgZrqkdZGjcd4VFCuvl
Yk3KmTjkVcvjmdtuIK8eSj+QUav7732KyI0ve2ZQP8irNt16g/rhRUB5+H3k
4UsO+od1owigndBtL0S9cWThmuAL6FMGEsuaTqNv/WX3yPcA9uVdx3NzMrAv
a5ro6G3uIMEUOWU46yHOp/HikFoB6qi7SxbHYl+LXPj5VxT2NWXXtLEp7Gtj
74IE1ysRcHnFS95qGTIE7nZjUTfC/hU0quvOSgBnhXSEL/bl51watJvYlw+c
/q73yQ95UtM34hrjvKVGpthe+3+/Xof7hydKthDQFfY8uxjzX+enAOvxo1S4
M1uvyidEggeXKdvVsU6bhKc3TodQIc8jMDIq5i2oe4r0mjogDxc7fNllToOB
Ahcezljssz+ZX3Mg/0TvDLx0x/n/+uA6UR22hulxkD39gpNVbPA/8fGcJr2c
tnEwm3LcI/l1APrtoss3Z5dDn9uyxo009FmiRk5WY1TYpV+oFY/+q957XJNA
nrR1erS7D/ddSHpmsE6aBC2CPjwyHmTou/uGnT/83+PA1du/01uNgOBvws63
kW9/iggLVRgzzsear4uJJsHerIOSMhFkcPtxvygngwo1+0LKwjDfWHkfjV3H
fBtY9WSiCvONME60TzlQAQou/rubllKAU+NpfeYyrIuQ46mrOychY3F9u5r3
KCgH3FkwQND+9TzDIi9erT5KAM95e2Ml1BvqJDX7PjP0R1sb5k6/Qh4mU+bM
kQ9lf0Qve4N8+M5LU3Ta9S3Up5psNjanQJ/N6u0Tx2jAS+1NYn1AAksz3o83
7qIvDjpo9+QJFfw6Nm04gn0zP3Kuoxr75hPy0+rYv5zf+FM82Th1Vbcy6r2T
tjWqu8jgvIlfOBvzebH1LTvG+Vuz1rQmxvnb+H4xH8b528bgNXqs4U3w4nXK
y++67cB/QfnX5/N0yBS6cWfN6iq4/+GhDFczBVpVp1W08miQJXTlWCsHCXJ3
5yRbOpGh0j/jy7cgKpwuU1js+AzzmXz82MsjZHjNXutieP6/eF4xk8Uuo0CC
rMdvXBo8ybAi4sTJw5GoV58Nj+1Xr4L9Io4eZ6kUuKri4nwH9/F1ar32DYly
GDi8XLK9CPVqi7V6PfrW6XLvLDXUvW0Uj0w11L2hHJ1Naqh7wwYuVyahHtt4
hq93Deqxqoue1QdRj/HPfGP23U6A3ylOTkPkN+ORzl3P/nKO60/xHa3toR9t
CKiN0PTSwv7bf5Q7VRj7L/fA2FuOOgIeRTesTjcmw+ZWW212Lyo0q+gHnXpP
gHSpI9tmUzLIiNADNS9RQWeDhY+BKwFqB2PuFCmToWHLcbWlNlTQVzXSV2E8
D5H2PpyB9Zgl8UykHuuxoCl4/BonAa5aFg5uqGMfsGh2SuP+rr7XLhLC+I6e
e0fODON7CoXlfn04z/2cr+sbGN9Z1L/gZ5xjv/LUSYxxjj37UGNQFF5/zuuJ
9jeMZ3s/et7PeP8V9GgVGa9nVrXTJGFc3g3E8/6Cw3aaxVYxHCc8iibdifGM
55P6tYzzvV8Lwq+hHmiP+y6TiXpgzeWPzydQD7w7vvAAfQL947nb8cvRH02M
S6bP+FOhPeXeDEMvrRkhz5/TPv3YTuev5+p9n19gnCs2XDr6hPF8KTM1j4tx
rphD6jJFaUk5EE+lPvqgzuGVls5gvB+xbYqe1xUmtevmddcGh5S/7q/lurWc
jPlIkp7Pf2/bP3B5/ntb7viiuKQ09MVDPT4qqMdmP6bVqqMec1a+YT70mADH
WRG98/pkEGZJ6lV3pcIqhcvrb6+thBihd8XDtyhw+bFuu5g3DSILdLd4aRNg
/JMv1Q/1Xv2BhbL2qPf2UzYHSX8tgwO87FJB6E8LHx4Z3IV+8PWHsscCyIc2
nDenbZAPH6xJnstEPkz2+j1/3aDf82+k/z1vPyaNzsefPvt9zpPU9VtHOdd1
JMXuJqCBu/2kiBTqxgWZ/suO//u6ePxPXPSfuM4/8XfVph7rHAkoKbR8Wrgf
87zZWvjEKeyPiRk7Z8NRv7XbkUbuoL+4Sl8Zn/7v7/uneK/Eh9JcWQLKrZc2
DKDfsfniM9aKfids6WtvYh8BdyIeJh6WJsOXKR8jthNUODMX9O0H8saqp6t3
SSNvKK102GSs97+bz50HW3aS2gi4pvjKYqc5GRYfsLjEwjiH3GBttJjR7+z8
Si1wf8dZ58rScX8/vY7Y7nyqHORvNT98X4t5YhHv59hOhWL2nWNvPWrh6fW+
pP2kFnhY676v2L8P2k85MdUqYJ/yX7i7G9dlwKcrd/nE/27+PjXKceflUC8x
a1lZYZ5IJafxKGGe7Asc9PmUR8DYka42HgP0X32zaQYX/k+ffGZxXdQ+NAZi
jw0ro2X64TrV56rGExJYR61kfXKPDB84Oe7rZP3v5pmhLqS2KZMEKmUvEnbc
x/3tirxWjOMHpuxTq9QjwBBabU7vIcPZ7GaeHdjfmdf60a+vIGC/hZRCG/bN
JVdbSOY6/7v5qH7bwX1JhwCfJ74LK7He9fP9TVKx3q9vCBU8gjzfL8rhkIk8
LyDXGteIPG8X9XRr9jkCnu87HKSOfYE3VjEpzZoKKtTnwinfy4DqWVLvvZEM
arbJPItQD5cmTmVL8mEf3GhR1MX4rnmd67F9/4U+/1P8SP8d48uoV/tf+IxZ
o17tDGWR4URfuXWIzX76FAE1c6Hlxdh/F38UlHmC/dfvlxnPKuQTss1uDSbM
E4nkAIVJw//dfIbtzhi4nqwAgzvTwuk8FOh9ve3QRj4anG7grDybj/okcUWa
LOZhhse2H66Yh/JxkxNW8SSQubB6/7tIMpi2/pJZhnqMa3vRUWXcl8nbjV9z
cV+2KS9sjTL9382TMjdE2of90amwq6GX8R3Ho2OyDf/FOH+K+/Ys685ZR0Ds
4vaWJ8hXrrWT6aXIV0K9AgeldhGwMGpzgify3kDC10JvxN/tm5DXadS3weIH
Pougvt0juWFrHea/tVnc56dbCUiOu5yqJkGGO4sCz9cc/fN9b9V9f2N5l4DP
rTF3L6Jfe7BBIFsR/dosSWPs1kMSGFVm8/JGk+HC0Xhi5ikVxPqmZQVQp11V
dt4qj3myb7GigKLVn8ePzGMt9ME+Pkwy7LyNfVz1DX10I/Zx7YqSgPOqFZBf
2Qsx7BQYWb/IznA5DWJ8zbyOLCLB7Di1lfk0GTocmczmAv7yvumeUuJbdwKY
Bk+cmz1AhgA+viu8tn++/oFZo8/FJQREUS5fPYa+1e7CZhkl9K1r77lFKIkh
/sftiFzEjfeA0PTWv+jJA+vHROlYF8Hs35X2Yl3Eqt1WEkT+VFfcUhE1OAQX
UsTdIj1oQNItCq8RbvnjOF5nlxz7/LIcbnY8HXHvI4PWUAXJf/z/+Da8wrZg
ecg4HI3w9zZ1GABay4JP564REFO3QLpcjQz3VU1D1f7yXcCJvXsXDngQ8Lrd
8Kc64/nJ5a7bhoiPn3nXgy+nGmDkqFOV/Ewr8EZ8a7h0jf6f+x40vHn/dek4
JAm/faBUMwBbznC0dzlPglzu7e4XZ0ZARajkgxX6Bcf7jSObKQSkMh/ilTQj
g2ZRTry495/nU8pBi+f+UQavPa4f7UPeO/fhcOoJ5L1kafUtn5G3ZSS+ds0g
b0fL+0ReRN5OkWAllndVwgvrqrF0ggKX3ozlXsygwcqFZwr2KUwCR/qV24If
hiFtT6CxY0svNG97u+fjk0kYDqHV2bKNgln8upzCnTRYpyMl8ll8AuLtJ9Y5
qw0CTTc+/zVfB0hEnBnzRj3jbm3zhoJ6ZqrMftoZ9YzASlNuhm4XK3Pex9Dt
XjMeqxi6/U/r6lh6ypY7kQT6cKxJFX2iqdrADxfkJXamgeSCNQS8YN50qg31
QK5E8tnlf9Ehkabcy5cwEZAk5m79Q4gMzRs2nFTTpMJUjz9bPPYFbjOldXPY
Fy4vCZ04in1hl5tdeEZROQSw7V0a2486TXhdme4k6lXmPavIhSRQvdoi8SMO
9SFP86pPz6nA8tD84O4jBF639ngj9s0d7KsXXEPesMzXrvi6jIBw6nOnTvQj
Rl9vZt3Q/vM88+wCJPaplAPl86q0XgL9yJCKCe09FQZOC6xINCdB3oXUfRZX
UVfzmV4ziaNi/1Norj5EAjkhtwwdH+wjjwvHIZoKEatH9gYzk8A9ulSu1gHr
ly3Oqw7r/cYa80XSqaiTCQ7VKF0yPCo9XPvqHOr/5Xud0/UJiNw/PZWO8+9I
Hqo++ZdzZdNJYTmM79BPGPsMMZ4nP2nZeprx/tqgINC7XYUA/5sii5rRd3t9
zNkmakwF1lAl59XIq4EuRX3L9zLOV8fl5uP4kTPWnzfZloNk5TGz8Doy5BfY
TC7rQB5eBFMKx5EHil6XyO5jnFfMVHxuToVtJa8WfZgug6xznmsfCDLOORsc
clGnwiYpMVXhmHZgSmu2iK3qhH3Lo5+0n6TBgwtnSlmD24Fs+aHrI9EJpRXC
P+/a0YDf7LUjH/a1PpfoisfY13TzTofewr623kjy1TvjctgTcs7dqZoMBz9U
P7FtpUJT6iGl7o9jYFP2qHeXLB3WLtrx3dKrCVwEWKNDLkzA9KRv/plfg2Bk
cDOTs6cTJCc3WX0tnYCG2VqpmcIh+FqdHR73vBtcb+Yr5K2ZgBn/khaDVYNQ
aeOjeSGhHQKPW9bv/jQB4kynSlfuHoY1x0XvferrgV6PrFDrpklYrL9lS6Dt
KIxsPcI58YgGHHvEosfbx+H88849Jp4D4CkBl7bwt8HqJyWXbm8YgKBrLco5
qJsdXbS/kGzbQVGjmg30R0He1sBngX4fOJ68VndGqxkE6I8P8Z0gwd4eHs3b
/mQo70xLaY6hQmLVInF5ZwK+CXA13lNC35FSYMaL+mqwUOkNz2YCtsVcNV2w
nQx52+7dzz2COnzbA0rOFgLcDJ4uK8S4hJXt56PYH1WTaVetv5TBzaTrHYvR
r9lcTi+swv1Se/SqhH056r0ApRwq1sVMx+KNt7EuDvwwvSbD6L/7fj5btZdx
bodZs4TxfGlzXogA8gbBuaQhAXnDYIG8Tjnyxnq1/tdlewlY3pCszYp6Pq77
nkIN9guzyhsueZQaWLHvjIF8VAs8nuTtrzXpgwWD4sEPawjwuLT17kFjMngm
DJ/J9aSCxSMiImd3Odjt50izLiFDgkqf+ad3yKsP4oZ/ksjAuuyTiBfymuvD
mZG37/rgZ7CiZBulFTI6jSsfbeuE5fpCqTVDNLB5mB0RfKsctE3qVyu24vit
YTc56FRQWDUUJadKwEMvU5VdsmRY+jpNyBProns9WTzyQiM8KbYLhNttwLNJ
/2QsSz80v5sN7orpg1DbNYc3xvbCU9Np6hvTLvjIr+ZW0UuCp3PFe/c/wvp6
YKt3rxD3OeirsmEECe5uTe9fFE6G1PrZtS/QJw58oOd77u+H+Bc8gp92UIGX
j6Nmj3MHsEW+5FzSNwF20a96EpcNw5cYqcB43x5INKDLsJ6ogpx0B441AxQw
FC2I8X1LgxN5h/ZrfH8Ln9kjl3+6SgFRr5myODcanFOA7U9Zm2HxEqGniRfb
YYvFofQZfToUsl5Uj8luBEEXvgs1aW3w3MKk6sLCfrBPFx38pjUKYbWTL0V0
+yBKz+h9gn4zMEecPq8rij7XNZ6J2Y0MJwSdzO1D0Q8Oj/v4ChJgF/w90VUM
9epBGVl7fSq4rg0WrrxBggli6SbTUDIExn0Xc0+jAlG7M+1+MAFFrdvEvQ6R
QXSZ96u801Q4NDgeqXaVBGt7G4iQEORVU8/zU6lU0Hl3fb1mehWsXyTCaz1H
gX3jIeka7TQQi3eSMd5EwNAdf/46ceTzXxXWNzHPD62W/MK+CvPWPCOVH/vO
jdprj89gv/Aw3bVCC1An+xkMqMuQodS0U7gDfaXfbg2rH+hTfIs8XpqiT9nr
+MQzH+vIaVVBjtFi1EsvdNtZUC9pBJUu+6lFhTbDPmtWJQLMfbNJBI6jGX4y
R9kI9eSh2rB9YwQkBr0N0j+F693Vq3Hcnwor4yq806sJ4NhxVPOXEdZRCWXH
Jcxn9fxtPpEXCFhbTrVNQf3WfT+QUmtDhc/XmdxmkOdrqHvi3yDPv5DVWX4B
60uO2JzzZisJHMXeL7JH/FnuvDN6g/ivfHzMOBb1Q+6mG9I9qB+mnh2tOYX6
4foiDoeP29GX3ZFY676DDNl73PbWoK6TYiv62MNCwHWtA4HHNpHB0qfpnD2u
Kzi3h+0L9uvpe+59w4hbiIXW8fXYrynL7zAf24PrHb2fm4h6L6/ZKs4N65fX
zefD3XcENJloPzc1IcPHw0nXk70wzsLJ0TNKgq3tEurHHmO9i5+0kCqmgn5t
xNU7e0ggLv9Lsf4iGYryc8pcI6jwkT7KzLu9Aga+e6aHLKLAL7Noa5slNBCW
rRyOjayGKtsKlqWKLbDmq1ZzPHcfcGyq+fD521vQq9iW0oF5Tt75jOUG5rmF
fFfCqPVbKHY3knMzpkC4527uVH0aqOjnQaMR+mj+IIOlV8igPSBlJB9LhWPp
Ge9pjOdvp2vXVjKej72VOVnIeM9bnbAzxYMEc1zTW44Ek8H6l9DA7mQqXJnb
WTiF+sFgn58wC/Kec0J32jPcF4VKeKKI61pSmDn9CtdlFKe/0QLXVZ4fHiIh
0wR3S2ZoD3e0w4Ybe25QQumQ3j2bWxHWC7yccyomK3vg6a/EzeoevWDTzTwh
0EUHS4KddTEn7tPGc8Ls9A54ECin7rl5EDJrjDys26nQ+JGXi2lXGwTY7R98
PFcLlW4rrvUNtUBpQahGwoM+2Lpnxq2gtRM6DQ7J7AzvAh93WVVl5J8X925P
6KGv7BM8slICfeU5PRGSI/rKq7u3DRJWBKjvOMgjjL7G5LzqrCj6mkaa1PjW
l3Wg4+X3VWhdK3Af2Lu+sbYP9p9eIZgzSMCZLJadXCfJcC8w4576ZSqw9QwR
RRYELB4b3RyuQAbqxFB6liUVzqwT8vEtq4ZV3g/bLmu0gM11/5Ub1/VBknl3
7PPkdqD1aPnG13bCG/l14QaWNDBL15o7MNcCLz936bR0dcDdAImN7Fv74EXA
8tJ4OgHHdhtJ77IiA/lTFkuoH/oFI/YUqx/I52QNMZ0ZMsQE6Lfv+0mFzZFU
2/QSEtiERtw/moDrui1/k/cFFTSWNNzg4RwHrkKP1YXudJhLoNzuWd8MLpW3
PiqvGIfp1U0x2Vfp8IXT+SKLYjPYyg8feB0+Ca0FuwctKkegtVnQMaLv/3xK
Ps1S/87oKKwLcjd6fqIPxBRP6gy/nQTlN0NXGjVGQa99QPnINdp/rs+edHZ9
VzAGTtRzX1W66ZB6bDG7ohfqk4ss/19fXx5NdRf1bypJVFIKSUimpAkVdlQi
GRORWYRICBmSkDGZKzSQlAZDUmmg3GvOzL3m4d5r5qKSpOjd32c9633W+q31
/v7d66x99t3z53vP2cdsG/co1Pc7HhH80wPqhx+MHlechvkvSyEVfWMQYBCh
NFo2AAm2ua8TLk8DSeBhqE/YOHjn1Z2wjKPBlcSJh8Ebm6DOWOU0qQ7t5bVa
Kd92ELbQ38j+Mm6BwuQm2EfrhPDsUwKeLYPQdntIeFtJOaif/nzd/D5xPruF
jJUeDPPFYm7MkcErylpC4RwFSJIP75HCaNCk4vjZiaUSPivI7pGcR7t7Nwt8
RT3v0ayKWTCthRT1WIkJuXZQbVtboMCO9rr2TuZBRBU82xdAaj5LhW7d36xR
5nSY3Tq1LsKNDOdmmhxFsW+Z66CsyEPcQTN8wZjsrYC3i+s4EqcocDI35VDf
HA0+sfB8PeFYDY4/vAtfPKTC3pZVqc9v0kG69LLab4dySIwt3S8RToEwFc/A
zvv/2WVVqVa2eP0kmJwdpTxkGfpf+t+TPw3CvzAhqKSxPb13+H/pIjz+b2sK
mZDawaF8IP8/etFsgvCs+BS0PEt47Rw6AgWkGhu3+TGwTV9F9e2gw2jTs2oO
xK9/qVesXOPGYS+/Rk4kDwMUWHpsr+yi/C8fSfd04FqYBKeLh02bjYdAZI33
gcTKaRA7kdLoe2wC5l4qvZa9RgeB006rS9SmYG3wDwbdawRkxN4nLoR1g5zx
lKOA9hQc8SkvLY8dgdWq1i9lKrrhXXChl/CLSqA292zu2E6F0EoWfYntdHhk
c6X1xrYKsHYQZ5h/oAAX6wOPV19osNOjSVFtewtAWclvseZO2HV37WZG/yBw
WF8P8/RqAInvC2pLBh1gl9/GbfWLAb9mHkl4jtSBD4+ZxBK9HYS9Zzc9ucuA
lrFHt80GquFWy/HypHIqCPTqNak9p8MyY8tlByuZwPjRWXtPcxjeUryN21I7
gCJqdmXH9SmY8G6c+y0+CrO7SA9f7e+FkMNR3y65TsHduqp0+eERkFAJf3Eh
owcCH87tUYplAs8BM+AvHoKGU3IaJsrt0PilK2X30hRYZK4rijQaA6e13xXZ
+QfAfoLFKWOSCboznurpj4dhZ3BLGD2iE4zV78SduUUG1t3Bfb4nKGB9VlZN
9QIN7mxjpHWdG4M3XsyunGw6UFi3rvp7gwrD+1+7fLk4CN7l/VvmhgdAqoKS
es6+B3r5UiZsL5BhYMNXBUv0W/J4wFEa+q21yuKxM+iHXWp3a39ep8CMIWVF
Efqh6HD10c9pZDgQ+Fe7QJcCHWTvhCPuNLgYupH11S4yaFxpmL6GfTJvWZ/o
C+yTEzyE3YciOiBq7d67Y7zYr7ZM+7OwoJ6lvGxjQmthQZhj+7L97WCss6LP
Hv1LWXWmoGh1E/SrJx/ur+oA+vvbH+ItB0G3K+p93edWWG1qxMJn2wU78+PT
r50bhLDqG/sHolvhkhJ7soheF6S0m0mfwfz37tjjxgu0SjhgwEd/ooi//1zv
/ow9iF+2KHtviCTDylXaIte0KMCpNfV9yIUGdZ55JRvVKmF9Cn+vFhcVyIxQ
VzseOnRZyd1TV66C6Li3SxV6VPi+vlFk9hgdDvD6yVq5VYG/3lhTuCUV9PkY
F+2N6WCdH/UkbakCOJhMajbmbVqu8UgY5pOhLCfVgUPl8FWB9R4lgAIuv/RC
pW/RQHp5UB6Nrxy8zdeP8Lpj36V1ruJsDA1m4jueKL0hw6V73l6GpyhwNo61
odAb8chjvft8rGTYvl6dh0WcAiHCAhnaOjSoJPke/LSXDF28Bc/MMT5LNNnr
7UxpsENBSPEa4pStw78EZ7HPEUuUHf6AfY6a0IubFAfEBa+XGSxifbw39Hh8
AOtj0/jL70aHyyFycder0EDinKSq2keUk1bkbU57WA5PX5c08t7G/GYrFSuU
S4Mvz+wML2HfO/9LKPwi9r3R6Y0f87HvPdwvmuV7F/3KsuiUjB4FVv+86P7c
nZi3UP0wuaIcjia12RdmUODiegGrg69pULAlTSZAmwzGrVUCm5UQr5msilS1
QJxibRJzAnGf1JCbxBpiLkFry3A59sMC4U7NbxDvV057CtUi3g9Tymjacub/
/m7gPbvGSVyIDJveZaapylDghuXrEXvsA+mCN4477SPD08AnGVGot3SZJtto
1JuZBYvx/PFyCN1/1iUriALOHZFh4XdokEMap+2RrgUbz8mOyS3t4PAtdqPq
PB3+fphpE0a7zN+IZrChXS4KAMcJtAvz/McnObjve5+N9+1w39gPsoKhuO9M
gftZLbE2EJwstTAO7YKhx1EOXccH4dxsvPhG22pY/87W70AmFc7yh8Z6xdLB
Pdmns1WSCRMbNvUKFQ/CwK7+3oiSNijum0+oe8eErCarZ1Hbh4H3ToZY2YkO
yBTqqT5mWgXZUzrbz5pSgSew+o6SPh2EN5yotHBuAc/TjvesRzthdcbioFfd
IFxL0Er90NkAFNldtHNuHSD8QzLPYO1/3w/3lgRNyB+ahLKHsb9M5xlw/trp
ULIBGfynon+KYT+vxfRbTsx5yKjmWKu6kgzqbDXtMYgvGouWFi/o0iCEWl+Q
lUUGz+5gMykDCqSY8emZedBg/4vtMWqh5bBt0kbiKOIjlsEqx1eIj2xshnJD
WxF/yfnVXrJEXOxykHwzkAalZ76mmW4kA9vZfRU+2M/LJfIbfkQc1CJv+Upf
kQz6Ne6jNWhHZkd+3Ge04yDdn/NpSSV4aCznLZajgkykg8QhWTrsqxjsMTnR
DBuUGclVvp3ge2ljTNvXQZCSzdFOT68Gvvc6ZgEvqWAyZpskmU6HY901BQH3
K2HJRYZjtQQVtvm2BX0Xp0NZWnQYMRear3PjcuJ7lPxX0xf/v/8FGiaOK3Gx
k8FPKuJpDPqJ8gquE5/QT5Q9ytwb+clwPrz8LjGP6PpzywxiHlFj+se8U4h3
Us41J+gg3pESDYl8jHhnV2D+6DnWcngpIK6w14UCQu4CproRiFs3Mk2+bCBD
/mODwhWoH3WfE9HWqB9XufcvtsiTIVurKDcU+YTYKrZSkE+Yb0X4sAoZZCOD
Lt/fQ4GlF2JBOogfVSK32UQpdQF/5e1L6Y97oHV95uGtWMdl4nMW6lix3wpR
Einb3wPaFnVPZHvoMCKv8ufZZCfYLjY+1bnRA2oOsxSx23SwOP7ysP8CCXQk
I9bIbsU8k7fKpFSbBpSdTueqV5EhIGRzxSFJxMU7GYGceog3DVfvswgZglHt
GG1B1FfVePb78q3dYHdin4WR2jjcUK8SXPGVDltnac/aEX8/+LdvOfNv3/Ln
375lc/bie2KeMzX52S5inrNaz0trYp5z9bFrR+99J0HStka2w1swTqM4F8S0
aHAzpJ4e9osEZkZnR4ZFMZ88n6CHoZz6WoFxcabl8C57OUksBHFrYqu0Xzrm
E8k3pgkC0/CHvfjOj4Qx4DiiN5djNwDaQSZWy1ym4Ga/1lOBAexzNDYGGif0
wPXMyBCZril4pFQZeml+FDKE9aUztfphr9WGL2dXToNZnNT+5R5jkFkiu7pl
/wDEz/0MsS2ZBrUbPRxBChPwkb5HO+/sf/32xaD1Dx/rTQKf7qj75XWDELLr
ZRu/yhew38JZdcy1HSzrUyzvKP83p8g37NAGtlNTYDNfPGbfPwJu/r+UOoun
YfSUjbb0tgkQSSm2yTShg1PH6TSS3SBQ0pz8OOkD4PPBrWTEqwcy9+h6hmeP
QzDPJRHd9QwYefyd75oABeYLdI/MnpyABf9H29OMGND6zfyj6/E2UJRffvOI
wQSsob2+qmfIgGhSqiZJrw2+Oy7+No4qBxV5ysq5OMT74sc0RZ/QgFQS/yfm
Jwm+0N2fmKP+n3Jz5PKi/rlNdR2pmFfvpPu5K2K8iKdE+vhivDxZ9bD0Hh/W
8YaDt4jvA/UrwkQcMF6suJj5q3ZWQpKzRqfgMipUve2N7UHcLUpzytWkT8ET
6tzU+1VjYMNj6jjm3w+tdszojGoylLUL3LpgToFmo79KbP40UBSTnJhF3GT1
Sz/qQvggjGRsGTLWaIPQfOXRT/RJuG++xDrOMwjlnRUPPAVaoWVxIZpNDXHi
WmVLJsZRbOuGxViMo70WES48UmQ4JPt4phXrl4Wp7ZSzMQ2WGq7dIs5Fn+nc
bUTkDYfAwEkiPzTZnbJxQP98tGZl4B70z9vlvYz16J/vxLt8/edIUC2tmsuL
+mFnO5bVgHTBWOnKSBYyBDMWSZ1imD+n5c2lUT8dq5qSkmdJQL7M+joC+Qzq
5A1Y4PqVMMNKw3hk8r9OO4bxuOHS5rNU1PMRjs2vs3nJ8EbnlXmFJHHu6IuK
L8aj+mCdHyfiptSiqh3ciJtWTnL4ZCFucqheLVCI/LPu54/EIf91UQk19sjf
aZwrJPcPCX4NOF6vQv5NIcN7NY5jfN3fmdiC+S1P9VlAIOa3yzEqFmNor6Oc
SUnXF0lQ3nU0mh3lF809/uUSrveusj/ZOEmGQAdGcsBZ5C/QR/cPwX5yreXH
INSPTqRtxAHcV9emtVUI9z0skZLgiPGbanw1oA/1s1KlseYKcT4nqCXpIdaj
2QDTlTlEPZrbSwrHehTw71zfqH/n+lr9O9fXXTmdSkljArNgliLaNQRDliSH
qhvtsD1OK4p37RQUcz1OzmMdgWjfM+GfnbvA/IHccp4KJijvHqh7dngYqnL4
v4cmdkCk3yj/B88xyDfudQ14SgfnEnHtfcFUkChOHfHAvLQlLbiLOIffLs3J
IM7h033L8p79IAHPgs2PNvxdWfEmi/e0iHt8e/k5fpMgLfn5aUPUp4W9bFof
/q4mZbvJN8gn0unqcnZcv924nJtJ5LeQmJfhyMfZaPtcLdLd6JrxKcgn5f95
N2Htv+8m/JDhaxI5ToaRb103jmPfxUy42GFv8X+/Z+GmVqdBRT0vPOQNWUQ9
zxUdmrmN8nzu+ZYzqE+GHw371gwpY19tfrGzBPuBhf6XpayCuO8GPwkm1iOd
g+c2iGL/87OEXWhLItZ3zReZx3UokKv9yhTcaPBAI/W1JtZxq0Je5yqs4/Z8
1ZdKsY7XvY+4qChJhp6038KGGEc+zwqe/j5JnGu6rGmCepj2Kd0xivpM9uQ4
T0U9REn5nXmI/llzaiwyBvUw/D1V0Ab1oFMcNl2K/i8j2CSuhvr05J1zbkD5
PbS9m1/Nk+AOl3nbC/xdlnJbZ04ifdmmqu13ObHPMTSeJO7LO7/j8Ww+QYPT
VY6e8ogTD5rKVkcgTlxxy/DNXsSJG9ua0u3R/z2WmS4VIv9q7YcjCujPrzqo
rU/XkeHY+DCPDPp//l8fehb6f+tFxv2T7OWwYV638iPWcdHTST6fsY6X2LXt
TkC7O7J1xFxGPn9E7B/9RXlcV5bV+a4lg6vulelYzHvn1145IYp83ir48Dhg
nz2inNxZN9kFZpa5so++MWDlhheKQgLlsLdomfHBi5hnBh//sb2B/UlQblc/
5pOEx2NUGfy9dvdZbIZRP+MDsmdusJGheiFtwQnzbcB+6zUPMZ8s3lQ7ZKQw
Dp8yL4UKT9LhPGtIom4m+qnq53/m0j/unvpnLn17sec/c+kLyy8zifsg9nFq
4sT7Prsiyn8T90EKMxvO+aO9GOtYlb4hPeVs1EAfce7di5WvD9fr6lFriLkf
c1bmmsTcD0chmx2taEdO7tiNxDsRhvGFkedRzkDFIPEorAvGYRa/RFHOR5Qq
aweUc2opgsqQawabYdVaPcdOmJH1vS/IGASe0WMrfHxrIPpCvUfsFBU6z8XS
HzTQgURpEbrmXA0jyy5dj8+iAl2J5cH5ODrkjDsYEfn5dcwxIPLzxNHonn/O
25i3zf1AervHrGs9cV/ggm/fJ6Qnn/Xou4V5yfxB6EF1lNOLdsx/KzEHxv3A
MWLOvHpySAIxZz6L86MuMZ/c05K14QeuL/oWnHYG13tGOxYp4XoXLguDdrT7
iuWty26g3ec0unnXoP+Yb+ju4bCugo8Gc17RZlQ405t/ntOQDoxtjltj0Y48
lFM969COroFtrK3I5892B6VZtOMn4f1t11E/S9fO2r5H/Riu8Txr/54MWpbn
uP1NsN4ludS+8KHBpTda4jb+ZLgQE85ZeATrSH2aQME5Glz9YKOjw0UG+4tC
+VqYB1jZl9srYh44XOOfMI59eEzN2O0HGNdGqyLf9WGfmbGK3aBIDvv/nnul
IthnNkbFfg7GPjNhm9RiDzcZMq9esfmLfFrTpqyakM/FaN2rnVsQhwptyLwh
S4G++iStICPct0BCYzGeDML9hiILxymwdYdmwG9XGuidiKkWySUD72tydK0R
Bep6I2MTvGiQyswcmEd55nMuJWShPGsfya2jozzLdqRwaGL+kRDxEGJB/GVQ
58mngPlHYJX4qqy5chDqcaMa5WF+c3DNfPsZ8wl/hchGpwbgJrWOUXU74JtQ
WPLrOQZ4xBdXm5bUwl7qq8PrtNth9/FqwUlBBoQbLXIW1FaASdrXv/vHMG9I
1uyz/EYD3+XGnh1oX6O6Js+TRD7fVNWmgHa5sMfXubeUDNvyttoVm1JAw6VN
8J0v4s0XPi/PJJAhtCzfjB/z4REvq9trMR96kL126pQgfp4MUqxc1w2aD+N9
uVsYUHtAV+ZkLeb73s+ruL/2A4/7ouTGa/3wnv8M597PXcBefXt9FrUHvAtf
Zf84TQeOce9t55DvCqfAvE8TXSDmvEbn+ncGNLA/K1jX3QDi+aPeVYj7nL9V
q2Yi7vv6p9fRWpQMCWwKrS/RLqQdZg/voV1q0370rEd/2yUq+OHbFqJeW6x/
S8zTi/lgIofx+IuusZsT/W1rUl62PvrbJNfbR/oKDXD/ofrm02odMH8wXezx
GAP2Te0mGR+vh/G0qgXvpXYor0qbrn/FgLPNV/VTyxpBLJNTrCm3A3bEbJ5M
PTYInLGb42qf1ID6qUn1hd9UiG65kzHTSQdqc/iEU04lBOQ3jTVJUqG/AHw1
JenAd+0Ob7c4E0KHmbTcgkFYzhSYFXzehvVcf8P0pSlI5lX6Yf17BMz9jKIX
23qAnkMXOJI6DdzHt/vtpY7D1my1K+9m//vuyjd7Xqj6ySQ8dHbZc/v9f7g4
IjQnoCVzEu7NdT9xyR+Et6GwTil7GkTd4pjrZsdBeOjIqfJNdIgv+CHXcWsA
BiNPKU9u7IdDI29ddb0HgO2biooB4vy4EK6iUftuaGHPHVwWxwDevLvbFXWb
oYp3NvPw5U64PULepPptEPbxpyosfRqHVSEDVmkiuO6GR5HdfBu4mWxtN8W8
atDGdY+og7ypLdVEHZT+U0MRQnuNDFXazxLvtdnpBhSjva5bSZp9+EsCk+qp
wgzsu5TlctV/YZ5pUHve3I51P2u5c9sbjNMPNkb7kzBOZVt6KO5CZPiimkQx
wjjK7b9V541xpLnM8NObdZVwMnPhV8giBUo8arb5sNPB+MWtXVOqU5Bj5Pyu
12MEOCuBZf5qN1xkBJ+oj54C957CQO7doxA6OuGVaNELJb0L62dWTYFCg/0l
u5/DYDf/YkrRoAvuJvH3PXhSAb9Wz98P7CfuZw1kbx/HOpJi0C9+sRyetes/
mIvEPkdnueWBTBpE6rncs6chbn6+NzGDZQA6VOZ2LDj0w5DxdoP6v0MQ7iTA
rYD5QrSjqlTrZheIMjzyHkUPwf7yy8Hs+PtX3a3L5tzYDc4XPLxz6xCP/zx9
/+YZCmgeFV2cRVzAeBRUmYN6pne7bCTmRcd+r/4whnqemDfY04l6NnlftiiF
edhr/Z7mQdRzuINOZN0SCRzW+llYoZ6tbjJMy3CfY1aHJ93ZySB9beuuHIyX
r71M03aMlwvcM4bbVmC/AYeX/8R+o7pzf9xX7DfitQsvmPyshTCSxamdZu0g
18clFCzFADmzxCENxS+QqSw9+tSlHWbHxCMPKjGgZkWM8gsSFc5LDf3KdO8G
B66+j0rXGbBgmSL1alMLiH28FmFT1wkPadt39mF9LOoUi5IYbAXpIXrJFbcu
SLRUVhCxHISgvoyVQXkVoKyey+JAo0DnpQIt8gTWo6uGnLPjZFAhL8T+tKfA
jqNK6dzYn3+4t6P+LVcFiIy+ffPgFQW4VMK1dlbQYGd0uUqncxUsK067ecOC
CjevSITEn6SDqTb35bz5cjhYFHpwEPMwSAbs2F1GA9upNym/eMhQuqovOgXx
yGNVyxk9xCNLnxKvl3mQQVc4nV9XA/3wfKnPcwcaeIXtSSEFksHQ/dudM0cp
sJgbww5O2Fcs7DQ1+EkGiC+OcET88qlmufcM4hfTB9/1dNeUA9syRV/TCxS4
FTEjVRuNdc3IZGRvMOIgRXK+gSb2LQ8qb/E4I97cUhRgWFYFT6Q2ZHRcosKe
Wmlxbic6tL3bYrD0ugmM1n/hUZfoBCG3GNELzwehjvZm/XhSA1y+XcbObt4B
20wl2OZZBuFZlcHdXNYvMHBg21Nei3ZQ7n1Cm5BhwKkdp1wv1NcAWzyTfmF5
O4S2PXr0mU4Hn51nNFdgP8Cdyx1qhP0A6UhOSj/2gV+6ryttXUaGy5+3xYqi
nygX8daqoJ8cAD9mkxgZ6+iyFGs57N+Oy29QwD758MV7zf4qZOD+pBUbjPg0
eD5Uczfi08mdYT337lfDPe1sB8NCKuxMHfjTlU6Hub0Lg/Xz9aDaqTQ3Kd8B
G2v64UkP+o9D1g0P7J+7HGsfEe99JPK/0iHe+9hplWVssZwM9bIq6y+jPNEV
/fHhKE+cl6G4NuIReb2+nneYf5LqOiUjiHe+Sqd2pWG/LcLQeJON8bJfxGpG
F/nwiu1L7cC+JVpJSyoI42Lfdr7xVxgXfm+3Nuo1TkLY1JHD+r0MEOFuvjd6
vwVcFedfuuYwYVjWcmHTnyFwNXTWX0Frh8k249EqyynEW0fYgspGoKisa7nH
0R7Yud0v+fWvaai7/kDBoGoCLv/qHSrcwoAyS8HYIxjXI/st1RnEPegrhUda
MK452JTCt2IdP5/BmiWK8h8O5WFdTuDxNKGMixjvq830v3Kj/KWnTE7VEf1z
4pJCEP7ezQ43U6txfS13QWIS0sd2PVenIG66N5x5gMBNDto8RQRu6tu6NuY1
4gXbi3Wfa1CfX+9c23AE88MVpkFmEuqZZ0ZfeDfSC+ttNctx/d22F95W3Uyw
eJth7xMwDFZmf4Ah3AmbhmdtLJC/x0ra427kv6S7+lwArvcX9j5WwUKGpwOJ
CeOYfwR2Z4cpoT4/Hgubk0L5hxf0ueZRzu/PxG9+QDmpC/tP/LUehS3Jp3ZO
adHBTvhWgRK5HRrfNZwOqxqG+zl5bDbvaCDTavzeL6sTWDR0vKjY17kciohb
xHrhKMzV3oD14u3Iwh0hATLst4ldFCHuCe7MN/bEfqz7NNM9HOvOha9WLIko
T2iLdd8k/t5kylcPJ37Eg85TLNaIjzauOB/ahLhmg8BshBbqh+dOy+XnqIfP
zwvjZXD9ySK7ny/Q3+QGjANuShDz+Yc8UtHfiqxtWrQrqsHzQJvd6hIq8MZP
UUYf0SHpw45Db941wPW3T6khZzvAXvCjdzPXIMCwu+EzxBeS+9g/xaIeLO1L
f9qiHhgO2vMuv4l7WxZMN9yXPibwcg71+WNAjiePuOfuL+1F4IJK7qMriPOH
mvkZce+QrvT7fvZHpN8S3vz+JXE+eRff52voV30fTqjPIj2ho+zOANJ5d3z7
av+TBIHfV0bqo70W36zVXYb8Ww3STXRXlIOAVHWKCDH/x4/8YjySmJtaFdfY
j/F1nb6n34YCGe/0yt2v0iB4ILDwDN8kPCvjdw9LYcD3B14y8S6tkFVOFSxY
zQT3uKhgjaBBkCQ7Pl4j3wZnP3ziUTdhgsF8Y/8L+SG4b6J2KolBgTu3qu0K
sO4nh2gOPEc7qu+hLkYR56gnPbLPo/zCN7w3T6H85+Imznej/K4b9rovfGTC
YLfnycCdw8AW0yPga9MB/p9Orn3waAoi2Uijtz1H4ZMK57Inf3qBnV1L62jf
FFg9l/B8yzYGOXynv6+z64dkAee3OrFMqLH6GXGseAiqZbd5UJTb4WS28CKr
YAd01g9K6Yx2Q3Ju5zZdUQY8Kv2bHOzZBMZuwQakpQ6gN7WSWmMGYaxyE9ck
6n9Qa+JZLco5zm8bU4JyLt86Jvzn6xQ4+ry9zrJ3DKJTO07o9fSDTdSBJ4pm
06D5O6SfT3EcNNY0ukrL04BkvDZYCeM942hpiST6g3KgkiQ3+kNBjQZJvrUX
Zg8+26A40QvvfdpvPfKgQQR3a5Eqrr8fvsaXeIfCm10hjXiHIon78jT/JiZU
fRZOLk0ZBAvatVQ9pzbY/cRUVQntPn/5j/BhtHu7eDJlAdcLryRJJ6CfDxaU
Z39Cf0uzZUs8iH5u2By/MIT6r/6Ss3ENgSOcBdfME+d5/HLk/5lj8IrDhsDd
5iMWov+cj32VwSDej5N/os1HvN/qqSftSNy/q23hYOcg3nv1GTrZjHQ5nwtS
xHuvt+haSecoTNDverwj1nUYsrRlEkZ/dsDWpVN7iPlRbLVuOcQ8h81eh5IJ
/C69jjOFG/nouB680Ip05ozsVDnSR+o2+qwn3svYslt7M8r5J3ilPDFn+2rq
luXNyOdugoAMcc9RIMhBmbjn2NVg3k3MMTM0W4wj5phdW2blRswxi6e9FjbB
eCxxXXHGB/ncu/zhkB7y8Z+YI/cgn9Yjq9yI7wM+wtvmiXche8/LkAKJd1V8
TvYR3xMOPY1360f6rnSf+ibk8+ljvvsd5HPI3WrBBfnsuPquh5Dn+sYiB0Ke
huksF0Ke0uxZX+I+yGmXKx+I+yC8xwwliPsg3U1k625ivivfkoYx8vn8VjVs
F/H+Zvxj+xW4HqjyMcT7uV+V6uUJPfz59KZbBek/K/VFaEh/J3/Xl+CTKl2z
Tw357NrMSCP8xCrEtpjwk2M3jKtPopy2MtavvZEuFy74Whfpj8S52p4r0UF9
4s2e7yIDwK5163EkSz/c/P03KA7X8x18rhOO6/dtES04Q/hn/ztOL9z3aWSU
zAzuO3kuULKX8P84v4kuXN94SgzScb17Hum0G66/XBI654L5v1U622ol+mFk
1TfWL0g3pc3v6kD9HGxaJUW8vynYrTRM3DPl/D69OQz5c928M0bkMRnzdEka
8R3jjd0dReI780Bbqhvxjueh4n5N5NMcrFhUXM4E+tD833aNYThSLCLjF98B
7Nf//mzUZ4L1RXfpRJEh4H9wizXrAwXmNajZAYtMOKKzwuJT0zCwuypwOMx0
wl8bNi/i/zULkQOsRL69K6qa88959QGbZcQ58Pf+QsUE3d3PVoPIwyKtq394
+vRBV5N85y2JPijLbPV4ok4Djdz8P/psw/Dwp+LI2ks02Fe8U+JUTBdwukS2
073H4MgPf93bz+jw4M/eW1cDqfA/BqNd4Q==
      "]], {
    DisplayFunction -> Identity, Ticks -> {Automatic, Automatic}, 
     AxesOrigin -> {8.719308035714285*^-6, 0.}, 
     FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
     GridLines -> {None, None}, AxesLabel -> {None, None}, 
     FrameLabel -> {{None, None}, {None, None}}, DisplayFunction -> Identity, 
     AspectRatio -> 1, AxesLabel -> {None, None}, DisplayFunction :> Identity,
      Frame -> True, FrameLabel -> {{None, None}, {None, None}}, 
     FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
     GridLinesStyle -> Directive[
       GrayLevel[0.5, 0.4]], 
     Method -> {
      "DefaultBoundaryStyle" -> Automatic, "DefaultPlotStyle" -> Automatic, 
       "GridLinesInFront" -> True}, PlotRange -> {All, All}, 
     PlotRangeClipping -> True, PlotRangePadding -> {{
        Scaled[0.02], 
        Scaled[0.02]}, {
        Scaled[0.02], 
        Scaled[0.02]}}, Ticks -> {Automatic, Automatic}}],FormBox[
    FormBox[
     TemplateBox[{
       FormBox[
        StyleBox[
         StyleBox[
          PaneBox[
           GraphicsBox[{{}, {}, 
             RasterBox[CompressedData["
1:eJxFlwmcjvUWxyV3YmZsZbtJ9i1q7q2spR9ZrrJUuLYUQiEx1iRRdmNwiySk
kEtlS8qSUYYwxjIz774/27svM7JV0r3P/zznmff9fHx4/6/3Of/fOd/zO+dt
/tqMIZPurVKlSpH+5x79j/j3mmnhO19dK0TT5fGXjj1sQ395Qb0D/5LxUFzp
/ODPZ/Br2aDX6oy0YeIr3n73zpexf4p/Zt66s/il2aFZkz+04X1Xj3dG7pXx
TMS1748xv2Dz9PuX/nzRhm3//vyb/U4ZV1+3had2OIc3T87Z0KiaHUdLq/ir
1lAwLljSwvP7OTyT6diV28MOiwjTTcG1CZdeGXDhPOqO7Hbkwjw7yovOPLt/
ioIlyvnNP266AG331rPNDtmR1a/13KpbFDww/oylw6QiHL1+1zo/akfbwhV7
RlxU8GXgVK1tT1xEXi8RyYHedEEFnV898Vx21WKMWV94s90YB+htBxXnvN8v
W1hSjH/4WmV88LEDCzp/88yIl1WMfPnbnxLbL6Fqh5UNXFcc2HQ4e+a+NSpI
7luXYZsfbfPP6k4cfmz6rntOqlgwcm+ny09dwd5zA7qs7uXEla+v2oYnVJDM
zKt4V2R3gRMx/Vv7HtI4T1cxWMg/4kTGro+63zNIQ4Opg66vG1qC5iLNSSda
NL0xbfh7Gsa2L65z/kQJbgi5bVzosVU8QMPeSP/H/te8FOdF2HEujGooMq2h
Yo+4SCm2fCpeLszVqzC8VhDtFuUff7NGGR49/Fn5nnMu2LaEmuyJaBg/bEjb
L1aX4fTFHX1/uO7CkzuPdPmtUMOWRxp9bK1hwTBVFMCNDV8teem5zzSUiXLm
WRDRKSob5Mb1Qy++ueVtjfVaWa8bQ449rBOmoTcBZEVtCuzGYT27T3fUsHBp
zsD/ZtqwU4961+pG3fMnjq3N0HBk1M3j7jwb182D3CuryvySikTOj21rZ9lR
NO/U5w/meFCiZznnRxWt9er1XmPn+nqQQ/VU8arAN8vB+fBgvfZr07IZKtfT
AR1aXbAHqfjP3Vo+r+KqCJPlREPXldtDVQ8G69WY00pF9bGbB9bPd+Lra6Ix
vNivU/DLXwp6dhIXdDHPXmQTMArmZ7XSU+1CqU77oqleTKvx+xdvfKfgkBz9
+HC2G5OeCryw9hMvLtURghXEjh68N5zvxm9DRaK96CDSPllBi3VzZzau6UE+
9asXeU0npEY/q2D0RPEAD/evj7lSsEGnaFlNL6i8A3x4Tqfj7i0ZxRTHi34i
zfN9+EqndXCpjGrh/HbJmj6Q3N0+VCewZDxdMGRT83U+vCXClvkwuc+UGRXL
ZebIjyr08jN3Mg6QP/ixUXz8qB9thv5NJ1pGiDoygLbieqP9WDHaclKtJ+Nh
Aj0AasOVfgTH77A/WS6h461PfBvuk7gv/OgrwhZJ7E8SFLIpP77UKXPskjBR
2MZKie8VQDVKvIT3j/bUrUOCiKqnChMWC8OSsG1ny+N/3idj21lhZAGQbTwu
4fjajIoG/WX2gQDnXYJd9PkqGWd0qtYXBtgPAviV/EqGLlZPdQCndXreXRpA
rRcObpxUXeH76Pfo/9N7TboG8Ej3j4oX91cgqPW1ktBzhDAYP8jOVil8Twl/
Tbqtjd/hB8m/oHD/SCgQNjrcj0V3uudeqa7y/SXuHz+2ijbur4LsVddP4X7y
Gb67WuV+kPCHTmFstg9WvQpNilS8I2xsn4RjAr92PqNPamisV8LbdGGda2p8
jfmW0Fng86HX8JPVGudBwg2P44PZ/bwgXIo0fCHkZsg4Epv9ff07HsNnagSZ
Vxmzfq8dPXrQg/cgGimIJ+gl43FKiMfwn9VB/lyGXqQX/2zkwQ/17xSnioKV
/B1srS3dftlt+FJmCJS+eTKmPylIcHN/h7hvZHTsLSad2/CrvBDzJEN00/K4
i+dJCKmFIrCMr8eJi7vQR/hYZhhdV4sBI4NkDnNhnLCH58MQLjQtLLPPuoz6
5IVRLGz1rgxqt1NOnpdh1Bc2WU8BjbdZTsP3MiM8nxRMpMBOlPZxVtx4PsLz
RYFO8Uo910gKP1wT4XmtgMblfxyo3nj7q+2LIyCbma5gp95dXfs6ICjonRUF
2cZyBeMJGDt60QCO4qqwga0Kmomxc8AOGv9roqC2PqwgoLt+0wl2vHO+vY5Y
tJLT7XV3F5xuaDd8NSsGo+0UjGkmnmxjP4rhlsDqpoLGBKQNZOtrYuyjKtxi
rehkQ1zgUhwDpbmFik9FemNW3Cd8ODvOe4M5p60Q3ZczMI4mVGCV56jVeG5+
HK/TS+W5beV7xUFjdqGKjSuX685kMXw7O4E/Fk3VNxcVQ3UXnDvTgo23MvTI
CfShxUnF/UJuGwu+FX6en4CeZL2gKkq+EwUsw+VL4pUAjUOHChpL68v4fQIz
BW4VKvtJGYz1K8HzUwO1ey0LEsRjwuCzpQay40EWzmOC547GXFvYl5NGnBEa
38PCz00acXM1UDtlW5m/JPeBxnWy8lxJcl9oPF+taE8DNmnELdCYYyv3Lcd1
mHuAje/HcSs07mNz3+S4mUGezzb2O47bMogJxJeN5zrH7RHkOtrxBtXV1Bvk
59pZZxJ0/dwgjtOcsfNeYuoNoiMJtnM/mHqDIJvOcHDdkqDHFARxPy0MDt6r
TL1BLKPvO/j7HLciCGqnsw5Qmg+YekOYSnuxEyT3mKk3BC81qJP9l+P2COEF
AszJ9TT1hkDXK3TyPcz6hozyVnUxV8lKnzP8w8VcmXpDILkfuJgrs74h5trF
XKU4Tojfs39mpri+IVA7wc1cpfjzMM8lN3OVYr1hkE2fcjNXKa5vGLSG3nUz
Vxx3RJg/9/DfHDc3zH3s4fpzXN13W4r166SHueK4u8LYJGxan0cGVxy3IMxz
2MtcpeAkvWHmxsvxUlzfMBI0H7z8uak3grEk2MtcmXojIJvu5mOuUlzfCPrw
XmZwZeqNMB8+vjfHzY3gEWEbt33Mlak3AmrfLn7+nqk3wvPQz1yZeiNYQvuv
n7ky6xsB/bebfn5v6o2AytspwFyZeqNwizTPCTBXpt4oSO6RAHNl1jcKCns9
wByVs94oSP7fJX5uOec7yn4vMVflrDdq7COjJOaqnPVGec+VmKty1hvFYtF4
myXmiuM6opwvie/HcSuixlx1mvsnx82MgWz7tsRccdyWMZANN5SZK47bI4bt
VBeZuSo3uBoRY90y6zT1xir3F4MrU28MZGObZObK1BvjOsrMVTn7VQzGz2+Z
uTL1xri+Mn+/nOsbw2PCBuorzJWpN851VzhPpt648bttmMJcmXrjzIPCXJn1
jYPkbFT4Hhw3N877vsJcmXrjWCHSbFGYK1NvnPlRmCuzvnFjbj+gMlcVHCfO
9U6fG/WLc77T58RHowTrT58b/CX4Hulz43dNgjnTKs+NuiW43ulzY79KVM5V
89zwqQT/vkmfG/t3gvkOVp4b+2QCr1DB0+fm3DbqnT6fTQVMsp+lz2kcjEsa
dakXqjw35kESJPel9DmN+x1J5ix9Xsjzisp9KX3egH5YJivngHluzL8U91X6
3Ph9lGK+0+fG3E1xnPT56+zTxvsI/g9eITvj
              "], {{
                Rational[-15, 2], 
                Rational[-225, 2]}, {
                Rational[15, 2], 
                Rational[225, 2]}}], {Antialiasing -> False, 
              AbsoluteThickness[0.1], 
              Directive[
               Opacity[0.3], 
               GrayLevel[0]], 
              LineBox[
               NCache[{{
                  Rational[15, 2], 
                  Rational[-225, 2]}, {
                  Rational[-15, 2], 
                  Rational[-225, 2]}, {
                  Rational[-15, 2], 
                  Rational[225, 2]}, {
                  Rational[15, 2], 
                  Rational[225, 2]}, {
                  Rational[15, 2], 
                  Rational[-225, 2]}}, {{7.5, -112.5}, {-7.5, -112.5}, {-7.5, 
                112.5}, {7.5, 112.5}, {7.5, -112.5}}]]}, {
              CapForm[None], {}}, {Antialiasing -> False, 
              StyleBox[
               LineBox[{{7.5, -112.5}, {7.5, 112.5}}], 
               Directive[
                AbsoluteThickness[0.2], 
                Opacity[0.3], 
                GrayLevel[0]], StripOnInput -> False], 
              StyleBox[
               StyleBox[{{
                  StyleBox[
                   LineBox[{{{7.5, -112.5}, 
                    Offset[{4., 0}, {7.5, -112.5}]}, {{
                    7.5, -73.70689655172414}, 
                    Offset[{4., 0}, {7.5, -73.70689655172414}]}, {{
                    7.5, -34.91379310344828}, 
                    Offset[{4., 0}, {7.5, -34.91379310344828}]}, {{7.5, 
                    3.8793103448275863`}, 
                    Offset[{4., 0}, {7.5, 3.8793103448275863`}]}, {{7.5, 
                    42.672413793103445`}, 
                    Offset[{4., 0}, {7.5, 42.672413793103445`}]}, {{7.5, 
                    81.4655172413793}, 
                    Offset[{4., 0}, {7.5, 81.4655172413793}]}}], 
                   Directive[
                    AbsoluteThickness[0.2], 
                    GrayLevel[0.4]], StripOnInput -> False], 
                  StyleBox[
                   LineBox[{{{7.5, -104.74137931034483`}, 
                    Offset[{2.5, 0.}, {7.5, -104.74137931034483`}]}, {{
                    7.5, -96.98275862068965}, 
                    Offset[{2.5, 0.}, {7.5, -96.98275862068965}]}, {{
                    7.5, -89.22413793103448}, 
                    Offset[{2.5, 0.}, {7.5, -89.22413793103448}]}, {{
                    7.5, -81.4655172413793}, 
                    Offset[{2.5, 0.}, {7.5, -81.4655172413793}]}, {{
                    7.5, -65.94827586206897}, 
                    Offset[{2.5, 0.}, {7.5, -65.94827586206897}]}, {{
                    7.5, -58.189655172413794`}, 
                    Offset[{2.5, 0.}, {7.5, -58.189655172413794`}]}, {{
                    7.5, -50.43103448275862}, 
                    Offset[{2.5, 0.}, {7.5, -50.43103448275862}]}, {{
                    7.5, -42.672413793103445`}, 
                    Offset[{2.5, 0.}, {7.5, -42.672413793103445`}]}, {{
                    7.5, -27.155172413793103`}, 
                    Offset[{2.5, 0.}, {7.5, -27.155172413793103`}]}, {{
                    7.5, -19.396551724137932`}, 
                    Offset[{2.5, 0.}, {7.5, -19.396551724137932`}]}, {{
                    7.5, -11.637931034482758`}, 
                    Offset[{2.5, 0.}, {7.5, -11.637931034482758`}]}, {{
                    7.5, -3.8793103448275863`}, 
                    Offset[{2.5, 0.}, {7.5, -3.8793103448275863`}]}, {{7.5, 
                    11.637931034482758`}, 
                    Offset[{2.5, 0.}, {7.5, 11.637931034482758`}]}, {{7.5, 
                    19.396551724137932`}, 
                    Offset[{2.5, 0.}, {7.5, 19.396551724137932`}]}, {{7.5, 
                    27.155172413793103`}, 
                    Offset[{2.5, 0.}, {7.5, 27.155172413793103`}]}, {{7.5, 
                    34.91379310344828}, 
                    Offset[{2.5, 0.}, {7.5, 34.91379310344828}]}, {{7.5, 
                    50.43103448275862}, 
                    Offset[{2.5, 0.}, {7.5, 50.43103448275862}]}, {{7.5, 
                    58.189655172413794`}, 
                    Offset[{2.5, 0.}, {7.5, 58.189655172413794`}]}, {{7.5, 
                    65.94827586206897}, 
                    Offset[{2.5, 0.}, {7.5, 65.94827586206897}]}, {{7.5, 
                    73.70689655172414}, 
                    Offset[{2.5, 0.}, {7.5, 73.70689655172414}]}, {{7.5, 
                    89.22413793103448}, 
                    Offset[{2.5, 0.}, {7.5, 89.22413793103448}]}, {{7.5, 
                    96.98275862068965}, 
                    Offset[{2.5, 0.}, {7.5, 96.98275862068965}]}, {{7.5, 
                    104.74137931034483`}, 
                    Offset[{2.5, 0.}, {7.5, 104.74137931034483`}]}, {{7.5, 
                    112.5}, 
                    Offset[{2.5, 0.}, {7.5, 112.5}]}}], 
                   Directive[
                    AbsoluteThickness[0.2], 
                    GrayLevel[0.4], 
                    Opacity[0.3]], StripOnInput -> False]}, 
                 StyleBox[
                  StyleBox[{{
                    StyleBox[{
                    InsetBox[
                    FormBox["0", TraditionalForm], 
                    Offset[{7., 0.}, {7.5, -112.5}], {-1, 0.}, Automatic, {1, 
                    0}], 
                    InsetBox[
                    FormBox[
                    TagBox[
                    InterpretationBox["\"2.5\"", 2.5, AutoDelete -> True], 
                    NumberForm[#, {
                    DirectedInfinity[1], 1}]& ], TraditionalForm], 
                    Offset[{7., 0.}, {7.5, -73.70689655172414}], {-1, 0.}, 
                    Automatic, {1, 0}], 
                    InsetBox[
                    FormBox[
                    TagBox[
                    InterpretationBox["\"5.0\"", 5., AutoDelete -> True], 
                    NumberForm[#, {
                    DirectedInfinity[1], 1}]& ], TraditionalForm], 
                    Offset[{7., 0.}, {7.5, -34.91379310344828}], {-1, 0.}, 
                    Automatic, {1, 0}], 
                    InsetBox[
                    FormBox[
                    TagBox[
                    InterpretationBox["\"7.5\"", 7.5, AutoDelete -> True], 
                    NumberForm[#, {
                    DirectedInfinity[1], 1}]& ], TraditionalForm], 
                    Offset[{7., 0.}, {7.5, 3.8793103448275863`}], {-1, 0.}, 
                    Automatic, {1, 0}], 
                    InsetBox[
                    FormBox[
                    TagBox[
                    InterpretationBox["\"10.0\"", 10., AutoDelete -> True], 
                    NumberForm[#, {
                    DirectedInfinity[1], 1}]& ], TraditionalForm], 
                    Offset[{7., 0.}, {7.5, 42.672413793103445`}], {-1, 0.}, 
                    Automatic, {1, 0}], 
                    InsetBox[
                    FormBox[
                    TagBox[
                    InterpretationBox["\"12.5\"", 12.5, AutoDelete -> True], 
                    NumberForm[#, {
                    DirectedInfinity[1], 1}]& ], TraditionalForm], 
                    Offset[{7., 0.}, {7.5, 81.4655172413793}], {-1, 0.}, 
                    Automatic, {1, 0}]}, 
                    Directive[
                    AbsoluteThickness[0.2], 
                    GrayLevel[0.4]], {
                    Directive[
                    Opacity[1]], 
                    Directive[
                    Opacity[1]]}, StripOnInput -> False], 
                    
                    StyleBox[{{}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, \
{}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}}, 
                    Directive[
                    AbsoluteThickness[0.2], 
                    GrayLevel[0.4], 
                    Opacity[0.3]], {
                    Directive[
                    Opacity[1]], 
                    Directive[
                    Opacity[1]]}, StripOnInput -> False]}, {}}, {
                    Directive[
                    Opacity[1]], 
                    Directive[
                    Opacity[1]]}, StripOnInput -> False], "GraphicsLabel", 
                  StripOnInput -> False]}, "GraphicsTicks", StripOnInput -> 
                False], 
               Directive[
                AbsoluteThickness[0.2], 
                Opacity[0.3], 
                GrayLevel[0]], StripOnInput -> False]}}, PlotRangePadding -> 
            Scaled[0.02], PlotRange -> All, Frame -> True, 
            FrameTicks -> {{False, False}, {True, False}}, FrameStyle -> 
            Opacity[0], FrameTicksStyle -> Opacity[0], 
            ImageSize -> {Automatic, 225}, BaseStyle -> {}], Alignment -> 
           Left, AppearanceElements -> None, ImageMargins -> {{5, 5}, {5, 5}},
            ImageSizeAction -> "ResizeToFit"], LineIndent -> 0, StripOnInput -> 
          False], {FontFamily -> "Arial"}, Background -> Automatic, 
         StripOnInput -> False], TraditionalForm]}, "BarLegend", 
      DisplayFunction -> (#& ), 
      InterpretationFunction :> (RowBox[{"BarLegend", "[", 
         RowBox[{
           RowBox[{"{", 
             RowBox[{
               RowBox[{
                 RowBox[{"Blend", "[", 
                   RowBox[{"\"M10DefaultDensityGradient\"", ",", "#1"}], 
                   "]"}], "&"}], ",", 
               RowBox[{"{", 
                 RowBox[{"0.`", ",", "14.499829429478758`"}], "}"}]}], "}"}], 
           ",", 
           RowBox[{"LabelStyle", "\[Rule]", 
             RowBox[{"{", "}"}]}], ",", 
           RowBox[{"LegendLayout", "\[Rule]", "\"Column\""}], ",", 
           RowBox[{"Charting`TickAnnotations", "\[Rule]", "None"}], ",", 
           RowBox[{"ScalingFunctions", "\[Rule]", 
             RowBox[{"{", 
               RowBox[{"Identity", ",", "Identity"}], "}"}]}], ",", 
           RowBox[{"Charting`TickSide", "\[Rule]", "Right"}], ",", 
           RowBox[{"ColorFunctionScaling", "\[Rule]", "True"}]}], "]"}]& )], 
     TraditionalForm], TraditionalForm]},
  "Legended",
  DisplayFunction->(GridBox[{{
      TagBox[
       ItemBox[
        PaneBox[
         TagBox[#, "SkipImageSizeLevel"], Alignment -> {Center, Baseline}, 
         BaselinePosition -> Baseline], DefaultBaseStyle -> "Labeled"], 
       "SkipImageSizeLevel"], 
      ItemBox[#2, DefaultBaseStyle -> "LabeledLabel"]}}, 
    GridBoxAlignment -> {"Columns" -> {{Center}}, "Rows" -> {{Center}}}, 
    AutoDelete -> False, GridBoxItemSize -> Automatic, 
    BaselinePosition -> {1, 1}]& ),
  Editable->True,
  InterpretationFunction->(RowBox[{"Legended", "[", 
     RowBox[{#, ",", 
       RowBox[{"Placed", "[", 
         RowBox[{#2, ",", "After"}], "]"}]}], "]"}]& )]], "Output",ExpressionU\
UID->"9cf78127-a71b-4305-b6ec-9c119dd722fe"]
}, Open  ]],

Cell[CellGroupData[{

Cell["\<\
Check that out probabilities match
\
\>", "Subsubsection",ExpressionUUID->"02e59c17-957b-415f-a07f-74fd30cd6e67"],

Cell[BoxData[{
 RowBox[{
  RowBox[{"lapList", " ", "=", " ", 
   RowBox[{
    RowBox[{"Function", "[", 
     RowBox[{"x", ",", 
      RowBox[{"pOutputLap", "[", 
       RowBox[{"x", ",", "k", ",", "V"}], "]"}]}], "]"}], "/@", 
    RowBox[{"Range", "[", 
     RowBox[{"0.005", ",", "0.995", ",", "0.01"}], "]"}]}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"exactList", " ", "=", " ", 
   RowBox[{
    RowBox[{"Function", "[", 
     RowBox[{"x", ",", 
      RowBox[{"pOutput", "[", 
       RowBox[{"x", ",", "k", ",", "V"}], "]"}]}], "]"}], "/@", 
    RowBox[{"Range", "[", 
     RowBox[{"0.005", ",", "0.995", ",", "0.01"}], "]"}]}]}], 
  ";"}], "\[IndentingNewLine]"}], "Input",ExpressionUUID->"6d551452-13a0-4599-\
8e6d-676e4513d33e"],

Cell[BoxData[""], "Input",ExpressionUUID->"19a96fab-908d-4596-8d0f-4a2c63068ec6"],

Cell[CellGroupData[{

Cell[BoxData[
 RowBox[{"\[IndentingNewLine]", 
  RowBox[{"ListLinePlot", "[", 
   RowBox[{
    RowBox[{"{", 
     RowBox[{"exactList", ",", "lapList"}], "}"}], ",", 
    RowBox[{"PlotLegends", "\[Rule]", 
     RowBox[{"{", 
      RowBox[{
      "\"\<P(\!\(\*SuperscriptBox[\(O\), \(*\)]\))\>\"", ",", 
       "\"\<\!\(\*SubscriptBox[\(P\), \(Laplace\)]\)(\!\(\*SuperscriptBox[\(O\
\), \(*\)]\))\>\""}], " ", "}"}]}], ",", 
    RowBox[{"PlotStyle", "\[Rule]", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
         RowBox[{"Thickness", "[", "0.005", "]"}], ",", "Blue"}], "}"}], ",", 
       
       RowBox[{"{", 
        RowBox[{"Red", ",", "Dashed", ",", 
         RowBox[{"Thickness", "[", "0.004", "]"}]}], "}"}]}], "}"}]}], ",", 
    RowBox[{"TicksStyle", "\[Rule]", 
     RowBox[{"Directive", "[", 
      RowBox[{"\"\<Label\>\"", ",", "30"}], "]"}]}], ",", 
    RowBox[{"Ticks", "\[Rule]", 
     RowBox[{"{", "Automatic", "}"}]}], ",", 
    RowBox[{"LabelStyle", "\[Rule]", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
       RowBox[{"FontSize", "\[Rule]", "24"}]}], "}"}]}], ",", 
    RowBox[{"FrameLabel", "\[Rule]", 
     RowBox[{"{", 
      RowBox[{
      "\"\<\!\(\*SuperscriptBox[\(O\), \(*\)]\)\>\"", ",", 
       "\"\<Probability \>\""}], "}"}]}], ",", 
    RowBox[{"Frame", "\[Rule]", "True"}], ",", 
    RowBox[{"FrameStyle", "\[Rule]", 
     RowBox[{"Directive", "[", "Black", "]"}]}], ",", 
    RowBox[{"PlotRange", "\[Rule]", "All"}]}], "]"}]}]], "Input",ExpressionUUI\
D->"e0498b92-5db2-4c00-8494-470d552300c1"],

Cell[BoxData[
 TemplateBox[{GraphicsBox[{{}, {{{}, {}, {
        Hue[0.67, 0.6, 0.6], 
        Directive[
         PointSize[0.01388888888888889], 
         AbsoluteThickness[1.6], 
         Thickness[0.005], 
         RGBColor[0, 0, 1]], 
        LineBox[CompressedData["
1:eJw1lAkwnGcchy11JGnLkJkoUixd1pGsEImI+EWcce1aeyCsKJKWoqVJJMqm
cWwcaQZlJJFJTaSoZCQVI4hZR9vQVikjhxln65pUBXFlE03Hf3dmZ+fZZ55v
3u+9TKMSg2JUVVRUot9+///d/My7Lrn4Zk6FmLnSH+DkvGI/mzQl1oJjz2JP
aosFsQ4MWemqf2RxiLfDcWk5J9xgH7E+nDQMTDK0QWwEVb5Lqi3Lk9gEDjrL
Wo98AoiZuNS8OGWXG0xsDjGzxuPMzTBiFjxT2fnap6OILZF+I+WD5xknia3g
6s7uuP9RErENPnPZyX3F+pJ4F/yzbXMmcs8Rc1DyMKt8sP88sR1Oiaf0ixey
ifcgzrzyx2pJAbE9JrO0vb3vFRI74HxCze8DP5QS70VR+2W/xz3lxI6wep2Q
/XNNBfE+aB/X6ws//D3xfthIpjtM+2qJneAV1lwR3X6X+AC2Wu4PW//pPrEz
ur8eqy4raCI+CEOvwtFVAzmxC4Y4D9LXEjqID+F6zqk5T/4vxK7Y0vOokf30
V2IgTi104eDF3k2WAvXVVUOVsn7yh9GtK8vsXBwkfxhX1D3infufkXfD5bg7
nXq7R8i74V6pbrej5jj5I1iT2mgUN/9F/gi+vVX/vK1hirw7wkNmxkfXZsm7
QxLlETyaOUfeA5az8zIL6xfkPXC27Pal0r2L5D3hdCE494TkJXlP1Hf1dgkq
V8h7YTgmP9lWbZ28F5qrtWrbkxXkvcFw6tVenn9D3hvvjc3sGn2ggk3vgzCe
/RmzdcYmS33Q0sepUTiokT8K3YE9q0Mx75A/ivkNaQVDpk7eF4MM9cSyEg3y
vtghLPZ/mq9J3g9nG7a5M2O1yPvBeIFfwPxwC3l/sBRZd1bqiKX+6Dz3rlWg
4VbyAeB4NcRnhhJLA5CXEFnSFK/0gWBHpTyZ5yp9IOoE+lwdTaXnIsJdlrHw
DT0fXMQL3B5L/lGOh4u0a3P6GkxiORfWn+S3xtopx89DZLL64kNjej/w8HHk
tObEC+V88FBv3F5ldpvmS85D7e5OnqFIlfoghLx/TC57SfONIPx7wCjM02iD
1iMIYkW527gfrZc8CG2JayyPnDVaPz4iekwbRwaWNxl8KBQbotcuS9TzsW17
8cjVLto/cj4+v6Y4cXNdub+C8dWQRUNeE+0/BONkzJv58CeT1L/lmcK+P4UT
1AfDXt3cgek4Sr0AO270+5ZP03mAALbXpbw1A+V5EUASWXS3YrWPegGKjC+a
+8/8Rr0QsohwmRmDzieE2BnQtgKDduqFMKnyXv7UuoV6Iabk2ceja5T3gQic
W8OLLafrqBfh2N/OUcPNVdSLkLShyF0Q0/0jF6HFbpwROnaFejG6XdJmryYV
US+GVUpB53fyPOrFOJQW1qqXf4F6MRixhq0jbOX9GQI/+6BGVvkXrv8BnHfF
Bg==
         "]]}, {
        Hue[0.9060679774997897, 0.6, 0.6], 
        Directive[
         PointSize[0.01388888888888889], 
         AbsoluteThickness[1.6], 
         RGBColor[1, 0, 0], 
         Dashing[{Small, Small}], 
         Thickness[0.004]], 
        LineBox[CompressedData["
1:eJw1lAlQVHUcxxdoBSEEDGGVRMSVa5CzuOL4LjfIubAsCwoGBpIEq6QTBdOC
XBqBKCClMImFSEGOaBJHbMiOHNOQIlcgKDgwMsQZl1zV8Ns38+bN533m8+b/
/v//ewejErkfyTIYjJP/nf9ft49Zp6aEt5Xbh8yc6AYS2JvqB8bYxAr4o0nW
uDjNmFgVHsd74vQiLInV0bnWMZVibEfMwuB4tv7jaxzidxHHNFBZzPYg1sFB
5v1v2Vx/Yl0IHL2bHJuDidmwdH4cLTwWTqwHm/LJYf+lD4kN0KpeMhv1RSyx
EUIYZd3HCxKIjfGgMqWO+WcSsQlmwtOFWVeTic0QI384okQoIjbHdFpv/gul
TGILqBXkXXj07BKxJWTmbVNH5AuI38M7nCfT+tlFxO/DwvD2DJ91ndgKXpVh
zInV74it0biUZ5955AdiGwQ+rHsu9qkitsXI1IUgHucusR1qCseHVrZqiT/A
DlNF/ezUOmJ7OHAaqnX7m4gdIPEtLItgtRA7Qm6Uf+RMj4TYCYKalUYTqw5i
oCZtn2+4X9c2i4B7SifYOt88Jc/B5N/2gv1rPeQ56FrZe0ezb4C8M1oMOs6d
tRom74zKhc6kJ5dfkndBYYPfxKj/K/Iu8BQlCDXCJ8i7Yqi41GmhdpK8K679
+uXO8rBp8m5gtY2t9evNkXdDaOSC5pzrAnl3LHATM1YzFsm7o3XgpQZnbJm8
B5oXc8++CXtD3gP8+FvNtuPr5D1xvr+Bs566Rd4TFnO+J/cry2Dbe+Hzi5sS
OYHsNou8UDMyIR4slSPvjdwGjZCvet8i740aO6ZO9waT/FGECz/+9MwuefJH
0Wk+Gu8pp0DeB41FhxMz+4hFPpjU1ihty9pJ3he5zjqfeSsrkvfFcOj8xkYs
McMP6o9cS6oLpd4PKsq9MWpXpN4fv+f43X0eKfX+MNNi32zblD4/AFOftL/O
EhIjAHtKlTnND6XjCUDOKxe3xT4avzgAg91TFfE9O6gPRLWiZJL5gN4Xgdiz
W3JCI0U6H4G4HnGD7WBG8yUOxLq2uaZ1n3R+uQjIiP+t9jyDei76Wcu1AoNN
Wg8uTottsjPi1rZZzMUWI7ndqn6F1i8Ie3t1Ist0l7YZQdjHDvOpuEP7QxSE
ZIMVo7Fg2j/iIByznjPSCpTur2CslsUM7Pan/YdgCM3+qpqtGKc+GC2Ssa4q
wRj1wTiwkn7TNOYF9Tzc48lZn7o6SD0Pp5VYqrsYfdTzEGEhE9su3009Dx2q
+dbaDvS9MULQkWNn3ZnQTn0I3DPbSrsvtlIfgnwFtR/TksTUh+Cfn/H1s0P1
1PMxE12PtZL71PORp397foRF/w8RHxqvn0qWFyqp56Mj6hDPxeQW9aGIVxRH
w7GU+lBM+50zPKVVTH0oyn8q6vvlymXqQ6FyQy3K8Psc6gXIEZuquBmlO/0L
nwDC9g==
         "]]}}}, {}, {}, {}, {}}, {
    DisplayFunction -> Identity, PlotRangePadding -> {{
        Scaled[0.02], 
        Scaled[0.02]}, {
        Scaled[0.02], 
        Scaled[0.05]}}, AxesOrigin -> {0, 0}, 
     PlotRange -> {{0, 100.}, {0, 3.3225967308029656`}}, PlotRangeClipping -> 
     True, ImagePadding -> All, DisplayFunction -> Identity, AspectRatio -> 
     NCache[GoldenRatio^(-1), 0.6180339887498948], Axes -> {True, True}, 
     AxesLabel -> {None, None}, AxesOrigin -> {0, 0}, DisplayFunction :> 
     Identity, Frame -> {{True, True}, {True, True}}, FrameLabel -> {{
        FormBox["\"Probability \"", TraditionalForm], None}, {
        FormBox[
        "\"\\!\\(\\*SuperscriptBox[\\(O\\), \\(*\\)]\\)\"", TraditionalForm], 
        None}}, FrameStyle -> Directive[
       GrayLevel[0]], 
     FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
     GridLines -> {None, None}, GridLinesStyle -> Directive[
       GrayLevel[0.5, 0.4]], 
     LabelStyle -> {FontFamily -> "Helvetica", FontSize -> 24}, 
     Method -> {"CoordinatesToolOptions" -> {"DisplayFunction" -> ({
           (Identity[#]& )[
            Part[#, 1]], 
           (Identity[#]& )[
            Part[#, 2]]}& ), "CopiedValueFunction" -> ({
           (Identity[#]& )[
            Part[#, 1]], 
           (Identity[#]& )[
            Part[#, 2]]}& )}}, 
     PlotRange -> {{0, 100.}, {0, 3.3225967308029656`}}, PlotRangeClipping -> 
     True, PlotRangePadding -> {{
        Scaled[0.02], 
        Scaled[0.02]}, {
        Scaled[0.02], 
        Scaled[0.05]}}, Ticks -> {Automatic, Automatic}, TicksStyle -> 
     Directive["Label", 30]}],FormBox[
    FormBox[
     TemplateBox[{
      "\"P(\\!\\(\\*SuperscriptBox[\\(O\\), \\(*\\)]\\))\"", 
       "\"\\!\\(\\*SubscriptBox[\\(P\\), \
\\(Laplace\\)]\\)(\\!\\(\\*SuperscriptBox[\\(O\\), \\(*\\)]\\))\""}, 
      "LineLegend", DisplayFunction -> (FormBox[
        StyleBox[
         StyleBox[
          PaneBox[
           TagBox[
            GridBox[{{
               TagBox[
                GridBox[{{
                   GraphicsBox[{{
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.125], 
                    AbsoluteThickness[1.6], 
                    Thickness[0.045], 
                    RGBColor[0, 0, 1]], {
                    LineBox[{{0, 10}, {40, 10}}]}}, {
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.125], 
                    AbsoluteThickness[1.6], 
                    Thickness[0.045], 
                    RGBColor[0, 0, 1]], {}}}, AspectRatio -> Full, 
                    ImageSize -> {40, 10}, PlotRangePadding -> None, 
                    ImagePadding -> Automatic, 
                    BaselinePosition -> (Scaled[-0.33399999999999996`] -> 
                    Baseline)], #}, {
                   GraphicsBox[{{
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.125], 
                    AbsoluteThickness[1.6], 
                    RGBColor[1, 0, 0], 
                    Dashing[{Small, Small}], 
                    Thickness[0.036000000000000004`]], {
                    LineBox[{{0, 10}, {40, 10}}]}}, {
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.125], 
                    AbsoluteThickness[1.6], 
                    RGBColor[1, 0, 0], 
                    Dashing[{Small, Small}], 
                    Thickness[0.036000000000000004`]], {}}}, AspectRatio -> 
                    Full, ImageSize -> {40, 10}, PlotRangePadding -> None, 
                    ImagePadding -> Automatic, 
                    BaselinePosition -> (Scaled[-0.33399999999999996`] -> 
                    Baseline)], #2}}, 
                 GridBoxAlignment -> {
                  "Columns" -> {Center, Left}, "Rows" -> {{Baseline}}}, 
                 AutoDelete -> False, 
                 GridBoxDividers -> {
                  "Columns" -> {{False}}, "Rows" -> {{False}}}, 
                 GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}},
                  GridBoxSpacings -> {
                  "Columns" -> {{0.5}}, "Rows" -> {{0.8}}}], "Grid"]}}, 
             GridBoxAlignment -> {"Columns" -> {{Left}}, "Rows" -> {{Top}}}, 
             AutoDelete -> False, 
             GridBoxItemSize -> {
              "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}}, 
             GridBoxSpacings -> {"Columns" -> {{1}}, "Rows" -> {{0}}}], 
            "Grid"], Alignment -> Left, AppearanceElements -> None, 
           ImageMargins -> {{5, 5}, {5, 5}}, ImageSizeAction -> 
           "ResizeToFit"], LineIndent -> 0, StripOnInput -> False], {
         FontFamily -> "Helvetica", FontSize -> 24, FontFamily -> "Arial"}, 
         Background -> Automatic, StripOnInput -> False], TraditionalForm]& ),
       InterpretationFunction :> (RowBox[{"LineLegend", "[", 
         RowBox[{
           RowBox[{"{", 
             RowBox[{
               RowBox[{"Directive", "[", 
                 RowBox[{
                   RowBox[{"PointSize", "[", "0.01388888888888889`", "]"}], 
                   ",", 
                   RowBox[{"AbsoluteThickness", "[", "1.6`", "]"}], ",", 
                   RowBox[{"Thickness", "[", "0.005`", "]"}], ",", 
                   InterpretationBox[
                    ButtonBox[
                    TooltipBox[
                    GraphicsBox[{{
                    GrayLevel[0], 
                    RectangleBox[{0, 0}]}, {
                    GrayLevel[0], 
                    RectangleBox[{1, -1}]}, {
                    RGBColor[0, 0, 1], 
                    RectangleBox[{0, -1}, {2, 1}]}}, DefaultBaseStyle -> 
                    "ColorSwatchGraphics", AspectRatio -> 1, Frame -> True, 
                    FrameStyle -> RGBColor[0., 0., 0.6666666666666666], 
                    FrameTicks -> None, PlotRangePadding -> None, ImageSize -> 
                    Dynamic[{
                    Automatic, 
                    1.35 (CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[
                    Magnification])}]], "RGBColor[0, 0, 1]"], Appearance -> 
                    None, BaseStyle -> {}, BaselinePosition -> Baseline, 
                    DefaultBaseStyle -> {}, ButtonFunction :> 
                    With[{Typeset`box$ = EvaluationBox[]}, 
                    If[
                    Not[
                    AbsoluteCurrentValue["Deployed"]], 
                    SelectionMove[Typeset`box$, All, Expression]; 
                    FrontEnd`Private`$ColorSelectorInitialAlpha = 1; 
                    FrontEnd`Private`$ColorSelectorInitialColor = 
                    RGBColor[0, 0, 1]; 
                    FrontEnd`Private`$ColorSelectorUseMakeBoxes = True; 
                    MathLink`CallFrontEnd[
                    FrontEnd`AttachCell[Typeset`box$, 
                    FrontEndResource["RGBColorValueSelector"], {
                    0, {Left, Bottom}}, {Left, Top}, 
                    "ClosingActions" -> {
                    "SelectionDeparture", "ParentChanged", 
                    "EvaluatorQuit"}]]]], BaseStyle -> Inherited, Evaluator -> 
                    Automatic, Method -> "Preemptive"], 
                    RGBColor[0, 0, 1], Editable -> False, Selectable -> 
                    False]}], "]"}], ",", 
               RowBox[{"Directive", "[", 
                 RowBox[{
                   RowBox[{"PointSize", "[", "0.01388888888888889`", "]"}], 
                   ",", 
                   RowBox[{"AbsoluteThickness", "[", "1.6`", "]"}], ",", 
                   InterpretationBox[
                    ButtonBox[
                    TooltipBox[
                    GraphicsBox[{{
                    GrayLevel[0], 
                    RectangleBox[{0, 0}]}, {
                    GrayLevel[0], 
                    RectangleBox[{1, -1}]}, {
                    RGBColor[1, 0, 0], 
                    RectangleBox[{0, -1}, {2, 1}]}}, DefaultBaseStyle -> 
                    "ColorSwatchGraphics", AspectRatio -> 1, Frame -> True, 
                    FrameStyle -> RGBColor[0.6666666666666666, 0., 0.], 
                    FrameTicks -> None, PlotRangePadding -> None, ImageSize -> 
                    Dynamic[{
                    Automatic, 
                    1.35 (CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[
                    Magnification])}]], "RGBColor[1, 0, 0]"], Appearance -> 
                    None, BaseStyle -> {}, BaselinePosition -> Baseline, 
                    DefaultBaseStyle -> {}, ButtonFunction :> 
                    With[{Typeset`box$ = EvaluationBox[]}, 
                    If[
                    Not[
                    AbsoluteCurrentValue["Deployed"]], 
                    SelectionMove[Typeset`box$, All, Expression]; 
                    FrontEnd`Private`$ColorSelectorInitialAlpha = 1; 
                    FrontEnd`Private`$ColorSelectorInitialColor = 
                    RGBColor[1, 0, 0]; 
                    FrontEnd`Private`$ColorSelectorUseMakeBoxes = True; 
                    MathLink`CallFrontEnd[
                    FrontEnd`AttachCell[Typeset`box$, 
                    FrontEndResource["RGBColorValueSelector"], {
                    0, {Left, Bottom}}, {Left, Top}, 
                    "ClosingActions" -> {
                    "SelectionDeparture", "ParentChanged", 
                    "EvaluatorQuit"}]]]], BaseStyle -> Inherited, Evaluator -> 
                    Automatic, Method -> "Preemptive"], 
                    RGBColor[1, 0, 0], Editable -> False, Selectable -> 
                    False], ",", 
                   RowBox[{"Dashing", "[", 
                    RowBox[{"{", 
                    RowBox[{"Small", ",", "Small"}], "}"}], "]"}], ",", 
                   RowBox[{"Thickness", "[", "0.004`", "]"}]}], "]"}]}], 
             "}"}], ",", 
           RowBox[{"{", 
             RowBox[{#, ",", #2}], "}"}], ",", 
           RowBox[{"LegendMarkers", "\[Rule]", 
             RowBox[{"{", 
               RowBox[{
                 RowBox[{"{", 
                   RowBox[{"False", ",", "Automatic"}], "}"}], ",", 
                 RowBox[{"{", 
                   RowBox[{"False", ",", "Automatic"}], "}"}]}], "}"}]}], ",", 
           RowBox[{"Joined", "\[Rule]", 
             RowBox[{"{", 
               RowBox[{"True", ",", "True"}], "}"}]}], ",", 
           RowBox[{"LabelStyle", "\[Rule]", 
             RowBox[{"{", 
               RowBox[{
                 RowBox[{"FontFamily", "\[Rule]", "\"Helvetica\""}], ",", 
                 RowBox[{"FontSize", "\[Rule]", "24"}]}], "}"}]}], ",", 
           RowBox[{"LegendLayout", "\[Rule]", "\"Column\""}]}], "]"}]& ), 
      Editable -> True], TraditionalForm], TraditionalForm]},
  "Legended",
  DisplayFunction->(GridBox[{{
      TagBox[
       ItemBox[
        PaneBox[
         TagBox[#, "SkipImageSizeLevel"], Alignment -> {Center, Baseline}, 
         BaselinePosition -> Baseline], DefaultBaseStyle -> "Labeled"], 
       "SkipImageSizeLevel"], 
      ItemBox[#2, DefaultBaseStyle -> "LabeledLabel"]}}, 
    GridBoxAlignment -> {"Columns" -> {{Center}}, "Rows" -> {{Center}}}, 
    AutoDelete -> False, GridBoxItemSize -> Automatic, 
    BaselinePosition -> {1, 1}]& ),
  Editable->True,
  InterpretationFunction->(RowBox[{"Legended", "[", 
     RowBox[{#, ",", 
       RowBox[{"Placed", "[", 
         RowBox[{#2, ",", "After"}], "]"}]}], "]"}]& )]], "Output",ExpressionU\
UID->"5973dc1b-7585-4ce2-a899-5c90f482c2e6"]
}, Open  ]],

Cell[BoxData[""], "Input",ExpressionUUID->"b088cd8d-8ff9-4f59-8d06-2caa669e6c28"]
}, Open  ]]
}, Open  ]]
}, Open  ]],

Cell[CellGroupData[{

Cell["Compute Mutual Information", "Section",ExpressionUUID->"57cbb4d3-e671-436a-8418-63778b56f74b"],

Cell[BoxData[
 RowBox[{"\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{
   "Use", " ", "java", " ", "program", " ", "to", " ", "compute", " ", 
    "these", " ", "integrals"}], " ", "*)"}]}]], "Input",ExpressionUUID->\
"e1e0ec2b-45e5-4ae7-a50e-56e92b152c77"],

Cell[BoxData[""], "Input",ExpressionUUID->"0bb0cdb7-b3b5-4131-973b-47e441113497"],

Cell[CellGroupData[{

Cell[BoxData[{
 RowBox[{
  RowBox[{"MI", "[", "k_", "]"}], ":=", 
  RowBox[{"Sum", "[", 
   RowBox[{
    RowBox[{
     RowBox[{"pJoint", "[", 
      RowBox[{"y", ",", "x", ",", "k", ",", "V"}], "]"}], "*", 
     RowBox[{"Log", "[", 
      FractionBox[
       RowBox[{"pJoint", "[", 
        RowBox[{"y", ",", "x", ",", "k", ",", "V"}], "]"}], 
       RowBox[{
        RowBox[{"pOutput", "[", 
         RowBox[{"y", ",", "k", ",", "V"}], "]"}], 
        RowBox[{"pInput", "[", 
         RowBox[{"x", ",", "0.5", ",", "0.1"}], "]"}]}]], "]"}], "*", "0.1", 
     "*", "0.1"}], ",", 
    RowBox[{"{", 
     RowBox[{"x", ",", "0.01", ",", "1", ",", "0.1"}], "}"}], ",", 
    RowBox[{"{", 
     RowBox[{"y", ",", "0.01", ",", "1", ",", "0.1"}], "}"}]}], 
   "]"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"MILap", "[", "k_", "]"}], ":=", 
   RowBox[{"Sum", "[", 
    RowBox[{
     RowBox[{
      RowBox[{"pJointLaplace", "[", 
       RowBox[{"y", ",", "x", ",", "k", ",", "V"}], "]"}], "*", 
      RowBox[{"Log", "[", 
       FractionBox[
        RowBox[{"pJointLaplace", "[", 
         RowBox[{"y", ",", "x", ",", "k", ",", "V"}], "]"}], 
        RowBox[{
         RowBox[{"pOutputLap", "[", 
          RowBox[{"y", ",", "k", ",", "V"}], "]"}], 
         RowBox[{"pInput", "[", 
          RowBox[{"x", ",", "0.5", ",", "0.1"}], "]"}]}]], "]"}], "*", "0.1", 
      "*", "0.1"}], ",", 
     RowBox[{"{", 
      RowBox[{"y", ",", "0.01", ",", "1", ",", "0.1"}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{"x", ",", "0.01", ",", "1", ",", "0.1"}], "}"}]}], "]"}]}], 
  "\[IndentingNewLine]"}], "\[IndentingNewLine]", 
 RowBox[{"MI", "[", "3", "]"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"MILap", "[", "3", "]"}], 
  "\[IndentingNewLine]"}], "\[IndentingNewLine]"}], "Input",ExpressionUUID->\
"9afdc3fe-e282-49ab-9ef2-cfa37c2223e3"],

Cell[BoxData["0.09779083476657152`"], "Output",ExpressionUUID->"c34229be-22d8-456a-9f7c-40c2f0618711"],

Cell[BoxData["0.0981962079921231`"], "Output",ExpressionUUID->"90144933-6c61-4f6b-8e76-ceb8e98e2d11"]
}, Open  ]],

Cell[CellGroupData[{

Cell[BoxData[{
 RowBox[{
  RowBox[{"V", "=", "20"}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"k", "=", "1.5"}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"MILoop", " ", "=", " ", "0"}], ";"}], "\[IndentingNewLine]", 
 RowBox[{"For", "[", 
  RowBox[{
   RowBox[{"y", "=", 
    SuperscriptBox["10", 
     RowBox[{"-", "6"}]]}], ",", 
   RowBox[{"y", "<", "1"}], ",", 
   RowBox[{"y", "+=", "0.1"}], ",", "\[IndentingNewLine]", 
   RowBox[{"For", "[", "\[IndentingNewLine]", 
    RowBox[{
     RowBox[{"x", "=", 
      SuperscriptBox["10", 
       RowBox[{"-", "6"}]]}], ",", 
     RowBox[{"x", "<", "1"}], ",", 
     RowBox[{"x", "+=", "0.1"}], ",", "\[IndentingNewLine]", 
     RowBox[{"(*", 
      RowBox[{
       RowBox[{"Print", "[", 
        RowBox[{"y", ",", "\"\< - \>\"", ",", "x"}], "]"}], ";"}], "*)"}], 
     "\[IndentingNewLine]", 
     RowBox[{"If", "[", 
      RowBox[{
       RowBox[{
        RowBox[{"pJoint", "[", 
         RowBox[{"y", ",", "x", ",", "k", ",", "V"}], "]"}], ">", "0"}], ",", 
       "\[IndentingNewLine]", 
       RowBox[{
        RowBox[{"MILoop", " ", "+=", 
         RowBox[{
          RowBox[{"pJoint", "[", 
           RowBox[{"y", ",", "x", ",", "k", ",", "V"}], "]"}], "*", 
          RowBox[{"Log", "[", 
           FractionBox[
            RowBox[{"pJoint", "[", 
             RowBox[{"y", ",", "x", ",", "k", ",", "V"}], "]"}], 
            RowBox[{
             RowBox[{"pOutput", "[", 
              RowBox[{"y", ",", "k", ",", "V"}], "]"}], 
             RowBox[{"pInput", "[", 
              RowBox[{"x", ",", "0.5", ",", "0.1"}], "]"}]}]], "]"}], "*", 
          "0.1", "*", "0.1"}]}], ";"}]}], "\[IndentingNewLine]", 
      RowBox[{"(*", 
       RowBox[{"Print", "[", "MILoop", "]"}], "*)"}], "\[IndentingNewLine]", 
      "]"}]}], "\[IndentingNewLine]", "]"}]}], "\[IndentingNewLine]", 
  "]"}], "\[IndentingNewLine]", "MILoop", "\[IndentingNewLine]"}], "Input",Exp\
ressionUUID->"e74da34a-0059-4b34-89ac-26493dcc7086"],

Cell[BoxData["0.29444521892034187`"], "Output",ExpressionUUID->"3bb54e8f-e38d-44c9-8350-bdedf067f6a9"]
}, Open  ]],

Cell[BoxData[""], "Input",ExpressionUUID->"d832e1a9-eaff-4811-a65e-b5b7d1b78170"],

Cell[BoxData[""], "Input",ExpressionUUID->"d7a96cc3-f128-4c8b-aaa6-5a0fcaa31802"],

Cell[BoxData[{
 RowBox[{
  RowBox[{
   RowBox[{"MIList", " ", "=", " ", 
    RowBox[{
     RowBox[{"Function", "[", 
      RowBox[{"k", ",", 
       RowBox[{"MI", "[", "k", "]"}]}], "]"}], "/@", 
     RowBox[{"Range", "[", 
      RowBox[{"0.1", ",", "20", ",", "0.5"}], "]"}]}]}], ";"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", 
   RowBox[{
    RowBox[{"MILapList", " ", "=", " ", 
     RowBox[{
      RowBox[{"Function", "[", 
       RowBox[{"k", ",", 
        RowBox[{"MILap", "[", "k", "]"}]}], "]"}], "/@", 
      RowBox[{"Range", "[", 
       RowBox[{"0.1", ",", "50", ",", "0.5"}], "]"}]}]}], ";"}], 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"MILapList", " ", "=", " ", 
    RowBox[{
     RowBox[{"Function", "[", 
      RowBox[{"k", ",", 
       RowBox[{"MILap", "[", "k", "]"}]}], "]"}], "/@", 
     RowBox[{"Range", "[", 
      RowBox[{"0.1", ",", "20", ",", "0.5"}], "]"}]}]}], ";"}], " ", 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{"ListPlot", "[", 
    RowBox[{"{", 
     RowBox[{"MIList", ",", "MILapList"}], "}"}], "]"}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"(*", 
   RowBox[{"ListPlot", "[", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{"MIList", ",", "MILapList"}], "}"}], ",", 
     RowBox[{"PlotRange", "\[Rule]", "All"}]}], "]"}], "*)"}]}]}], "Input",Exp\
ressionUUID->"ea663ccf-096c-4892-bc8d-81a6aebe2993"],

Cell[CellGroupData[{

Cell[BoxData[
 RowBox[{"ListPlot", "[", 
  RowBox[{
   RowBox[{"{", 
    RowBox[{
     RowBox[{"MIList", "*", "10"}], ",", 
     RowBox[{"MILapList", "*", "10"}]}], "}"}], ",", 
   RowBox[{"PlotRange", "\[Rule]", 
    RowBox[{"{", 
     RowBox[{
      RowBox[{"{", 
       RowBox[{"0.5", ",", "20"}], "}"}], ",", 
      RowBox[{"{", 
       RowBox[{"0.4", ",", "1"}], "}"}]}], "}"}]}], ",", 
   RowBox[{"PlotLegends", "\[Rule]", 
    RowBox[{"{", 
     RowBox[{"\"\<MI-Exact\>\"", ",", "\"\<MI-Laplace\>\""}], "}"}]}], ",", 
   RowBox[{"PlotLabel", "\[Rule]", "\"\<V=20\>\""}]}], "]"}]], "Input",Express\
ionUUID->"1b9d0840-35a8-4482-844c-c0cf7e6dcb28"],

Cell[BoxData[
 TemplateBox[{GraphicsBox[{{}, {{{}, {
        Hue[0.67, 0.6, 0.6], 
        Directive[
         PointSize[0.011000000000000001`], 
         RGBColor[0.368417, 0.506779, 0.709798], 
         AbsoluteThickness[1.6]], 
        PointBox[CompressedData["
1:eJxTTMoPSmViYGDQAGIQDQEf7PM9Lurf2XLLHirg0PQi9uC+9mdQPofDl6IG
8eo9b6B8AYdp6S47+Y3eQ/kiDsLOumqNL2B8CYe0qxt71h2A8WUcet9HqzKr
wfgKDgcn3/oR7f8OyldyUFtmEesb9RbKV3GQ/VHrUxcOs0/NIaNi57xFAa+h
fA2H2DDH9bXer6B8LYe9HzabFXm/hPJ1HM7yBV2MDXoB5es5TOTrNJBPfA7l
Gzh8KWTO+1QO85+hA4NMx7It055C+UYOTQGfmUX3PoHyjR12q/prbHv1GMo3
cdA69XWxkzyMb+rw56V1YnnUIyjfzOHkWf93anMeQvnmDm1vC8wsnjyA8i0c
BKYpbllkBONbOnRfDNe9zHcfyrdyuNCwbaXu0btQvrVDnaRsU27HHSjfxmF/
ltiFFRG3oXxbB5EDKxe/NYXFn51DzJI55nYKN6F8e4dPT04s7xO/AeU7OGxd
3v/ssPR1CL/BwcGad9a6zTrXoPKODgbZdwxUva5C5R0d7OpWul0qvgKVd3Lg
di77kbHyMlTeyeHaYS7j3leXoPLODnvZUnpvmUH5Dc4OK4T1OZ/1XoTKuzh8
+WK3z/79BXsArOS34w==
         "]]}, {
        Hue[0.9060679774997897, 0.6, 0.6], 
        Directive[
         PointSize[0.011000000000000001`], 
         RGBColor[0.880722, 0.611041, 0.142051], 
         AbsoluteThickness[1.6]], 
        PointBox[CompressedData["
1:eJxTTMoPSmViYGDQAGIQDQEf7BVKq4/Efb9lDxVwCHqcrFb86xmUz+Fg5mjB
WLPtDZQv4KC6carMBan3UL6IgxbD6cvr78P4Eg7LHBX2cp6H8WUc+E/Kvzzo
DeMrOHD3q2zp73oH5Ss52LW2PO1c+RbKV3EQLyuInLkLZp+awzWZnDfTTryG
8jUc9niW9AjcegXlazlMvmV66smnl1C+joPDJLcAW1EYX8/hwft593WcXkD5
Bg5bnrBWilY9h/INHX7rcaa/3gPzr5FDBlNeZiIXjG/skLFhRi9r0lMo38SB
bV1Af/DhJ1C+qcPO6eJ8B3RhfDOHQqMPL/YteAzlmzus+vg3/5g0jG/h4H2u
YVnAvEdQvqXDz7KS4tvqML6Vw+zS5ugX2x9C+dYOe0TnqvzxhfFtHKZwfpw1
7+UDKN/WYRXjqv6oLhjfzkHjk5PoPn0Y397Bz+nog62b7kP5Dg573lVH9/FC
+Q0ODjp7F0+zzr8HlXd0eB3if+LftbtQeUeHO62R81VdoHwGJ4dtm2PDubfd
gco7OZw4/a9bUgfKZ3B2uBsUcerE0ttQeWeHD+dNbi5QgvIZXBzWLW3dbbHo
lj0A0Am+Mw==
         "]]}, {}}}, {}, {}, {}, {}}, {
    DisplayFunction -> Identity, PlotRangePadding -> {{0, 0}, {0, 0}}, 
     AxesOrigin -> {0.5975, 0.403}, PlotRange -> {{0.5, 20}, {0.4, 1}}, 
     PlotRangeClipping -> True, ImagePadding -> All, DisplayFunction -> 
     Identity, AspectRatio -> NCache[GoldenRatio^(-1), 0.6180339887498948], 
     Axes -> {True, True}, AxesLabel -> {None, None}, 
     AxesOrigin -> {0.5975, 0.403}, DisplayFunction :> Identity, 
     Frame -> {{False, False}, {False, False}}, 
     FrameLabel -> {{None, None}, {None, None}}, 
     FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
     GridLines -> {None, None}, GridLinesStyle -> Directive[
       GrayLevel[0.5, 0.4]], 
     Method -> {"CoordinatesToolOptions" -> {"DisplayFunction" -> ({
           (Identity[#]& )[
            Part[#, 1]], 
           (Identity[#]& )[
            Part[#, 2]]}& ), "CopiedValueFunction" -> ({
           (Identity[#]& )[
            Part[#, 1]], 
           (Identity[#]& )[
            Part[#, 2]]}& )}}, PlotLabel -> 
     FormBox["\"V=20\"", TraditionalForm], PlotRange -> {{0.5, 20}, {0.4, 1}},
      PlotRangeClipping -> True, PlotRangePadding -> {{0, 0}, {0, 0}}, 
     Ticks -> {Automatic, Automatic}}],FormBox[
    FormBox[
     TemplateBox[{"\"MI-Exact\"", "\"MI-Laplace\""}, "PointLegend", 
      DisplayFunction -> (FormBox[
        StyleBox[
         StyleBox[
          PaneBox[
           TagBox[
            GridBox[{{
               TagBox[
                GridBox[{{
                   GraphicsBox[{{}, {
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.396], 
                    RGBColor[0.368417, 0.506779, 0.709798], 
                    AbsoluteThickness[1.6]], {
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.396], 
                    RGBColor[0.368417, 0.506779, 0.709798], 
                    AbsoluteThickness[1.6]], 
                    PointBox[
                    NCache[{
                    Scaled[{
                    Rational[1, 2], 
                    Rational[1, 2]}]}, {
                    Scaled[{0.5, 0.5}]}]]}}}, AspectRatio -> Full, 
                    ImageSize -> {10, 10}, PlotRangePadding -> None, 
                    ImagePadding -> Automatic, 
                    BaselinePosition -> (Scaled[0.1] -> Baseline)], #}, {
                   GraphicsBox[{{}, {
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.396], 
                    RGBColor[0.880722, 0.611041, 0.142051], 
                    AbsoluteThickness[1.6]], {
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.396], 
                    RGBColor[0.880722, 0.611041, 0.142051], 
                    AbsoluteThickness[1.6]], 
                    PointBox[
                    NCache[{
                    Scaled[{
                    Rational[1, 2], 
                    Rational[1, 2]}]}, {
                    Scaled[{0.5, 0.5}]}]]}}}, AspectRatio -> Full, 
                    ImageSize -> {10, 10}, PlotRangePadding -> None, 
                    ImagePadding -> Automatic, 
                    BaselinePosition -> (Scaled[0.1] -> Baseline)], #2}}, 
                 GridBoxAlignment -> {
                  "Columns" -> {Center, Left}, "Rows" -> {{Baseline}}}, 
                 AutoDelete -> False, 
                 GridBoxDividers -> {
                  "Columns" -> {{False}}, "Rows" -> {{False}}}, 
                 GridBoxItemSize -> {"Columns" -> {{All}}, "Rows" -> {{All}}},
                  GridBoxSpacings -> {
                  "Columns" -> {{0.5}}, "Rows" -> {{0.8}}}], "Grid"]}}, 
             GridBoxAlignment -> {"Columns" -> {{Left}}, "Rows" -> {{Top}}}, 
             AutoDelete -> False, 
             GridBoxItemSize -> {
              "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}}, 
             GridBoxSpacings -> {"Columns" -> {{1}}, "Rows" -> {{0}}}], 
            "Grid"], Alignment -> Left, AppearanceElements -> None, 
           ImageMargins -> {{5, 5}, {5, 5}}, ImageSizeAction -> 
           "ResizeToFit"], LineIndent -> 0, StripOnInput -> False], {
         FontFamily -> "Arial"}, Background -> Automatic, StripOnInput -> 
         False], TraditionalForm]& ), 
      InterpretationFunction :> (RowBox[{"PointLegend", "[", 
         RowBox[{
           RowBox[{"{", 
             RowBox[{
               RowBox[{"Directive", "[", 
                 RowBox[{
                   RowBox[{"PointSize", "[", "0.011000000000000001`", "]"}], 
                   ",", 
                   InterpretationBox[
                    ButtonBox[
                    TooltipBox[
                    GraphicsBox[{{
                    GrayLevel[0], 
                    RectangleBox[{0, 0}]}, {
                    GrayLevel[0], 
                    RectangleBox[{1, -1}]}, {
                    RGBColor[0.368417, 0.506779, 0.709798], 
                    RectangleBox[{0, -1}, {2, 1}]}}, DefaultBaseStyle -> 
                    "ColorSwatchGraphics", AspectRatio -> 1, Frame -> True, 
                    FrameStyle -> 
                    RGBColor[
                    0.24561133333333335`, 0.3378526666666667, 
                    0.4731986666666667], FrameTicks -> None, PlotRangePadding -> 
                    None, ImageSize -> 
                    Dynamic[{
                    Automatic, 
                    1.35 (CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[
                    Magnification])}]], 
                    "RGBColor[0.368417, 0.506779, 0.709798]"], Appearance -> 
                    None, BaseStyle -> {}, BaselinePosition -> Baseline, 
                    DefaultBaseStyle -> {}, ButtonFunction :> 
                    With[{Typeset`box$ = EvaluationBox[]}, 
                    If[
                    Not[
                    AbsoluteCurrentValue["Deployed"]], 
                    SelectionMove[Typeset`box$, All, Expression]; 
                    FrontEnd`Private`$ColorSelectorInitialAlpha = 1; 
                    FrontEnd`Private`$ColorSelectorInitialColor = 
                    RGBColor[0.368417, 0.506779, 0.709798]; 
                    FrontEnd`Private`$ColorSelectorUseMakeBoxes = True; 
                    MathLink`CallFrontEnd[
                    FrontEnd`AttachCell[Typeset`box$, 
                    FrontEndResource["RGBColorValueSelector"], {
                    0, {Left, Bottom}}, {Left, Top}, 
                    "ClosingActions" -> {
                    "SelectionDeparture", "ParentChanged", 
                    "EvaluatorQuit"}]]]], BaseStyle -> Inherited, Evaluator -> 
                    Automatic, Method -> "Preemptive"], 
                    RGBColor[0.368417, 0.506779, 0.709798], Editable -> False,
                     Selectable -> False], ",", 
                   RowBox[{"AbsoluteThickness", "[", "1.6`", "]"}]}], "]"}], 
               ",", 
               RowBox[{"Directive", "[", 
                 RowBox[{
                   RowBox[{"PointSize", "[", "0.011000000000000001`", "]"}], 
                   ",", 
                   InterpretationBox[
                    ButtonBox[
                    TooltipBox[
                    GraphicsBox[{{
                    GrayLevel[0], 
                    RectangleBox[{0, 0}]}, {
                    GrayLevel[0], 
                    RectangleBox[{1, -1}]}, {
                    RGBColor[0.880722, 0.611041, 0.142051], 
                    RectangleBox[{0, -1}, {2, 1}]}}, DefaultBaseStyle -> 
                    "ColorSwatchGraphics", AspectRatio -> 1, Frame -> True, 
                    FrameStyle -> 
                    RGBColor[
                    0.587148, 0.40736066666666665`, 0.09470066666666668], 
                    FrameTicks -> None, PlotRangePadding -> None, ImageSize -> 
                    Dynamic[{
                    Automatic, 
                    1.35 (CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[
                    Magnification])}]], 
                    "RGBColor[0.880722, 0.611041, 0.142051]"], Appearance -> 
                    None, BaseStyle -> {}, BaselinePosition -> Baseline, 
                    DefaultBaseStyle -> {}, ButtonFunction :> 
                    With[{Typeset`box$ = EvaluationBox[]}, 
                    If[
                    Not[
                    AbsoluteCurrentValue["Deployed"]], 
                    SelectionMove[Typeset`box$, All, Expression]; 
                    FrontEnd`Private`$ColorSelectorInitialAlpha = 1; 
                    FrontEnd`Private`$ColorSelectorInitialColor = 
                    RGBColor[0.880722, 0.611041, 0.142051]; 
                    FrontEnd`Private`$ColorSelectorUseMakeBoxes = True; 
                    MathLink`CallFrontEnd[
                    FrontEnd`AttachCell[Typeset`box$, 
                    FrontEndResource["RGBColorValueSelector"], {
                    0, {Left, Bottom}}, {Left, Top}, 
                    "ClosingActions" -> {
                    "SelectionDeparture", "ParentChanged", 
                    "EvaluatorQuit"}]]]], BaseStyle -> Inherited, Evaluator -> 
                    Automatic, Method -> "Preemptive"], 
                    RGBColor[0.880722, 0.611041, 0.142051], Editable -> False,
                     Selectable -> False], ",", 
                   RowBox[{"AbsoluteThickness", "[", "1.6`", "]"}]}], "]"}]}],
              "}"}], ",", 
           RowBox[{"{", 
             RowBox[{#, ",", #2}], "}"}], ",", 
           RowBox[{"LegendMarkers", "\[Rule]", 
             RowBox[{"{", 
               RowBox[{
                 RowBox[{"{", 
                   RowBox[{"False", ",", "Automatic"}], "}"}], ",", 
                 RowBox[{"{", 
                   RowBox[{"False", ",", "Automatic"}], "}"}]}], "}"}]}], ",", 
           RowBox[{"Joined", "\[Rule]", 
             RowBox[{"{", 
               RowBox[{"False", ",", "False"}], "}"}]}], ",", 
           RowBox[{"LabelStyle", "\[Rule]", 
             RowBox[{"{", "}"}]}], ",", 
           RowBox[{"LegendLayout", "\[Rule]", "\"Column\""}]}], "]"}]& ), 
      Editable -> True], TraditionalForm], TraditionalForm]},
  "Legended",
  DisplayFunction->(GridBox[{{
      TagBox[
       ItemBox[
        PaneBox[
         TagBox[#, "SkipImageSizeLevel"], Alignment -> {Center, Baseline}, 
         BaselinePosition -> Baseline], DefaultBaseStyle -> "Labeled"], 
       "SkipImageSizeLevel"], 
      ItemBox[#2, DefaultBaseStyle -> "LabeledLabel"]}}, 
    GridBoxAlignment -> {"Columns" -> {{Center}}, "Rows" -> {{Center}}}, 
    AutoDelete -> False, GridBoxItemSize -> Automatic, 
    BaselinePosition -> {1, 1}]& ),
  Editable->True,
  InterpretationFunction->(RowBox[{"Legended", "[", 
     RowBox[{#, ",", 
       RowBox[{"Placed", "[", 
         RowBox[{#2, ",", "After"}], "]"}]}], "]"}]& )]], "Output",ExpressionU\
UID->"1f8dd279-e5b8-46e6-964b-ff26287d34ed"]
}, Open  ]]
}, Open  ]],

Cell[CellGroupData[{

Cell["Under Development", "Section",ExpressionUUID->"665856d4-cc92-444a-bdd6-46b7997a5550"],

Cell[BoxData[
 RowBox[{"(*", " ", 
  RowBox[{
   RowBox[{
    RowBox[{
    "Compute", " ", "MI", " ", "\[IndentingNewLine]", "\[IndentingNewLine]", 
     RowBox[{"pOutputLap", "[", 
      RowBox[{"y_", ",", "k_", ",", "V_"}], "]"}]}], ":=", 
    RowBox[{
     RowBox[{
      RowBox[{"NIntegrate", "[", 
       RowBox[{
        RowBox[{
         RowBox[{"pLaplace", "[", 
          RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", 
         RowBox[{"pInput", "[", 
          RowBox[{"Inp", ",", "0.5", ",", "0.1"}], "]"}]}], ",", 
        RowBox[{"{", 
         RowBox[{"Inp", ",", "0", ",", "1"}], "}"}]}], "]"}], 
      "\[IndentingNewLine]", 
      RowBox[{"pJointApprox", "[", 
       RowBox[{"y_", ",", "Inp_", ",", "k_", ",", "V_"}], "]"}]}], ":=", 
     RowBox[{
      RowBox[{
       RowBox[{"pLaplace", "[", 
        RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", 
       RowBox[{"pInput", "[", 
        RowBox[{"Inp", ",", "0.5", ",", "0.1"}], "]"}], "\[IndentingNewLine]",
        "\[IndentingNewLine]", 
       RowBox[{"pOutput", "[", 
        RowBox[{"y_", ",", "k_", ",", "V_"}], "]"}]}], ":=", 
      RowBox[{
       RowBox[{
        RowBox[{"NIntegrate", "[", 
         RowBox[{
          RowBox[{
           RowBox[{"p", "[", 
            RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", 
           RowBox[{"pInput", "[", 
            RowBox[{"Inp", ",", "0.5", ",", "0.1"}], "]"}]}], ",", 
          RowBox[{"{", 
           RowBox[{"Inp", ",", "0", ",", "1"}], "}"}]}], "]"}], 
        "\[IndentingNewLine]", 
        RowBox[{"pJoint", "[", 
         RowBox[{"y_", ",", "Inp_", ",", "k_", ",", "V_"}], "]"}]}], ":=", 
       RowBox[{
        RowBox[{
         RowBox[{"p", "[", 
          RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", 
         RowBox[{"pInput", "[", 
          RowBox[{"Inp", ",", "0.5", ",", "0.1"}], "]"}], "\n", "\n", 
         RowBox[{"MI", "[", "k_", "]"}]}], ":=", 
        RowBox[{
         RowBox[{"Sum", "[", 
          RowBox[{
           RowBox[{"Sum", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"pJoint", "[", 
               RowBox[{"y", ",", "x", ",", "k"}], "]"}], "*", 
              RowBox[{"Log", "[", 
               FractionBox[
                RowBox[{"pJoint", "[", 
                 RowBox[{"y", ",", "x", ",", "k", ",", "V"}], "]"}], 
                RowBox[{
                 RowBox[{"pOutput", "[", 
                  RowBox[{"y", ",", "k", ",", "V"}], "]"}], 
                 RowBox[{"pInput", "[", 
                  RowBox[{"x", ",", "0.5", ",", "0.01"}], "]"}]}]], "]"}]}], 
             ",", 
             RowBox[{"{", 
              RowBox[{"x", ",", "0.01", ",", "1", ",", "0.05"}], "}"}]}], 
            "]"}], ",", 
           RowBox[{"{", 
            RowBox[{"y", ",", "0.01", ",", "1", ",", " ", "0.05"}], "}"}]}], 
          "]"}], "\[IndentingNewLine]", 
         RowBox[{"MI", "[", "0.2", "]"}], "\n", "$Aborted", "\n", 
         "\[IndentingNewLine]", 
         RowBox[{"MIList2", " ", "=", " ", 
          RowBox[{
           RowBox[{"Function", "[", 
            RowBox[{"k", ",", 
             RowBox[{"MI", "[", "k", "]"}]}], "]"}], "/@", 
           RowBox[{"Range", "[", 
            RowBox[{"0.5", ",", "5", ",", "0.5"}], "]"}]}]}]}]}]}]}]}]}], ";",
    "\[IndentingNewLine]", "\n", "\[IndentingNewLine]", 
   RowBox[{"pIn", " ", "=", " ", 
    RowBox[{
     RowBox[{
      RowBox[{"ProbabilityDistribution", "[", 
       RowBox[{
        RowBox[{"pInput", "[", 
         RowBox[{"x", ",", "0.5", ",", "0.1"}], "]"}], ",", 
        RowBox[{"{", 
         RowBox[{"x", ",", "0", ",", "1"}], "}"}], ",", 
        RowBox[{"Method", "\[Rule]", "\"\<Normalize\>\""}]}], "]"}], 
      "\[IndentingNewLine]", "\[IndentingNewLine]", 
      RowBox[{"Plot", "[", 
       RowBox[{
        RowBox[{"{", 
         RowBox[{
          RowBox[{"PDF", "[", 
           RowBox[{"pIn", ",", "x"}], "]"}], ",", 
          FractionBox[
           RowBox[{"pInput", "[", 
            RowBox[{"x", ",", "0.5", ",", "0.1"}], "]"}], "nIn"]}], "}"}], 
        ",", 
        RowBox[{"{", 
         RowBox[{"x", ",", "0", ",", "1"}], "}"}], ",", 
        RowBox[{"PlotRange", "\[Rule]", "All"}], ",", 
        RowBox[{"PlotLegends", "\[Rule]", "Automatic"}]}], "]"}], "\n", 
      RowBox[{"ProbabilityDistribution", "[", 
       RowBox[{
        RowBox[{"3.9894250911642475`", " ", 
         SuperscriptBox["\[ExponentialE]", 
          RowBox[{
           RowBox[{"-", "49.99999999999999`"}], " ", 
           SuperscriptBox[
            RowBox[{"(", 
             RowBox[{
              RowBox[{"-", "0.5`"}], "+", "\[FormalX]"}], ")"}], "2"]}]]}], 
        ",", 
        RowBox[{"{", 
         RowBox[{"\[FormalX]", ",", "0", ",", "1"}], "}"}]}], "]"}], "\n", 
      TemplateBox[{GraphicsBox[{{{{}, {}, 
            TagBox[{
              Directive[
               Opacity[1.], 
               RGBColor[0.368417, 0.506779, 0.709798], 
               AbsoluteThickness[1.6]], 
              LineBox[CompressedData["
1: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
3/n/AEKVxh8=
               "]]}, 
             Annotation[#, "Charting`Private`Tag$78081#1"]& ], 
            TagBox[{
              Directive[
               Opacity[1.], 
               RGBColor[0.880722, 0.611041, 0.142051], 
               AbsoluteThickness[1.6]], 
              LineBox[CompressedData["
1:eJw1Wnc4le//t4okJzMzUYk+VmUUOq+nyAiVlZRsikIhRbRpKRVJUUQDZW/Z
OyMjlHmOfXA4RCmF3/n+8fvnea7X9dz3+36/3vN+X9cj7ehl5sLGwsLSwnz8
733QhdZaNm5JNpN7s1KjyCBz6Iido4iqwuicE7tQEoO8KzYqJktUF/95158V
F5ghGzV9y3wpagHL6FUHuMJnyFbOC8VPRJ1BL3Ly8Hg1S+5Tvq3zQNQXz2LM
j94ZnSNHxIqUbT1xAVpn0tN9VefJB0nJ2iUxfoh6qncr+OY8OXe6UY0h5Y/E
FP7lLVt/kh+m8MuZyl6B/uSkRKvfL3Kc0OGQe6K3wO8r31Vk9Yd8uelZ1dHC
W5hNPVWwWPCHfDSYyrb5RDAUfh4/+lZykczz8/zVTzEh0KOv+Wcxvki+1B5x
iS51FwPsStvSnvwjm4Z3ux+SfYjPWr/114iyQMFoc5JY7UP8XW3q1nyUBVzs
Z8dGT4WhCeO2l56ywK5gOaPU4hHydbV3XBVihZD8I6VjkY9BO/4kpmATG65z
ZW27KxqB3+7aq7+4cmC7/uM7meciMLZJ1qSvkANtwV7jPbURUOuXTtZYvwpb
2BU+KPk9hVpfAN9AySrULL1Ram+LRLujwcEnCpzw0r4ZtiT/DMeWdt4Ivs8J
kcsOM7LXnyFz97AHfZITbr8lM/2Vo7Ctopbzvwwu8MxFqkmFPgfLM9IqzkPc
yNlxIdJg6DkuX9wotquAG7bnzBfOa77AxDCfPIvsWqRNkQqqaC+w3HXD4ikH
D8xod7TdD8TAMemhrXXnOkT1BejkLL/CgVdbpZZp67FPwvpNv2UsdCOE7CQU
+TBxXGMVV0osTnqm2hPefNhI93dKUYzDxnbzqMQVPni5qW9l6Y1Dmc60mss2
AVTdedsRwfka5bSFOxK+AhBJFAyR3/Ua3YN1zb/LBVA2+mPU7N5r/Nnwq1XF
QRD8zmmJb3fHQyls8IdQqhBcb2601nSOx4T7/JczHML4FP9gTXNYPEzN+fXX
2AjDeeCM++/ReBxobPdXJG1Ajq2cgtHTBNziCD8WGyyCY9av02Zm38Cy+0r2
9TxxMHosuOPF3uL0HhFxC0kJhNhyuZjrvEUO60jovVsSyHLyEsuNeAuDTpMV
NStJ8HruDb6s8Q56bfN71/JKofpG1/HVQe+xfu1WZxYnadisepCT9/Y9JlJd
whXjpfHjNrHe7ct7rJpJWqU2KI1ND95XN0glQtpn5lufswwCn11QeVyRiKS2
b3xjvpux8yPfagmuZNg9F6n2qdiKz0rV9k0qyUhhufrk7XpZ2Gdc+nTFOhlJ
kev2DtrJ4kEu5dxAcjJ+XCt8fpd1G2hlKT3vTD7AwiI0f8ZUDq86DDN2hH/E
75sxGw9s/w9qx5bWDn36iPtFxxya7v2Hxu5014jhj2hOceWKpv+HP5QNEgtq
KeA8+vCDcLYCLCZGQoq+p6Cuo/wfm7kS1q5ct9HfmIYH/vI8WhMqiHKzslXQ
S8MN1QzOoC07sKVdwZ7PMw3713NKBtntADnpm1NPcRqGCpWyy77tQEFSZE6e
UjreKIR0R7buxJQ5vcjoSzp0XP7Tj6WrQnXNt73kwXRkvNlpMbhLDQEl5SUq
v9JxgmTEtTZQDZzyz8qENmbgm1PhfhGSOqSX91VRPDIwpt+X60VowDLxWYPP
ukzUZu+XetG2BzEnbxi7Smfi6hnB9Afymhjk92g6ppYJSasPP55e14RX0P7m
vSczYRhU4DOrqoW7ZlNtnCmZEK1Le6uSpI3if/u7o42zMC99kdZcD3BkKp54
aJ8FD8lvWmZiBA6eEum95puF4grh8sHDBL61TvW5xGRBeCRv23IRgdl3UQPK
9CwkHupfsojdh62m07TK+9lQ0wnkMwzRgdhVfc1Tr7NRkgcbtkYdrE+Ju8+d
l437yrIm7/h1schlrmQ6mA3+F2qJBfG6aCnL8+nfnYME//pq+y8HEKByffn3
SA7kBPaP2x81gJdt9+GYvzkIG/UhqXw0gHMoM7n4clFySmlInN0Qh2kjOiHa
uSi3trawyjbElriDdwXCcyHG2lJTusUIX0iCgkpEHlCVqHSfOITKvR4urZZ5
4Ml6faEz6RDyz9Tk+p7Jw9zZ2+wBgoeRUHvJ+lNkHgp20Q91Tx3Gpat9rwym
8jCd9Mqk8PsRyDDeyTu9yMfP07fWM0rNsEGS5fLq9Hw4/Y3df4zFHDxG1o1J
1fnYtUfj96N95lh4t9ZrZiYf4p/2vgqoMUej7bnsIP0CKEiPifh2W+Dilz2I
mi+A8SeqJKFoBV6lnJHPXIUQYxEUDvWzwpsHKqF/JQpBf9AukFFmhVbjbV22
BwrRnnTbLczqGP5rEPTZGlmImwVVFZQwa1BqGe8yNT5BrBQF8wo28Nt2xmTY
6BNY/dRa527ZgOf26JyQ/ScoRUjUzPXZYI9eH+F/5xPeru0XYg0/ifCq+m50
fUL1IyPXV2vsoF/+lrcpoAjHrQwsMoQc0L9JOmf5YRE+aqh+sz/oAN9rMSdU
EopgR97JeH/VAa8RnhjeUITUiCeb2ekO+Ft8bf9xiWJkObwyE2t0RFrhCb+x
4mJ0CDF+7v3gDD2xbxKibcWYVbtetn3CGb3+ZpUHR4vBkKLMfpZ3wZo9hutT
eUvA4mr9WfaDC5zy1JN97UqgL3n7z4NcV2zI5utnZS1FyZ6SheLZ07AJ/Wsx
LFyKi4FtH9h13BDvPNJQo1CKRvU6fzx1g4JQQcH9Y6WwXbnB/UbbHfv97J8K
pZdCRfcEi8ezM1CPsXoXoVwGY8WnFr5XPWF9p3EgWLMMv6eG+N0GPBHou0/y
4oEyRF089ey6rhfKjf+LsD5RBq958+1CnOdgvLR8fePtMphlel7o+nYODrbv
Tib2lyEyY2ps7zNv3Doo/vz5eBnKFi05RNq88U79Ufu9+TJwFePGq3U+oPMG
GHlwl2PDnGNTXogP/EpN9uxUL8e/mNjcQ9d9Ebrpp2DRg3KUWAs8zH7uh1Qe
9yMpUeWYt/WPVKX6ofV3//1XCeW4qez86uq2ixBurWO7XlCOUNeMmSv5F/H6
eszMgZFybOcTu35y+BJyB3Uam7UrcF9jSfy6y2V0GlyffaBfAZcQh6Jt+Zfx
K7VE2NisAtZ/k+ay1gZC4/Ieh7pTFdCI2CITkhOIfAHlX2VPKsD318PDVPAK
CnTFNmXSKnCXzbfI8No1dCdbHTg3V4HmmD1RlhnXsLj+qbvScgWWlOuOjg1e
g2YfKSdZoBIayQYxr/Wvo/DCqoMJ5EpMMTJLn264gaJ3Mz4REZVISSuWSmq7
iV4epedmsZUQrzv1KJPnFv55nylZn1yJbSbXE6z0b2EvRrkelFZCX+PWrYCS
Wyj51vMyeKISFqF6ccbZwSjlqq31I6ogvov64mLpbVS4vxS3pldhb6RBpYZd
KLLGz5yx/VUFtY2Pvpkmh+LNac1PTizVcCVkfm/6FYpg12/HPQWrEfrGzXU4
7AEMHPle3NSuRsFHmdrtjQ/RbB0skhpajTSloqv3sh+h7Lv56azIavyo1FST
pD5CupVMfn5cNR77Okpt4HmMJ5alVpXZ1Qi4rTSt4/IYR03/RH7vrYbjDobJ
c4kn6DXwEGJXrMH0EYWEX7HhaKrVcuHSqAEldvcV57ZwlOhx56zbV4NXZyeT
9FdFIFb3vYWIZQ02Gpelnj8bAUdiIFwxqAZ9bb+GPYmnoGlY8h9rqkGm3r2e
0ZVI/JTdS0rxqEWqnmH72NALkAf31967xMROoR9fKEQj5KXB1dM3a9Fy8fGB
5QvREBa0YGx+XguFCps7fdwxUGc90/yiqhZ2N5SCtu59iUs9UWF3xOuwYOV4
U6E6FmWRr/RdZevwK7TmSDNPHLjM3qzo7KgDeVX1xFmNODyvS/Na0atD9KW4
n98fxKEwp+awn3cdFOx/yt1nVpN/YfMk5891aJ5zNeHPi4eu0WLtvnam/GM9
axbG4hG6muWaFKUOGzOmS0xEEyAZtHame74OJ+KEYiyCEkB2l2kx3fQZap+o
CgMH3+Cq7pFHuPgZVyrEnCr+vQXbnw/rxbfW43rThJtZRRLEm/bPQbkerC1e
3TPLSVB93dXhvKce8WanLq3TToaLIWd0qkk94vbUfKjKS0bdc8et+/2YjbR7
j7pOzgeEaYpqutXWY03Vlad6n1OQyJsu8bC1HqGVBht38KaibFBvJbOnHhFS
R3kTzVPx455v1T9GPbbrfvpOpqbiaE/zoUciDdhrnCUtxJIOicAQpzy3BsjI
NuWOKmZA7YikXq9PA5w7v0wsWGTg0JZsOdYrDXg8OLd4NDADVxsHpg4+aUBW
/p+PLI0ZGJTYe6m/sAGr/o47rffKRFLxXOgqnkZojT8zDK/KgvqKfa55aiPO
euZzdqTnwtik7MaB/Eb4q68hvR/IhVO01GGNikYYHd6xq50/D480+sfEOhvB
rWg5t88vD+NeJ0QHlxqZ9467VgH78hFDtQw8Z9KEs9G7Pb+PF4C14uD+B/Qm
DGuxsu4IKYbI+mTeq7+aIO2SOXaxshhKtmt6zrF8QfadKcp/rCU4vljrYyH4
BXbmxY59QSXI3nngrbj2F3y8X3r8YmApXBPAlXz/C+6HZ42E1Jah8Zbql5rt
zWipumbCxVkJTVID22O1ZigHPb1oplKJxOcOGieIZkhatwTQrCtxM/VhHMOy
GQH8whl/Upjrv9O8Ra43I/6BtVuRdRUS/3u5wf1bMzCvp76prBr7x2+kr97f
AoV9+S5HG+rgr/Sl/ox+C+i//NIml+uQ7iM60mLcgt62PY7dOz9j43KaaLRV
C7hXPSuvjv6MP/x9N5Q9WhC6NvU99Xw90rU1LI9FtSC9hR58V7kRUo8m/yRN
t0Cp7fau8rlmlDhcTcqca8G9TEZB6IYW2O4SsP70uwVpvEYMF7UWxHRq5jey
tYJ0rNE87nwLRCTv+TGEW3Eo94e4Jr0F/Mlyc6pohVTT8untE61YVeVCLw1r
RaEv3ykd7na8efonui6iFYNZWfp0xXbonnpg1Pq8FQL94K4zbccN7pzkwYRW
sHyOoYu8aMfyEQ63VfmtSPvHIrdVsQO/++JHDlJb4bWWZEY62YnJBUp/h0ob
NqS+JLuMfEfuqbCKg+ptyHBzEzIX6MK1b+R3pVptGM0KMbq5rwvCeS89kvTa
sCZhOf3Dqy5m/z3xL9CmDbTiAUsZm268mO8U3XKnDXp6bqTOoR4YzDZZeFPa
EBx+SKdqGwUCDkG7x4bbsClN29XZloL+FgUJm4k2yL2IOG7ylALf9PtDB362
IfCWheYSKxWxXobeomu/olYhW9fJm4pfU1VhZepfsd16O+u24wN4M/Gpnvfh
V3h7J+tu1huC57PDm93Dv0LS6WVzhMsQNHSHLldHfYXRl6Q//wUPof4lt2Jg
wlc8drLwbKgawuwR67Dx/K/MuerY25YDwyDn/TKvGvqK9fIxc6yHR9B1c0ef
/552nNpq+tzhyhh4Jd8zhofbMZa/S+SIOR3E0Zt9Q+PtSDxDaiv1p8M7zK5h
cLodDsmv9pvF0dHJKvKe+rsd7Qty+pQpOl6N3rHt5enAZyOphuf3pqCU7t70
VbUD0TeszcLqp2Gio/Sh4lYH9v1Vjt6ROYNQtxzXuC2d+Lo7b8vylTlMECl3
1m7vBGuAe45P1Bz0Rd4m+yl3QtZP8NS/jDmw1UZMG2t2Ykl4aUlvZA7+W3z9
/hzuxPr+x/GlxvNwpewMMbvciZhfHId3Sf3EPsv0txxtnbA9xvasqPUXfhEf
hk9f/YaHT/rTrNwW4buzKlu48jsYNXYnzTpZCEpq1f1W6W50T4Ueif7FQfx1
b4+WD+gBzWFu43wvF2GhfvKx0tUe8Jmx/cub4CI+so6G7LrVg4OpPBK+v7kI
26iF83sf9mCvkG1Lu8AaorxazMA0vgey828K1A6uIW5vcpi/VN/DnIcdRJxy
1hACnVPGdWK98GB8su66xU2cfe23v0mqF5f7q6mBj7mJqrMrGm1behH+JpRX
+BU34cfOt7lXqRdDVz/yyOdxE10qqn8Y+3sxkdFn3UzjJmLvB7zdcKYXBzo+
ha09uJb4bx/nsuunXiw8fRqex85DpCd/2Pa9rBd/Iibnukg8hKrgEVPDGub+
U9X7p8R5CO2xqDf/tfbC00bHeHQXD3HowXajmdFeyJC3XpBy4iG8u0yi/Pn7
ILd6LP55CQ9RcD5iZ6hbH+rZ7vbwuq0jyD27bZY8+1Ccs3xNxmcdUaHbF+zp
2wcx2yn9LUHriPoNsl1HrvZBYVGKNvtoHdFbnHdV6GkfaBQzzay8dQQLd0/j
q7I++Nw511nEykvox0ufytjQD+PVx56cvs1LqEbxytZI9COPEbfu+UNeYtPD
v8Pd0v0gmejO5T3lJRb9Oxw5FPrxr+7057IEXiLF9K6tFdGP4sxLmkslvIQA
66zl0ul+xLO12Qn/4CVYFvoE+T37IXShNX7bb16CTq//KuvTD0bsxQeyy7xE
zfc3pkeC+mFm5aI4tYZE+KcfM0l43I+jzqmnYzaRCIpdma5hYT+evNtY7n+Q
RDRaprDblvZj+ZDVY/vDJCLf6EWFd1U/xlmXXVUtSMQTDV8i5ks/RIySl+NP
kogDJDltxmA/rELfTb7wIhE7Vwn9Zaf1Y5br9g+aD4nY+Je1UGSqHxrkO4Wb
L5GI36M9GvsX+pHZZ+9pd41EfCgJ2/V0LQXyEiceTYeRiKjswB9J6ymYWdas
fx1OIm4lu2WUCFHQ7lB+d98zEmEbqaNMk6KAOzLlit4rEsHnubBdS5WCBcLx
nssHErHsNDx+eA8FnD+/awamkogJ69ZEZzIFUYFB5KsZJKLqwAfZhwYUHF/1
We5gHom4KGknM2BDgTRX9dyPchLheETlaK8DBea2imfsqkjEoZus9765UhCr
urotv4ZEyI4nzDado6BBaOvJnQ0kgl/iwtbPFyhI+XnI36yJRCwd0rOuCqBg
u0iGr10ziWjPppUV3qKgScZhEV9JRNlYwXzOXQrC/rwLEuhg8he7L5fxkAKn
VUJtXztJxI1rio/eR1Gg09g5INZNIjyzlivjX1LwcEjj2eseEmE92rzwMp6C
U4s9fEJ9JELF2Nsu4iMFWkkhxhUUEiFxVSc8LIOCse0ZnP+oJIIzU7D2Xi4F
ag9dr8gMkogfwyOLwZ8oaFn/KFVtiET0b8hTul5Ggejonki1YRJRf/COY2A1
BUJ7w3fJjJCInCDryIv1FIwcmL71l4nj0rfXezdTMKxedqd8lESEDv1d8min
wMj09V7fMRJxSbhph1sXBRdHjOMFaCTCyfCVi3M/BaVHX+fEMvGhQK/ndkMU
UDbG+YmMkwjNNKLpOI2CxRaP4SAmlh3kYz06RYGmhNlyMxPzCQ2pmv6g4FdT
XiXvBIn4p5992niBAv+vCqqaTEwLCI7R/8f0p5bjYTMmnjj63+nXLFScU1cQ
smJi+xkBNil2KkxFbgQbMHHn3X/RMauo8LS6EifLxEabR9TEuKioDrV1nWWe
V1bU1PyMm4rNPje/JTKx+tFcN6F1VLCNnps3ZuKPjFfs4SQq/lCaivuYfGTu
3n65np+KkkvOqieYOErmnMZDQSrqB6Wtq5n24C061rp2AxXBp4UVJZj4luW+
M3dFqVjndiHDjmm/P9PyqzglqIiyeDoWxrSv5x3+2FsbqbBNZLR8ZPpjWPrv
bjZpKnZMbPLKZfqrxaLx7NJWKg7ZyLeGDZAIvens1ZflqFj/lBZlx/R30e2X
cb+3UzFXdkhSghkP7ws92+eUqfhpbu9yopcZHxZWnud3UmEvvVapnxlfT6bA
xVCl4nTsiWKTLhJxZROf9sQeKpLvntGaY8bnfMGfjlPaVOY8SKjIt5MId/NB
rxEyFYqGYwtGbSTCMiQrgapDxSqy6bpjX0iEAt1y7TcTKgICPufdq2bWDwvj
0ntHqPjKbsv4UUEidIr3+5DNqQiq4CrULSMRxx8q97w9RgX1ygept4Uk4q7K
mg++TlSkCDZ8s2bmr+DzFVs5VyomdNzMSMz8jmP9xd97moomjpUnqe+Z8tsG
A3Q8qVCvW76RHEciRn2LDvL7UwGVlR9Rj0mEd1/mcvVlKu57lp97+YCZnweS
Mv2vUKFwkXfu/l2m/A2RYgM3qTAsW9m27TpTvwKvibSHTP3O7z1ixaxncUsy
90zeUCGV+Fb4vT6Tn6somfU9FfQSL+Mj+0lE3hfSj+wkKoylRGWHtUnEl9h/
1hJpTP7aP3407GCet69TfrKACuGYqL0vxZh8Q+5+vtNMhU2SToApjZcYnb4a
pN1GhdlZ48TkAV7ivJXfjpl2KgqmNQ4wunmJO3JOz626qVgwVOHQauIlcuu1
3WRHmPYc7djen87sB6QZrqpFKhpPXoK9Ly/RVnzvtuESFTStGuWVs7zEk7Nb
VzevUKFxwZn7gQsvwV9/nL2bYwC2nHbZF44ycXD1EoN3AO7LQscDNXiJ9X9f
/BDfPAAn4/a9Z36tI3jGDvT6GA3gmJ2cu8bpdUTjU+rxPyYDiNMyyWm2XUeE
6l7uunJkAENfhBuPWa4j1sand961HMCLDTqxe/evI7hPirfG2g3AlSc2y1x8
HcH1daa6wWcAvrnXnpfV8xAcpdFpm2MGsJcyViUqxUN0a+saUF4NoGzhy7ol
fh4irZBOffF6AK2GgZKtq3mI47l7+fnfD+CwevQPo+m1REYKxZclcwDzlH6Z
sKK1hG3MZs2+2gEYLt055nB0LZHv/6Eqcm4AqhrR06NXuYkzqp+61hgNgs+4
U0IjnYv4YOgYV/JrEE/6LMjP6ziIn5Ni60cjh7B289a5o+ysRI65PnuE7jCs
Ztec8dFcxFWD4Njs0WHMFz92KS2bQ5vtaRuV8WF8YRMcNU6aw5YLRqIfJ4eh
LRvoPfF4DvWv+cITZoZxJmG0zdJpDoJ/X4Y8WRzGhVWdb6ZXzyExJdfjHO8I
hF5Vv1Y2/YEWfpqWgvoIri/qfpL6MQMZ+Ybf73ePYFWsJLm4dwYXkJqzWWsE
G1TD4s/UzkDsrK+yODEClhVl6b8xM3CuZtnMfXAEDke1nEcMZvD7oig37eQI
8oh0rnZ9BqT7Dn5PCB6BqVm7590mOu5VaKVZ3hmBhgZ7b0UCHT/eK4Rw3h9B
8IHVaVsC6Kjw5lU982gEWjpfQhy20eHM1Ra2M3oEi9nR9NKbk0jcZa1fnj6C
71srFtbpTkDp3qnc/p4RnFQlZ7UOjiHS69iDR/0j4C4qmuQrGcOKhaHz/oER
WCQr/vB/PoYWqf/4342O4OP7nfvLj4zhfM60h8fsCKT5q1v+Kx9FNvXC1n+r
R8GdIdrjmjQCTY1bEaI7R2Fh3Soh+mwInY6i6i9VR5lzgc4nG/8h+DxM/bZJ
YxRJXCZ3y08M4ePIdzE57VE8J89dnt40hI0Riq/V9UZxrP+xfPvHQbDNdqaY
Hx9F7rtHUeX1A2hIlq99cGMU67P1FrKZdfh0Z8lpvuBRBJX0hbMw+wYHmwX3
09ujeCnulNXZSQHZ+opJTOgozkWlKGufpyCd8+vX5MhRdMzRUumJ/YhwDqTW
Jo/ii05gasXmPthsbP7D+nUUn6Kd25T1usEr6p3zqWMUMbrUE1HC3SgXEDp/
4fsoFCR0D+uOdUF2zQkarW8UAQkCMS53u8CYG+1spo3iUWVuX23Ld9z4vJz1
cnkUQudnGN1nviHRV8lzz/YxdEq7TmW2tuO4V6v8nALTD0bkXZLJ7Vjn7jvy
UXkMgzOMnsob7fC2K7TZpDaG6WYbj2rVdmgd1DPiJMawUfGdSnj0V3yRspXr
sBzDwrvbq2R92/Cz/sHguetjGHu5NB56mDmXr+04/e/mGM5NF9PVd7SgwliC
cTtkDEabzVVIAi149CX576v7Y/Bom/cOT23G9rZawaanYyhZkyNdWfkFtl2s
+vLJY/gSwvFseLkRNWMXPgy0jcFkm6T+k5zP+ChXvMOjYwyPLB5Ri25+Rrgb
R/7vb2OgLG/k3W32GfaTT6pIfWPY+aI8ZeNMHRan0/r2jo1B3O9UabxyHZR/
jZNe/B2D3le1l6rFNYjisPU120LDRp5ttxZJVVh22qDWKktDwu7zsYdolXCu
bPl5WJ6G8Gd3to+UVULlhs5FEyUaXobL7PvuXYm6FbkAg900cBmQTZu6KrCw
OHeVbEzDDnfDhOSschydvXt/uy8NH1rX88zJl6LoiI5xkh8NA9LRtD/jJZBJ
/8cj50/DzQj4nE8uwbSn18OtV2jY5V0ul65QgmC6xeNNd2jw0lPXP6FejOwx
qWfCMTRMu1Y5D538BIH+nHi2Khq6Lo9rfPyTh4c8QUF5NTRYylbKHavIA7eW
7rGzn2nYlFAy430/D2xRbes6v9Ag8lwGPzbm4Ycp41JSFw287G73OYxy0VIt
d/gIg4YHkRmdlNxsGM8z5Ff9oIH0/JzVtpBs1MnkcRTO08BIZgy1WWaj9OqB
ws2LNLRkdgSb/8pC6h7HrQsc4/A0duqlaWUhNCX670uxcYSLusZ1fM8AV69j
p5nkOBoY7Xv5UjNwi5t5Md00jr78huZ3NzNw+VS+6/mt49ifYO/cp5wBd+mO
Vl2VcagdOh90KjQdBk95EycPjEMn87ZtZ3IqVgVdt9hzfhzOeZ5Zp/SSkT1L
/1PqM46pCJmXPmuS4ex6LFbPbxxOBuu6/zUmoeqw8oT55XGcfew/fdMyCbdk
+q56BjO/F1tvNjmbCI663R8Sno9j0wZJbcH0d8jWfnNke8w4Ztd7Oz4JeAfn
DNKv9FfjyBB26Lp24B2qXowSJQnjWBSujvnQ+xY3PSI6v6eMY7vwtimZ9W/B
JjDDylvxP3n8SYx7Cci8feJ9RNU4VnZNvN1gkwDHfzXG4rXjsL9l+ztBMQEV
Iy+fyTWOo0cxqmK0NR438o0UdTrHsUds12ZWqXiw2r4/dmliHDN/i1dN18eB
Jck2bZB/Aqk7VhRW1ceghv7NM1ZoAoFzLea9gTEIVTFVshGZwIc4nvceKjHY
kK+T0iE5gePeR6ryn0dDqVbuQ53cBL75qrLU+r6AzciPd6nkCXDuESkNNIpi
9puzrmf2TSD9u7eQ3poo0M6ObJVj1vVs9o6AzNpn8Jn/9ua14QTcZl1XPPWe
4R57cXyEBVM+NdbI1CgSBdK3XwW4T+DqVkv7p+cjcMWFxVbDYwJXjr2fntkd
Ad0kf8l5rwnwJb3+3M0SgRaVszEeFyYgXRF70+JJOGgwfWF/bQK2syYWJZ+e
QNhWPFIvcgJnT+7qct72GN4v0h7wV0zg9uGg+bPZocj79+J2RdUEnBwUH352
CsU/25Dr3rUTmHLePzgvEIrbm09eaGucwPizhlPpfvcR85H75JNvExBb2e5x
58A9VJe4KPBPTYCXTShlF+cdcEubylYwJrB07p7r+4rbOHJTe5P3jwl0Og2K
f7lyG936AoJtCxNo/pyzpPgnBNMtZX8fs03ix5roMe5fwRAZEm/gE5nEBZXQ
jj7eWzh5gLO6XGwSzUbVCtXtNxH//kfJeclJ1AXs/KkXfRMKZz9ntspMgrJ+
0ybB7Tex76ffi8eKk1hLEvBIOXQD7pxtbnw6k3iX+yg+KfMaiv+7w8XnOYlD
F38u3yu4DPX2DU1Pzk2iTDR2z/Gzl5Ee+P6xoM8kLm9g5DGkLiO+qUZM5NIk
NCpsjrreDcAdr1WKUjeY6/WDKo64+MMs64apYuQkjnAeF47GRTSc4BdOi5rE
nhumbCtLftDliO9WiZ7E4QZBGd5iP2hYljupxk2CpCuWtm2vHyQWVi5oJU+C
e23yE4kDFzCmGfTCsGQSSdkagUXePrAf4rFrKJuE214/EUktH3Tfj9lsUjmJ
DQX7N/Jx+KCx99PHI3WTUD7v2J/yzBuZVxZLrNomMWrpIRZRfR6BFReHXEaZ
5/u2ytA1z+GnO2fiGG0Srx12Hxpaew6eAs/Ouk1Ooq9sQSiz0AsOzrk/z85M
4oWaTJUfuxf0Vv/k9F2cxAOXWV/fOA/wHfRWuMFLR7XHBfG9q88grpy0VpOP
jpvTX6KR7g6VPSnjswJ0SIp55BAn3HFIjvbOQZSOsY07FnZlu+HealvpfVvo
mFnIm444fxoclYbCLJp0WD8d/rdaxBXhmmPzedp0jOpyjBW3uEAm89ZXL9Bx
MCZSz+ieC4jXpY8ounREKn5UpK44I+iq2trSw3TEVZ8xzpp3wi8taZYrLnRQ
vM2shNkdEZxV0q92mo7ZWS+P/+ocIPifTfGUOx271TUezz9wwC6xZwEnz9Gx
z71pY7S4A8795vmpfZmO2LHqHeywx3j27/G/j+j4Ux/P0DI8iUsKkXVZ4XQo
K71+K0OzAdebXe/PRNJB3yGfrX7HBrLhns490XQsvW5062o4Acfzw/2F75j6
d3GwpdgfR69iy1f/IjqSvm7NqCm2QuwkXxB7KR3JDxscDvhYwTHJXPZBOZN/
xxbTWHkrjG/5dimuho5SUlpsT9RR/BLvl6xtoUO96uUW9euW4OOmnxIcoWOQ
JqMZctEc7bWKfK/G6Nhmf9wzZY85ooK9CrdN0JEvNab04J8ZNrLO8WgxmPdQ
4Wf2nrfMoPD7T4bDHzrG7xNT12JMoTfK+S913RQ4bAO8b34/jDVvDN/uXj+F
xIH9WmLeh9HocP9QBf8UdheafB3lOQzzPt7XHRumIOefQBHQOwT7diG9f9JT
6J6rMbWvMcblis2PDNWncKT9n/PPv4YgX3PZ83X3FAJt9z5yem8INvL7QRut
KaR5n+bpMDfEnYLtaueIKYStHPLMTTNAZPqO7siDU1gdmVq42Ucf6a+wZdh2
CuqlP07oyx7AXMAcp4PDFI5NSUv8G9CFutX7yT6nKewjL9CvvdLFJxIp69vp
KcxGe9+dFdVFzbX+fQ3eU2jlKvRXFtNBj2OQXWYIk08a9bkGeR82YoeO8t0p
GK95zTayah/sxUdkP96fwnBFkxbxhcDoV+Ppt4+mwLN0/PBvewIzuhJBz19M
4Z/WZIe3BLB626cX11KnEP6iOrmGRxuG7F5XltKnQDkpHi2eroVQioxjQNYU
Pm+VVHhioQW+qHvyvvlTEGSvW+yM04TEmuP5pyqmYNH1pcpZbw92Tv7uONQ5
BYGSGM6uCnVcqP1Y0PB9CqLjP5Oa/NSRn2D/0qBnCmKKxT8z/1PHXps65/3U
KezQizmlG6UGg6Znc2oT//PXEy/1QFWcTFPnk1yegu2aAa51HjuRe/w3bwvL
NHZT9RUfyO8E7+pCnpvs0+B91dm1MLoDZTZ7uca5pqF+80HfNecdkOHWXckW
mMbey93s691VMOpsOmUsPw3rlL5Elz2KIK8XmFz5bxoqI2usSU0KePapnZap
NI0ZyrEL6o4KMOA/NiyiOo3TV3X2jT/6D8mltj3D5Gm0dsxFHV+Rh6fY2c+B
FtOQMTp/65TENtRUK9YqW00jiLPlbl61LKTOM6oGrafB7ZFQpn1OFi213mUG
dtN45j4V1lm3FTsv+OcJuk+j50gJf9ftLVhoDnn78eo0FC/WeTTukcGRywYJ
djemcXC6az5+XhqJstyv+YOnkTL82NonXRrHgx7EXLo3DdfvJ3znt0ujaHtE
uO7TaVgqRR+Q/G8TrgW/vt6bPI3+LYe8kpMlsO4i50GHlGnwe7zpYOhI4MVp
D/7RtGlocX4Vi+gXR5bRngRGNlOeqvjzsQ3iGOVrrWIrnYZG0pXUfS9EYfSK
hUv+6zSu9L0PkO0Wxvcw15aUDia/Ur9Sx9vCcL7eGLXz+zROWdizFasJ44pz
lLx23zQqq2Knip4KIX27itHhMeb+j8vZIY6CEM6ze3jh7zSSAko5DBT4kZBY
fXRxaRr1fDeEq8f5oPLiP6mrLAwkMljfbk/kg2HQQtqdVQw4lSSqJ2zjQ6BO
WGs0iYGtsxVyv1XWY7C5RLBiMwM89yRYgzp44FW+pU9PlgGBq9oFPe48+Jd5
722DHAMlN/66Z7HyQCjyqHqHIgN3d3pxFO5aCwObaSuaBgPOOfe9XiauQQpN
MprXmIHNscIsBa2rcedlsH3/IQYoHm4Bcd6r4Ww2tTXVlAHDkGFVdqHVkCgq
SjexYmDpr9+qLbarcC/sRE2oIwNeDHKSJisHXHUr79u4MOXF9cg5pbBj/5/t
pgqnGbBmyxHrPc6OP46LPQ0eDKhxuVl8KGDDafXns9z+DGy4o8GmEcIK3UmW
vO7LDOZcbOx7XJsVUnGnA5OvMPXl2r0pcY4FnWt2cx68xYAdkW2U7sKCA32d
EnfDGBCS9F+aT1kmy9wSNOB8x4CB7P1zhR2L5OXdgeu+JTLQtODabe20SO6a
Gmp794Gp78uE4sYff8iPrLJO6mUwUCdJRKzf8Ie8st3MJ7iIAQ51rfeHfRfI
3ZSC3ZalDMjNDOl+Ji2QcyOkl7dUMBBh6K5J/fiL7Lk8c6eqloHIGmFG4+RP
cm9r2Cv2rwxmnQqn/rw8T84PWXD62sHA49Hanymb58kRWnbyCd8ZUBzu2f21
cY5s9FYpe38/A9cLVjoObZ4jF15q+nx9nAEWM2rHeeosOVJRLcyUzsD3/H0K
i09myecHYyykGQzMNxGUKr1ZspzxWUrZPAMHMgXd1mXOkDlY2988WmDgXkzT
hetuM2Rqjpa7/SIDJhWnDm6RmSEXuScoqywxUE3bbDeYxyA/k1r7c2WFgf//
3/n/AEKVxh8=
               "]]}, 
             Annotation[#, "Charting`Private`Tag$78081#2"]& ]}}, {}, {}}, {
         DisplayFunction -> Identity, Ticks -> {Automatic, Automatic}, 
          AxesOrigin -> {0, 0}, FrameTicks -> {{Automatic, 
             Charting`ScaledFrameTicks[{Identity, Identity}]}, {Automatic, 
             Charting`ScaledFrameTicks[{Identity, Identity}]}}, 
          GridLines -> {None, None}, DisplayFunction -> Identity, 
          PlotRangePadding -> {{
             Scaled[0.02], 
             Scaled[0.02]}, {
             Scaled[0.05], 
             Scaled[0.05]}}, PlotRangeClipping -> True, ImagePadding -> All, 
          DisplayFunction -> Identity, AspectRatio -> 
          NCache[GoldenRatio^(-1), 0.6180339887498948], Axes -> {True, True}, 
          AxesLabel -> {None, None}, AxesOrigin -> {0, 0}, DisplayFunction :> 
          Identity, Frame -> {{False, False}, {False, False}}, 
          FrameLabel -> {{None, None}, {None, None}}, 
          FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
          GridLines -> {None, None}, GridLinesStyle -> Directive[
            GrayLevel[0.5, 0.4]], 
          Method -> {
           "DefaultBoundaryStyle" -> Automatic, "DefaultMeshStyle" -> 
            AbsolutePointSize[6], "ScalingFunctions" -> None, 
            "CoordinatesToolOptions" -> {"DisplayFunction" -> ({
                (Identity[#]& )[
                 Part[#, 1]], 
                (Identity[#]& )[
                 Part[#, 2]]}& ), "CopiedValueFunction" -> ({
                (Identity[#]& )[
                 Part[#, 1]], 
                (Identity[#]& )[
                 Part[#, 2]]}& )}}, PlotRange -> {All, All}, 
          PlotRangeClipping -> True, 
          PlotRangePadding -> {{Automatic, Automatic}, {
            Automatic, Automatic}}, Ticks -> {Automatic, Automatic}}],FormBox[
        
         FormBox[
          TemplateBox[{
            TagBox[
             FrameBox[
              StyleBox["1", Smaller, StripOnInput -> False]], "Placeholder"], 
            
            TagBox[
             FrameBox[
              StyleBox["2", Smaller, StripOnInput -> False]], "Placeholder"]},
            "LineLegend", DisplayFunction -> (FormBox[
             StyleBox[
              StyleBox[
               PaneBox[
                TagBox[
                 GridBox[{{
                    TagBox[
                    GridBox[{{
                    GraphicsBox[{{
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.5], 
                    Opacity[1.], 
                    RGBColor[0.368417, 0.506779, 0.709798], 
                    AbsoluteThickness[1.6]], {
                    LineBox[{{0, 10}, {20, 10}}]}}, {
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.5], 
                    Opacity[1.], 
                    RGBColor[0.368417, 0.506779, 0.709798], 
                    AbsoluteThickness[1.6]], {}}}, AspectRatio -> Full, 
                    ImageSize -> {20, 10}, PlotRangePadding -> None, 
                    ImagePadding -> Automatic, 
                    BaselinePosition -> (Scaled[0.1] -> Baseline)], #}, {
                    GraphicsBox[{{
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.5], 
                    Opacity[1.], 
                    RGBColor[0.880722, 0.611041, 0.142051], 
                    AbsoluteThickness[1.6]], {
                    LineBox[{{0, 10}, {20, 10}}]}}, {
                    Directive[
                    EdgeForm[
                    Directive[
                    Opacity[0.3], 
                    GrayLevel[0]]], 
                    PointSize[0.5], 
                    Opacity[1.], 
                    RGBColor[0.880722, 0.611041, 0.142051], 
                    AbsoluteThickness[1.6]], {}}}, AspectRatio -> Full, 
                    ImageSize -> {20, 10}, PlotRangePadding -> None, 
                    ImagePadding -> Automatic, 
                    BaselinePosition -> (Scaled[0.1] -> Baseline)], #2}}, 
                    GridBoxAlignment -> {
                    "Columns" -> {Center, Left}, "Rows" -> {{Baseline}}}, 
                    AutoDelete -> False, 
                    GridBoxDividers -> {
                    "Columns" -> {{False}}, "Rows" -> {{False}}}, 
                    GridBoxItemSize -> {
                    "Columns" -> {{All}}, "Rows" -> {{All}}}, 
                    GridBoxSpacings -> {
                    "Columns" -> {{0.5}}, "Rows" -> {{0.8}}}], "Grid"]}}, 
                  GridBoxAlignment -> {
                   "Columns" -> {{Left}}, "Rows" -> {{Top}}}, AutoDelete -> 
                  False, GridBoxItemSize -> {
                   "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}}, 
                  GridBoxSpacings -> {"Columns" -> {{1}}, "Rows" -> {{0}}}], 
                 "Grid"], Alignment -> Left, AppearanceElements -> None, 
                ImageMargins -> {{5, 5}, {5, 5}}, ImageSizeAction -> 
                "ResizeToFit"], LineIndent -> 0, StripOnInput -> False], {
              FontFamily -> "Arial"}, Background -> Automatic, StripOnInput -> 
              False], TraditionalForm]& ), 
           InterpretationFunction :> (RowBox[{"LineLegend", "[", 
              RowBox[{
                RowBox[{"{", 
                  RowBox[{
                    RowBox[{"Directive", "[", 
                    RowBox[{
                    RowBox[{"Opacity", "[", "1.`", "]"}], ",", 
                    InterpretationBox[
                    ButtonBox[
                    TooltipBox[
                    GraphicsBox[{{
                    GrayLevel[0], 
                    RectangleBox[{0, 0}]}, {
                    GrayLevel[0], 
                    RectangleBox[{1, -1}]}, {
                    RGBColor[0.368417, 0.506779, 0.709798], 
                    RectangleBox[{0, -1}, {2, 1}]}}, DefaultBaseStyle -> 
                    "ColorSwatchGraphics", AspectRatio -> 1, Frame -> True, 
                    FrameStyle -> 
                    RGBColor[0.24561133333333335`, 0.3378526666666667, 
                    0.4731986666666667], FrameTicks -> None, PlotRangePadding -> 
                    None, ImageSize -> 
                    Dynamic[{Automatic, 
                    1.35 (CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[
                    Magnification])}]], 
                    "RGBColor[0.368417, 0.506779, 0.709798]"], Appearance -> 
                    None, BaseStyle -> {}, BaselinePosition -> Baseline, 
                    DefaultBaseStyle -> {}, ButtonFunction :> 
                    With[{Typeset`box$ = EvaluationBox[]}, 
                    If[
                    Not[
                    AbsoluteCurrentValue["Deployed"]], 
                    SelectionMove[Typeset`box$, All, Expression]; 
                    FrontEnd`Private`$ColorSelectorInitialAlpha = 1; 
                    FrontEnd`Private`$ColorSelectorInitialColor = 
                    RGBColor[0.368417, 0.506779, 0.709798]; 
                    FrontEnd`Private`$ColorSelectorUseMakeBoxes = True; 
                    MathLink`CallFrontEnd[
                    FrontEnd`AttachCell[Typeset`box$, 
                    FrontEndResource["RGBColorValueSelector"], {
                    0, {Left, Bottom}}, {Left, Top}, 
                    "ClosingActions" -> {
                    "SelectionDeparture", "ParentChanged", 
                    "EvaluatorQuit"}]]]], BaseStyle -> Inherited, Evaluator -> 
                    Automatic, Method -> "Preemptive"], 
                    RGBColor[0.368417, 0.506779, 0.709798], Editable -> False,
                     Selectable -> False], ",", 
                    RowBox[{"AbsoluteThickness", "[", "1.6`", "]"}]}], "]"}], 
                    ",", 
                    RowBox[{"Directive", "[", 
                    RowBox[{
                    RowBox[{"Opacity", "[", "1.`", "]"}], ",", 
                    InterpretationBox[
                    ButtonBox[
                    TooltipBox[
                    GraphicsBox[{{
                    GrayLevel[0], 
                    RectangleBox[{0, 0}]}, {
                    GrayLevel[0], 
                    RectangleBox[{1, -1}]}, {
                    RGBColor[0.880722, 0.611041, 0.142051], 
                    RectangleBox[{0, -1}, {2, 1}]}}, DefaultBaseStyle -> 
                    "ColorSwatchGraphics", AspectRatio -> 1, Frame -> True, 
                    FrameStyle -> 
                    RGBColor[0.587148, 0.40736066666666665`, 
                    0.09470066666666668], FrameTicks -> None, 
                    PlotRangePadding -> None, ImageSize -> 
                    Dynamic[{Automatic, 
                    1.35 (CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[
                    Magnification])}]], 
                    "RGBColor[0.880722, 0.611041, 0.142051]"], Appearance -> 
                    None, BaseStyle -> {}, BaselinePosition -> Baseline, 
                    DefaultBaseStyle -> {}, ButtonFunction :> 
                    With[{Typeset`box$ = EvaluationBox[]}, 
                    If[
                    Not[
                    AbsoluteCurrentValue["Deployed"]], 
                    SelectionMove[Typeset`box$, All, Expression]; 
                    FrontEnd`Private`$ColorSelectorInitialAlpha = 1; 
                    FrontEnd`Private`$ColorSelectorInitialColor = 
                    RGBColor[0.880722, 0.611041, 0.142051]; 
                    FrontEnd`Private`$ColorSelectorUseMakeBoxes = True; 
                    MathLink`CallFrontEnd[
                    FrontEnd`AttachCell[Typeset`box$, 
                    FrontEndResource["RGBColorValueSelector"], {
                    0, {Left, Bottom}}, {Left, Top}, 
                    "ClosingActions" -> {
                    "SelectionDeparture", "ParentChanged", 
                    "EvaluatorQuit"}]]]], BaseStyle -> Inherited, Evaluator -> 
                    Automatic, Method -> "Preemptive"], 
                    RGBColor[0.880722, 0.611041, 0.142051], Editable -> False,
                     Selectable -> False], ",", 
                    RowBox[{"AbsoluteThickness", "[", "1.6`", "]"}]}], 
                    "]"}]}], "}"}], ",", 
                RowBox[{"{", 
                  RowBox[{
                    TagBox[#, HoldForm], ",", 
                    TagBox[#2, HoldForm]}], "}"}], ",", 
                RowBox[{"LegendMarkers", "\[Rule]", "None"}], ",", 
                RowBox[{"LabelStyle", "\[Rule]", 
                  RowBox[{"{", "}"}]}], ",", 
                RowBox[{"LegendLayout", "\[Rule]", "\"Column\""}]}], "]"}]& ),
            Editable -> True], TraditionalForm], TraditionalForm]},
       "Legended",
       DisplayFunction->(GridBox[{{
           TagBox[
            ItemBox[
             PaneBox[
              TagBox[#, "SkipImageSizeLevel"], 
              Alignment -> {Center, Baseline}, BaselinePosition -> Baseline], 
             DefaultBaseStyle -> "Labeled"], "SkipImageSizeLevel"], 
           ItemBox[#2, DefaultBaseStyle -> "LabeledLabel"]}}, 
         GridBoxAlignment -> {"Columns" -> {{Center}}, "Rows" -> {{Center}}}, 
         AutoDelete -> False, GridBoxItemSize -> Automatic, 
         BaselinePosition -> {1, 1}]& ),
       Editable->True,
       InterpretationFunction->(RowBox[{"Legended", "[", 
          RowBox[{#, ",", 
            RowBox[{"Placed", "[", 
              RowBox[{#2, ",", "After"}], "]"}]}], "]"}]& )], "\n", "nIn"}], 
     " ", "=", " ", 
     RowBox[{
      RowBox[{
       RowBox[{"NIntegrate", "[", 
        RowBox[{
         RowBox[{"pInput", "[", 
          RowBox[{"x", ",", "0.5", ",", "0.1"}], "]"}], ",", 
         RowBox[{"{", 
          RowBox[{"x", ",", "0", ",", "1"}], "}"}]}], "]"}], "\n", 
       "0.25066268375731465`", "\n", 
       RowBox[{"Manipulate", "[", "\[IndentingNewLine]", 
        RowBox[{
         RowBox[{
          RowBox[{"nOutLap", " ", "=", " ", 
           RowBox[{"Sum", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"pOutputLap", "[", 
               RowBox[{"y", ",", "k", ",", "V"}], "]"}], "*", "0.005"}], ",", 
             
             RowBox[{"{", 
              RowBox[{"y", ",", "0.0", ",", "1", ",", "0.005"}], "}"}]}], 
            "]"}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"nOut", " ", "=", " ", 
           RowBox[{"Sum", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"pOutput", "[", 
               RowBox[{"y", ",", "k", ",", "V"}], "]"}], "*", "0.005"}], ",", 
             
             RowBox[{"{", 
              RowBox[{"y", ",", "0", ",", "1", ",", "0.005"}], "}"}]}], 
            "]"}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"Plot", "[", 
           RowBox[{
            RowBox[{"{", 
             RowBox[{
              FractionBox[
               RowBox[{"pOutput", "[", 
                RowBox[{"y", ",", "k", ",", "V"}], "]"}], "nOut"], ",", 
              FractionBox[
               RowBox[{"pOutputLap", "[", 
                RowBox[{"y", ",", "k", ",", "V"}], "]"}], "nOutLap"]}], "}"}],
             ",", 
            RowBox[{"{", 
             RowBox[{"y", ",", "0.0", ",", "1"}], "}"}], ",", 
            RowBox[{"PlotRange", "\[Rule]", "All"}]}], "]"}]}], ",", 
         RowBox[{"{", 
          RowBox[{"V", ",", "1", ",", "1000"}], "}"}]}], "]"}], "\n", "\n", 
       RowBox[{"Sum", "[", 
        RowBox[{
         RowBox[{
          RowBox[{"pOutputLap", "[", 
           RowBox[{"y", ",", "k", ",", "V"}], "]"}], "*", 
          FractionBox["0.01", "nOutLap"]}], ",", 
         RowBox[{"{", 
          RowBox[{"y", ",", "0", ",", "1", ",", "0.01"}], "}"}]}], "]"}], 
       "\n", "1.0000000017787507`", "\n", "V"}], "=", 
      RowBox[{
       RowBox[{
       "5", "\n", "5", " ", "\[IndentingNewLine]", "Normalize", " ", "via", 
        " ", "lists", "\n", "dx"}], "=", 
       RowBox[{"dy", "=", "0.1"}]}]}]}]}], ";", "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{"pNormalizedList", "[", 
     RowBox[{"y_", ",", "Inp_", ",", "k_", ",", "V_"}], "]"}], ":=", 
    RowBox[{
     RowBox[{
      RowBox[{"p", "[", 
       RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], 
      SuperscriptBox[
       RowBox[{"(", 
        RowBox[{"Total", "[", 
         RowBox[{
          RowBox[{"Function", "[", 
           RowBox[{"yp", ",", 
            RowBox[{
             RowBox[{"p", "[", 
              RowBox[{"yp", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", 
             "dy"}]}], "]"}], "/@", 
          RowBox[{"Range", "[", 
           RowBox[{"0", ",", "1", ",", "dy"}], "]"}]}], "]"}], ")"}], 
       RowBox[{"-", "1"}]], "\[IndentingNewLine]", 
      RowBox[{"(*", 
       RowBox[{"Sum", "[", 
        RowBox[{
         RowBox[{
          RowBox[{"pNormalizedList", "[", 
           RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", "dy"}], 
         ",", 
         RowBox[{"{", 
          RowBox[{"y", ",", "0", ",", "1", ",", "dy"}], "}"}]}], "]"}], 
       "*)"}], " ", 
      RowBox[{"(*", " ", 
       RowBox[{"Output", " ", "\[Rule]", " ", "1"}], " ", "*)"}], " ", 
      "\[IndentingNewLine]", 
      RowBox[{"pOutput", "[", 
       RowBox[{"y_", ",", "k_", ",", "V_"}], "]"}]}], ":=", 
     RowBox[{
      RowBox[{
       RowBox[{"Sum", "[", 
        RowBox[{
         RowBox[{
          RowBox[{"pNormalizedList", "[", 
           RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", 
          RowBox[{"pInput", "[", 
           RowBox[{"Inp", ",", "0.5", ",", "0.1"}], "]"}], "*", "dx"}], ",", 
         RowBox[{"{", 
          RowBox[{"Inp", ",", 
           SuperscriptBox["10", 
            RowBox[{"-", "8"}]], ",", "1", ",", "dx"}], "}"}]}], "]"}], 
       "\[IndentingNewLine]", 
       RowBox[{"(*", " ", 
        RowBox[{
         RowBox[{
          RowBox[{"Sum", "[", 
           RowBox[{
            RowBox[{
             RowBox[{"pOutput", "[", 
              RowBox[{"y", ",", "k", ",", "V"}], "]"}], "*", "dy"}], ",", 
            RowBox[{"{", 
             RowBox[{"y", ",", "0", ",", "1", ",", "dy"}], "}"}]}], "]"}], 
          " ", "Output"}], " ", "\[Rule]", " ", "0.9999991426275873"}], " ", 
        "*)"}], "\[IndentingNewLine]", 
       RowBox[{"pJoint", "[", 
        RowBox[{"y_", ",", "Inp_", ",", "k_", ",", "V_"}], "]"}]}], ":=", 
      RowBox[{
       RowBox[{"pNormalizedList", "[", 
        RowBox[{"y", ",", "Inp", ",", "k", ",", "V"}], "]"}], "*", 
       RowBox[{"pInput", "[", 
        RowBox[{"Inp", ",", "0.5", ",", "0.1"}], "]"}]}]}]}]}]}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{"Output", " ", "probability", " ", "as", " ", "list"}], " ", 
   "*)"}], "\[IndentingNewLine]", 
  RowBox[{"(*", 
   RowBox[{"Column", "[", 
    RowBox[{
     RowBox[{"Function", "[", 
      RowBox[{"y", ",", 
       RowBox[{"pOutput", "[", 
        RowBox[{"y", ",", "0.2", ",", "5"}], "]"}]}], "]"}], "/@", 
     RowBox[{"Range", "[", 
      RowBox[{"0.005", ",", "0.995", ",", "0.1"}], "]"}]}], "]"}], "*)"}], 
  "\n", 
  RowBox[{"(*", 
   RowBox[{"Plot", "[", 
    RowBox[{
     RowBox[{"pOutput", "[", 
      RowBox[{"y", ",", "0.4", ",", "10"}], "]"}], ",", 
     RowBox[{"{", 
      RowBox[{"y", ",", "0", ",", "1"}], "}"}]}], "]"}], "*)"}], 
  "\[IndentingNewLine]", "*)"}]], "Input",ExpressionUUID->"f461a345-e0d4-4d99-\
afe7-4c611ff78a77"]
}, Open  ]]
},
WindowSize->{1440, 801},
Visible->True,
ScrollingOptions->{"VerticalScrollRange"->Fit},
PrintingCopies->1,
PrintingPageRange->{32000, 32000},
PrintingOptions->{"Magnification"->1.,
"PaperOrientation"->"Portrait",
"PaperSize"->{594.9599999999999, 780.}},
PrivateNotebookOptions->{"VersionedStylesheet"->{"Default.nb"[8.] -> False}},
ShowCellBracket->Automatic,
Deployed->True,
CellContext->Notebook,
TrackCellChangeTimes->False,
FrontEndVersion->"11.2 for Mac OS X x86 (32-bit, 64-bit Kernel) (September \
10, 2017)",
StyleDefinitions->"Default.nb"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[CellGroupData[{
Cell[1510, 35, 95, 0, 67, "Section",ExpressionUUID->"fc2fc132-6bf7-4de0-8e09-8fef5b298426"],
Cell[1608, 37, 218, 3, 35, "Text",ExpressionUUID->"711e4e16-d0ce-4c58-a71f-248af966f642"],
Cell[CellGroupData[{
Cell[1851, 44, 11844, 350, 858, "Input",ExpressionUUID->"59b50278-88fb-423d-ac0f-05dbe0426058"],
Cell[13698, 396, 783, 25, 50, "Output",ExpressionUUID->"1a9d04e4-f7d1-471b-9d40-5b4f532c7aa0"],
Cell[14484, 423, 40798, 730, 270, "Output",ExpressionUUID->"d428bd41-3b99-4772-ab4d-b95c393edefe"]
}, Open  ]],
Cell[CellGroupData[{
Cell[55319, 1158, 506, 13, 52, "Input",ExpressionUUID->"e7e6f8c3-6906-4f8c-90c6-6075b72629d4"],
Cell[55828, 1173, 102, 0, 34, "Output",ExpressionUUID->"2431798e-7361-4a51-b59b-68847881a265"]
}, Open  ]],
Cell[55945, 1176, 81, 0, 30, "Input",ExpressionUUID->"04447605-31e0-4f7e-9e6e-644f292ca94f"],
Cell[56029, 1178, 170, 3, 30, "Input",ExpressionUUID->"74942d00-9ac9-474b-bee5-c97abe8052d3"],
Cell[CellGroupData[{
Cell[56224, 1185, 445, 11, 73, "Input",ExpressionUUID->"0cabf08d-530f-4c5e-978f-9f5215542f98"],
Cell[56672, 1198, 101, 0, 34, "Output",ExpressionUUID->"b638d9e7-0a8d-493b-bb9c-3b3701726254"]
}, Open  ]]
}, Open  ]],
Cell[CellGroupData[{
Cell[56822, 1204, 97, 0, 67, "Section",ExpressionUUID->"b23bf8b3-9b43-4405-a8a2-3e35551c44b4"],
Cell[56922, 1206, 597, 14, 30, "Input",ExpressionUUID->"3b69e0d5-5fdb-4654-aaed-7a547a4c0b5e"],
Cell[CellGroupData[{
Cell[57544, 1224, 3599, 99, 223, "Input",ExpressionUUID->"e12af993-1271-45e7-b7d8-09cdf98a46e7"],
Cell[61146, 1325, 13497, 283, 268, "Output",ExpressionUUID->"ff475cd1-8e70-49a0-8ea4-1d422628dd10"]
}, Open  ]],
Cell[CellGroupData[{
Cell[74680, 1613, 99, 0, 54, "Subsection",ExpressionUUID->"f157a6a8-39bb-4a60-b985-6875ca133d59"],
Cell[CellGroupData[{
Cell[74804, 1617, 6531, 187, 442, "Input",ExpressionUUID->"16685f8c-70f9-4421-8cbf-d88eef8211f6"],
Cell[81338, 1806, 101, 0, 34, "Output",ExpressionUUID->"d173d3c4-a28d-47b7-9037-16c094d22c98"],
Cell[81442, 1808, 75599, 1301, 378, "Output",ExpressionUUID->"e487eb29-ab9e-4031-9b62-49bb348de559"],
Cell[157044, 3111, 74856, 1288, 378, "Output",ExpressionUUID->"9cf78127-a71b-4305-b6ec-9c119dd722fe"]
}, Open  ]],
Cell[CellGroupData[{
Cell[231937, 4404, 123, 3, 71, "Subsubsection",ExpressionUUID->"02e59c17-957b-415f-a07f-74fd30cd6e67"],
Cell[232063, 4409, 750, 21, 73, "Input",ExpressionUUID->"6d551452-13a0-4599-8e6d-676e4513d33e"],
Cell[232816, 4432, 81, 0, 30, "Input",ExpressionUUID->"19a96fab-908d-4596-8d0f-4a2c63068ec6"],
Cell[CellGroupData[{
Cell[232922, 4436, 1616, 41, 98, "Input",ExpressionUUID->"e0498b92-5db2-4c00-8494-470d552300c1"],
Cell[234541, 4479, 15441, 317, 264, "Output",ExpressionUUID->"5973dc1b-7585-4ce2-a899-5c90f482c2e6"]
}, Open  ]],
Cell[249997, 4799, 81, 0, 30, "Input",ExpressionUUID->"b088cd8d-8ff9-4f59-8d06-2caa669e6c28"]
}, Open  ]]
}, Open  ]]
}, Open  ]],
Cell[CellGroupData[{
Cell[250139, 4806, 100, 0, 67, "Section",ExpressionUUID->"57cbb4d3-e671-436a-8418-63778b56f74b"],
Cell[250242, 4808, 267, 6, 52, "Input",ExpressionUUID->"e1e0ec2b-45e5-4ae7-a50e-56e92b152c77"],
Cell[250512, 4816, 81, 0, 30, "Input",ExpressionUUID->"0bb0cdb7-b3b5-4131-973b-47e441113497"],
Cell[CellGroupData[{
Cell[250618, 4820, 1850, 50, 202, "Input",ExpressionUUID->"9afdc3fe-e282-49ab-9ef2-cfa37c2223e3"],
Cell[252471, 4872, 102, 0, 34, "Output",ExpressionUUID->"c34229be-22d8-456a-9f7c-40c2f0618711"],
Cell[252576, 4874, 101, 0, 34, "Output",ExpressionUUID->"90144933-6c61-4f6b-8e76-ceb8e98e2d11"]
}, Open  ]],
Cell[CellGroupData[{
Cell[252714, 4879, 1989, 51, 390, "Input",ExpressionUUID->"e74da34a-0059-4b34-89ac-26493dcc7086"],
Cell[254706, 4932, 102, 0, 34, "Output",ExpressionUUID->"3bb54e8f-e38d-44c9-8350-bdedf067f6a9"]
}, Open  ]],
Cell[254823, 4935, 81, 0, 30, "Input",ExpressionUUID->"d832e1a9-eaff-4811-a65e-b5b7d1b78170"],
Cell[254907, 4937, 81, 0, 30, "Input",ExpressionUUID->"d7a96cc3-f128-4c8b-aaa6-5a0fcaa31802"],
Cell[254991, 4939, 1404, 43, 115, "Input",ExpressionUUID->"ea663ccf-096c-4892-bc8d-81a6aebe2993"],
Cell[CellGroupData[{
Cell[256420, 4986, 653, 18, 30, "Input",ExpressionUUID->"1b9d0840-35a8-4482-844c-c0cf7e6dcb28"],
Cell[257076, 5006, 13113, 273, 264, "Output",ExpressionUUID->"1f8dd279-e5b8-46e6-964b-ff26287d34ed"]
}, Open  ]]
}, Open  ]],
Cell[CellGroupData[{
Cell[270238, 5285, 91, 0, 67, "Section",ExpressionUUID->"665856d4-cc92-444a-bdd6-46b7997a5550"],
Cell[270332, 5287, 52806, 1019, 1005, "Input",ExpressionUUID->"f461a345-e0d4-4d99-afe7-4c611ff78a77"]
}, Open  ]]
}
]
*)

(* End of internal cache information *)

(* NotebookSignature wx0qgEgicioayA16IfwlJIHW *)
