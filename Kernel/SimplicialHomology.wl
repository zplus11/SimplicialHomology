(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["Taggar`SimplicialHomology`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


SimplicialComplex;
SubComplexQ;
BettiNumber;
HomologyGroup;


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


(* ::Text:: *)
(*A simplicial complex is stored in the following form:*)


SimplicialComplexQ[SimplicialComplex[
	<| (_Integer?NonNegative -> _List) ... |>
]] := True
SimplicialComplexQ[_] := False


(* ::Text:: *)
(*Some properties:*)


SimplicialComplex /: sc_SimplicialComplex["Dimension"] := Dimension[sc]
SimplicialComplex /: sc_SimplicialComplex["Simplices"] := Simplices[sc]
SimplicialComplex /: sc_SimplicialComplex["EulerCharacteristic"] := EuChar[sc]


(* ::Text:: *)
(*Input methods:*)


SimplicialComplex[data : { (_Simplex | _List) ... }] :=
	SimplicialComplex[
		GroupBy[
			DeleteDuplicates @ Flatten[Subsets[#, {1, \[Infinity]}] & /@ Replace[data,
				Simplex[v_] :> Sort[v], {1}], 1],
			Length[#] - 1 &]]


SimplicialComplex[reg : (_MeshRegion | _BoundaryMeshRegion)] :=
	SimplicialComplex[
		Flatten[{MeshCells[reg, 0], MeshCells[reg, 1], MeshCells[reg, 2]}] /.
			{Point[v_] :> Simplex[{v}], Line[v_] | Polygon[v_] | Tetrahedron[v_] :> Simplex[v]}]


(* ::Text:: *)
(*Formatting:*)


SimplicialComplex /:
	MakeBoxes[sc : SimplicialComplex[<|(_Integer?NonNegative -> _List) ..|>], form_] :=
		BoxForm`ArrangeSummaryBox[
			"SimplicialComplex", sc,
			Graphics[{
				EdgeForm[Black],
				FaceForm[LightRed],
				Rectangle[],
				Text[Style["SC", Black, 14], {0.5, 0.5}]
			}, ImageSize -> {30, 30}],
			{BoxForm`SummaryItem[{"Dimension: ", sc["Dimension"]}],
			 BoxForm`SummaryItem[{"#(Simplices): ", Length /@ Values[sc["Simplices"]]}]},
			{BoxForm`SummaryItem[{"\[Chi]: ", sc["EulerCharacteristic"]}]}, form, "Interpretable" -> Automatic]


(* ::Text:: *)
(*Some helper functions to do basic examination:*)


ClearAll[Simplices];
Simplices[sc : _SimplicialComplex] := First @ sc


ClearAll[Dimension];
Dimension[sc : _SimplicialComplex] := Max @ Keys @ Simplices @ sc


ClearAll[Boundary];
Boundary[s : _List] :=
	Table[
		{Delete[s, i], (-1)^(i - 1)},
		{i, Length[s]}]


ClearAll[EuChar];
EuChar[sc : _SimplicialComplex] :=
	Total @ KeyValueMap[
		(-1)^#1 Length[#2] &, Simplices[sc]]


ClearAll[BoundaryMatrix];
BoundaryMatrix[sc_?SimplicialComplexQ, 0] :=
	ConstantArray[{}, {Length @ Lookup[Simplices[sc], 0, {}]}]
BoundaryMatrix[sc_?SimplicialComplexQ, k : _Integer?Positive] :=
	Module[
		{scs = Simplices[sc], rows, cols, lookup, mat, terms, r},
		rows = Lookup[scs, k - 1, {}];
		cols = Lookup[scs, k, {}];
		lookup = AssociationThread[rows, Range[Length[rows]]];
		mat = ConstantArray[0, {Length[rows], Length[cols]}];
		Do[
			terms = Boundary[cols[[c]]];
			Do[
				r = lookup[terms[[i, 1]]];
				mat[[r, c]] = terms[[i, 2]],
				{i, Length[terms]}],
			{c, Length[cols]}];
		mat]


(* ::Subsection:: *)
(*SubComplexQ*)


ClearAll[SubComplexQ];
SubComplexQ::usage = "SubComplexQ[k1, k2] checks if k2 is a subcomplex of k1.";
SubComplexQ[sc1_?SimplicialComplexQ, sc2_?SimplicialComplexQ] :=
	And @@ KeyValueMap[
		SubsetQ[Lookup[Simplices @ sc1, #1, {}], #2] &,
		Simplices @ sc2]


(* ::Subsection:: *)
(*Betti number*)


ClearAll[BettiNumber];
BettiNumber::usage = "BettiNumber[k, n] computes the nth betti number of SimplicialComplex k.";
BettiNumber::InvalidDimension = "Betti number of dimension `1` does not exist.";
Options[BettiNumber] = {"Reduced" -> False};
BettiNumber[sc_?SimplicialComplexQ, 0, opts : OptionsPattern[]] :=
	With[
		{b0 = Length @ Lookup[Simplices[sc], 0, {}] -
			MatrixRank[BoundaryMatrix[sc, 1]]},
		If[TrueQ @ OptionValue["Reduced"],
			Max[0, b0 - 1], b0]]
BettiNumber[sc_?SimplicialComplexQ, k : _Integer?Positive, opts : OptionsPattern[]] /;
	k <= Dimension[sc] :=
		If[k == Dimension[sc],
			Length[NullSpace[BoundaryMatrix[sc, k]]],
			Length[NullSpace[BoundaryMatrix[sc, k]]] -
				MatrixRank[BoundaryMatrix[sc, k + 1]]]
BettiNumber[sc_?SimplicialComplexQ, k : _Integer, opts : OptionsPattern[]] :=
	(Message[BettiNumber::InvalidDimension, k]; $Failed)
BettiNumber[sc_?SimplicialComplexQ, opts : OptionsPattern[]] :=
	AssociationMap[BettiNumber[sc, #, opts] &, Range[0, Dimension[sc]]]


(* ::Subsection:: *)
(*Homology group*)


ClearAll[HomologyGroup];
HomologyGroup::usage = "HomologyGroup[k, n] computes the nth homology group of SimplicialComplex k.";
Options[HomologyGroup] = {"Reduced" -> False};
HomologyGroup[sc_?SimplicialComplexQ, 0, opts : OptionsPattern[]] :=
	With[
		{o = Max[0, 
			Length[Lookup[Simplices[sc], 0, {}]] -
				MatrixRank[BoundaryMatrix[sc, 1]] -
				Boole[TrueQ @ OptionValue["Reduced"]]]},
		If[o == 0, 0, {Power[Integers, o]}]]
HomologyGroup[sc_?SimplicialComplexQ, k_Integer?Positive, opts : OptionsPattern[]] /;
	k <= Dimension[sc] :=
		Module[
			{\[Delta]k, \[Delta]kp1, diag, torsion, free, out},
			\[Delta]k = BoundaryMatrix[sc, k];
			\[Delta]kp1 = BoundaryMatrix[sc, k + 1];
			
			diag = If[Dimensions[\[Delta]kp1][[2]] == 0, {},
				DeleteCases[Diagonal[SmithDecomposition[\[Delta]kp1][[2]]],
					0 | 1]];
			torsion = CyclicGroup /@ diag;
			free = Length[NullSpace[\[Delta]k]] - MatrixRank[\[Delta]kp1];
			
			out = Join[If[free == 0, {}, {Power[Integers, free]}], torsion];
			If[out === {}, 0, out]]
HomologyGroup[sc_?SimplicialComplexQ, _Integer, opts : OptionsPattern[]] := 0
HomologyGroup[sc_?SimplicialComplexQ, opts : OptionsPattern[]] :=
	AssociationMap[HomologyGroup[sc, #, opts] &, Range[0, Dimension[sc]]]


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
