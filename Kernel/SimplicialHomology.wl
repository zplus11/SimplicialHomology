(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["Taggar`SimplicialHomology`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


SimplicialComplex;
SubComplexQ;
SimplicialJoin;
SimplicialCone;
SimplicialSuspension;
BettiNumber;
HomologyGroup;


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


(* ::Text:: *)
(*A global cache system is maintained:*)


If[\[Not] AssociationQ @ $SimplicialCache,
	$SimplicialCache = <||>];


(* ::Text:: *)
(*Each simplicial complex is to have a hash ID corresponding to the facet data:*)


SimplicialComplexUUID[sc_] := First[sc]["UUID"]


(* ::Text:: *)
(*Helper functions to deal with cache:*)


CacheGet[sc_, key_] :=
	Lookup[
		Lookup[$SimplicialCache, SimplicialComplexUUID[sc], <||>],
		HoldComplete[key],
		Missing["NotCached"]]

CacheSet[sc_, key_, value_] := (
	If[!KeyExistsQ[$SimplicialCache, SimplicialComplexUUID[sc]],
		$SimplicialCache[SimplicialComplexUUID[sc]] = <||>];
		$SimplicialCache[SimplicialComplexUUID[sc], HoldComplete[key]] = value; value)


(* ::Text:: *)
(*A simplicial complex is stored in the following form:*)


ClearAll[SimplicialComplex, SimplicialComplexQ];


SimplicialComplexQ[SimplicialComplex[
	KeyValuePattern[{
		"Facets" -> {_List ..},
		"UUID" -> _}]
]] := True
SimplicialComplexQ[_] := False


(* ::Subsection:: *)
(*Some properties:*)


SimplicialComplex /: sc_SimplicialComplex["Dimension"] :=
	Dimension[sc]


(* ::Text:: *)
(*Association of all simplices sorted by dimension:*)


SimplicialComplex /: sc_SimplicialComplex["Simplices"] :=
	Association @ Table[i -> Simplices[sc, i], {i, 0, sc["Dimension"]}]


(* ::Text:: *)
(*All simplices count:*)


SimplicialComplex /: sc_SimplicialComplex["SimplexCount"] :=
	Total[Length /@ Values[sc["Simplices"]]]


SimplicialComplex /: sc_SimplicialComplex["EulerCharacteristic"] :=
	EuChar[sc]


SimplicialComplex /: sc_SimplicialComplex["Vertices"] :=
	Flatten @ Simplices[sc, 0]


SimplicialComplex /: sc_SimplicialComplex["VertexCount"] :=
	Length @ sc["Vertices"]


SimplicialComplex /: sc_SimplicialComplex["Edges"] :=
	Simplices[sc, 1]


SimplicialComplex /: sc_SimplicialComplex["EdgeCount"] :=
	Length @ sc["Edges"]


SimplicialComplex /: sc_SimplicialComplex["Facets"] :=
	First[sc]["Facets"]


SimplicialComplex /: sc_SimplicialComplex["FacetCount"] :=
	Length @ sc["Facets"]


SimplicialComplex /: sc_SimplicialComplex["FVector"] :=
	Length /@ Values @ sc["Simplices"]


(* ::Text:: *)
(*A simplicial complex is pure if all of its facets have the same dimension:*)


SimplicialComplex /: sc_SimplicialComplex["PureQ"] :=
	Length @ Union[(Length[#]-1)& /@ sc["Facets"]] == 1


(* ::Text:: *)
(*Graph made out of 0- and 1-dimensional simplices:*)


SimplicialComplex /: sc_SimplicialComplex["1Skeleton"] :=
	Graph[sc["Vertices"], UndirectedEdge @@@ sc["Edges"]]


(* ::Text:: *)
(*Simplicial complex's connected components are precisely the connected components of its 1-skeleton:*)


SimplicialComplex /: sc_SimplicialComplex["ConnectedComponents"] :=
	ConnectedComponents[sc["1Skeleton"]]


(* ::Text:: *)
(*It is connected if the 1-skeleton is connected:*)


SimplicialComplex /: sc_SimplicialComplex["ConnectedQ"] :=
	Length[sc["ConnectedComponents"]] == 1


SimplicialComplex /: sc_SimplicialComplex["Properties"] :=
	{"Dimension", "Simplices", "SimplexCount", "EulerCharacteristic",
	"Vertices", "VertexCount", "Edges", "EdgeCount", "Facets", "FacetCount",
	"FVector", "PureQ", "1Skeleton", "ConnectedComponents", "ConnectedQ",
	"Properties"}


(* ::Subsection:: *)
(*Input methods:*)


NormaliseSimplex[x : _List] := Sort @ x
NormaliseSimplex[Simplex[x_List]] := Sort @ x


(* ::Text:: *)
(*Main entry point:*)


SimplicialComplexFromFacets[facets_] :=
	SimplicialComplex[
		<|
			"Facets" -> DeleteDuplicates[NormaliseSimplex /@ facets],
			"UUID" -> Hash[facets, "SHA256"]
		|>]


SimplicialComplex[data : {(_List | _Simplex)...}] :=
	SimplicialComplexFromFacets[MaximalSimplices[data]]


(* ::Text:: *)
(*Other constructors:*)


SimplicialComplex[reg : (_MeshRegion | _BoundaryMeshRegion)] :=
	SimplicialComplexFromFacets[
		Flatten[MeshCells[reg, All]] /.
			{Point[v_] :> Simplex[{v}], Line[v_] | Polygon[v_] | Tetrahedron[v_] :> Simplex[v]}]


SimplicialComplex[{"Simplex", n_Integer?NonNegative}] :=
	SimplicialComplexFromFacets[{Range[n + 1]}]
SimplicialComplex["Point"] := SimplicialComplex[{"Simplex", 0}]
SimplicialComplex["Line"] := SimplicialComplex[{"Simplex", 1}]
SimplicialComplex["Triangle"] := SimplicialComplex[{"Simplex", 2}]
SimplicialComplex["Tetrahedron"] := SimplicialComplex[{"Simplex", 3}]


SimplicialComplex[{"Circle", n_Integer?NonNegative}] :=
	SimplicialComplexFromFacets[Subsets[Range[n + 2], {n + 1}]]
SimplicialComplex["Circle"] := SimplicialComplex[{"Circle", 1}]
SimplicialComplex["Sphere"] := SimplicialComplex[{"Circle", 2}]


SimplicialComplex[{"CircleWedge", n_Integer?Positive}] :=
	SimplicialComplexFromFacets[
		Flatten[Table[{{1, 2 i}, {2 i, 2 i + 1}, {2 i + 1, 1}}, {i, 1, n}], 1]]


(* ::Text:: *)
(*For the spaces below, facets were taken from sagemath Python:*)


SimplicialComplex["Torus"] :=
	SimplicialComplexFromFacets[
		{{1,2,3}, {1,2,6}, {1,3,7}, {1,4,5}, {1,4,6},
		{1,5,7}, {2,3,5}, {2,4,5}, {2,4,7}, {2,6,7},
		{3,4,6}, {3,4,7}, {3,5,6}, {5,6,7}}]


SimplicialComplex["KleinBottle"] :=
	SimplicialComplexFromFacets[
		{{1,2,5}, {1,2,7}, {1,3,5}, {1,3,6}, {1,6,7},
		{2,3,4}, {2,3,7}, {2,4,6}, {2,5,8}, {2,6,8},
		{3,4,8}, {3,5,7}, {3,6,8}, {4,5,7}, {4,5,8},
		{4,6,7}}]


SimplicialComplex["MobiusStrip"] :=
	SimplicialComplexFromFacets[
		{{1, 2, 4}, {2, 3, 4}, {3, 4, 5},
		{1, 3, 5}, {1, 2, 5}}]


SimplicialComplex["RealProjectivePlane"] :=
	SimplicialComplexFromFacets[
		{{1,2,3}, {1,2,6}, {1,3,4}, {1,4,5}, {1,5,6},
		{2,3,5}, {2,4,5}, {2,4,6}, {3,4,6}, {3,5,6}}]


SimplicialComplex["ComplexProjectivePlane"] :=
	SimplicialComplexFromFacets[
		{{2,3,4,8,9}, {2,3,4,8,10}, {2,3,4,9,10}, {2,3,5,6,7},
		{2,3,5,6,10}, {2,3,5,7,8}, {2,3,5,8,10}, {2,3,6,7,9},
		{2,3,6,9,10}, {2,3,7,8,9}, {2,4,5,6,7}, {2,4,5,6,8},
		{2,4,5,7,9}, {2,4,5,8,9}, {2,4,6,7,10}, {2,4,6,8,10},
		{2,4,7,9,10}, {2,5,6,8,10}, {2,5,7,8,9}, {2,6,7,9,10},
		{3,4,5,6,7}, {3,4,5,6,9}, {3,4,5,7,10}, {3,4,5,9,10},
		{3,4,6,7,8}, {3,4,6,8,9}, {3,4,7,8,10}, {3,5,6,9,10},
		{3,5,7,8,10}, {3,6,7,8,9}, {4,5,6,8,9}, {4,5,7,9,10},
		{4,6,7,8,10}, {5,6,8,9,10}, {5,7,8,9,10}, {6,7,8,9,10}}]


SimplicialComplex["DunceHat"] :=
	SimplicialComplexFromFacets[
		{{2,3,5}, {2,3,8}, {2,3,9}, {2,4,5}, {2,4,6}, {2,4,7},
		{2,6,7}, {2,8,9}, {3,4,6}, {3,4,8}, {3,4,9}, {3,5,6},
		{4,5,9}, {4,7,8}, {5,6,7}, {5,7,9}, {7,8,9}}]


(* same homology as sS3 *)
SimplicialComplex["PoincareHomologyThreeSphere"] :=
	SimplicialComplexFromFacets[
		{{2,3,5,10}, {2,3,5,16}, {2,3,7,15}, {2,3,7,16},
		{2,3,10,15}, {2,4,5,13}, {2,4,5,16}, {2,4,8,11},
		{2,4,8,13}, {2,4,11,16}, {2,5,10,13}, {2,6,7,14},
		{2,6,7,15}, {2,6,9,12}, {2,6,9,14}, {2,6,12,15},
		{2,7,14,16}, {2,8,9,11}, {2,8,9,12}, {2,8,12,13},
		{2,9,11,14}, {2,10,12,13}, {2,10,12,15}, {2,11,14,16},
		{3,4,6,11}, {3,4,6,12}, {3,4,8,11}, {3,4,8,14}, {3,4,12,14},
		{3,5,10,14}, {3,5,12,14}, {3,5,12,16}, {3,6,9,12}, {3,6,9,13},
		{3,6,11,13}, {3,7,11,13}, {3,7,11,15}, {3,7,13,16}, {3,8,10,14},
		{3,8,10,15}, {3,8,11,15}, {3,9,12,16}, {3,9,13,16}, {4,5,6,15},
		{4,5,6,16}, {4,5,13,15}, {4,6,11,16}, {4,6,12,15}, {4,8,13,14},
		{4,12,14,15}, {4,13,14,15}, {5,6,7,8}, {5,6,7,15}, {5,6,8,16},
		{5,7,8,12}, {5,7,11,12}, {5,7,11,15}, {5,8,12,16}, {5,9,10,13},
		{5,9,10,14}, {5,9,11,14}, {5,9,11,15}, {5,9,13,15}, {5,11,12,14},
		{6,7,8,14}, {6,8,10,14}, {6,8,10,16}, {6,9,10,13}, {6,9,10,14},
		{6,10,11,13}, {6,10,11,16}, {7,8,12,13}, {7,8,13,14}, {7,11,12,13},
		{7,13,14,16}, {8,9,11,15}, {8,9,12,16}, {8,9,15,16}, {8,10,15,16},
		{9,13,15,16}, {10,11,12,13}, {10,11,12,17}, {10,11,16,17}, {10,12,15,17},
		{10,15,16,17}, {11,12,14,17}, {11,14,16,17}, {12,14,15,17}, {13,14,15,16},
		{14,15,16,17}}]


SimplicialComplex["RudinBall"] :=
	SimplicialComplexFromFacets[
		{{2,3,6,10}, {2,3,6,11}, {2,5,9,10}, {2,5,9,13},
		{2,6,10,13}, {2,6,11,12}, {2,6,12,14}, {2,8,11,12},
		{2,8,12,14}, {2,9,10,13}, {3,4,7,11}, {3,4,7,12},
		{3,6,10,11}, {3,7,10,11}, {3,7,12,13}, {3,7,13,15},
		{3,9,12,13}, {3,9,13,15}, {4,5,8,12}, {4,5,8,13},
		{4,6,10,13}, {4,6,10,14}, {4,7,11,12}, {4,8,10,13},
		{4,8,10,14}, {4,8,11,12}, {5,7,10,11}, {5,7,11,15},
		{5,8,12,13}, {5,9,10,11}, {5,9,11,15}, {5,9,12,13},
		{6,10,11,12}, {6,10,12,14}, {7,11,12,13}, {7,11,13,15},
		{8,10,12,13}, {8,10,12,14}, {9,10,11,13}, {9,11,13,15},
		{10,11,12,13}}]


SimplicialComplex["ZieglerBall"] :=
	SimplicialComplexFromFacets[
		{{1,2,3,4}, {1,2,3,6}, {1,3,4,8}, {1,3,6,7}, {1,3,7,8},
		{2,3,4,5}, {2,3,5,10}, {2,3,6,7}, {2,3,7,10}, {2,4,5,8},
		{2,5,6,8}, {2,5,6,9}, {2,5,9,10}, {2,6,7,10}, {2,6,9,10},
		{3,4,5,9}, {3,4,7,8}, {3,4,7,9}, {4,5,8,9}, {4,7,8,9},
		{5,6,8,9}}]


(* ::Subsection:: *)
(*Formatting:*)


SimplicialComplex /:
	MakeBoxes[sc_SimplicialComplex?SimplicialComplexQ, form_] :=
		Module[
			{smallQ = sc["FacetCount"] <= 1000, largeicon},
			
			largeicon = Graphics3D[
				{Opacity[.6], MeshRegion[{{0, 0, 0}, {2, 0, 0}, {2, 2, 0},
					{0, 2, 0}, {1, 1, 2}}, {Tetrahedron[{1, 2, 3, 5}],
					Tetrahedron[{1, 3, 4, 5}]}]},
				Boxed -> False, ImageSize -> {30, 25}];
			
			BoxForm`ArrangeSummaryBox[
				"SimplicialComplex", sc,
				If[smallQ,
					Show[sc["1Skeleton"], ImageSize -> {30, 25}],
					largeicon],
				{BoxForm`SummaryItem[{"Dimension: ", sc["Dimension"]}], (* main *)
				 BoxForm`SummaryItem[{"Vertices: ", sc["VertexCount"]}]},
				If[smallQ,
					(* then *)
					{BoxForm`SummaryItem[{"FVector: ", sc["FVector"]}], (* + *)
					 BoxForm`SummaryItem[{"\[Chi]: ", sc["EulerCharacteristic"]}],
					 BoxForm`SummaryItem[{"PureQ: ", sc["PureQ"]}],
					 BoxForm`SummaryItem[{"ConnectedQ: ", sc["ConnectedQ"]}]},
					 (* else *)
					 {BoxForm`SummaryItem[{"Facets: ", sc["FacetCount"]}],
					  BoxForm`SummaryItem[{"PureQ: ", sc["PureQ"]}]}], form, "Interpretable" -> Automatic]]


(* ::Subsection:: *)
(*Some helper functions to do basic examination:*)


ClearAll[Simplices];
Simplices[sc_, k_] :=
	Module[
		{cached},
		(* We look in the cache *)
		cached = CacheGet[sc, {"Simplices", k}];
		(* If it is not missing, *)
		If[cached =!= Missing["NotCached"],
			(* Return it *)
			Return[cached]];
		
		(* else calculate the required *)
		cached = DeleteDuplicates @ Flatten[
			Subsets[#, {k + 1}] & /@ sc["Facets"], 1];
		
		(* cache it, and return it *)
		CacheSet[sc, {"Simplices", k}, cached]]


ClearAll[MaximalSimplices];
MaximalSimplices[simplices_List] :=
	Module[
		{s, facets = {}},
		
		s = SortBy[simplices, -Length[#] &];
		
		Do[
			If[!AnyTrue[facets, SubsetQ[#, simplex] &],
			AppendTo[facets, simplex]],
			{simplex, s}];
		
		facets]


ClearAll[Dimension];
Dimension[sc : _SimplicialComplex] := Max[Length /@ sc["Facets"]] - 1


ClearAll[Boundary];
Boundary[s : _List] :=
	Table[
		{Delete[s, i], (-1)^(i - 1)},
		{i, Length[s]}]


ClearAll[EuChar];
EuChar[sc_SimplicialComplex] :=
	Total[
		MapIndexed[
			(-1)^(First[#2]-1) #1 &, sc["FVector"]]]


ClearAll[BoundaryMatrix];
BoundaryMatrix[sc_?SimplicialComplexQ, 0] :=
	SparseArray[{}, {Length @ Simplices[sc, 0], 0}]
BoundaryMatrix[sc_?SimplicialComplexQ, k_Integer?Positive] :=
	Module[
		{cached, rows, cols, lookup, rules},
		
		cached = CacheGet[sc, {"BoundaryMatrix", k}];
		If[cached =!= Missing["NotCached"],
			Return[cached]];
		
		rows = Simplices[sc, k - 1];
		cols = Simplices[sc, k];
		
		lookup = AssociationThread[rows -> Range[Length[rows]]];
		
		rules = Flatten @ MapIndexed[
			Function[{simp, col},
				({lookup[#1[[1]]], col[[1]]} -> #1[[2]]) & /@ Boundary[simp]],
			cols];
		
		CacheSet[
			sc, {"BoundaryMatrix", k},
			SparseArray[rules, {Length[rows], Length[cols]}]]]


ClearAll[BoundaryRank];
BoundaryRank::coeffs = "Invalid coefficients `1`";
BoundaryRank[sc_, k_, coeffs_ : Integers] :=
	Module[
		{cached, rank},
		
		cached = CacheGet[sc, {"BoundaryRank", k, coeffs}];
		If[cached =!= Missing["NotCached"],
			Return[cached]];
		
		rank = Switch[coeffs,
			Integers | Rationals,
				MatrixRank[BoundaryMatrix[sc, k]],
			
			_Integer,
				MatrixRank[
					BoundaryMatrix[sc, k], Modulus -> coeffs],
			
			_,
				Message[BoundaryRank::coeffs, coeffs];
				Return[$Failed]];
		
		CacheSet[sc, {"BoundaryRank", k, coeffs}, rank]]


(* ::Subsection:: *)
(*New complexes from old*)


SimplexJoin[s1_List, s2_List] :=
	Join[Table["L" <> ToString[i], {i, s1}],
		Table["R" <> ToString[i], {i, s2}]]


ClearAll[SimplicialComplexJoin];
SimplicialJoin::usage =
	"SimplicialJoin[sc1, sc2, ...] constructs the join product of given simplicial complexes.";
SimplicialJoin[sc_?SimplicialComplexQ] :=
	sc
SimplicialJoin[sc1_?SimplicialComplexQ, sc2_?SimplicialComplexQ] :=
	SimplicialComplex[
		Flatten[Table[
			SimplexJoin[x, y], {x, sc1["Facets"]},
				{y, sc2["Facets"]}], 1]]
SimplicialJoin[
	sc1_?SimplicialComplexQ,
	sc2_?SimplicialComplexQ,
	scs__?SimplicialComplexQ
] :=
	Fold[
		SimplicialJoin,
		SimplicialJoin[sc1, sc2],
        {scs}]


ClearAll[SimplicialCone];
SimplicialCone::usage =
	"SimplicialCone[sc] returns the cone of the simplicial complex sc.";
SimplicialCone[sc_?SimplicialComplexQ] :=
	SimplicialJoin[
		sc, SimplicialComplex["Point"]]


ClearAll[SimplicialSuspension];
SimplicialSuspension::usage =
	"SimplicialSuspension[sc] returns the suspension of the simplicial complex sc.";
SimplicialSuspension[sc_?SimplicialComplexQ] :=
	SimplicialJoin[
		sc, SimplicialComplex[{"Circle", 0}]]


(* ::Subsection:: *)
(*SubComplexQ*)


ClearAll[SubComplexQ];
SubComplexQ::usage = "SubComplexQ[k1, k2] checks if k2 is a subcomplex of k1.";
SubComplexQ[
    sc1_?SimplicialComplexQ,
    sc2_?SimplicialComplexQ
] :=
	Nothing[] (* pending implementation *)


(* ::Subsection:: *)
(*Betti number*)


ClearAll[BettiNumber];
BettiNumber::usage =
	"BettiNumber[k, n] computes the nth betti number of SimplicialComplex k.";
BettiNumber::InvalidDimension =
	"Betti number of dimension `1` does not exist.";
General::NonPrimeCoefficients =
	"\"Coefficients\" specification `1` cannot be a composite integer.";
Options[BettiNumber] = {"Reduced" -> False, "Coefficients" -> Integers};
BettiNumber[sc_?SimplicialComplexQ, 0, opts : OptionsPattern[]] :=
	With[
		{b0 = Length @ Simplices[sc, 0] -
			BoundaryRank[sc, 1, OptionValue["Coefficients"]]},
		If[TrueQ @ OptionValue["Reduced"],
			Max[0, b0 - 1], b0]]
BettiNumber[sc_?SimplicialComplexQ, k_Integer?Positive, opts : OptionsPattern[]] /;
	k <= Dimension[sc] :=
	Module[
		{cached, coeffs = OptionValue["Coefficients"]},
		
		If[MatchQ[coeffs, _Integer] \[And] \[Not] PrimeQ[coeffs],
			Message[BettiNumber::NonPrimeCoefficients, coeffs]; Return[$Failed]];
		
		cached = CacheGet[sc, {"BettiNumber", k, coeffs}];
		If[cached =!= Missing["NotCached"], Return[cached]];
			
		cached = If[k == Dimension[sc],
			Dimensions[BoundaryMatrix[sc, k]][[2]] - BoundaryRank[sc, k, coeffs],
				Dimensions[BoundaryMatrix[sc, k]][[2]] -
					BoundaryRank[sc, k, coeffs] - BoundaryRank[sc, k + 1, coeffs]];
		
		CacheSet[sc, {"BettiNumber", k, coeffs}, cached]]
BettiNumber[sc_?SimplicialComplexQ, k : _Integer, opts : OptionsPattern[]] :=
	(Message[BettiNumber::InvalidDimension, k]; $Failed)
BettiNumber[sc_?SimplicialComplexQ, opts : OptionsPattern[]] :=
	Association @ Table[
		k -> BettiNumber[sc, k, opts], {k, 0, Dimension[sc]}]


(* ::Subsection:: *)
(*Homology group*)


ClearAll[HomologyGroup];
HomologyGroup::usage =
	"HomologyGroup[k, n] computes the nth homology group of SimplicialComplex k.";
Options[HomologyGroup] = {"Reduced" -> False, "Coefficients" -> Integers};
HomologyGroup[sc_?SimplicialComplexQ, 0, opts : OptionsPattern[]] :=
	Module[
		{o, coeffs = OptionValue["Coefficients"], grp},
		
		If[MatchQ[coeffs, _Integer] \[And] \[Not] PrimeQ[coeffs],
			Message[BettiNumber::NonPrimeCoefficients, coeffs]; Return[$Failed]];
			
		o = Max[0, 
			Length[Simplices[sc, 0]] -
				BoundaryRank[sc, 1, OptionValue["Coefficients"]] -
				Boole[TrueQ @ OptionValue["Reduced"]]];
				
		grp = Which[o == 0, 0,
			coeffs === Integers \[Or] coeffs === Rationals, 
				If[o == 1, coeffs, Superscript[coeffs, o]],
			MatchQ[coeffs, _Integer],
				If[o == 1, Subscript[Integers, coeffs], Subsuperscript[Integers, coeffs, o]]]]
HomologyGroup[sc_?SimplicialComplexQ, k_Integer?Positive, opts : OptionsPattern[]] /;
	k <= Dimension[sc] :=
	Module[
		{cached, \[Delta]k, \[Delta]kp1, diag, torsion, free, out, coeffs = OptionValue["Coefficients"]},
		
		cached = CacheGet[sc, {"HomologyGroup", k, coeffs}];
		
		If[cached =!= Missing["NotCached"],
			Return[cached]];
		
		\[Delta]k = BoundaryMatrix[sc, k];
		\[Delta]kp1 = BoundaryMatrix[sc, k + 1];
		
		free = Dimensions[\[Delta]k][[2]] - BoundaryRank[sc, k, coeffs]
			- BoundaryRank[sc, k + 1, coeffs];
		
		Switch[coeffs,
			Integers,
				diag = If[
					Dimensions[\[Delta]kp1][[2]] == 0, {},
					DeleteCases[
						Diagonal[SmithDecomposition[\[Delta]kp1][[2]]],
							0 | 1]];
				torsion = CyclicGroup /@ diag;
				out = If[torsion == {}, If[free == 1,
					Integers, Superscript[Integers, free]],
					Join[
						If[free == 0, {}, If[free == 1,
							{Integers}, {Superscript[Integers, free]}]], torsion]],
			
			Rationals,
				out = If[free == 0, 0, If[free == 1,
					Rationals, Superscript[Rationals, free]]],
			
			_Integer?PrimeQ,
				out = If[free == 0, 0, If[free == 1,
					Subscript[Integers, coeffs], Subsuperscript[Integers, coeffs, free]]],
			
			_Integer,
				Message[HomologyGroup::NonPrimeCoefficients, coeffs];
					Return[$Failed]];

		CacheSet[sc, {"HomologyGroup", k, coeffs},
			If[out === {}, 0, out]]]
HomologyGroup[sc_?SimplicialComplexQ, _Integer, opts : OptionsPattern[]] := 0
HomologyGroup[sc_?SimplicialComplexQ, opts : OptionsPattern[]] :=
	Association @ Table[
		k -> HomologyGroup[sc, k, opts], {k, 0, Dimension[sc]}]


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
