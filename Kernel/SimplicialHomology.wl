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
SimplicialStar;
SimplicialLink;
BettiNumber;
HomologyGroup;
SimplicialAutomorphismGroup;
SimplicialIsomorphicQ;


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


ClearAll[SimplicialComplex, SimplicialComplexObject, SimplicialComplexQ];


SimplicialComplexQ[SimplicialComplexObject[
	KeyValuePattern[{
		"Facets" -> {_List ... },
		"UUID" -> _}]
]] := True
SimplicialComplexQ[_] := False


(* ::Subsection:: *)
(*Some properties:*)


SimplicialComplexObject /: sc_SimplicialComplexObject["Dimension"] :=
	Dimension[sc]


(* ::Text:: *)
(*Association of all simplices sorted by dimension:*)


EmptyComplexQ[sc_?SimplicialComplexQ] := sc["Dimension"] == -Infinity


SimplicialComplexObject /: sc_SimplicialComplexObject["Simplices"] :=
	If[EmptyComplexQ[sc], <||>, (* special case the empty complex *)
	Association @ Table[i -> Simplices[sc, i], {i, 0, sc["Dimension"]}]]


(* ::Text:: *)
(*All simplices count:*)


SimplicialComplexObject /: sc_SimplicialComplexObject["SimplexCount"] :=
	Total[Length /@ Values[sc["Simplices"]]]


SimplicialComplexObject /: sc_SimplicialComplexObject["EulerCharacteristic"] :=
	EuChar[sc]


SimplicialComplexObject /: sc_SimplicialComplexObject["Vertices"] :=
	Flatten @ Simplices[sc, 0]


SimplicialComplexObject /: sc_SimplicialComplexObject["VertexCount"] :=
	Length @ sc["Vertices"]


SimplicialComplexObject /: sc_SimplicialComplexObject["Edges"] :=
	Simplices[sc, 1]


SimplicialComplexObject /: sc_SimplicialComplexObject["EdgeCount"] :=
	Length @ sc["Edges"]


SimplicialComplexObject /: sc_SimplicialComplexObject["Facets"] :=
	First[sc]["Facets"]


SimplicialComplexObject /: sc_SimplicialComplexObject["FacetCount"] :=
	Length @ sc["Facets"]


SimplicialComplexObject /: sc_SimplicialComplexObject["FVector"] :=
	Length /@ Values @ sc["Simplices"]


(* ::Text:: *)
(*A simplicial complex is pure if all of its facets have the same dimension:*)


SimplicialComplexObject /: sc_SimplicialComplexObject["PureQ"] :=
	Length @ Union[(Length[#]-1)& /@ sc["Facets"]] == 1


(* ::Text:: *)
(*Graph made out of 0- and 1-dimensional simplices:*)


SimplicialComplexObject /: sc_SimplicialComplexObject["1Skeleton"] :=
	Graph[sc["Vertices"], UndirectedEdge @@@ sc["Edges"]]


(* ::Text:: *)
(*Simplicial complex's connected components are precisely the connected components of its 1-skeleton:*)


SimplicialComplexObject /: sc_SimplicialComplexObject["ConnectedComponents"] :=
	ConnectedComponents[sc["1Skeleton"]]


(* ::Text:: *)
(*It is connected if the 1-skeleton is connected:*)


SimplicialComplexObject /: sc_SimplicialComplexObject["ConnectedQ"] :=
	Length[sc["ConnectedComponents"]] == 1


SimplicialComplexObject /: sc_SimplicialComplexObject["Properties"] :=
	{"Dimension", "Simplices", "SimplexCount", "EulerCharacteristic",
	"Vertices", "VertexCount", "Edges", "EdgeCount", "Facets", "FacetCount",
	"FVector", "PureQ", "1Skeleton", "ConnectedComponents", "ConnectedQ",
	"Properties"}


(* ::Subsection:: *)
(*Input methods:*)


Listify[x : _List] := Sort @ x
Listify[Simplex[x_List]] := Sort @ x


(* ::Text:: *)
(*Main entry point:*)


SimplicialComplex::usage =
	"SimplicialComplex[{Simplex[\[Ellipsis]], ...}] represents an abstract simplicial complex created from given Simplex objects.
SimplicialComplex[{{\[Ellipsis]}, ...}] represents an abstract simplicial complex created from given List objects.
SimplicialComplex[mesh] represents an abstract simplicial complex created from given Mesh region.
SimplicialComplex[\"name\"] represents an abstract simplicial complex created from the given name specification.";
SimplicialComplex::InvalidOptionValue =
	"The option value \"`1`\" -> `2` is invalid.";
Options[SimplicialComplex] =
	{"MaximalityCheck" -> True};


SimplicialComplex[data : { (_List | _Simplex) ... }, opts : OptionsPattern[]] :=
	Module[
		{good = {}, maximal = Listify /@ data, mcheck, ordering, order, norm},
		
		mcheck = OptionValue["MaximalityCheck"];
		If[\[Not] BooleanQ[mcheck],
			Message[SimplicialComplex::InvalidOptionValue, "MaximalityCheck", mcheck];
			Return[$Failed]];
		
		(* calculating maximals is easier when sorted by length *)
		If[mcheck,
			maximal = ReverseSortBy[maximal, Length]];
			
		Scan[Function[face,
				If[\[Not] mcheck \[Or] \[Not] AnyTrue[good, SubsetQ[#, face] &],
					AppendTo[good, face]]],
			maximal];
		
		SimplicialComplexObject[
			<|
				"Facets" -> good,
				"UUID" -> Hash[good, "SHA256"]
			|>]]


(* ::Text:: *)
(*Other constructors:*)


SimplicialComplex[reg : (_MeshRegion | _BoundaryMeshRegion), opts : OptionsPattern[]] :=
	SimplicialComplex[
		Flatten[MeshCells[reg, All]] /.
			{Point[v_] :> Simplex[{v}], Line[v_] | Polygon[v_] | Tetrahedron[v_] :> Simplex[v]},
		"MaximalityCheck" -> False] (* mesh gives too many maximal points, sorry *)


SimplicialComplex[{"Simplex", n_Integer?NonNegative}, opts : OptionsPattern[]] :=
	SimplicialComplex[{Range[n + 1]}, "MaximalityCheck" -> False, opts]
SimplicialComplex["Point", opts : OptionsPattern[]] := SimplicialComplex[{"Simplex", 0}, opts]
SimplicialComplex["Line", opts : OptionsPattern[]] := SimplicialComplex[{"Simplex", 1}, opts]
SimplicialComplex["Triangle", opts : OptionsPattern[]] := SimplicialComplex[{"Simplex", 2}, opts]
SimplicialComplex["Tetrahedron", opts : OptionsPattern[]] := SimplicialComplex[{"Simplex", 3}, opts]


SimplicialComplex[{"Circle", n_Integer?NonNegative}, opts : OptionsPattern[]] :=
	SimplicialComplex[Subsets[Range[n + 2], {n + 1}], "MaximalityCheck" -> False, opts]
SimplicialComplex["Circle", opts : OptionsPattern[]] := SimplicialComplex[{"Circle", 1}, opts]
SimplicialComplex["Sphere", opts : OptionsPattern[]] := SimplicialComplex[{"Circle", 2}, opts]


SimplicialComplex[{"CircleWedge", n_Integer?Positive}, opts : OptionsPattern[]] :=
	SimplicialComplex[
		Flatten[Table[{{1, 2 i}, {2 i, 2 i + 1}, {2 i + 1, 1}}, {i, 1, n}], 1],
			"MaximalityCheck" -> False, opts]


(* ::Text:: *)
(*For the spaces below, facets were taken from sagemath Python:*)


SimplicialComplex["Torus", opts : OptionsPattern[]] :=
	SimplicialComplex[
		{{1,2,3}, {1,2,6}, {1,3,7}, {1,4,5}, {1,4,6},
		{1,5,7}, {2,3,5}, {2,4,5}, {2,4,7}, {2,6,7},
		{3,4,6}, {3,4,7}, {3,5,6}, {5,6,7}}, "MaximalityCheck" -> False, opts]


SimplicialComplex["KleinBottle", opts : OptionsPattern[]] :=
	SimplicialComplex[
		{{1,2,5}, {1,2,7}, {1,3,5}, {1,3,6}, {1,6,7},
		{2,3,4}, {2,3,7}, {2,4,6}, {2,5,8}, {2,6,8},
		{3,4,8}, {3,5,7}, {3,6,8}, {4,5,7}, {4,5,8},
		{4,6,7}}, "MaximalityCheck" -> False, opts]


SimplicialComplex["MobiusStrip", opts : OptionsPattern[]] :=
	SimplicialComplex[
		{{1, 2, 4}, {2, 3, 4}, {3, 4, 5},
		{1, 3, 5}, {1, 2, 5}}, "MaximalityCheck" -> False, opts]


SimplicialComplex["RealProjectivePlane", opts : OptionsPattern[]] :=
	SimplicialComplex[
		{{1,2,3}, {1,2,6}, {1,3,4}, {1,4,5}, {1,5,6},
		{2,3,5}, {2,4,5}, {2,4,6}, {3,4,6}, {3,5,6}}, "MaximalityCheck" -> False, opts]


SimplicialComplex["ComplexProjectivePlane", opts : OptionsPattern[]] :=
	SimplicialComplex[
		{{2,3,4,8,9}, {2,3,4,8,10}, {2,3,4,9,10}, {2,3,5,6,7},
		{2,3,5,6,10}, {2,3,5,7,8}, {2,3,5,8,10}, {2,3,6,7,9},
		{2,3,6,9,10}, {2,3,7,8,9}, {2,4,5,6,7}, {2,4,5,6,8},
		{2,4,5,7,9}, {2,4,5,8,9}, {2,4,6,7,10}, {2,4,6,8,10},
		{2,4,7,9,10}, {2,5,6,8,10}, {2,5,7,8,9}, {2,6,7,9,10},
		{3,4,5,6,7}, {3,4,5,6,9}, {3,4,5,7,10}, {3,4,5,9,10},
		{3,4,6,7,8}, {3,4,6,8,9}, {3,4,7,8,10}, {3,5,6,9,10},
		{3,5,7,8,10}, {3,6,7,8,9}, {4,5,6,8,9}, {4,5,7,9,10},
		{4,6,7,8,10}, {5,6,8,9,10}, {5,7,8,9,10}, {6,7,8,9,10}}, "MaximalityCheck" -> False, opts]


SimplicialComplex["DunceHat", opts : OptionsPattern[]] :=
	SimplicialComplex[
		{{2,3,5}, {2,3,8}, {2,3,9}, {2,4,5}, {2,4,6}, {2,4,7},
		{2,6,7}, {2,8,9}, {3,4,6}, {3,4,8}, {3,4,9}, {3,5,6},
		{4,5,9}, {4,7,8}, {5,6,7}, {5,7,9}, {7,8,9}}, "MaximalityCheck" -> False, opts]


(* same homology as S3 *)
SimplicialComplex["PoincareHomologyThreeSphere", opts : OptionsPattern[]] :=
	SimplicialComplex[
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
		{14,15,16,17}}, "MaximalityCheck" -> False, opts]


SimplicialComplex["RudinBall", opts : OptionsPattern[]] :=
	SimplicialComplex[
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
		{10,11,12,13}}, "MaximalityCheck" -> False, opts]


SimplicialComplex["ZieglerBall", opts : OptionsPattern[]] :=
	SimplicialComplex[
		{{1,2,3,4}, {1,2,3,6}, {1,3,4,8}, {1,3,6,7}, {1,3,7,8},
		{2,3,4,5}, {2,3,5,10}, {2,3,6,7}, {2,3,7,10}, {2,4,5,8},
		{2,5,6,8}, {2,5,6,9}, {2,5,9,10}, {2,6,7,10}, {2,6,9,10},
		{3,4,5,9}, {3,4,7,8}, {3,4,7,9}, {4,5,8,9}, {4,7,8,9},
		{5,6,8,9}}, "MaximalityCheck" -> False, opts]


(* ::Subsection:: *)
(*Formatting:*)


SimplicialComplexObject /:
	MakeBoxes[sc_SimplicialComplexObject?SimplicialComplexQ, form_] :=
		Module[
			{smallQ = sc["FacetCount"] <= 100, largeicon},
			
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

(* Similarly other routines are also cached *)


ClearAll[Dimension];
Dimension[sc : _SimplicialComplexObject] := Max[Length /@ sc["Facets"]] - 1


ClearAll[Boundary];
Boundary[s : _List] :=
	Table[
		{Delete[s, i], (-1)^(i - 1)},
		{i, Length[s]}]


ClearAll[EuChar];
EuChar[sc : _SimplicialComplexObject] :=
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
	Join[Table[Subscript[i, "L"], {i, s1}],
		Table[Subscript[j, "R"], {j, s2}]]


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


SimplexInQ[sc_, sim_] :=
	MemberQ[
		(* vvv if sim is of length k, then whether it is a
		   vvv		part of the (k-1)-dimensional simplices of sc *)
		Lookup[sc["Simplices"], Length @ DeleteDuplicates @ sim - 1, {}],
		sim]


ClearAll[SimplicialStar];
SimplicialStar::usage =
	"SimplicialStar[sc, sigma] returns the star of the complex sigma in the simplicial complex sc.";
SimplicialStar::NotSimplex =
	"`1` is not a simplex present in the complex.";
SimplicialStar[sc_?SimplicialComplexQ, sigma : (_List | _Simplex)] :=
	Module[
		{norm = Listify @ sigma},
		
		If[\[Not] SimplexInQ[sc, norm],
			Message[SimplicialStar::NotSimplex, norm]; $Failed,
			SimplicialComplex[
				Select[sc["Facets"], SubsetQ[#, norm] &],
				"MaximalityCheck" -> False]]]


ClearAll[SimplicialLink];
SimplicialLink::usage =
	"SimplicialLink[sc, sigma] returns the link of the complex sigma in the simplicial complex sc.";
SimplicialLink::NotSimplex =
	"`1` is not a simplex present in the complex.";
SimplicialLink[sc_?SimplicialComplexQ, sigma : (_List | _Simplex)] :=
	Module[
		{facets, norm = Listify @ sigma},
		
		facets = Select[sc["Facets"], SubsetQ[#, norm] &];
		
		If[\[Not] SimplexInQ[sc, norm],
			Message[SimplicialLink::NotSimplex, norm]; $Failed,
			SimplicialComplex[
				DeleteCases[Complement[#, norm] & /@ facets, {}],
				"MaximalityCheck" -> False]]]


(* ::Subsection:: *)
(*SubComplexQ*)


ClearAll[SubComplexQ];
SubComplexQ::usage = "SubComplexQ[sc1, sc2] checks if sc2 is a subcomplex of sc1.";
SubComplexQ[
    sc1_?SimplicialComplexQ,
    sc2_?SimplicialComplexQ
] :=
	AllTrue[sc2["Facets"], SimplexInQ[sc1, #] &]


(* ::Subsection:: *)
(*Betti number*)


ClearAll[BettiNumber];
BettiNumber::usage =
	"BettiNumber[sc, n] computes the nth betti number of simplicial complex sc.
BettiNumber[sc] computes all betti numbers of the simplicial complex sc.";
General::NonPrimeCoefficients =
	"\"Coefficients\" specification `1` cannot be a composite integer.";
Options[BettiNumber] = {"Reduced" -> False, "Coefficients" -> Integers};


(* ::Text:: *)
(*Special case the empty complex:*)


BettiNumber[sc_?EmptyComplexQ, k : _Integer, opts : OptionsPattern[]] := 0


(* ::Text:: *)
(*0th betti number of a complex:*)


BettiNumber[sc_?SimplicialComplexQ, 0, opts : OptionsPattern[]] :=
	With[
		{b0 = Length @ Simplices[sc, 0] -
			BoundaryRank[sc, 1, OptionValue["Coefficients"]]},
			
		If[TrueQ @ OptionValue["Reduced"],
			Max[0, b0 - 1], b0]]


(* ::Text:: *)
(*General case:*)


(* upto dimension *)
BettiNumber[sc_?SimplicialComplexQ, k_Integer?Positive, opts : OptionsPattern[]] /;
	k <= Dimension[sc] :=
	Module[
		{coeffs = OptionValue["Coefficients"]},
		
		If[MatchQ[coeffs, _Integer] \[And] \[Not] PrimeQ[coeffs],
			Message[BettiNumber::NonPrimeCoefficients, coeffs]; Return[$Failed]];
			
		If[k == Dimension[sc],
			Dimensions[BoundaryMatrix[sc, k]][[2]] - BoundaryRank[sc, k, coeffs],
				Dimensions[BoundaryMatrix[sc, k]][[2]] -
					BoundaryRank[sc, k, coeffs] - BoundaryRank[sc, k + 1, coeffs]]]
(* beyond dimension *)
BettiNumber[sc_?SimplicialComplexQ, k : _Integer, opts : OptionsPattern[]] := 0


(* ::Text:: *)
(*All betti numbers (in association form) :*)


BettiNumber[sc_?EmptyComplexQ, opts : OptionsPattern[]] := <||> (* empty *)
BettiNumber[sc_?SimplicialComplexQ, opts : OptionsPattern[]] := (* non empty *)
	Association @ Table[
		k -> BettiNumber[sc, k, opts], {k, 0, Dimension[sc]}]


(* ::Subsection:: *)
(*Homology group*)


ClearAll[FormatGroup];
FormatGroup[0, {}, coeffs_] := 0
FormatGroup[free_, torsion_List, coeffs_] :=
	Module[
		{freePart},
		
		freePart = Switch[
			coeffs,
				Integers, Which[
						free == 0, Nothing,
						free == 1, Integers,
						True, Superscript[Integers, free]],
				
				Rationals, Which[
						free == 0, Nothing,
						free == 1, Rationals,
						True, Superscript[Rationals, free]],
				
				_Integer?PrimeQ,
					With[{p = coeffs}, Which[
							free == 0, Nothing,
							free == 1, Subscript[Integers, p],
							True, Subsuperscript[Integers, p, free]]]];
		Which[
			freePart === Nothing && torsion === {}, 0,
			freePart === Nothing, If[Length[torsion] == 1,
				First@torsion, torsion],
			torsion === {}, freePart,
			Length[torsion] == 1, {freePart, First@torsion},
			True, Prepend[torsion, freePart]]]


getSmith[sc_, k_] :=
	Module[
		{cached},
		
		cached = CacheGet[sc, {"Smith", k}]; (* Smith decomposition of
		                                              k + 1 boundary matrix *)
		If[cached =!= Missing["NotCached"],
			Return[cached]];
		
		res = SmithReduce[BoundaryMatrix[sc, k]];
		CacheSet[sc, {"Smith", k}, res]]


ClearAll[HomologyGroup];
HomologyGroup::usage =
	"HomologyGroup[sc, n] computes the nth homology group of the simplicial complex sc.
HomologyGroup[sc] computes all homology groups of the simplicial complex sc.";
Options[HomologyGroup] = {"Reduced" -> False, "Coefficients" -> Integers};


(* ::Text:: *)
(*Likewise as for betti numbers we follow the same scheme, first off special case the empty complex:*)


HomologyGroup[sc_?EmptyComplexQ, n : _Integer, opts : OptionsPattern[]] := 0


(* ::Text:: *)
(*0th homology group:*)


HomologyGroup[sc_?SimplicialComplexQ, 0, opts : OptionsPattern[]] :=
	Module[
		{o, coeffs = OptionValue["Coefficients"], grp},
		
		If[MatchQ[coeffs, _Integer] \[And] \[Not] PrimeQ[coeffs],
			Message[HomologyGroup::NonPrimeCoefficients, coeffs]; Return[$Failed]];
			
		o = Max[0, Length[Simplices[sc, 0]] -
			BoundaryRank[sc, 1, coeffs] -
			Boole[TrueQ @ OptionValue["Reduced"]]];
		
		Switch[coeffs,
			Integers | Rationals, FormatGroup[o, {}, coeffs],
			_Integer?PrimeQ, FormatGroup[o, {}, coeffs]]]


(* ::Text:: *)
(*General case:*)


(* upto dimension *)
HomologyGroup[sc_?SimplicialComplexQ, k_Integer?Positive, opts : OptionsPattern[]] /;
	k <= Dimension[sc] :=
	Module[
		{\[Delta]k, \[Delta]kp1, diag, torsion, free, out, coeffs = OptionValue["Coefficients"]},
		
		If[MatchQ[coeffs, _Integer] \[And] \[Not] PrimeQ[coeffs],
			Message[HomologyGroup::NonPrimeCoefficients, coeffs]; Return[$Failed]];
		
		\[Delta]k = BoundaryMatrix[sc, k];
		\[Delta]kp1 = BoundaryMatrix[sc, k + 1];
		
		free = Dimensions[\[Delta]k][[2]] - BoundaryRank[sc, k, coeffs]
			- BoundaryRank[sc, k + 1, coeffs];
		
		Switch[coeffs,
			Integers,
				diag = If[Dimensions[\[Delta]kp1][[2]] == 0, {},
					DeleteCases[
						Diagonal[getSmith[sc, k + 1]], 0 | 1]];
			
				out = FormatGroup[free, CyclicGroup /@ diag, coeffs],			
			Rationals, out = FormatGroup[free, {}, coeffs],
			_Integer?PrimeQ, out = FormatGroup[free, {}, coeffs]]]
(* beyond dimension *)
HomologyGroup[sc_?SimplicialComplexQ, _Integer, opts : OptionsPattern[]] := 0


(* ::Text:: *)
(*All groups in association form:*)


HomologyGroup[sc_?EmptyComplexQ, opts : OptionsPattern[]] := <||> (* empty *)
HomologyGroup[sc_?SimplicialComplexQ, opts : OptionsPattern[]] := (* non empty *)
	Association @ Table[
		k -> HomologyGroup[sc, k, opts], {k, 0, Dimension[sc]}]


(* ::Subsection:: *)
(*Automorphism group*)


SimplicialIncidenceGraph[sc_] :=
	Module[
		{verts, facets, v, f},
		verts = v /@ sc["Vertices"];
		facets = sc["Facets"];
		
		Graph[
			Join[verts, f /@ Range[Length[facets]]], 
			Flatten @ MapIndexed[Thread[DirectedEdge[v /@ #1, f[#2[[1]]]]] &,
				facets]]]


ClearAll[SimplicialAutomorphismGroup];
SimplicialAutomorphismGroup::usage =
	"SimplicialAutomorphismGroup[sc] returns the automorphism group of the simplicial complex sc.";
SimplicialAutomorphismGroup[sc_?SimplicialComplexQ] :=
	Module[
		{verts, v, gens},
		verts = v /@ sc["Vertices"];
		
		gens = With[
			{img = PermutationReplace[verts, #]},
			FindPermutation[verts, img]] & /@
				GroupGenerators @ GraphAutomorphismGroup @ SimplicialIncidenceGraph[sc];
				
		PermutationGroup[gens]]


(* ::Subsection:: *)
(*SimplicialIsomorphicQ*)


ClearAll[SimplicialIsomorphicQ];
SimplicialIsomorphicQ::usage =
	"SimplicialIsomorphicQ[sc1, sc2] checks whether the given simplicial complexes are isomorphic or not.";
SimplicialIsomorphicQ[sc1_?SimplicialComplexQ, sc2_?SimplicialComplexQ] :=
	IsomorphicGraphQ[SimplicialIncidenceGraph[sc1],
		SimplicialIncidenceGraph[sc2]]


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
