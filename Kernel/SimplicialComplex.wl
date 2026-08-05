(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


Begin["Taggar`SimplicialHomology`Private`"];


(* ::Section:: *)
(*Definitions*)


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
	Join[
		<| -1 -> {{}} |>, (* empty simplex *)
		Association @ Table[i -> Simplices[sc, i], {i, 0, sc["Dimension"]}]]]


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
(*Graph made out of the 0- and 1-dimensional simplices of the given complex:*)


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


SimplicialComplex[] := SimplicialComplex[{}]


SimplicialComplex[reg : (_MeshRegion | _BoundaryMeshRegion), opts : OptionsPattern[]] :=
	SimplicialComplex[
		Flatten[MeshCells[reg, All]] /.
			{Point[v_] :> Simplex[{v}], Line[v_] | Polygon[v_] | Tetrahedron[v_] :> Simplex[v]},
		opts]


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
(*New complexes from old*)


(* ::Text:: *)
(*Join of given simplicial complexes:*)


ClearAll[SimplicialComplexJoin];
SimplicialJoin::usage =
	"SimplicialJoin[sc1, sc2, ...] constructs the join product of given simplicial complexes.";
SimplicialJoin[sc_?SimplicialComplexQ] :=
	sc
SimplicialJoin[sc1_?SimplicialComplexQ, sc2_?SimplicialComplexQ] :=
	SimplicialComplex[
		Flatten[Table[
			SimplexJoin[x, y], {x, sc1["Facets"]},
				{y, sc2["Facets"]}], 1],
		"MaximalityCheck" -> False]
SimplicialJoin[
	sc1_?SimplicialComplexQ,
	sc2_?SimplicialComplexQ,
	scs__?SimplicialComplexQ,
	opts : OptionsPattern[]
] :=
	Fold[
		SimplicialJoin,
		SimplicialJoin[sc1, sc2],
        {scs}]


(* ::Text:: *)
(*Cone, which is now a join with single point:*)


ClearAll[SimplicialCone];
SimplicialCone::usage =
	"SimplicialCone[sc] returns the cone of the simplicial complex sc.";
SimplicialCone[sc_?SimplicialComplexQ] :=
	SimplicialJoin[
		sc, SimplicialComplex["Point"]]


(* ::Text:: *)
(*Suspension, which is now a join with 2 distinct points:*)


ClearAll[SimplicialSuspension];
SimplicialSuspension::usage =
	"SimplicialSuspension[sc] returns the suspension of the simplicial complex sc.";
SimplicialSuspension[sc_?SimplicialComplexQ] :=
	SimplicialJoin[
		sc, SimplicialComplex[{"Circle", 0}]]


(* ::Text:: *)
(*Star of sigma, the complex formed by all simplices which contain it:*)


ClearAll[SimplicialStar];
SimplicialStar::usage =
	"SimplicialStar[sc, sigma] returns the star of sigma in sc.";
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


(* ::Text:: *)
(*Link of sigma, the complex formed by all simplices that are disjoint from it and whose union with it is a simplex of the complex:*)


ClearAll[SimplicialLink];
SimplicialLink::usage =
	"SimplicialLink[sc, sigma] returns the link of sigma in sc.";
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


(* ::Text:: *)
(*Simplicial product of given complexes:*)


ClearAll[SimplicialProduct];
SimplicialProduct::usage =
	"SimplicialProduct[sc1, sc2, ...] returns the simplicial product of given simplicial complexes.";
SimplicialProduct[sc_?SimplicialComplexQ] :=
	sc
SimplicialProduct[sc1_?SimplicialComplexQ, sc2_?SimplicialComplexQ] :=
	SimplicialComplex[
		DeleteDuplicates @ Flatten[
		Outer[SimplexProduct, sc1["Facets"], sc2["Facets"], 1],
		2], "MaximalityCheck" -> False]
SimplicialProduct[
	sc1_?SimplicialComplexQ,
	sc2_?SimplicialComplexQ,
	scs__?SimplicialComplexQ
] :=
	Fold[
		SimplicialProduct,
		SimplicialProduct[sc1, sc2],
        {scs}]


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
(*Automorphism group*)


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


(* ::Section:: *)
(*Package Footer*)


End[];
