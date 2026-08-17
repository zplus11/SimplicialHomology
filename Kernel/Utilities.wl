(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


Begin["Taggar`SimplicialHomology`Private`"];


(* ::Section:: *)
(*Definitions*)


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


EuChar[sc_SimplicialComplexObject] :=
	If[sc["FVector"] === {},
		0,
		Total @ MapIndexed[
			(-1)^(First[#2] - 1) #1 &,
			Rest @ sc["FVector"]]]


(* ::Subsection:: *)
(*Operations on simplices*)


SimplexJoin[s1_List, s2_List] :=
	Join[Table[Subscript[i, "L"], {i, s1}],
		Table[Subscript[j, "R"], {j, s2}]]


SimplexInQ[sc_, sim_] :=
	MemberQ[
		(* if sim is of length k, then whether it is a
		   part of the (k-1)-dimensional simplices of sc *)
		Lookup[sc["Simplices"], Length @ DeleteDuplicates @ sim - 1, {}],
		sim]


SimplexProduct[s1_List, s2_List] :=
    (Point /@ #) & /@ LatticePaths[s1, s2]


(* ::Text:: *)
(*Lattice paths:*)


LatticePaths[t1_, t2_, length_ : Automatic] :=
	Which[
		length == Automatic,
			Which[
				Length[t1] == 0 \[Or] Length[t2] == 0, {{}},
				Length[t1] == 1, {Thread[{ConstantArray[First @ t1, Length @ t2], t2}]},
				Length[t2] == 1, {Thread[{t1, ConstantArray[First[t2], Length[t1]]}]},
				True,
					Join[Append[#, {Last @ t1, Last @ t2}] & /@ LatticePaths[Most @ t1, t2],
					     Append[#, {Last @ t1, Last @ t2}] & /@ LatticePaths[t1, Most @ t2]]],
		length > Length @ t1 + Length @ t2 - 1, {},
		Length[t1] == 0 \[Or] Length[t2] == 0,
			If[length == 0, {{}}, {}],
		Length[t1] == 1,
			If[length == Length @ t2, {Thread[{ConstantArray[First[t1], Length[t2]], t2}]}, {}],
		Length[t2] == 1,
			If[length == Length @ t1, {Thread[{t1, ConstantArray[First[t2], Length[t1]]}]}, {}],
		True,
			Join[Append[#, {Last[t1], Last[t2]}] & /@ LatticePaths[Most[t1], t2, length - 1],
			     Append[#, {Last[t1], Last[t2]}] & /@ LatticePaths[t1, Most[t2], length - 1],
			     Append[#, {Last[t1], Last[t2]}] & /@ LatticePaths[Most[t1], Most[t2], length - 1]]]


ClearAll[ChainGroupDimension];
ChainGroupDimension[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, k_Integer] :=
	Length @ Complement[Simplices[sc, k], Simplices[sub, k]]
ChainGroupDimension[sc_?SimplicialComplexQ, k_Integer] :=
	ChainGroupDimension[sc, SimplicialComplex[], k]
ChainGroupDimension[cc_?ChainComplexQ, k_Integer] :=
	Module[
		{diffs = cc["Differentials"], mat},
		
		If[k < 0 || ! KeyExistsQ[diffs, k],
			0,
			mat = diffs[k];
			If[mat === {}, 0, Dimensions[mat][[2]]]]]


(* ::Subsection:: *)
(*Boundary matrices*)


ClearAll[BoundaryMatrix];
BoundaryMatrix[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, 0] := 
    SparseArray[{}, {0, Length @ Complement[Simplices[sc, 0], Simplices[sub, 0]]}]
BoundaryMatrix[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, k_Integer?Positive] := 
	Module[
		
		{rows, cols, lookup, rules},
		
		If[k > sc["Dimension"], Return[{}]]; (* easy exit *)
		
		(* the basis for C_k(sc, sub) and C_{k-1}(sc, sub) *)
		cols = Complement[Simplices[sc, k], Simplices[sub, k]];
		rows = Complement[Simplices[sc, k - 1], Simplices[sub, k - 1]];
		
		If[Length[cols] == 0, Return[SparseArray[{}, {Length[rows], 0}]]];
		If[Length[rows] == 0, Return[SparseArray[{}, {0, Length[cols]}]]];
		
		lookup = AssociationThread[rows -> Range[Length[rows]]];
		
		rules = Flatten @ MapIndexed[
			Function[{simp, col},
				Map[
					If[KeyExistsQ[lookup, #[[1]]],
						{lookup[#[[1]]], col[[1]]} -> #[[2]],
						Nothing] &,
					Boundary[simp]]],
			cols];
		
		SparseArray[rules, {Length[rows], Length[cols]}]]
(* now absolute matrix is just a special case of relative *)
BoundaryMatrix[sc_?SimplicialComplexQ, k_Integer?NonNegative] :=
	BoundaryMatrix[sc, SimplicialComplex[], k]


BoundaryRank[cc_?ChainComplexQ, k_, coeffs_] :=
	Module[
		{cached, rank, mat},
		
		cached = CacheGet[cc, {"BoundaryRank", k, coeffs}];
		If[cached =!= Missing["NotCached"], Return[cached]];
		
		mat = cc[{"Differential", k}];
		
		rank = If[Times @@ Dimensions[mat] == 0, 0,
			Switch[coeffs,
				Integers | Rationals, MatrixRank[mat],
				_Integer, MatrixRank[mat, Modulus -> coeffs],
				_,
					Message[BoundaryRank::coeffs, coeffs];
					Return[$Failed]]];
		
		CacheSet[cc, {"BoundaryRank", k, coeffs}, rank]]


(* ::Subsection:: *)
(*Homology groups*)


ClearAll[FormatGroup];
FormatGroup[0, {}, coeffs_] := 0
FormatGroup[free_, torsion_List, coeffs_] :=
	Module[
		{freePart, torsionPart, parts},
		
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
		
		torsionPart = Which[
			torsion === {}, Nothing,
			Length[torsion] == 1, Subscript[Integers, First @ torsion],
			True, CirclePlus @@ Thread[Subscript[Integers, torsion]]];
		
		parts = {freePart, torsionPart};
		
		Which[
			parts === {Nothing, Nothing}, 0,
			Count[parts, Except[Nothing]] == 1, First@DeleteCases[parts, Nothing],
			True, CirclePlus @@ DeleteCases[parts, Nothing]]]


getSmith[cc_?ChainComplexQ, k_] :=
	Module[
		{cached, res},
		
		(* Smith decomposition of k + 1th boundary matrix *)
		cached = CacheGet[cc, {"Smith", k}];
		
		If[cached =!= Missing["NotCached"],
			Return[cached]];
		
		res = SmithReduce[cc[{"Differential", k}]];
		CacheSet[cc, {"Smith", k}, res]]


ClearAll[ComputeAlgebraicHomology];
ComputeAlgebraicHomology[
	dimCk_, rankOut_, rankIn_, smithDiag_, coeffs_, k_Integer, reduced_ : False
] :=
	Module[
		{free, torsion},
		
		free = Max[0, dimCk - rankOut - rankIn - Boole[TrueQ[reduced] \[And] (k == 0)]];
		torsion = Switch[coeffs, Integers, DeleteCases[smithDiag, 0 | 1], _, {}];
		
		FormatGroup[free, torsion, coeffs]]


(* ::Subsection:: *)
(*Incidence graphs*)


SimplicialIncidenceGraph[sc_] :=
	Module[
		{verts, facets, v, f},
		verts = v /@ sc["Vertices"];
		facets = sc["Facets"];
		
		Graph[
			Join[verts, f /@ Range[Length[facets]]], 
			Flatten @ MapIndexed[Thread[DirectedEdge[v /@ #1, f[#2[[1]]]]] &,
				facets]]]


(* ::Section:: *)
(*Package Footer*)


End[];
