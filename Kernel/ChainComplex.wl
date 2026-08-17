(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


Begin["Taggar`SimplicialHomology`Private`"];


(* ::Section:: *)
(*Definitions*)


(* ::Text:: *)
(*A chain complex is represented as follows:*)


ClearAll[ChainComplex, ChainComplexObject, ChainComplexQ];
ChainComplexQ[ChainComplexObject[
	KeyValuePattern[{
		"Differentials" -> <| (_Integer?NonNegative -> _SparseArray | {} | _?MatrixQ) ... |>,
		"UUID" -> _}]
]] := True
ChainComplexQ[_] := False


(* ::Subsection:: *)
(*Some properties:*)


ChainComplexObject /: cc_ChainComplexObject["Differentials"] :=
	cc[[1, "Differentials"]]


ChainComplexObject /: cc_ChainComplexObject[{"Differential", n_Integer?NonNegative}] :=
	If[KeyExistsQ[cc["Differentials"], n], cc["Differentials"][n], {}]


ChainComplexObject /: cc_ChainComplexObject["Dimension"] :=
	Max[0, Keys[cc["Differentials"]]]


(* ::Subsection:: *)
(*Input methods:*)


ChainComplex::usage =
	"ChainComplex[assoc] represents a chain complex defined by assoc of its differential matrices.
ChainComplex[sc] constructs the chain complex associated with the simplicial complex sc.";


ChainComplex::InvalidOptionValue =
	"The option value \"`1`\" -> `2` is invalid.";
ChainComplex::IncompatibleDifferentials =
	"The differentials \[PartialD]_`1` and \[PartialD]_`2` are not composable.";
ChainComplex::NonZeroComposition =
	"The dot product \[PartialD]_`1` \[CenterDot] \[PartialD]_`2` is not zero.";
Options[ChainComplex] = {"DifferentialsCheck" -> True, "Dimensions" -> All, "Dual" -> False};


ChainComplex[data : <| (_Integer?NonNegative -> (_?SparseArrayQ | _?MatrixQ | {})) ... |>,
	opts : OptionsPattern[]] :=
		Module[
			{dcheck = OptionValue["DifferentialsCheck"],
			 diffs,
			 dual = OptionValue["Dual"],
			 dims = OptionValue["Dimensions"]},
			
			If[\[Not] BooleanQ[dcheck],
				Message[ChainComplex::InvalidOptionValue, "DifferentialsCheck", dcheck];
				Return[$Failed]];
			
			If[\[Not] BooleanQ[dual],
				Message[ChainComplex::InvalidOptionValue, "Dual", dual];
				Return[$Failed]];
				
			If[\[Not] (dims === All \[Or] VectorQ[dims, IntegerQ]),
				Message[ChainComplex::InvalidOptionValue, "Dimensions", dims];
					Return[$Failed]];
	
			(* convert all matrices to sparsearrays if not already *)
			diffs = Map[
				mat |-> If[Times @@ Dimensions[mat] == 0, mat, SparseArray[mat]], 
					data];
			
			If[dims =!= All,
				diffs = KeySelect[diffs, MemberQ[dims, #] &]];
			
			(* provision for dual *)
			If[dual,
				If[KeyExistsQ[diffs, 0],
					Message[ChainComplex::InvalidDualDifferential];
					Return[$Failed]];
				diffs = Association @ KeyValueMap[
					(Max[Keys[diffs]] - #1 + 1) -> Transpose[#2] &,
					diffs]];
			
			diffs = KeySort @ diffs;
			
			Catch[
				If[dcheck,
					Do[
						If[KeyExistsQ[diffs, n + 1],
							If[Dimensions[diffs[n]][[2]] !=
								Dimensions[diffs[n + 1]][[1]],
								Message[ChainComplex::IncompatibleDifferentials, n, n + 1];
								Throw[$Failed]];
							
							If[diffs[n] . diffs[n + 1] !=
								SparseArray[{}, {Dimensions[diffs[n]][[1]],
									Dimensions[diffs[n + 1]][[2]]}],
								Message[ChainComplex::NonZeroComposition, n, n + 1];
								Throw[$Failed]]],
						{n, Keys @ diffs}]];
				
				Throw @ ChainComplexObject[
					<|"Differentials" -> diffs,
					"UUID" -> Hash[diffs, "SHA256"]|>]]]


(* ::Text:: *)
(*Way of constructing a chain complex from given sc relative to sub:*)


ChainComplex[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, opts : OptionsPattern[]] :=
	Module[
		{data, dims = OptionValue["Dimensions"]},
		
		dims = If[dims === All,
			Range[1, sc["Dimension"]], dims];
		
		If[\[Not] VectorQ[dims, IntegerQ],
			Message[ChainComplex::InvalidOptionValue, "Dimensions", dims];
			Return[$Failed]];
		
		data = AssociationMap[
			n |-> BoundaryMatrix[sc, sub, n], dims];
		
		ChainComplex[data, opts, "DifferentialsCheck" -> False]]


(* ::Text:: *)
(*Absolute chain complex of sc:*)


ChainComplex[sc_?SimplicialComplexQ, opts : OptionsPattern[]] :=
	ChainComplex[sc, SimplicialComplex[], opts]


(* ::Subsection:: *)
(*Formatting:*)


ChainComplexObject /:
	MakeBoxes[cc_ChainComplexObject?ChainComplexQ, form_] :=
		Module[
			{diffs = cc["Differentials"], dims, icon},
			
			icon = If[diffs == <||>,
				Graphics @ Rectangle[],
				ArrayPlot[
					First[Values[diffs]],
					ImageSize -> {30, 25}]];
			
			dims = Association @ KeyValueMap[
				#1 -> Dimensions[#2] &, diffs];
			
			BoxForm`ArrangeSummaryBox[
				"ChainComplex", cc,
				icon,
				{BoxForm`SummaryItem[{"Dimension: ", If[dims === <||>, 0, Max[Keys[dims]]]}], (* main *)
				 BoxForm`SummaryItem[{"Coefficients: ", Integers}]},
				{BoxForm`SummaryItem[{"Chain Group Dimensions: \n", (* + *)
					Column @ ReplaceAll[dims, Association -> List]}],
				 Nothing}, form, "Interpretable" -> Automatic]]


(* ::Section:: *)
(*Package Footer*)


End[];
