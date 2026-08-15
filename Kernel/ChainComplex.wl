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
		"Differentials" -> <| (_Integer?NonNegative -> _SparseArray | {}) ... |>}]
]] := True
ChainComplexQ[_] := False


(* ::Subsection:: *)
(*Some properties:*)


ChainComplexObject /: cc_ChainComplexObject["Differentials"] :=
	cc[[1, "Differentials"]]


(* ::Subsection:: *)
(*Input methods:*)


ChainComplex::usage =
	"Usage string tbd";


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
			 dual = OptionValue["Dual"]},
			
			If[\[Not] BooleanQ[dcheck],
				Message[ChainComplex::InvalidOptionValue, "DifferentialsCheck", dcheck];
				Return[$Failed]];
			
			If[\[Not] BooleanQ[dual],
				Message[ChainComplex::InvalidOptionValue, "Dual", dual];
				Return[$Failed]];
			
			diffs = KeySort @ Map[
				mat |-> If[mat === {}, {}, SparseArray[mat]], 
					data];
			
			If[dual, If[KeyExistsQ[diffs, 0],
				Message[ChainComplex::InvalidDualDifferential];
				Return[$Failed]];
				
				diffs = Association @ KeyValueMap[
					(#1 - 1) -> Transpose[#2] &, diffs]];
			
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
					<|"Differentials" -> diffs|>]]]


(* ::Text:: *)
(*Way of constructing a chain complex from the given simplicial complex:*)


ChainComplex[sc_?SimplicialComplexQ, opts : OptionsPattern[]] :=
	Module[
		{data, dims = OptionValue["Dimensions"]},
		
		dims = If[dims === All,
			Range[1, sc["Dimension"]], dims];
		
		If[\[Not] VectorQ[dims, IntegerQ] \[Or] AnyTrue[dims, # < 1 \[Or] # > sc["Dimension"] &],
			Message[ChainComplex::InvalidOptionValue, "Dimensions", dims];
			Return[$Failed]];
		
		data = AssociationMap[
			n |-> BoundaryMatrix[sc, n], dims];
		
		ChainComplex[data, opts, "DifferentialsCheck" -> False]]


(* ::Subsection:: *)
(*Formatting:*)


ChainComplexObject /:
	MakeBoxes[cc_ChainComplexObject?ChainComplexQ, form_] :=
		Module[
			{diffs = cc["Differentials"], dims},
			
			dims = Association @ KeyValueMap[
				#1 -> Dimensions[#2] &, diffs];
			
			BoxForm`ArrangeSummaryBox[
				"ChainComplex", cc,
				ArrayPlot[
					First[Values[diffs]],
					ImageSize -> {30, 25}],
				{BoxForm`SummaryItem[{"Dimension: ", Max[Keys[dims]]}], (* main *)
				 BoxForm`SummaryItem[{"Coefficients: ", Integers}]},
				{BoxForm`SummaryItem[{"Chain Group Dimensions: \n", (* + *)
					Column @ ReplaceAll[dims, Association -> List]}],
				 Nothing}, form, "Interpretable" -> Automatic]]


(* ::Section:: *)
(*Package Footer*)


End[];
