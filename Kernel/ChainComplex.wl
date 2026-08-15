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
        "Differentials" -> <| (_Integer?NonNegative -> _SparseArray | {}) ... |>,
        "Coefficients" -> Integers | Rationals | _Integer?PrimeQ }]
]] := True
ChainComplexQ[_] := False


(* ::Subsection:: *)
(*Some properties:*)


ChainComplexObject /: cc_ChainComplexObject["Differentials"] :=
	cc[[1, "Differentials"]]


ChainComplexObject /: cc_ChainComplexObject["Coefficients"] :=
	cc[[1, "Coefficients"]]


(* ::Subsection:: *)
(*Input methods:*)


ChainComplex::usage =
	"Usage string tbd";


ChainComplex::InvalidCoefficients =
	"The coefficients specification `1` is invalid.";
ChainComplex::InvalidOptionValue =
	"The option value \"`1`\" -> `2` is invalid.";
ChainComplex::IncompatibleDifferentials =
	"The differentials \[PartialD]_`1` and \[PartialD]_`2` are not composable.";
ChainComplex::NonZeroComposition =
	"The dot product \[PartialD]_`1` \[CenterDot] \[PartialD]_`2` is not zero.";
Options[ChainComplex] = {"Coefficients" -> Integers, "DifferentialsCheck" -> True};


ChainComplex[data : <| (_Integer?NonNegative -> (_?SparseArrayQ | _?MatrixQ | {})) ... |>,
	opts : OptionsPattern[]] :=
		Module[
			{coeffs = OptionValue["Coefficients"],
			 dcheck = OptionValue["DifferentialsCheck"],
			 diffs},
			
			If[\[Not] MatchQ[coeffs,
				Integers | Rationals | _Integer?PrimeQ],
				Message[ChainComplex::InvalidCoefficients, coeffs];
				Return[$Failed]];
			
			If[\[Not] BooleanQ[dcheck],
				Message[ChainComplex::InvalidOptionValue, "DifferentialsCheck", dcheck];
				Return[$Failed]];
			
			diffs = KeySort @ Map[
				mat |-> If[mat === {}, {}, SparseArray[mat]], 
					data];
			If[MatchQ[coeffs, _Integer],
				diffs = Map[Mod[#, coeffs] &, diffs]];
			
			Catch[
				If[dcheck,
					Do[
						If[KeyExistsQ[diffs, n + 1],
							If[Dimensions[diffs[n]][[2]] !=
								Dimensions[diffs[n + 1]][[1]],
								Message[ChainComplex::IncompatibleDifferentials, n, n + 1];
								Throw[$Failed]];
							
							If[If[MatchQ[coeffs, _Integer],
									Mod[diffs[n] . diffs[n + 1], coeffs],
									diffs[n] . diffs[n + 1]] !=
								SparseArray[{}, {Dimensions[diffs[n]][[1]],
									Dimensions[diffs[n + 1]][[2]]}],
								Message[ChainComplex::NonZeroComposition, n, n + 1];
								Throw[$Failed]]],
						{n, Keys @ diffs}]];
				
				Throw @ ChainComplexObject[
					<|"Differentials" -> diffs,
						"Coefficients" -> coeffs |>]]]


(* ::Text:: *)
(*Way of constructing a chain complex from the given simplicial complex:*)


ChainComplex[sc_?SimplicialComplexQ, opts : OptionsPattern[]] :=
	Module[
		{data},
		
		data = AssociationMap[
			n |-> BoundaryMatrix[sc, n],
			Range[1, sc["Dimension"]]];
		
		ChainComplex[data, opts]]


(* ::Section:: *)
(*Package Footer*)


End[];
