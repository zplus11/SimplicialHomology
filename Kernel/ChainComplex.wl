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
        "Differentials" -> <| (_Integer?NonNegative -> _SparseArray) ... |>,
        "Coefficients" -> Integers | Rationals | _Integer?PrimeQ }]
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
ChainComplex::InvalidCoefficients =
	"The coefficients specification `1` is invalid.";
ChainComplex::InvalidOptionValue =
	"The option value \"`1`\" -> `2` is invalid.";
ChainComplex::IncompatibleDifferentials =
	"The differentials \[PartialD]_`1` and \[PartialD]_`2` are not composable.";
ChainComplex::NonZeroComposition =
	"The dot product \[PartialD]_`1` \[CenterDot] \[PartialD]_`2` is not zero.";
Options[ChainComplex] = {"Coefficients" -> Integers, "DifferentialsCheck" -> True};


ChainComplex[data : <| (_Integer?NonNegative -> (_?SparseArrayQ | _?MatrixQ)) ... |>,
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
			
			diffs = Sort @ Map[SparseArray, data];
			
			Catch[If[dcheck,
				Do[
					If[KeyExistsQ[diffs, n + 1],
						If[Dimensions[diffs[n]] [[2]] =!=
							Dimensions[diffs[n + 1]] [[1]],
							Message[ChainComplex::IncompatibleDifferentials, n, n + 1];
							Throw[$Failed]];
						
						If[diffs[n] . diffs[n + 1] =!=
							SparseArray[{}, {Dimensions[diffs[n]][[1]],
							Dimensions[diffs[n + 1]][[2]]}],
							Message[ChainComplex::NonZeroComposition, n, n + 1];
							Throw[$Failed]]],
					{n, Keys @ diffs}]];
			
			Throw @ ChainComplexObject[
				<|"Differentials" -> Map[SparseArray, Sort @ data],
					"Coefficients" -> coeffs |>]]]


(* ::Section:: *)
(*Package Footer*)


End[];
