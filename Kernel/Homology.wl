(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


Begin["Taggar`SimplicialHomology`Private`"];


(* ::Section:: *)
(*Definitions*)


(* ::Subsection:: *)
(*Betti number*)


ClearAll[BettiNumber];
BettiNumber::usage =
	"BettiNumber[sc, n] computes the nth betti number of sc.
BettiNumber[sc] computes all betti numbers of sc.
BettiNumber[sc, sub, n] computes the nth betti number of sc relative to sub.
BettiNumber[sc, sub] computes all betti numbers of sc relative to sub.";
General::NonPrimeCoefficients =
	"\"Coefficients\" specification `1` must be a prime number.";
General::InvalidSubcomplex =
	"`1` is not a valid subcomplex.";
Options[BettiNumber] = {"Reduced" -> False, "Coefficients" -> Integers};


(* ::Text:: *)
(*Special case the empty complex:*)


BettiNumber[sc_?EmptyComplexQ, sub_?SimplicialComplexQ, k : _Integer, opts : OptionsPattern[]] := 0


(* ::Text:: *)
(*0th betti number of a complex:*)


BettiNumber[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, 0, opts : OptionsPattern[]] :=
	Module[
		{b0, coeffs = OptionValue["Coefficients"]},
		
		If[MatchQ[coeffs, _Integer] \[And] \[Not] PrimeQ[coeffs],
			Message[BettiNumber::NonPrimeCoefficients, coeffs]; Return[$Failed]];
		
		If[\[Not] SubComplexQ[sc, sub],
			Message[BettiNumber::InvalidSubcomplex, sub]; Return[$Failed]];
		
		b0 = Length @ Complement[Simplices[sc, 0], Simplices[sub, 0]] -
			BoundaryRank[sc, sub, 1, coeffs];
		
		If[TrueQ @ OptionValue["Reduced"] \[And] EmptyComplexQ[sub],
			Max[0, b0 - 1], b0]]


(* ::Text:: *)
(*General case:*)


(* upto dimension *)
BettiNumber[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, k : _Integer?Positive, opts : OptionsPattern[]] /;
	k <= Dimension[sc] :=
	Module[
		{coeffs = OptionValue["Coefficients"], size},
		
		If[MatchQ[coeffs, _Integer] \[And] \[Not] PrimeQ[coeffs],
			Message[BettiNumber::NonPrimeCoefficients, coeffs]; Return[$Failed]];
		
		If[\[Not] SubComplexQ[sc, sub],
			Message[BettiNumber::InvalidSubcomplex, sub]; Return[$Failed]];
		
		size = Length @ Complement[Simplices[sc, k], Simplices[sub, k]];
			
		If[k == Dimension[sc],
			size - BoundaryRank[sc, sub, k, coeffs],
			size - BoundaryRank[sc, sub, k, coeffs] - BoundaryRank[sc, sub, k+1, coeffs]]]
(* beyond dimension *)
BettiNumber[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, k : _Integer, opts : OptionsPattern[]] := 0


(* ::Text:: *)
(*All betti numbers (in association form) :*)


BettiNumber[sc_?EmptyComplexQ, sub_?SimplicialComplexQ, opts : OptionsPattern[]] := <||> (* empty *)
BettiNumber[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, opts : OptionsPattern[]] := (* non empty *)
	Association @ Table[
		k -> BettiNumber[sc, sub, k, opts], {k, 0, Dimension[sc]}]


(* ::Text:: *)
(*Absolute betti numbers:*)


BettiNumber[sc_?SimplicialComplexQ, k_Integer?NonNegative, opts : OptionsPattern[]] :=
	BettiNumber[sc, SimplicialComplex[], k, opts]
BettiNumber[sc_?SimplicialComplexQ, opts : OptionsPattern[]] :=
	BettiNumber[sc, SimplicialComplex[], opts]


(* ::Subsection:: *)
(*Homology group*)


ClearAll[HomologyGroup];
HomologyGroup::usage =
	"HomologyGroup[sc, n] computes the nth homology group of sc.
HomologyGroup[sc] computes all homology groups of sc.
HomologyGroup[sc, sub, n] computes the nth homology group of sc relative to sub.
HomologyGroup[sc, sub] computes all homology groups of sc relative to sub.";
Options[HomologyGroup] = {"Reduced" -> False, "Coefficients" -> Integers};


(* ::Text:: *)
(*Likewise as for betti numbers we follow the same scheme, first off special case the empty complex:*)


HomologyGroup[sc_?EmptyComplexQ, sub_?SimplicialComplexQ, n : _Integer, opts : OptionsPattern[]] := 0


(* ::Text:: *)
(*0th homology group:*)


HomologyGroup[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, 0, opts : OptionsPattern[]] :=
	Module[
		{o, coeffs = OptionValue["Coefficients"], grp},
		
		If[MatchQ[coeffs, _Integer] \[And] \[Not] PrimeQ[coeffs],
			Message[HomologyGroup::NonPrimeCoefficients, coeffs]; Return[$Failed]];
		
		If[\[Not] SubComplexQ[sc, sub],
			Message[HomologyGroup::InvalidSubcomplex, sub]; Return[$Failed]];
			
		o = Max[0,
			Length @ Complement[Simplices[sc, 0], Simplices[sub, 0]] -
				BoundaryRank[sc, sub, 1, coeffs] - Boole[
					TrueQ[OptionValue["Reduced"]] \[And] EmptyComplexQ[sub]]];
		
		Switch[coeffs,
			Integers | Rationals, FormatGroup[o, {}, coeffs],
			_Integer?PrimeQ, FormatGroup[o, {}, coeffs]]]


(* ::Text:: *)
(*General case:*)


(* upto dimension *)
HomologyGroup[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, k_Integer?Positive, opts : OptionsPattern[]] /;
	k <= Dimension[sc] :=
	Module[
		{\[Delta]kp1, diag, torsion, free, out, size, coeffs = OptionValue["Coefficients"]},
		
		If[MatchQ[coeffs, _Integer] \[And] \[Not] PrimeQ[coeffs],
			Message[HomologyGroup::NonPrimeCoefficients, coeffs]; Return[$Failed]];
		
		If[\[Not] SubComplexQ[sc, sub],
			Message[HomologyGroup::InvalidSubcomplex, sub]; Return[$Failed]];
		
		size = Length @ Complement[Simplices[sc, k], Simplices[sub, k]];
		
		\[Delta]kp1 = BoundaryMatrix[sc, sub, k + 1];
		
		free = size - BoundaryRank[sc, sub, k, coeffs] -
			BoundaryRank[sc, sub, k+1, coeffs];
		
		Switch[coeffs,
			Integers,
				diag = If[Times @@ Dimensions[\[Delta]kp1] == 0, {},
					DeleteCases[
						Diagonal[getSmith[sc, sub, k + 1]], 0 | 1]];
			
				out = FormatGroup[free, diag, coeffs],			
			Rationals, out = FormatGroup[free, {}, coeffs],
			_Integer?PrimeQ, out = FormatGroup[free, {}, coeffs]]]
(* beyond dimension *)
HomologyGroup[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, _Integer, opts : OptionsPattern[]] := 0


(* ::Text:: *)
(*All groups in association form:*)


HomologyGroup[sc_?EmptyComplexQ, sub_?SimplicialComplexQ, opts : OptionsPattern[]] := <||> (* empty *)
HomologyGroup[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, opts : OptionsPattern[]] := (* non empty *)
	Association @ Table[
		k -> HomologyGroup[sc, sub, k, opts], {k, 0, Dimension[sc]}]


(* ::Text:: *)
(*Absolute homology:*)


HomologyGroup[sc_?SimplicialComplexQ, k_Integer?NonNegative, opts : OptionsPattern[]] :=
	HomologyGroup[sc, SimplicialComplex[], k, opts]
HomologyGroup[sc_?SimplicialComplexQ, opts : OptionsPattern[]] :=
	HomologyGroup[sc, SimplicialComplex[], opts]


(* ::Section:: *)
(*Package Footer*)


End[];
