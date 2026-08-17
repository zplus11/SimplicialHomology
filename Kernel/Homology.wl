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
General::InvalidOptionValue =
	"The option value `2` for `1` is invalid. Reason: `3`.";
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
		{b0, coeffs = OptionValue["Coefficients"], cc},
		
		If[MatchQ[coeffs, _Integer] \[And] \[Not] PrimeQ[coeffs],
			Message[BettiNumber::InvalidOptionValue, "Coefficients", coeffs,
				"expected Integers, Rationals, or a prime number"]; Return[$Failed]];
		
		If[\[Not] SubComplexQ[sc, sub],
			Message[BettiNumber::InvalidSubcomplex, sub]; Return[$Failed]];
		
		cc = ChainComplex[sc, sub];
		
		b0 = Length @ Complement[Simplices[sc, 0], Simplices[sub, 0]] -
			BoundaryRank[cc, 1, coeffs];
		
		If[TrueQ @ OptionValue["Reduced"] \[And] EmptyComplexQ[sub],
			Max[0, b0 - 1], b0]]


(* ::Text:: *)
(*General case:*)


(* upto dimension *)
BettiNumber[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, k : _Integer?Positive, opts : OptionsPattern[]] /;
	k <= Dimension[sc] :=
	Module[
		{coeffs = OptionValue["Coefficients"], size, cc},
		
		If[MatchQ[coeffs, _Integer] \[And] \[Not] PrimeQ[coeffs],
			Message[BettiNumber::NonPrimeCoefficients, "Coefficients", coeffs,
				"expected Integers, Rationals, or a prime number"]; Return[$Failed]];
		
		If[\[Not] SubComplexQ[sc, sub],
			Message[BettiNumber::InvalidSubcomplex, sub]; Return[$Failed]];
		
		cc = ChainComplex[sc, sub];
		
		size = Length @ Complement[Simplices[sc, k], Simplices[sub, k]];
			
		If[k == Dimension[sc],
			size - BoundaryRank[cc, k, coeffs],
			size - BoundaryRank[cc, k, coeffs] - BoundaryRank[cc, k+1, coeffs]]]
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
Options[HomologyGroup] = {"Reduced" -> False, "Coefficients" -> Integers, "CoHomology" -> False};


(* ::Text:: *)
(*Homology group of a chain complex:*)


HomologyGroup[cc_?ChainComplexQ, dims : { _Integer?NonNegative ... }, opts : OptionsPattern[]] :=
	Module[
		{coeffs, red, co, dimCk, rankOut, rankIn,
			matK, matKp1, smithDiag = {}},
		
		coeffs = OptionValue["Coefficients"];
		red = OptionValue["Reduced"];
		co = OptionValue["CoHomology"];
		
		If[MatchQ[coeffs, _Integer] \[And] \[Not] PrimeQ[coeffs],
			Message[General::InvalidOptionValue, "Coefficients", coeffs,
				"expected Integers, Rationals, or a prime number"]; Return[$Failed]];
		
		If[\[Not] BooleanQ[red],
			Message[General::InvalidOptionValue, "Reduced", red,
				"expected boolean value"]; Return[$Failed]];
		
		If[\[Not] BooleanQ[co],
			Message[General::InvalidOptionValue, "CoHomology", co,
				"expected boolean value"]; Return[$Failed]];
		
		Association @ Table[
			matK = cc[{"Differential", k}];
			dimCk = ChainGroupDimension[cc, k];
		
			rankOut = BoundaryRank[cc, k, coeffs];
			rankIn  = BoundaryRank[cc, k + 1, coeffs];
		
			If[co,
				(* for cohomology *)
				If[coeffs === Integers && k > 0,
					matK = cc[{"Differential", k}];
					If[Times @@ Dimensions[matK] > 0,
						smithDiag = Diagonal @ getSmith[cc, k]]],
				(* for homology *)
				If[coeffs === Integers,
					matKp1 = cc[{"Differential", k + 1}];
					If[Times @@ Dimensions[matKp1] > 0,
						smithDiag = Diagonal @ getSmith[cc, k + 1]]]];
		
			k -> ComputeAlgebraicHomology[dimCk, rankOut, rankIn,
				smithDiag, coeffs, k, red],
			{k, dims}]]


HomologyGroup[cc_?ChainComplexQ, k_Integer?NonNegative, opts : OptionsPattern[]] :=
	HomologyGroup[cc, {k}, opts][k]


HomologyGroup[cc_?ChainComplexQ, opts : OptionsPattern[]] :=
	HomologyGroup[cc, Range[0, cc["Dimension"]], opts]


(* ::Text:: *)
(*Now, likewise as for betti numbers we follow the same scheme, first off special case the empty complex:*)


HomologyGroup[sc_?EmptyComplexQ, sub_?SimplicialComplexQ, k_Integer?NonNegative, opts : OptionsPattern[]] := 0


(* ::Text:: *)
(*Special case for 0th homology group:*)


(* ::Text:: *)
(*Due to representational reasons, 0th homology of chain complexes cannot yet be computed. So we fall back to computing it from scratch rather than (completely) routing through chain complexes:*)


HomologyGroup[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, 0 | {0}, opts : OptionsPattern[]] :=
	Module[
		{coeffs = OptionValue["Coefficients"],
		 red = OptionValue["Reduced"],
		 co = OptionValue["CoHomology"],
		 cc, dimC0, rankIn},

		If[!SubComplexQ[sc, sub],
			Message[General::InvalidSubcomplex, sub]; Return[$Failed]];

		cc = ChainComplex[sc, sub, "Dimensions" -> {1}];
		dimC0 = Length @ Complement[Simplices[sc, 0], Simplices[sub, 0]];
		rankIn = BoundaryRank[cc, 1, coeffs];

		ComputeAlgebraicHomology[
			dimC0, 0, rankIn, {}, coeffs, 0, red]]


(* ::Text:: *)
(*General case:*)


HomologyGroup[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, dims : { _Integer?NonNegative ... }, opts : OptionsPattern[]] :=
	Module[
		{cc},
		
		If[\[Not] SubComplexQ[sc, sub],
			Message[General::InvalidSubcomplex, sub]; Return[$Failed]];
		
		Association @ Table[
			k -> Which[
				k > sc["Dimension"], (* trivial case *)
					0,
				k == 0,  (* detour *)
					HomologyGroup[sc, sub, 0, opts],
				True, (* else *)
					(* we only need the kth and k+1th differentials, this 'Dimensions': *)
					cc = ChainComplex[sc, sub, "Dimensions" -> {k, k + 1}];
					HomologyGroup[cc, k, opts]], {k, dims}]]


HomologyGroup[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, k_Integer?Positive, opts : OptionsPattern[]] :=
	HomologyGroup[sc, sub, {k}, opts][k]


(* ::Text:: *)
(*All groups in association form:*)


HomologyGroup[sc_?EmptyComplexQ, sub_?SimplicialComplexQ, opts : OptionsPattern[]] := <||> (* empty *)
HomologyGroup[sc_?SimplicialComplexQ, sub_?SimplicialComplexQ, opts : OptionsPattern[]] := (* non empty *)
	If[sc["Dimension"] == 0,
		<|0 -> HomologyGroup[sc, sub, Range[0, sc["Dimension"]], opts]|>,
		HomologyGroup[sc, sub, Range[0, sc["Dimension"]], opts]]


(* ::Text:: *)
(*Absolute homology:*)


HomologyGroup[sc_?SimplicialComplexQ, k_Integer?NonNegative, opts : OptionsPattern[]] :=
	HomologyGroup[sc, SimplicialComplex[], k, opts]
HomologyGroup[sc_?SimplicialComplexQ, opts : OptionsPattern[]] :=
	HomologyGroup[sc, SimplicialComplex[], opts]


(* ::Section:: *)
(*Package Footer*)


End[];
