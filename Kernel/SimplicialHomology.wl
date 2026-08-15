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
SimplicialProduct;
BettiNumber;
HomologyGroup;
SimplicialAutomorphismGroup;
SimplicialIsomorphicQ;

ChainComplex;


Begin["`Private`"];


(* ::Subsection:: *)
(*Caching*)


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


(* ::Subsection:: *)
(*Load all files*)


Get /@ {
	"Taggar`SimplicialHomology`SimplicialComplex`",
	"Taggar`SimplicialHomology`Examples`",
	"Taggar`SimplicialHomology`ChainComplex`",
	"Taggar`SimplicialHomology`Homology`",
	"Taggar`SimplicialHomology`Utilities`"
}


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
