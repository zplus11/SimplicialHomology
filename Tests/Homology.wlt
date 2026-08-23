PacletDirectoryLoad[DirectoryName[DirectoryName[$TestFileName]]];
Needs["Taggar`SimplicialHomology`"];

TestCreate[HomologyGroup[SimplicialComplex["Point"]], <|0 -> Integers|>, TestID -> "Homology-Point"]
TestCreate[HomologyGroup[SimplicialComplex["Line"]], <|0 -> Integers, 1 -> 0|>, TestID -> "Homology-Line"]
TestCreate[HomologyGroup[SimplicialComplex["Circle"]], <|0 -> Integers, 1 -> Integers|>, TestID -> "Homology-Circle"]
TestCreate[HomologyGroup[SimplicialComplex["Sphere"]], <|0 -> Integers, 1 -> 0, 2 -> Integers|>, TestID -> "Homology-Sphere"]
TestCreate[HomologyGroup[SimplicialComplex["Torus"]], <|0 -> Integers, 1 -> Superscript[Integers, 2], 2 -> Integers|>, TestID -> "Homology-Torus"]
TestCreate[HomologyGroup[SimplicialComplex["KleinBottle"]], <|0 -> Integers, 1 -> Integers\[CirclePlus]Subscript[Integers, 2], 2 -> 0|>, TestID -> "Homology-KleinBottle"]
TestCreate[HomologyGroup[SimplicialComplex["MobiusStrip"]], <|0 -> Integers, 1 -> Integers, 2 -> 0|>, TestID -> "Homology-MobiusStrip"]
TestCreate[HomologyGroup[SimplicialComplex["RealProjectivePlane"]], <|0 -> Integers, 1 -> Subscript[Integers, 2], 2 -> 0|>, TestID -> "Homology-RP2"]
TestCreate[HomologyGroup[SimplicialComplex["ComplexProjectivePlane"]], <|0 -> Integers, 1 -> 0, 2 -> Integers, 3 -> 0, 4 -> Integers|>, TestID -> "Homology-CP2"]

TestCreate[BettiNumber[SimplicialComplex["Point"]], <|0 -> 1|>, TestID -> "Betti-Point"]
TestCreate[BettiNumber[SimplicialComplex["Line"]], <|0 -> 1, 1 -> 0|>, TestID -> "Betti-Line"]
TestCreate[BettiNumber[SimplicialComplex["Circle"]], <|0 -> 1, 1 -> 1|>, TestID -> "Betti-Circle"]
TestCreate[BettiNumber[SimplicialComplex["Sphere"]], <|0 -> 1, 1 -> 0, 2 -> 1|>, TestID -> "Betti-Sphere"]
TestCreate[BettiNumber[SimplicialComplex["Torus"]], <|0 -> 1, 1 -> 2, 2 -> 1|>, TestID -> "Betti-Torus"]
TestCreate[BettiNumber[SimplicialComplex["KleinBottle"]], <|0 -> 1, 1 -> 1, 2 -> 0|>, TestID -> "Betti-KleinBottle"]
TestCreate[BettiNumber[SimplicialComplex["MobiusStrip"]], <|0 -> 1, 1 -> 1, 2 -> 0|>, TestID -> "Betti-MobiusStrip"]
TestCreate[BettiNumber[SimplicialComplex["RealProjectivePlane"]], <|0 -> 1, 1 -> 0, 2 -> 0|>, TestID -> "Betti-RP2"]
TestCreate[BettiNumber[SimplicialComplex["ComplexProjectivePlane"]], <|0 -> 1, 1 -> 0, 2 -> 1, 3 -> 0, 4 -> 1|>, TestID -> "Betti-CP2"]

TestCreate[HomologyGroup[SimplicialComplex["Circle"], "CoHomology" -> True], <|0 -> Integers, 1 -> Integers|>, TestID -> "Cohomology-Circle"]
TestCreate[HomologyGroup[SimplicialComplex["Sphere"], "CoHomology" -> True], <|0 -> Integers, 1 -> 0, 2 -> Integers|>, TestID -> "Cohomology-Sphere"]
TestCreate[HomologyGroup[SimplicialComplex["Torus"], "CoHomology" -> True], <|0 -> Integers, 1 -> Superscript[Integers, 2], 2 -> Integers|>, TestID -> "Cohomology-Torus"]
TestCreate[HomologyGroup[SimplicialComplex["RealProjectivePlane"], "CoHomology" -> True], <|0 -> Integers, 1 -> 0, 2 -> Subscript[Integers, 2]|>, TestID -> "Cohomology-RP2"]