PacletDirectoryLoad[DirectoryName[DirectoryName[$TestFileName]]];
Needs["Taggar`SimplicialHomology`"];

TestCreate[SimplicialAutomorphismGroup[SimplicialComplex["Point"]], PermutationGroup[{}], TestID -> "Automorphism-Point"]
TestCreate[SimplicialAutomorphismGroup[SimplicialComplex["Line"]], PermutationGroup[{Cycles[{{1, 2}}]}], TestID -> "Automorphism-Line"]
TestCreate[SimplicialAutomorphismGroup[SimplicialComplex["Circle"]], PermutationGroup[{Cycles[{{1, 2}}], Cycles[{{2, 3}}]}], TestID -> "Automorphism-Circle"]
TestCreate[SimplicialAutomorphismGroup[SimplicialComplex["Sphere"]], PermutationGroup[{Cycles[{{1, 2}}], Cycles[{{2, 3}}], Cycles[{{3, 4}}]}], TestID -> "Automorphism-Sphere"]
TestCreate[SimplicialAutomorphismGroup[SimplicialComplex["Torus"]], PermutationGroup[{Cycles[{{2, 3, 5, 7, 6, 4}}], Cycles[{{1, 2}, {3, 4}, {6, 7}}]}], TestID -> "Automorphism-Torus"]
TestCreate[SimplicialAutomorphismGroup[SimplicialComplex["KleinBottle"]], PermutationGroup[{Cycles[{{1, 8}, {2, 5}, {3, 6}, {4, 7}}]}], TestID -> "Automorphism-KleinBottle"]
TestCreate[SimplicialAutomorphismGroup[SimplicialComplex["MobiusStrip"]], PermutationGroup[{Cycles[{{1, 3}, {4, 5}}], Cycles[{{1, 4}, {2, 3}}]}], TestID -> "Automorphism-MobiusStrip"]
TestCreate[SimplicialAutomorphismGroup[SimplicialComplex["RealProjectivePlane"]], PermutationGroup[{Cycles[{{3, 4}, {5, 6}}], Cycles[{{2, 3}, {4, 5}}], Cycles[{{1, 2}, {3, 4}}]}], TestID -> "Automorphism-RP2"]
TestCreate[SimplicialAutomorphismGroup[SimplicialComplex["ComplexProjectivePlane"]], PermutationGroup[{Cycles[{{4, 6, 5}, {7, 8, 9}}], Cycles[{{2, 3}, {5, 6}, {8, 9}}], Cycles[{{1, 2}, {4, 6}, {8, 9}}], Cycles[{{1, 4, 7}, {2, 5, 8}, {3, 6, 9}}]}], TestID -> "Automorphism-CP2"]

TestCreate[GroupOrder[SimplicialAutomorphismGroup[SimplicialComplex["Point"]]], 1, TestID -> "AutomorphismOrder-Point"]
TestCreate[GroupOrder[SimplicialAutomorphismGroup[SimplicialComplex["Line"]]], 2, TestID -> "AutomorphismOrder-Line"]
TestCreate[GroupOrder[SimplicialAutomorphismGroup[SimplicialComplex["Circle"]]], 6, TestID -> "AutomorphismOrder-Circle"]
TestCreate[GroupOrder[SimplicialAutomorphismGroup[SimplicialComplex["Sphere"]]], 24, TestID -> "AutomorphismOrder-Sphere"]