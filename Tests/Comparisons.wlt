PacletDirectoryLoad[DirectoryName[DirectoryName[$TestFileName]]];
Needs["Taggar`SimplicialHomology`"];

TestCreate[SimplicialComplex[{{1, 2, 3}}] === SimplicialComplex[{{1, 2, 3}}], True, TestID -> "Equality-Identical"]
TestCreate[SimplicialComplex[{{1, 2, 3}}] === SimplicialComplex[{{3, 1, 2}}], True, TestID -> "Equality-VertexOrder"]
TestCreate[SimplicialComplex[{{1, 2, 3}, {3, 4}}] === SimplicialComplex[{{3, 4}, {1, 2, 3}}], True, TestID -> "Equality-FacetOrder"]
TestCreate[SimplicialComplex[{{1, 2}}] === SimplicialComplex[{{1, 2, 3}}], False, TestID -> "Equality-DifferentComplexes"]
TestCreate[SimplicialComplex[{{1, 2}, {2, 3}}] === SimplicialComplex[{{1, 2}, {2, 4}}], False, TestID -> "Equality-DifferentVertices"]

TestCreate[SubComplexQ[SimplicialComplex[{{1, 2, 3}}], SimplicialComplex[{{1, 2}}]], True, TestID -> "Subcomplex-EdgeInTriangle"]
TestCreate[SubComplexQ[SimplicialComplex[{{1, 2}}], SimplicialComplex[{{1, 2, 3}}]], False, TestID -> "Subcomplex-TriangleInEdge"]
TestCreate[SubComplexQ[SimplicialComplex[{{1, 2, 3}}], SimplicialComplex[{{1, 2}, {2, 3}}]], True, TestID -> "Subcomplex-BoundaryOfTriangle"]
TestCreate[SubComplexQ[SimplicialComplex[{{1, 2, 3}}], SimplicialComplex[{{1, 2, 3}}]], True, TestID -> "Subcomplex-Self"]
TestCreate[SubComplexQ[SimplicialComplex[{{1, 2, 3}}], SimplicialComplex[{}]], True, TestID -> "Subcomplex-Empty"]
TestCreate[SubComplexQ[SimplicialComplex[{{1, 2, 3}}], SimplicialComplex[{{1, 2}, {3, 4}}]], False, TestID -> "Subcomplex-Disjoint"]

TestCreate[SimplicialIsomorphicQ[SimplicialComplex[{{1, 2, 3}}], SimplicialComplex[{{4, 5, 6}}]], True, TestID -> "Isomorphism-Triangles"]
TestCreate[SimplicialIsomorphicQ[SimplicialComplex[{{1, 2}, {2, 3}, {3, 1}}], SimplicialComplex[{{4, 5}, {5, 6}, {6, 4}}]], True, TestID -> "Isomorphism-Circles"]
TestCreate[SimplicialIsomorphicQ[SimplicialComplex[{{1, 2, 3, 4}}], SimplicialComplex[{{5, 6, 7, 8}}]], True, TestID -> "Isomorphism-Tetrahedra"]
TestCreate[SimplicialIsomorphicQ[SimplicialComplex[{{1, 2}, {2, 3}, {3, 1}}], SimplicialComplex[{{1, 2}, {2, 3}}]], False, TestID -> "Isomorphism-DifferentEdgeCount"]
TestCreate[SimplicialIsomorphicQ[SimplicialComplex[{{1, 2, 3}}], SimplicialComplex[{{1, 2}, {2, 3}, {3, 1}}]], False, TestID -> "Isomorphism-SimplexVsBoundary"]
TestCreate[SimplicialIsomorphicQ[SimplicialComplex[{{1, 2}, {2, 3}}], SimplicialComplex[{{4, 5}, {5, 6}}]], True, TestID -> "Isomorphism-Paths"]
TestCreate[SimplicialIsomorphicQ[SimplicialComplex[{{1, 2}, {2, 3}}], SimplicialComplex[{{4, 5}, {5, 6}, {6, 4}}]], False, TestID -> "Isomorphism-PathVsCycle"]
TestCreate[SimplicialIsomorphicQ[SimplicialComplex[{{1, 2}, {2, 3}, {3, 4}, {4, 1}}], SimplicialComplex[{{5, 6}, {6, 7}, {7, 8}, {8, 5}}]], True, TestID -> "Isomorphism-Squares"]
TestCreate[SimplicialIsomorphicQ[SimplicialComplex[{{1, 2}, {2, 3}, {3, 4}}], SimplicialComplex[{{5, 6}, {6, 7}}]], False, TestID -> "Isomorphism-DifferentVertices"]