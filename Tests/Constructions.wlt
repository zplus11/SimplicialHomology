PacletDirectoryLoad[DirectoryName[DirectoryName[$TestFileName]]];
Needs["Taggar`SimplicialHomology`"];

TestCreate[SimplicialComplex[]["Dimension"], -Infinity, TestID -> "EmptyDimension"]
TestCreate[SimplicialComplex[{}]["Dimension"], -Infinity, TestID -> "EmptyDimension-2"]
TestCreate[SimplicialComplex[{{}}]["Dimension"], -1, TestID -> "VoidDimension"]
TestCreate[SimplicialComplex[{{1}}]["Dimension"], 0, TestID -> "SingeltonDimension"]
TestCreate[SimplicialComplex[{{{}}}]["Dimension"], 0, TestID -> "SingletonDimension-2"]
TestCreate[SimplicialComplex[{{{{}}}}]["Dimension"], 0, TestID -> "SingletonDimension-3"]

TestCreate[SimplicialComplex[{{1, 2, 3}}] === SimplicialComplex[{Simplex[{1, 2, 3}]}], True, TestID -> "ConstructSCFromSimplex"]
TestCreate[SimplicialComplex[{{1, 2, 3}}] === SimplicialComplex[MeshRegion[{{0, 0}, {1, 0}, {0, 1}}, {Triangle[{1, 2, 3}]}]], True, TestID -> "ConstructSCFromMeshRegion"]
TestCreate[SimplicialComplex[{{1, 2, 3}}] === SimplicialComplex[BoundaryMeshRegion[{{0, 0}, {1, 0}, {0, 1}}, Line[{1, 2, 3, 1}]]], True, TestID -> "ConstructSCFromBoundaryMeshRegion"]

TestCreate[SimplicialCone[SimplicialComplex[{{1, 2, 3}}]]["Dimension"], 3, TestID -> "ConeDiskDimension"]
TestCreate[SimplicialCone[SimplicialComplex[{{1, 2, 3}}]]["VertexCount"], 4, TestID -> "ConeDiskVertexCount"]
TestCreate[SimplicialCone[SimplicialComplex[{{1, 2, 3}}]]["EulerCharacteristic"], 1, TestID -> "ConeDiskEulerCharacteristic"]
TestCreate[SimplicialCone[SimplicialComplex[{{1, 2}, {2, 3}, {3, 1}}]]["EulerCharacteristic"], 1, TestID -> "ConeCircleEulerCharacteristic"]

TestCreate[SimplicialJoin[SimplicialComplex[{{1, 2}}], SimplicialComplex[{{3, 4}}]]["Dimension"], 3, TestID -> "ConeDisjointLinesDimension"]
TestCreate[SimplicialJoin[SimplicialComplex[{{1, 2}}], SimplicialComplex[{{3, 4}}]]["FacetCount"], 1, TestID -> "ConeDisjointLinesFacetCount"]

TestCreate[SimplicialJoin[SimplicialComplex[{{1, 2, 3}}], SimplicialComplex[{{4}}]]["Dimension"], 3, TestID -> ""]
TestCreate[SimplicialJoin[SimplicialComplex[{{1, 2, 3}}], SimplicialComplex[{{4}}]]["VertexCount"], 4, TestID -> ""]

TestCreate[SimplicialJoin[SimplicialComplex[{{1}}], SimplicialComplex[{{2}}]]["Dimension"], 1, TestID -> "DimensionOfJoins-1"]
TestCreate[SimplicialJoin[SimplicialComplex[{{1, 2}}], SimplicialComplex[{{3}}]]["Dimension"], 2, TestID -> "DimensionOfJoins-2"]
TestCreate[SimplicialJoin[SimplicialComplex[{{1, 2}}], SimplicialComplex[{{3, 4}}]]["Dimension"], 3, TestID -> "DimensionOfJoins-3"]

TestCreate[SimplicialIsomorphicQ[SimplicialCone[SimplicialComplex[{{1, 2, 3}}]], SimplicialJoin[SimplicialComplex[{{1, 2, 3}}], SimplicialComplex[{{4}}]]], TestID -> "ConeVsJoin"]
TestCreate[SimplicialIsomorphicQ[SimplicialSuspension[SimplicialComplex[{{1, 2, 3}}]], SimplicialJoin[SimplicialComplex[{{1, 2, 3}}], SimplicialComplex[{{4}, {5}}]]], TestID -> "SuspensionVsJoin"]

TestCreate[SimplicialProduct[SimplicialComplex[{{1}}], SimplicialComplex[{{2}}]]["Dimension"], 0, TestID -> "ProductDimension-0"]
TestCreate[SimplicialProduct[SimplicialComplex[{{1, 2}}], SimplicialComplex[{{3}}]]["Dimension"], 1, TestID -> "ProductDimension-1"]
TestCreate[SimplicialProduct[SimplicialComplex[{{1, 2}}], SimplicialComplex[{{3, 4}}]]["Dimension"], 2, TestID -> "ProductDimension-2"]
TestCreate[SimplicialProduct[SimplicialComplex[{{1, 2}, {2, 3}, {3, 1}}], SimplicialComplex[{{4, 5}, {5, 6}, {6, 4}}]]["Dimension"], 2, TestID -> "Product-Dimension-3"]

TestCreate[SimplicialStar[SimplicialComplex[{{1, 2, 3}}], {1}], SimplicialComplex[{{1, 2, 3}}], TestID -> "Star-Triangle-Vertex"]
TestCreate[SimplicialStar[SimplicialComplex[{{1, 2}, {2, 3}, {1, 3}}], {1}], SimplicialComplex[{{1, 3}, {1, 2}}], TestID -> "Star-Circle-Vertex"]
TestCreate[SimplicialStar[SimplicialComplex[{{1, 2, 3, 4}}], {1, 2}], SimplicialComplex[{{1, 2, 3, 4}}], TestID -> "Star-Tetrahedron-Edge"]
TestCreate[SimplicialStar[SimplicialComplex[{{1, 2}, {2, 3}, {3, 4}, {4, 1}}], {1}], SimplicialComplex[{{1, 4}, {1, 2}}], TestID -> "Star-Square-Vertex"]
TestCreate[SimplicialStar[SimplicialComplex[{{1, 2, 3}, {1, 2, 4}}], {1, 2}], SimplicialComplex[{{1, 2, 4}, {1, 2, 3}}], TestID -> "Star-SharedEdge"]
TestCreate[SimplicialStar[SimplicialComplex[{{1, 2, 3}, {3, 4}}], {1, 2}], SimplicialComplex[{{1, 2, 3}}], TestID -> "Star-NonMaximalSimplex"]
TestCreate[SimplicialStar[SimplicialComplex[{{1, 2}, {3, 4}, {5}}], {5}], SimplicialComplex[{{5}}], TestID -> "Star-IsolatedVertex"]

TestCreate[SimplicialLink[SimplicialComplex[{{1, 2, 3}}], {1}], SimplicialComplex[{{2, 3}}], TestID -> "Link-Triangle-Vertex"]
TestCreate[SimplicialLink[SimplicialComplex[{{1, 2}, {2, 3}, {1, 3}}], {1}], SimplicialComplex[{{3}, {2}}], TestID -> "Link-Circle-Vertex"]
TestCreate[SimplicialLink[SimplicialComplex[{{1, 2, 3, 4}}], {1, 2}], SimplicialComplex[{{3, 4}}], TestID -> "Link-Tetrahedron-Edge"]
TestCreate[SimplicialLink[SimplicialComplex[{{1, 2}, {2, 3}, {3, 4}, {4, 1}}], {1}], SimplicialComplex[{{4}, {2}}], TestID -> "Link-Square-Vertex"]
TestCreate[SimplicialLink[SimplicialComplex[{{1, 2, 3}, {1, 2, 4}}], {1, 2}], SimplicialComplex[{{4}, {3}}], TestID -> "Link-SharedEdge"]
TestCreate[SimplicialLink[SimplicialComplex[{{1, 2, 3}, {3, 4}}], {1, 2}], SimplicialComplex[{{3}}], TestID -> "Link-NonMaximalSimplex"]
TestCreate[SimplicialLink[SimplicialComplex[{{1, 2}, {3, 4}, {5}}], {5}], SimplicialComplex[{}], TestID -> "Link-IsolatedVertex"]