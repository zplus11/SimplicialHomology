PacletDirectoryLoad[ParentDirectory @ NotebookDirectory[]]
Needs["Taggar`SimplicialHomology`"]

With[
	{sc = SimplicialComplex[{{1, 2, 3}}]},
	
	TestCreate[sc["Dimension"], 2, TestID -> "PropertyDimension"]
	TestCreate[sc["Vertices"], {1, 2, 3}, TestID -> "PropertyVertices"]
	TestCreate[sc["VertexCount"], 3, TestID -> "PropertyVertexCount"]
	TestCreate[sc["Edges"], {{1, 2}, {1, 3}, {2, 3}}, TestID -> "PropertyEdges"]
	TestCreate[sc["EdgeCount"], 3, TestID -> "PropertyEdgeCount"]
	TestCreate[sc["Facets"], {{1, 2, 3}}, TestID -> "PropertyFacets"]
	TestCreate[sc["FacetCount"], 1, TestID -> "PropertyFacetCount"]
	TestCreate[sc["SimplexCount"], 8, TestID -> "PropertySimplexCount"]
	TestCreate[sc["FVector"], {1, 3, 3, 1}, TestID -> "PropertyFVector"]
	TestCreate[sc["EulerCharacteristic"], 1, TestID -> "PropertyEulerCharacteristic"]
	TestCreate[sc["PureQ"], True, TestID -> "PropertyPureQ"]
	TestCreate[sc["ConnectedQ"], True, TestID -> "PropertyConnectedQ"]
	TestCreate[sc["ConnectedComponents"], {{1, 2, 3}}, TestID -> "PropertyConnectedComponents"]]

TestCreate[SimplicialComplex[{{1, 2, 3}, {1, 3, 4}, {1, 4, 5}, {1, 5, 2}}]["Dimension"], 2, TestID -> "DiskDimension"]
TestCreate[SimplicialComplex[{{1, 2, 3}, {1, 3, 4}, {1, 4, 5}, {1, 5, 2}}]["FVector"], {1, 5, 8, 4}, TestID -> "DiskFVector"]
TestCreate[SimplicialComplex[{{1, 2, 3}, {1, 3, 4}, {1, 4, 5}, {1, 5, 2}}]["EulerCharacteristic"], 1, TestID -> "DiskEulerCharacteristic"]
TestCreate[SimplicialComplex[{{1, 2, 3}, {1, 3, 4}, {1, 4, 5}, {1, 5, 2}}]["ConnectedQ"], True, TestID -> "DiskConnectedQ"]

TestCreate[SimplicialComplex[{{1, 2, 3}, {1, 3, 4}, {1, 4, 5}, {1, 5, 2}, {2, 3, 4}, {2, 4, 5}}]["FVector"], {1, 5, 9, 6}, TestID -> "SphereFVector"]
TestCreate[SimplicialComplex[{{1, 2, 3}, {1, 3, 4}, {1, 4, 5}, {1, 5, 2}, {2, 3, 4}, {2, 4, 5}}]["EulerCharacteristic"], 2, TestID -> "SphereEulerCharacteristic"]

TestCreate[SimplicialComplex["Torus"]["Dimension"], 2, TestID -> "TorusDimension"]
TestCreate[SimplicialComplex["Torus"]["FacetCount"], 14, TestID -> "TorusFacetCount"]
TestCreate[SimplicialComplex["Torus"]["FVector"], {1, 7, 21, 14}, TestID -> "TorusFVector"]
TestCreate[SimplicialComplex["Torus"]["EulerCharacteristic"], 0, TestID -> "TorusEulerCharacteristic"]