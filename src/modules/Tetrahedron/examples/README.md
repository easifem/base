# Tetrahedron examples

These standalone programs contain `Test_Method::OK` checks.
Run from this directory using `easifem run <filename>` after installing the
current library. Building/installing and execution require explicit user
permission under `AGENTS.md`. See [the module guide](../README.md) for layouts
and known implementation limits. Workarounds and placeholder checks are
explicitly labeled; they do not validate the affected numerical backends.

## ReferenceTetrahedron_Method

| Public API | Example |
| --- | --- |
| `elementname_tetrahedron` | [ReferenceTetrahedronMetadata_test_1.F90](ReferenceTetrahedronMetadata_test_1.F90) |
| `elementorder_tetrahedron` | [ReferenceTetrahedronMetadata_test_1.F90](ReferenceTetrahedronMetadata_test_1.F90) |
| `elementtype_tetrahedron` | [ReferenceTetrahedronMetadata_test_1.F90](ReferenceTetrahedronMetadata_test_1.F90) |
| `facetelements_tetrahedron` | [ReferenceTetrahedronConnectivity_test_1.F90](ReferenceTetrahedronConnectivity_test_1.F90) |
| `facettopology_tetrahedron` | [ReferenceTetrahedronConnectivity_test_1.F90](ReferenceTetrahedronConnectivity_test_1.F90) |
| `getedgeconnectivity_tetrahedron` | [ReferenceTetrahedronConnectivity_test_1.F90](ReferenceTetrahedronConnectivity_test_1.F90) |
| `getfaceconnectivity_tetrahedron` | [ReferenceTetrahedronConnectivity_test_1.F90](ReferenceTetrahedronConnectivity_test_1.F90) |
| `getfaceelemtype_tetrahedron` | [ReferenceTetrahedronConnectivity_test_1.F90](ReferenceTetrahedronConnectivity_test_1.F90) |
| `highorderelement_tetrahedron` | [HighOrderElement_Tetrahedron_test_1.F90](HighOrderElement_Tetrahedron_test_1.F90) |
| `initiate` | [ReferenceTetrahedronConstructors_test_1.F90](ReferenceTetrahedronConstructors_test_1.F90) |
| `maxorder_tetrahedron` | [ReferenceTetrahedronMetadata_test_1.F90](ReferenceTetrahedronMetadata_test_1.F90) |
| `measure_simplex_tetrahedron` | [ReferenceTetrahedronMeasure_test_1.F90](ReferenceTetrahedronMeasure_test_1.F90) |
| `quality_tetrahedron` | [ReferenceTetrahedronMeasure_test_1.F90](ReferenceTetrahedronMeasure_test_1.F90) |
| `refcoord_tetrahedron` | [ReferenceTetrahedronConstructors_test_1.F90](ReferenceTetrahedronConstructors_test_1.F90) |
| `referencetetrahedron` | [ReferenceTetrahedronConstructors_test_1.F90](ReferenceTetrahedronConstructors_test_1.F90) |
| `referencetetrahedron_pointer` | [ReferenceTetrahedronConstructors_test_1.F90](ReferenceTetrahedronConstructors_test_1.F90) |
| `tetrahedron_quality` | [ReferenceTetrahedronMeasure_test_1.F90](ReferenceTetrahedronMeasure_test_1.F90) |
| `tetrahedronvolume3d` | [TetrahedronVolume3D_test_1.F90](TetrahedronVolume3D_test_1.F90) |
| `totalentities_tetrahedron` | [ReferenceTetrahedronMetadata_test_1.F90](ReferenceTetrahedronMetadata_test_1.F90) |
| `totalnodesinelement_tetrahedron` | [ReferenceTetrahedronMetadata_test_1.F90](ReferenceTetrahedronMetadata_test_1.F90) |

## TetrahedronInterpolationUtility

| Public API | Example |
| --- | --- |
| `barycentriccellbasis_tetrahedron` | [BarycentricCellBasis_Tetrahedron_test_1.F90](BarycentricCellBasis_Tetrahedron_test_1.F90) |
| `barycentricedgebasis_tetrahedron` | [BarycentricEdgeBasis_Tetrahedron_test_1.F90](BarycentricEdgeBasis_Tetrahedron_test_1.F90) |
| `barycentricfacetbasis_tetrahedron` | [BarycentricFacetBasis_Tetrahedron_test_1.F90](BarycentricFacetBasis_Tetrahedron_test_1.F90) |
| `barycentricheirarchicalbasis_tetrahedron` | [BarycentricHeirarchicalBasis_Tetrahedron_test_1.F90](BarycentricHeirarchicalBasis_Tetrahedron_test_1.F90) |
| `barycentricvertexbasis_tetrahedron` | [BarycentricVertexBasis_Tetrahedron_test_1.F90](BarycentricVertexBasis_Tetrahedron_test_1.F90) |
| `cellbasis_tetrahedron` | [CellBasis_Tetrahedron_test_1.F90](CellBasis_Tetrahedron_test_1.F90) |
| `edgebasis_tetrahedron` | [EdgeBasis_Tetrahedron_test_1.F90](EdgeBasis_Tetrahedron_test_1.F90) |
| `edgeconnectivity_tetrahedron` | [TetrahedronBasisConnectivity_test_1.F90](TetrahedronBasisConnectivity_test_1.F90) |
| `equidistanceinpoint_tetrahedron` | [EquidistanceInPoint_Tetrahedron_test_1.F90](EquidistanceInPoint_Tetrahedron_test_1.F90) |
| `equidistancepoint_tetrahedron` | [EquidistancePoint_Tetrahedron_test_1.F90](EquidistancePoint_Tetrahedron_test_1.F90) |
| `equidistancepoint_tetrahedron_` | [EquidistancePoint_Tetrahedron__test_1.F90](EquidistancePoint_Tetrahedron__test_1.F90) |
| `facetbasis_tetrahedron` | [FacetBasis_Tetrahedron_test_1.F90](FacetBasis_Tetrahedron_test_1.F90) |
| `facetconnectivity_tetrahedron` | [TetrahedronBasisConnectivity_test_1.F90](TetrahedronBasisConnectivity_test_1.F90) |
| `getcelldof_tetrahedron` | [TetrahedronEntityDOF_test_1.F90](TetrahedronEntityDOF_test_1.F90) |
| `getedgedof_tetrahedron` | [TetrahedronEntityDOF_test_1.F90](TetrahedronEntityDOF_test_1.F90) |
| `getfacetdof_tetrahedron` | [TetrahedronEntityDOF_test_1.F90](TetrahedronEntityDOF_test_1.F90) |
| `gettotaldof_tetrahedron` | [TetrahedronInterpolationMetadata_test_1.F90](TetrahedronInterpolationMetadata_test_1.F90) |
| `gettotalindof_tetrahedron` | [TetrahedronInterpolationMetadata_test_1.F90](TetrahedronInterpolationMetadata_test_1.F90) |
| `getvertexdof_tetrahedron` | [TetrahedronEntityDOF_test_1.F90](TetrahedronEntityDOF_test_1.F90) |
| `heirarchicalbasis_tetrahedron` | [HeirarchicalBasis_Tetrahedron_test_1.F90](HeirarchicalBasis_Tetrahedron_test_1.F90) |
| `heirarchicalbasis_tetrahedron_` | [HeirarchicalBasis_Tetrahedron__test_1.F90](HeirarchicalBasis_Tetrahedron__test_1.F90) |
| `heirarchicalbasisgradient_tetrahedron` | [HeirarchicalBasisGradient_Tetrahedron_test_1.F90](HeirarchicalBasisGradient_Tetrahedron_test_1.F90) |
| `heirarchicalbasisgradient_tetrahedron_` | [HeirarchicalBasisGradient_Tetrahedron__test_1.F90](HeirarchicalBasisGradient_Tetrahedron__test_1.F90) |
| `interpolationpoint_tetrahedron` | [InterpolationPoint_Tetrahedron_test_1.F90](InterpolationPoint_Tetrahedron_test_1.F90) |
| `interpolationpoint_tetrahedron_` | [InterpolationPoint_Tetrahedron__test_1.F90](InterpolationPoint_Tetrahedron__test_1.F90) |
| `lagrangecoeff_tetrahedron` | [LagrangeCoeff_Tetrahedron_test_1.F90](LagrangeCoeff_Tetrahedron_test_1.F90) |
| `lagrangecoeff_tetrahedron_` | [LagrangeCoeff_Tetrahedron__test_1.F90](LagrangeCoeff_Tetrahedron__test_1.F90) |
| `lagrangedegree_tetrahedron` | [TetrahedronInterpolationMetadata_test_1.F90](TetrahedronInterpolationMetadata_test_1.F90) |
| `lagrangedof_tetrahedron` | [TetrahedronInterpolationMetadata_test_1.F90](TetrahedronInterpolationMetadata_test_1.F90) |
| `lagrangeevalall_tetrahedron` | [LagrangeEvalAll_Tetrahedron_test_1.F90](LagrangeEvalAll_Tetrahedron_test_1.F90) |
| `lagrangeevalall_tetrahedron_` | [LagrangeEvalAll_Tetrahedron__test_1.F90](LagrangeEvalAll_Tetrahedron__test_1.F90) |
| `lagrangegradientevalall_tetrahedron` | [LagrangeGradientEvalAll_Tetrahedron_test_1.F90](LagrangeGradientEvalAll_Tetrahedron_test_1.F90) |
| `lagrangegradientevalall_tetrahedron_` | [LagrangeGradientEvalAll_Tetrahedron__test_1.F90](LagrangeGradientEvalAll_Tetrahedron__test_1.F90) |
| `lagrangeindof_tetrahedron` | [TetrahedronInterpolationMetadata_test_1.F90](TetrahedronInterpolationMetadata_test_1.F90) |
| `orthogonalbasis_tetrahedron` | [OrthogonalBasis_Tetrahedron_test_1.F90](OrthogonalBasis_Tetrahedron_test_1.F90) |
| `orthogonalbasis_tetrahedron_` | [OrthogonalBasis_Tetrahedron__test_1.F90](OrthogonalBasis_Tetrahedron__test_1.F90) |
| `orthogonalbasisgradient_tetrahedron` | [TetrahedronOrthogonalGradientWorkaround_test_1.F90](TetrahedronOrthogonalGradientWorkaround_test_1.F90) |
| `orthogonalbasisgradient_tetrahedron_` | [TetrahedronOrthogonalGradientWorkaround_test_1.F90](TetrahedronOrthogonalGradientWorkaround_test_1.F90) |
| `quadraturenumber_tetrahedron` | [TetrahedronQuadrature_test_1.F90](TetrahedronQuadrature_test_1.F90) |
| `quadraturepoint_tetrahedron` | [TetrahedronQuadrature_test_1.F90](TetrahedronQuadrature_test_1.F90) |
| `quadraturepoint_tetrahedron_` | [TetrahedronQuadrature_test_1.F90](TetrahedronQuadrature_test_1.F90) |
| `refelemdomain_tetrahedron` | [TetrahedronInterpolationMetadata_test_1.F90](TetrahedronInterpolationMetadata_test_1.F90) |
| `vertexbasis_tetrahedron` | [VertexBasis_Tetrahedron_test_1.F90](VertexBasis_Tetrahedron_test_1.F90) |
