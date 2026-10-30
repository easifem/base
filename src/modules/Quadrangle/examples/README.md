# Quadrangle examples

These standalone programs contain `Test_Method::OK` checks.
Run from this directory using `easifem run <filename>` after installing the
current library. Building/installing and execution require explicit user
permission under `AGENTS.md`. See [the module guide](../README.md) for layouts
and known implementation limits. Workarounds and placeholder checks are
explicitly labeled; they do not validate the affected numerical backends.

## QuadrangleInterpolationUtility

| Public API | Example |
| --- | --- |
| `cellbasis_quadrangle` | [CellBasis_Quadrangle_test_1.F90](CellBasis_Quadrangle_test_1.F90) |
| `dubiner_quadrangle` | [Dubiner_Quadrangle_test_1.F90](Dubiner_Quadrangle_test_1.F90) |
| `dubiner_quadrangle_` | [Dubiner_Quadrangle__test_1.F90](Dubiner_Quadrangle__test_1.F90) |
| `dubinergradient_quadrangle` | [DubinerGradient_Quadrangle_test_1.F90](DubinerGradient_Quadrangle_test_1.F90) |
| `dubinergradient_quadrangle_` | [DubinerGradient_Quadrangle__test_1.F90](DubinerGradient_Quadrangle__test_1.F90) |
| `equidistanceinpoint_quadrangle` | [EquidistanceInPoint_Quadrangle_test_1.F90](EquidistanceInPoint_Quadrangle_test_1.F90) |
| `equidistancepoint_quadrangle` | [EquidistancePoint_Quadrangle_test_1.F90](EquidistancePoint_Quadrangle_test_1.F90) |
| `equidistancepoint_quadrangle_` | [EquidistancePoint_Quadrangle__test_1.F90](EquidistancePoint_Quadrangle__test_1.F90) |
| `facetconnectivity_quadrangle` | [FacetConnectivity_Quadrangle_test_1.F90](FacetConnectivity_Quadrangle_test_1.F90) |
| `gethierarchicaldof_quadrangle` | [QuadrangleHierarchicalDOF_test_1.F90](QuadrangleHierarchicalDOF_test_1.F90) |
| `gettotaldof_quadrangle` | [QuadrangleInterpolationMetadata_test_1.F90](QuadrangleInterpolationMetadata_test_1.F90) |
| `gettotalindof_quadrangle` | [QuadrangleInterpolationMetadata_test_1.F90](QuadrangleInterpolationMetadata_test_1.F90) |
| `heirarchicalbasis_quadrangle` | [HeirarchicalBasis_Quadrangle_test_1.F90](HeirarchicalBasis_Quadrangle_test_1.F90) |
| `heirarchicalbasis_quadrangle_` | [HeirarchicalBasis_Quadrangle__test_1.F90](HeirarchicalBasis_Quadrangle__test_1.F90) |
| `heirarchicalbasisgradient_quadrangle` | [HeirarchicalBasisGradient_Quadrangle_test_1.F90](HeirarchicalBasisGradient_Quadrangle_test_1.F90) |
| `heirarchicalbasisgradient_quadrangle_` | [HeirarchicalBasisGradient_Quadrangle__test_1.F90](HeirarchicalBasisGradient_Quadrangle__test_1.F90) |
| `horizontaledgebasis_quadrangle` | [HorizontalEdgeBasis_Quadrangle_test_1.F90](HorizontalEdgeBasis_Quadrangle_test_1.F90) |
| `ij2vefc_quadrangle_anticlockwise` | [IJ2VEFC_Quadrangle_AntiClockwise_test_1.F90](IJ2VEFC_Quadrangle_AntiClockwise_test_1.F90) |
| `ij2vefc_quadrangle_clockwise` | [IJ2VEFC_Quadrangle_Clockwise_test_1.F90](IJ2VEFC_Quadrangle_Clockwise_test_1.F90) |
| `interpolationpoint_quadrangle` | [InterpolationPoint_Quadrangle_test_1.F90](InterpolationPoint_Quadrangle_test_1.F90) |
| `interpolationpoint_quadrangle_` | [InterpolationPoint_Quadrangle__test_1.F90](InterpolationPoint_Quadrangle__test_1.F90) |
| `lagrangecoeff_quadrangle` | [LagrangeCoeff_Quadrangle_test_1.F90](LagrangeCoeff_Quadrangle_test_1.F90) |
| `lagrangecoeff_quadrangle_` | [LagrangeCoeff_Quadrangle__test_1.F90](LagrangeCoeff_Quadrangle__test_1.F90) |
| `lagrangedegree_quadrangle` | [QuadrangleInterpolationMetadata_test_1.F90](QuadrangleInterpolationMetadata_test_1.F90) |
| `lagrangedof_quadrangle` | [QuadrangleInterpolationMetadata_test_1.F90](QuadrangleInterpolationMetadata_test_1.F90) |
| `lagrangeevalall_quadrangle` | [LagrangeEvalAll_Quadrangle_test_1.F90](LagrangeEvalAll_Quadrangle_test_1.F90) |
| `lagrangeevalall_quadrangle_` | [LagrangeEvalAll_Quadrangle__test_1.F90](LagrangeEvalAll_Quadrangle__test_1.F90) |
| `lagrangegradientevalall_quadrangle` | [LagrangeGradientEvalAll_Quadrangle_test_1.F90](LagrangeGradientEvalAll_Quadrangle_test_1.F90) |
| `lagrangegradientevalall_quadrangle_` | [LagrangeGradientEvalAll_Quadrangle__test_1.F90](LagrangeGradientEvalAll_Quadrangle__test_1.F90) |
| `lagrangeindof_quadrangle` | [QuadrangleInterpolationMetadata_test_1.F90](QuadrangleInterpolationMetadata_test_1.F90) |
| `orthogonalbasis_quadrangle` | [QuadrangleTensorBasis_test_1.F90](QuadrangleTensorBasis_test_1.F90) |
| `orthogonalbasis_quadrangle_` | [QuadrangleTensorBasis_test_1.F90](QuadrangleTensorBasis_test_1.F90) |
| `orthogonalbasisgradient_quadrangle` | [QuadrangleTensorBasis_test_1.F90](QuadrangleTensorBasis_test_1.F90) |
| `orthogonalbasisgradient_quadrangle_` | [QuadrangleTensorBasis_test_1.F90](QuadrangleTensorBasis_test_1.F90) |
| `quadraturenumber_quadrangle` | [QuadrangleQuadrature_test_1.F90](QuadrangleQuadrature_test_1.F90) |
| `quadraturepoint_quadrangle` | [QuadrangleQuadrature_test_1.F90](QuadrangleQuadrature_test_1.F90) |
| `quadraturepoint_quadrangle_` | [QuadrangleQuadrature_test_1.F90](QuadrangleQuadrature_test_1.F90) |
| `refelemdomain_quadrangle` | [QuadrangleInterpolationMetadata_test_1.F90](QuadrangleInterpolationMetadata_test_1.F90) |
| `tensorprodbasis_quadrangle` | [QuadrangleTensorBasis_test_1.F90](QuadrangleTensorBasis_test_1.F90) |
| `tensorprodbasisgradient_quadrangle` | [QuadrangleTensorBasis_test_1.F90](QuadrangleTensorBasis_test_1.F90) |
| `vertexbasis_quadrangle` | [VertexBasis_Quadrangle_test_1.F90](VertexBasis_Quadrangle_test_1.F90) |
| `verticaledgebasis_quadrangle` | [VerticalEdgeBasis_Quadrangle_test_1.F90](VerticalEdgeBasis_Quadrangle_test_1.F90) |

## ReferenceQuadrangle_Method

| Public API | Example |
| --- | --- |
| `default_opt_quadrangle_edge_con` | [ReferenceQuadrangleMetadata_test_1.F90](ReferenceQuadrangleMetadata_test_1.F90) |
| `elementname_quadrangle` | [ReferenceQuadrangleMetadata_test_1.F90](ReferenceQuadrangleMetadata_test_1.F90) |
| `elementorder_quadrangle` | [ReferenceQuadrangleMetadata_test_1.F90](ReferenceQuadrangleMetadata_test_1.F90) |
| `elementtype_quadrangle` | [ReferenceQuadrangleMetadata_test_1.F90](ReferenceQuadrangleMetadata_test_1.F90) |
| `faceshapemetadata_quadrangle` | [QuadrangleFaceOrientation_test_1.F90](QuadrangleFaceOrientation_test_1.F90) |
| `facetelements_quadrangle` | [ReferenceQuadrangleConnectivity_test_1.F90](ReferenceQuadrangleConnectivity_test_1.F90) |
| `facettopology_quadrangle` | [ReferenceQuadrangleConnectivity_test_1.F90](ReferenceQuadrangleConnectivity_test_1.F90) |
| `getedgeconnectivity_quadrangle` | [ReferenceQuadrangleConnectivity_test_1.F90](ReferenceQuadrangleConnectivity_test_1.F90) |
| `getfaceconnectivity_quadrangle` | [ReferenceQuadrangleConnectivity_test_1.F90](ReferenceQuadrangleConnectivity_test_1.F90) |
| `getfaceelemtype_quadrangle` | [ReferenceQuadrangleConnectivity_test_1.F90](ReferenceQuadrangleConnectivity_test_1.F90) |
| `highorderelement_quadrangle` | [HighOrderElement_Quadrangle_test_1.F90](HighOrderElement_Quadrangle_test_1.F90) |
| `initiate` | [ReferenceQuadrangleConstructors_test_1.F90](ReferenceQuadrangleConstructors_test_1.F90) |
| `maxorder_quadrangle` | [ReferenceQuadrangleMetadata_test_1.F90](ReferenceQuadrangleMetadata_test_1.F90) |
| `measure_simplex_quadrangle` | [ReferenceQuadrangleMeasure_test_1.F90](ReferenceQuadrangleMeasure_test_1.F90) |
| `quadarea2d` | [QuadrangleArea_test_1.F90](QuadrangleArea_test_1.F90) |
| `quadarea3d` | [QuadrangleArea_test_1.F90](QuadrangleArea_test_1.F90) |
| `quadrangle_quality` | [ReferenceQuadrangleMeasure_test_1.F90](ReferenceQuadrangleMeasure_test_1.F90) |
| `quadranglearea2d` | [QuadrangleArea_test_1.F90](QuadrangleArea_test_1.F90) |
| `quadranglearea3d` | [QuadrangleArea_test_1.F90](QuadrangleArea_test_1.F90) |
| `quadranglename` | [ReferenceQuadrangleMetadata_test_1.F90](ReferenceQuadrangleMetadata_test_1.F90) |
| `quality_quadrangle` | [ReferenceQuadrangleMeasure_test_1.F90](ReferenceQuadrangleMeasure_test_1.F90) |
| `refcoord_quadrangle` | [ReferenceQuadrangleConstructors_test_1.F90](ReferenceQuadrangleConstructors_test_1.F90) |
| `referencequadrangle` | [ReferenceQuadrangleConstructors_test_1.F90](ReferenceQuadrangleConstructors_test_1.F90) |
| `referencequadrangle_pointer` | [ReferenceQuadrangleConstructors_test_1.F90](ReferenceQuadrangleConstructors_test_1.F90) |
| `refquadranglecoord` | [ReferenceQuadrangleConstructors_test_1.F90](ReferenceQuadrangleConstructors_test_1.F90) |
| `totalentities_quadrangle` | [ReferenceQuadrangleMetadata_test_1.F90](ReferenceQuadrangleMetadata_test_1.F90) |
| `totalnodesinelement_quadrangle` | [ReferenceQuadrangleMetadata_test_1.F90](ReferenceQuadrangleMetadata_test_1.F90) |
