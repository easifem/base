# Triangle examples

Each new program contains `Test_Method::OK` checks. Run from this directory
using `easifem run <filename>` after installing the current library.
Building/installing and running require explicit permission under `AGENTS.md`.
See [the module guide](../README.md) for layouts and implementation limits.

Workaround and known behavior examples are explicitly labeled in their source.
They do not validate the affected numerical implementation.

## ReferenceTriangle_Method

| Public API | Example program |
| --- | --- |
| `angles` | [Angles_test_1.F90](Angles_test_1.F90) |
| `area` | [Area_test_1.F90](Area_test_1.F90) |
| `arealvector` | [ArealVector_test_1.F90](ArealVector_test_1.F90) |
| `barycentric` | [Barycentric_test_1.F90](Barycentric_test_1.F90) |
| `centroid` | [Centroid_test_1.F90](Centroid_test_1.F90) |
| `circumcenter` | [CircumCenter_test_1.F90](CircumCenter_test_1.F90) |
| `circumcircle` | [CircumCircle_test_1.F90](CircumCircle_test_1.F90) |
| `circumradius` | [CircumRadius_test_1.F90](CircumRadius_test_1.F90) |
| `contains_point_triangle` | [Contains_Point_Triangle_test_1.F90](Contains_Point_Triangle_test_1.F90) |
| `containsline` | [ContainsLine_test_1.F90](ContainsLine_test_1.F90) |
| `default_opt_triangle_edge_con` | [ReferenceTriangleMetadata_test_1.F90](ReferenceTriangleMetadata_test_1.F90) |
| `diameter` | [Diameter_test_1.F90](Diameter_test_1.F90) |
| `distancefrompoint` | [DistanceFromPoint_test_1.F90](DistanceFromPoint_test_1.F90) |
| `edgelength` | [EdgeLength_test_1.F90](EdgeLength_test_1.F90) |
| `elementname_triangle` | [ReferenceTriangleMetadata_test_1.F90](ReferenceTriangleMetadata_test_1.F90) |
| `elementorder_triangle` | [ReferenceTriangleMetadata_test_1.F90](ReferenceTriangleMetadata_test_1.F90) |
| `elementtype_triangle` | [ReferenceTriangleMetadata_test_1.F90](ReferenceTriangleMetadata_test_1.F90) |
| `faceshapemetadata_triangle` | [TriangleFaceOrientation_test_1.F90](TriangleFaceOrientation_test_1.F90) |
| `facetelements_triangle` | [ReferenceTriangleConnectivity_test_1.F90](ReferenceTriangleConnectivity_test_1.F90) |
| `facettopology_triangle` | [ReferenceTriangleConnectivity_test_1.F90](ReferenceTriangleConnectivity_test_1.F90) |
| `getedgeconnectivity_triangle` | [ReferenceTriangleConnectivity_test_1.F90](ReferenceTriangleConnectivity_test_1.F90) |
| `getfaceconnectivity_triangle` | [ReferenceTriangleConnectivity_test_1.F90](ReferenceTriangleConnectivity_test_1.F90) |
| `getfaceelemtype_triangle` | [ReferenceTriangleConnectivity_test_1.F90](ReferenceTriangleConnectivity_test_1.F90) |
| `highorderelement_triangle` | [HighorderElement_Triangle_test_1.F90](HighorderElement_Triangle_test_1.F90) |
| `incenter` | [Incenter_test_1.F90](Incenter_test_1.F90) |
| `incircle` | [Incircle_test_1.F90](Incircle_test_1.F90) |
| `initiate` | [ReferenceTriangleConstructors_test_1.F90](ReferenceTriangleConstructors_test_1.F90) |
| `inradius` | [Inradius_test_1.F90](Inradius_test_1.F90) |
| `maxorder_triangle` | [ReferenceTriangleMetadata_test_1.F90](ReferenceTriangleMetadata_test_1.F90) |
| `measure_simplex_triangle` | [Measure_Simplex_Triangle_test_1.F90](Measure_Simplex_Triangle_test_1.F90) |
| `nearestpoint` | [NearestPoint_test_1.F90](NearestPoint_test_1.F90) |
| `orthocenter` | [Orthocenter_test_1.F90](Orthocenter_test_1.F90) |
| `quality_triangle` | [Quality_Triangle_test_1.F90](Quality_Triangle_test_1.F90) |
| `randompoint` | [RandomPoint_test_1.F90](RandomPoint_test_1.F90) |
| `refcoord_triangle` | [ReferenceTriangleConstructors_test_1.F90](ReferenceTriangleConstructors_test_1.F90) |
| `referencetriangle` | [ReferenceTriangleConstructors_test_1.F90](ReferenceTriangleConstructors_test_1.F90) |
| `referencetriangle_pointer` | [ReferenceTriangleConstructors_test_1.F90](ReferenceTriangleConstructors_test_1.F90) |
| `reftrianglecoord` | [ReferenceTriangleConstructors_test_1.F90](ReferenceTriangleConstructors_test_1.F90) |
| `totalentities_triangle` | [ReferenceTriangleMetadata_test_1.F90](ReferenceTriangleMetadata_test_1.F90) |
| `totalnodesinelement_triangle` | [ReferenceTriangleMetadata_test_1.F90](ReferenceTriangleMetadata_test_1.F90) |
| `triangle_contains_point` | [Contains_Point_Triangle_test_1.F90](Contains_Point_Triangle_test_1.F90) |
| `triangle_quality` | [Quality_Triangle_test_1.F90](Quality_Triangle_test_1.F90) |
| `trianglearea2d` | [TriangleArea2D_test_1.F90](TriangleArea2D_test_1.F90) |
| `trianglearea3d` | [TriangleArea3D_test_1.F90](TriangleArea3D_test_1.F90) |

## TriangleInterpolationUtility

| Public API | Example program |
| --- | --- |
| `cellbasis_triangle` | [CellBasis_Triangle_test_1.F90](CellBasis_Triangle_test_1.F90) |
| `dubiner_triangle` | [Dubiner_Triangle_test_1.F90](Dubiner_Triangle_test_1.F90) |
| `dubiner_triangle_` | [Dubiner_Triangle__test_1.F90](Dubiner_Triangle__test_1.F90) |
| `edgebasis_triangle` | [EdgeBasis_Triangle_test_1.F90](EdgeBasis_Triangle_test_1.F90) |
| `equidistanceinpoint_triangle` | [EquidistanceInPoint_Triangle_test_1.F90](EquidistanceInPoint_Triangle_test_1.F90) |
| `equidistancepoint_triangle` | [EquidistancePoint_Triangle_test_1.F90](EquidistancePoint_Triangle_test_1.F90) |
| `equidistancepoint_triangle_` | [EquidistancePoint_Triangle__test_1.F90](EquidistancePoint_Triangle__test_1.F90) |
| `facetconnectivity_triangle` | [FacetConnectivity_Triangle_test_1.F90](FacetConnectivity_Triangle_test_1.F90) |
| `gethierarchicaldof_triangle` | [TriangleHierarchicalDOF_test_1.F90](TriangleHierarchicalDOF_test_1.F90) |
| `gettotaldof_triangle` | [TriangleInterpolationMetadata_test_1.F90](TriangleInterpolationMetadata_test_1.F90) |
| `gettotalindof_triangle` | [TriangleInterpolationMetadata_test_1.F90](TriangleInterpolationMetadata_test_1.F90) |
| `heirarchicalbasis_triangle` | [HeirarchicalBasis_Triangle_test_1.F90](HeirarchicalBasis_Triangle_test_1.F90) |
| `heirarchicalbasis_triangle_` | [HeirarchicalBasis_Triangle__test_1.F90](HeirarchicalBasis_Triangle__test_1.F90) |
| `heirarchicalbasisgradient_triangle` | [HeirarchicalBasisGradient_Triangle_test_1.F90](HeirarchicalBasisGradient_Triangle_test_1.F90) |
| `heirarchicalbasisgradient_triangle_` | [HeirarchicalBasisGradient_Triangle__test_1.F90](HeirarchicalBasisGradient_Triangle__test_1.F90) |
| `ij2vefc_triangle` | [IJ2VEFC_Triangle_test_1.F90](IJ2VEFC_Triangle_test_1.F90) |
| `interpolationpoint_triangle` | [InterpolationPoint_Triangle_test_1.F90](InterpolationPoint_Triangle_test_1.F90) |
| `interpolationpoint_triangle_` | [InterpolationPoint_Triangle__test_1.F90](InterpolationPoint_Triangle__test_1.F90) |
| `lagrangecoeff_triangle` | [LagrangeCoeff_Triangle_test_1.F90](LagrangeCoeff_Triangle_test_1.F90) |
| `lagrangecoeff_triangle_` | [LagrangeCoeff_Triangle__test_1.F90](LagrangeCoeff_Triangle__test_1.F90) |
| `lagrangedegree_triangle` | [TriangleInterpolationMetadata_test_1.F90](TriangleInterpolationMetadata_test_1.F90) |
| `lagrangedof_triangle` | [TriangleInterpolationMetadata_test_1.F90](TriangleInterpolationMetadata_test_1.F90) |
| `lagrangeevalall_triangle` | [LagrangeEvalAll_Triangle_test_1.F90](LagrangeEvalAll_Triangle_test_1.F90) |
| `lagrangeevalall_triangle_` | [LagrangeEvalAll_Triangle__test_1.F90](LagrangeEvalAll_Triangle__test_1.F90) |
| `lagrangegradientevalall_triangle` | [LagrangeGradientEvalAll_Triangle_test_1.F90](LagrangeGradientEvalAll_Triangle_test_1.F90) |
| `lagrangegradientevalall_triangle_` | [LagrangeGradientEvalAll_Triangle__test_1.F90](LagrangeGradientEvalAll_Triangle__test_1.F90) |
| `lagrangeindof_triangle` | [TriangleInterpolationMetadata_test_1.F90](TriangleInterpolationMetadata_test_1.F90) |
| `orthogonalbasis_triangle` | [OrthogonalBasis_Triangle_test_1.F90](OrthogonalBasis_Triangle_test_1.F90) |
| `orthogonalbasis_triangle_` | [OrthogonalBasis_Triangle__test_1.F90](OrthogonalBasis_Triangle__test_1.F90) |
| `orthogonalbasisgradient_triangle` | [OrthogonalBasisGradient_Triangle_test_1.F90](OrthogonalBasisGradient_Triangle_test_1.F90) |
| `orthogonalbasisgradient_triangle_` | [OrthogonalBasisGradient_Triangle__test_1.F90](OrthogonalBasisGradient_Triangle__test_1.F90) |
| `quadraturenumber_triangle` | [TriangleQuadrature_test_1.F90](TriangleQuadrature_test_1.F90) |
| `quadraturepoint_triangle` | [TriangleQuadrature_test_1.F90](TriangleQuadrature_test_1.F90) |
| `quadraturepoint_triangle_` | [TriangleQuadrature_test_1.F90](TriangleQuadrature_test_1.F90) |
| `refelemdomain_triangle` | [TriangleInterpolationMetadata_test_1.F90](TriangleInterpolationMetadata_test_1.F90) |
| `vertexbasis_triangle` | [VertexBasis_Triangle_test_1.F90](VertexBasis_Triangle_test_1.F90) |

## Triangle_Method

| Public API | Example program |
| --- | --- |
| `triangle_angles_2d` | [triangle_angles_2d_test_1.F90](triangle_angles_2d_test_1.F90) |
| `triangle_angles_3d` | [triangle_angles_3d_test_1.F90](triangle_angles_3d_test_1.F90) |
| `triangle_area_2d` | [triangle_area_2d_test_1.F90](triangle_area_2d_test_1.F90) |
| `triangle_area_3d` | [triangle_area_3d_test_1.F90](triangle_area_3d_test_1.F90) |
| `triangle_area_3d_2` | [triangle_area_3d_2_test_1.F90](triangle_area_3d_2_test_1.F90) |
| `triangle_area_3d_3` | [triangle_area_3d_3_test_1.F90](triangle_area_3d_3_test_1.F90) |
| `triangle_area_heron` | [triangle_area_heron_test_1.F90](triangle_area_heron_test_1.F90) |
| `triangle_area_vector_3d` | [triangle_area_vector_3d_test_1.F90](triangle_area_vector_3d_test_1.F90) |
| `triangle_barycentric_2d` | [triangle_barycentric_2d_test_1.F90](triangle_barycentric_2d_test_1.F90) |
| `triangle_centroid_2d` | [triangle_centroid_2d_test_1.F90](triangle_centroid_2d_test_1.F90) |
| `triangle_centroid_3d` | [triangle_centroid_3d_test_1.F90](triangle_centroid_3d_test_1.F90) |
| `triangle_circumcenter` | [triangle_circumcenter_test_1.F90](triangle_circumcenter_test_1.F90) |
| `triangle_circumcenter_2d` | [triangle_circumcenter_2d_test_1.F90](triangle_circumcenter_2d_test_1.F90) |
| `triangle_circumcenter_2d_2` | [triangle_circumcenter_2d_2_test_1.F90](triangle_circumcenter_2d_2_test_1.F90) |
| `triangle_circumcircle_2d` | [triangle_circumcircle_2d_test_1.F90](triangle_circumcircle_2d_test_1.F90) |
| `triangle_circumcircle_2d_2` | [triangle_circumcircle_2d_2_test_1.F90](triangle_circumcircle_2d_2_test_1.F90) |
| `triangle_circumradius_2d` | [triangle_circumradius_2d_test_1.F90](triangle_circumradius_2d_test_1.F90) |
| `triangle_contains_line_exp_3d` | [triangle_contains_line_exp_3d_test_1.F90](triangle_contains_line_exp_3d_test_1.F90) |
| `triangle_contains_line_par_3d` | [triangle_contains_line_par_3d_test_1.F90](triangle_contains_line_par_3d_test_1.F90) |
| `triangle_contains_point_2d_1` | [triangle_contains_point_2d_1_test_1.F90](triangle_contains_point_2d_1_test_1.F90) |
| `triangle_contains_point_2d_2` | [triangle_contains_point_2d_2_test_1.F90](triangle_contains_point_2d_2_test_1.F90) |
| `triangle_contains_point_2d_3` | [triangle_contains_point_2d_3_test_1.F90](triangle_contains_point_2d_3_test_1.F90) |
| `triangle_diameter_2d` | [triangle_diameter_2d_test_1.F90](triangle_diameter_2d_test_1.F90) |
| `triangle_edge_length_2d` | [triangle_edge_length_2d_test_1.F90](triangle_edge_length_2d_test_1.F90) |
| `triangle_incenter_2d` | [triangle_incenter_2d_test_1.F90](triangle_incenter_2d_test_1.F90) |
| `triangle_incircle_2d` | [triangle_incircle_2d_test_1.F90](triangle_incircle_2d_test_1.F90) |
| `triangle_inradius_2d` | [triangle_inradius_2d_test_1.F90](triangle_inradius_2d_test_1.F90) |
| `triangle_orthocenter_2d` | [triangle_orthocenter_2d_test_1.F90](triangle_orthocenter_2d_test_1.F90) |
| `triangle_point_dist_2d` | [triangle_point_dist_2d_test_1.F90](triangle_point_dist_2d_test_1.F90) |
| `triangle_point_dist_3d` | [triangle_point_dist_3d_test_1.F90](triangle_point_dist_3d_test_1.F90) |
| `triangle_point_dist_signed_2d` | [triangle_point_dist_signed_2d_test_1.F90](triangle_point_dist_signed_2d_test_1.F90) |
| `triangle_point_near_2d` | [triangle_point_near_2d_test_1.F90](triangle_point_near_2d_test_1.F90) |
| `triangle_quality_2d` | [triangle_quality_2d_test_1.F90](triangle_quality_2d_test_1.F90) |
| `triangle_sample` | [triangle_sample_test_1.F90](triangle_sample_test_1.F90) |

## Existing orientation regression

[HierarchicalEdgeGradient_test_1.F90](HierarchicalEdgeGradient_test_1.F90)
