# Line examples

Each new program contains `Test_Method::OK` checks. Run from this directory
using `easifem run <filename>` after installing the current library.
Building/installing and running require explicit permission under `AGENTS.md`.
See [the module guide](../README.md) for layouts and implementation limits.

Workaround and known behavior examples are explicitly labeled in their source.
They do not validate the affected numerical implementation.

## LineInterpolationUtility

| Public API | Example program |
| --- | --- |
| `basisevalall_line` | [BasisEvalAll_Line_test_1.F90](BasisEvalAll_Line_test_1.F90) |
| `basisevalall_line_` | [BasisEvalAll_Line__test_1.F90](BasisEvalAll_Line__test_1.F90) |
| `basisgradientevalall_line` | [BasisGradientEvalAll_Line_test_1.F90](BasisGradientEvalAll_Line_test_1.F90) |
| `basisgradientevalall_line_` | [BasisGradientEvalAll_Line__test_1.F90](BasisGradientEvalAll_Line__test_1.F90) |
| `basislaplacianevalall_line` | [BasisLaplacianEvalAll_Line_test_1.F90](BasisLaplacianEvalAll_Line_test_1.F90) |
| `basislaplacianevalall_line_` | [BasisLaplacianEvalAll_Line__test_1.F90](BasisLaplacianEvalAll_Line__test_1.F90) |
| `equidistanceinpoint_line` | [EquidistanceInPoint_Line_test_1.F90](EquidistanceInPoint_Line_test_1.F90) |
| `equidistanceinpoint_line_` | [EquidistanceInPoint_Line__test_1.F90](EquidistanceInPoint_Line__test_1.F90) |
| `equidistancepoint_line` | [EquidistancePoint_Line_test_1.F90](EquidistancePoint_Line_test_1.F90) |
| `equidistancepoint_line_` | [EquidistancePoint_Line__test_1.F90](EquidistancePoint_Line__test_1.F90) |
| `gettotaldof_line` | [LineInterpolationMetadata_test_1.F90](LineInterpolationMetadata_test_1.F90) |
| `gettotalindof_line` | [LineInterpolationMetadata_test_1.F90](LineInterpolationMetadata_test_1.F90) |
| `heirarchicalbasis_line` | [HeirarchicalBasis_Line_test_1.F90](HeirarchicalBasis_Line_test_1.F90) |
| `heirarchicalbasis_line_` | [HeirarchicalBasis_Line__test_1.F90](HeirarchicalBasis_Line__test_1.F90) |
| `heirarchicalbasisgradient_line` | [HeirarchicalBasisGradient_Line_test_1.F90](HeirarchicalBasisGradient_Line_test_1.F90) |
| `heirarchicalbasisgradient_line_` | [HeirarchicalBasisGradient_Line__test_1.F90](HeirarchicalBasisGradient_Line__test_1.F90) |
| `heirarchicalbasislaplacian_line` | [HeirarchicalBasisLaplacian_Line_test_1.F90](HeirarchicalBasisLaplacian_Line_test_1.F90) |
| `heirarchicalbasislaplacian_line_` | [HeirarchicalBasisLaplacian_Line__test_1.F90](HeirarchicalBasisLaplacian_Line__test_1.F90) |
| `interpolationpoint_line` | [InterpolationPoint_Line_test_1.F90](InterpolationPoint_Line_test_1.F90) |
| `interpolationpoint_line_` | [InterpolationPoint_Line__test_1.F90](InterpolationPoint_Line__test_1.F90) |
| `lagrangecoeff_line` | [LagrangeCoeff_Line_test_1.F90](LagrangeCoeff_Line_test_1.F90) |
| `lagrangecoeff_line_` | [LagrangeCoeff_Line__test_1.F90](LagrangeCoeff_Line__test_1.F90) |
| `lagrangedegree_line` | [LineInterpolationMetadata_test_1.F90](LineInterpolationMetadata_test_1.F90) |
| `lagrangedof_line` | [LineInterpolationMetadata_test_1.F90](LineInterpolationMetadata_test_1.F90) |
| `lagrangedof_point` | [LineInterpolationMetadata_test_1.F90](LineInterpolationMetadata_test_1.F90) |
| `lagrangeevalall_line` | [LagrangeEvalAll_Line_test_1.F90](LagrangeEvalAll_Line_test_1.F90) |
| `lagrangeevalall_line_` | [LagrangeEvalAll_Line__test_1.F90](LagrangeEvalAll_Line__test_1.F90) |
| `lagrangegradientevalall_line` | [LagrangeGradientEvalAll_Line_test_1.F90](LagrangeGradientEvalAll_Line_test_1.F90) |
| `lagrangegradientevalall_line_` | [LagrangeGradientEvalAll_Line__test_1.F90](LagrangeGradientEvalAll_Line__test_1.F90) |
| `lagrangeindof_line` | [LineInterpolationMetadata_test_1.F90](LineInterpolationMetadata_test_1.F90) |
| `lagrangelaplacianevalall_line` | [LagrangeLaplacianEvalAll_Line_test_1.F90](LagrangeLaplacianEvalAll_Line_test_1.F90) |
| `lagrangelaplacianevalall_line_` | [LagrangeLaplacianEvalAll_Line__test_1.F90](LagrangeLaplacianEvalAll_Line__test_1.F90) |
| `orthogonalbasis_line` | [OrthogonalBasis_Line_test_1.F90](OrthogonalBasis_Line_test_1.F90) |
| `orthogonalbasis_line_` | [OrthogonalBasis_Line__test_1.F90](OrthogonalBasis_Line__test_1.F90) |
| `orthogonalbasisgradient_line` | [OrthogonalBasisGradient_Line_test_1.F90](OrthogonalBasisGradient_Line_test_1.F90) |
| `orthogonalbasisgradient_line_` | [OrthogonalBasisGradient_Line__test_1.F90](OrthogonalBasisGradient_Line__test_1.F90) |
| `orthogonalbasislaplacian_line` | [OrthogonalBasisLaplacian_Line_test_1.F90](OrthogonalBasisLaplacian_Line_test_1.F90) |
| `orthogonalbasislaplacian_line_` | [OrthogonalBasisLaplacian_Line__test_1.F90](OrthogonalBasisLaplacian_Line__test_1.F90) |
| `quadraturenumber_line` | [LineQuadrature_test_1.F90](LineQuadrature_test_1.F90) |
| `quadraturepoint_line` | [LineQuadrature_test_1.F90](LineQuadrature_test_1.F90) |
| `quadraturepoint_line_` | [LineQuadrature_test_1.F90](LineQuadrature_test_1.F90) |
| `refelemdomain_line` | [LineInterpolationMetadata_test_1.F90](LineInterpolationMetadata_test_1.F90) |
| `tovefc_line` | [ToVEFC_Line_test_1.F90](ToVEFC_Line_test_1.F90) |

## Line_Method

| Public API | Example program |
| --- | --- |
| `line_exp2imp_2d` | [line_exp2imp_2d_test_1.F90](line_exp2imp_2d_test_1.F90) |
| `line_exp_is_degenerate_nd` | [line_exp_is_degenerate_nd_test_1.F90](line_exp_is_degenerate_nd_test_1.F90) |
| `line_exp_perp_2d` | [line_exp_perp_2d_test_1.F90](line_exp_perp_2d_test_1.F90) |
| `line_exp_point_dist_signed_2d` | [line_exp_point_dist_signed_2d_test_1.F90](line_exp_point_dist_signed_2d_test_1.F90) |
| `line_imp_is_degenerate_2d` | [line_imp_is_degenerate_2d_test_1.F90](line_imp_is_degenerate_2d_test_1.F90) |
| `lines_exp_int_2d` | [lines_exp_int_2d_test_1.F90](lines_exp_int_2d_test_1.F90) |
| `lines_imp_int_2d` | [lines_imp_int_2d_test_1.F90](lines_imp_int_2d_test_1.F90) |
| `segment_point_dist_2d` | [segment_point_dist_2d_test_1.F90](segment_point_dist_2d_test_1.F90) |
| `segment_point_dist_3d` | [segment_point_dist_3d_test_1.F90](segment_point_dist_3d_test_1.F90) |
| `segment_point_near_2d` | [segment_point_near_2d_test_1.F90](segment_point_near_2d_test_1.F90) |

## ReferenceLine_Method

| Public API | Example program |
| --- | --- |
| `default_ref_line_coord` | [ReferenceLineMetadata_test_1.F90](ReferenceLineMetadata_test_1.F90) |
| `elementname_line` | [ReferenceLineMetadata_test_1.F90](ReferenceLineMetadata_test_1.F90) |
| `elementorder_line` | [ReferenceLineMetadata_test_1.F90](ReferenceLineMetadata_test_1.F90) |
| `elementtype_line` | [ReferenceLineMetadata_test_1.F90](ReferenceLineMetadata_test_1.F90) |
| `facetelements_line` | [ReferenceLineConnectivity_test_1.F90](ReferenceLineConnectivity_test_1.F90) |
| `facettopology_line` | [ReferenceLineConnectivity_test_1.F90](ReferenceLineConnectivity_test_1.F90) |
| `getedgeconnectivity_line` | [ReferenceLineConnectivity_test_1.F90](ReferenceLineConnectivity_test_1.F90) |
| `getfaceconnectivity_line` | [ReferenceLineConnectivity_test_1.F90](ReferenceLineConnectivity_test_1.F90) |
| `getfaceelemtype_line` | [ReferenceLineConnectivity_test_1.F90](ReferenceLineConnectivity_test_1.F90) |
| `highorderelement_line` | [ReferenceLineGeometry_test_1.F90](ReferenceLineGeometry_test_1.F90) |
| `initiate` | [ReferenceLineConstructors_test_1.F90](ReferenceLineConstructors_test_1.F90) |
| `line_quality` | [ReferenceLineGeometry_test_1.F90](ReferenceLineGeometry_test_1.F90) |
| `linename` | [ReferenceLineMetadata_test_1.F90](ReferenceLineMetadata_test_1.F90) |
| `maxorder_line` | [ReferenceLineMetadata_test_1.F90](ReferenceLineMetadata_test_1.F90) |
| `measure_simplex_line` | [ReferenceLineGeometry_test_1.F90](ReferenceLineGeometry_test_1.F90) |
| `quality_line` | [ReferenceLineGeometry_test_1.F90](ReferenceLineGeometry_test_1.F90) |
| `refcoord_line` | [ReferenceLineConstructors_test_1.F90](ReferenceLineConstructors_test_1.F90) |
| `referenceline` | [ReferenceLineConstructors_test_1.F90](ReferenceLineConstructors_test_1.F90) |
| `referenceline_pointer` | [ReferenceLineConstructors_test_1.F90](ReferenceLineConstructors_test_1.F90) |
| `reflinecoord` | [ReferenceLineConstructors_test_1.F90](ReferenceLineConstructors_test_1.F90) |
| `totalentities_line` | [ReferenceLineMetadata_test_1.F90](ReferenceLineMetadata_test_1.F90) |
| `totalnodesinelement_line` | [ReferenceLineMetadata_test_1.F90](ReferenceLineMetadata_test_1.F90) |
