# BaseType

`BaseType` contains user-define data type.

|Data-type|Summary|Category|
|---|---|---|
|Math_|Contains mathematical constants.|Math|
|BoundingBox_|Data type for bounding box.|FEM|
|RealMatrix_|Extension for Fortran two-d array|Matrix|
|IntVector_|Vector of integers.|Vector|
|RealVector_|Vector of reals|Vector|
|Vector3D_|3D Vector|Vector|
|IndexValue_|Key (integer) and value (real), useful for defining nodal boundary conditions|FEM|
|DOF_|Degree of freedom object type|FEM|
|SparseMatixReOrdering_|Sparse matrix reordering scheme|LinearAlgebra|
|CSRSparisty_|Datatype for handling sparsity pattern|LinearAlgebra|
|SuperLU_|SuperLU data structure.|LinearAlgebra|
|CSRMatrix_|Compressed sparse row matrix|LinearAlgebra|
|IterationData_|Datatype for storing iteration data|FEM|
|VoigtRank2Tensor_|Rank2 tensor|Tensor|
|DeformationGradient_|Deformation Gradient tensor|Tensor|
|LeftCauchyGreen_|Left Cauchy Green tensor|Tensor|
|RightCauchyGreen_|Right Cauchy Green tensor|Tensor|
|Strain_|Strain tensor|Tensor|
|AlmansiStrain_|Almansi strain|Tensor|
|GreenStrain_|Green strain tensor|Tensor|
|SmallStrain_|Small strain tensor.|Tensor|
|ReferenceTopology_|Data type for handling reference element in FEM|FEM|
|ReferenceElement_|Data type for reference element in FEM|FEM|
|ReferencePoint_|Data type for reference point in FEM|FEM|
|ReferenceLine_|Data type for reference line in FEM|FEM|
|ReferenceTriangle_|Data type for reference triangle in FEM|FEM|
|ReferenceQuadrangle_|Data type for reference quadrangle in FEM|FEM|
|ReferenceTetrahedron_|Data type for reference tetrahedron in FEM|FEM|
|ReferenceHexahedron_|Data type for reference hexahedron in FEM|FEM|
|ReferencePrism_|Data type for reference prism in FEM|FEM|
|ReferencePyramid_|Data type for reference pyramid in FEM|FEM|
|KeyValue_|Poor man's implementation of dic.|Container|
|FEVariable_|Data type for finite element variables.|FEM|
|FEVariableConstant_|Constant finite element variable|FEM|
|FEVariableSpace_|Spatially variable finite element variable|FEM|
|FEVariableTime_|Time variable finite element variable|FEM|
|FEVariableSpaceTime_|Spatially and temporally changing finite element variable|FEM|
|FEVariableScalar_|Scalar finite element variable|FEM|
|FEVariableVector_|Vector finite element variable|FEM|
|FEVariableMatrix_|Matrix finite element variable|FEM|
|QuadraturePoint_|Quadrature points|FEM|
|BaseInterpolation_|Data type for basis interpolation|FEM|
|LagrangeInterpolation_|Lagrange interpolation|FEM|
|HermitInterpolation_|Hermit interpolation|FEM|
|SerendipityInterpolation_|Serendipity interpolation|FEM|
|HierarchyInterpolation_|Hierarchical interpolation|FEM|
|BaseContinuity_|Continuity type of basis functions.|FEM|
|H1_|H1 finite element basis|FEM|
|H1DIV_|H1(Div) finite element basis|FEM|
|H1Curl_|H1(Curl) finite element basis|FEM|
|DG_|Discontinuous Galerkin finite element basis|FEM|
|ElementData_|Data necessary for creating finite element.|FEM|
|ShapeData_|Storage for shape data|FEM|
|STShapeData_|Space-time shape function data|FEM|
|ElemshapeData_|Element shape function data|FEM|
|STElemShapeData_|Space-time element shape data.|FEM|
|QualityMeasure_|Datatype for mesh quality measure|FEM|
|Random_|Data type for random variables|FEM|
|OpenMP_|Data type for OpenMP parallel environment|FEM|
|MultiIndices_|Data type for multi indices|FEM|
