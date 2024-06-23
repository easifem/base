! This program is a part of EASIFEM library
! Copyright (C) 2020-2021 Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see < https://www.gnu.org/licenses/>
!

!> author: Dr. Vikas Sharma
!
! [[BaseType]] module contains several userful user defined data types.

MODULE BaseType
USE GlobalData
USE String_Class, ONLY: String
#ifdef USE_SuperLU
USE SuperLUInterface
USE ISO_C_BINDING, ONLY: C_CHAR, C_PTR, C_SIZE_T
#endif
IMPLICIT NONE
PRIVATE

PUBLIC :: Math
PUBLIC :: BoundingBox_
PUBLIC :: TypeBoundingBox
PUBLIC :: BoundingBoxPointer_
PUBLIC :: RealMatrix_
PUBLIC :: TypeRealMatrix
PUBLIC :: RealMatrixPointer_
PUBLIC :: IntVector_
PUBLIC :: TypeIntVector
PUBLIC :: IntVectorPointer_
PUBLIC :: RealVector_
PUBLIC :: TypeRealVector
PUBLIC :: RealVectorPointer_
PUBLIC :: Vector3D_
PUBLIC :: TypeVector3D
PUBLIC :: Vector3DPointer_
PUBLIC :: IndexValue_
PUBLIC :: IndexValuePointer_
PUBLIC :: DOF_
PUBLIC :: TypeDOF
PUBLIC :: DOFPointer_
PUBLIC :: SparseMatrixReOrdering_
PUBLIC :: TypeSparseMatrixReOrdering
PUBLIC :: CSRSparsity_
PUBLIC :: TypeCSRSparsity
PUBLIC :: CSRSparsityPointer_
PUBLIC :: CSRMatrix_
PUBLIC :: TypeCSRMatrix
PUBLIC :: CSRMatrixPointer_
PUBLIC :: IterationData_
PUBLIC :: TypeIterationData
PUBLIC :: IterationDataPointer_
PUBLIC :: VoigtRank2Tensor_
PUBLIC :: TypeVoigtRank2Tensor
PUBLIC :: VoigtRank2TensorPointer
PUBLIC :: Rank2Tensor_
PUBLIC :: TypeRank2Tensor
PUBLIC :: Rank2TensorPointer_
PUBLIC :: DeformationGradient_
PUBLIC :: DeformationGradientPointer_
PUBLIC :: TypeDeformationGradient
PUBLIC :: LeftCauchyGreen_
PUBLIC :: TypeLeftCauchyGreen
PUBLIC :: LeftCauchyGreenPointer_
PUBLIC :: RightCauchyGreen_
PUBLIC :: TypeRightCauchyGreen
PUBLIC :: RightCauchyGreenPointer_
PUBLIC :: Strain_
PUBLIC :: TypeStrain
PUBLIC :: StrainPointer_
PUBLIC :: AlmansiStrain_
PUBLIC :: TypeAlmansiStrain
PUBLIC :: AlmansiStrainPointer_
PUBLIC :: GreenStrain_
PUBLIC :: TypeGreenStrain
PUBLIC :: GreenStrainPointer_
PUBLIC :: SmallStrain_
PUBLIC :: TypeSmallStrain
PUBLIC :: SmallStrainPointer_
PUBLIC :: ReferenceTopology_
! PUBLIC :: TypeReferenceTopology
PUBLIC :: ReferenceTopologyPointer_
PUBLIC :: ReferenceElement_
PUBLIC :: ReferenceElementPointer_
PUBLIC :: ReferencePoint_
PUBLIC :: TypeReferencePoint
PUBLIC :: ReferenceLine_
PUBLIC :: TypeReferenceLine
PUBLIC :: ReferenceTriangle_
PUBLIC :: TypeReferenceTriangle
PUBLIC :: ReferenceQuadrangle_
PUBLIC :: TypeReferenceQuadrangle
PUBLIC :: ReferenceTetrahedron_
PUBLIC :: TypeReferenceTetrahedron
PUBLIC :: ReferenceHexahedron_
PUBLIC :: TypeReferenceHexahedron
PUBLIC :: ReferencePrism_
PUBLIC :: TypeReferencePrism
PUBLIC :: ReferencePyramid_
PUBLIC :: TypeReferencePyramid
PUBLIC :: KeyValue_
PUBLIC :: TypeKeyValue
PUBLIC :: FEVariable_
PUBLIC :: TypeFEVariable
PUBLIC :: FEVariableConstant_
PUBLIC :: TypeFEVariableConstant
PUBLIC :: TypeVariableConstant
PUBLIC :: FEVariableSpace_
PUBLIC :: TypeFEVariableSpace
PUBLIC :: TypeVariableSpace
PUBLIC :: FEVariableSpaceTime_
PUBLIC :: TypeFEVariableSpaceTime
PUBLIC :: TypeVariableSpaceTime
PUBLIC :: FEVariableTime_
PUBLIC :: TypeFEVariableTime
PUBLIC :: TypeVariableTime
PUBLIC :: FEVariableScalar_
PUBLIC :: TypeFEVariableScalar
PUBLIC :: TypeVariableScalar
PUBLIC :: FEVariableVector_
PUBLIC :: TypeFEVariableVector
PUBLIC :: TypeVariableVector
PUBLIC :: FEVariableMatrix_
PUBLIC :: TypeFEVariableMatrix
PUBLIC :: TypeVariableMatrix
PUBLIC :: QuadraturePoint_
PUBLIC :: TypeQuadraturePoint
PUBLIC :: QuadraturePointPointer_
PUBLIC :: BaseInterpolation_
PUBLIC :: LagrangeInterpolation_
PUBLIC :: TypeLagrangeInterpolation
PUBLIC :: HermitInterpolation_
PUBLIC :: TypeHermitInterpolation
PUBLIC :: SerendipityInterpolation_
PUBLIC :: TypeSerendipityInterpolation
PUBLIC :: HierarchyInterpolation_
PUBLIC :: TypeHierarchyInterpolation
PUBLIC :: OrthogonalInterpolation_
PUBLIC :: TypeOrthogonalInterpolation
PUBLIC :: BaseContinuity_
PUBLIC :: TypeBaseContinuity
PUBLIC :: H1_
PUBLIC :: TypeH1
PUBLIC :: HDIV_
PUBLIC :: TypeHDIV
PUBLIC :: HCURL_
PUBLIC :: TypeHCURL
PUBLIC :: DG_
PUBLIC :: TypeDG
PUBLIC :: DEL_NONE, DEL_X, DEL_Y, DEL_Z, DEL_X_ALL, DEL_t
PUBLIC :: ElementData_
PUBLIC :: TypeElementData
PUBLIC :: ElementDataPointer_
PUBLIC :: ShapeData_
PUBLIC :: TypeShapeData
PUBLIC :: ShapeDataPointer_
PUBLIC :: STShapeData_
PUBLIC :: STShapeDataPointer_
PUBLIC :: ElemShapeData_
PUBLIC :: TypeElemShapeData
PUBLIC :: ElemShapeDataPointer_
PUBLIC :: STElemShapeData_
PUBLIC :: TypeSTElemShapeData
PUBLIC :: QualityMeasure
PUBLIC :: Random_
PUBLIC :: TypeRandom
PUBLIC :: OMP
PUBLIC :: TypeOpenMP
PUBLIC :: MultiIndices_
PUBLIC :: iface_SpaceTimeFunction
PUBLIC :: iface_SpaceFunction
PUBLIC :: iface_TimeFunction
PUBLIC :: iface_1DFunction
PUBLIC :: iface_2DFunction
PUBLIC :: iface_3DFunction
PUBLIC :: iface_ScalarFunction
PUBLIC :: iface_VectorFunction
PUBLIC :: iface_MatrixFunction
PUBLIC :: Range_
PUBLIC :: Interval1D_
PUBLIC :: TypePrecondOpt
PUBLIC :: TypeConvergenceOpt
PUBLIC :: TypeSolverNameOpt

INTEGER(I4B), PARAMETER, PUBLIC :: MAX_RANK_FEVARIABLE = 6

!----------------------------------------------------------------------------
!                                                                 Math_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 March 2022
! summary: Math class

TYPE :: Math_
  REAL(DFP) :: PI = 3.14159265359_DFP
  REAL(DFP) :: e = 2.718281828459045_DFP
  COMPLEX(DFPC) :: i = (0.0_DFP, 1.0_DFP)
  COMPLEX(DFPC) :: j = (0.0_DFP, 1.0_DFP)
  REAL(DFP), DIMENSION(3, 3) :: Eye3 = RESHAPE([ &
                                & 1.0_DFP, 0.0_DFP, 0.0_DFP, &
                                & 0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                & 0.0_DFP, 0.0_DFP, 1.0_DFP], &
                                & [3, 3])
  REAL(DFP), DIMENSION(2, 2) :: Eye2 = RESHAPE([ &
                              & 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP], &
                              & [2, 2])
END TYPE Math_

TYPE(Math_), PARAMETER :: Math = Math_()

!----------------------------------------------------------------------------
!                                                               BoundingBox_
!----------------------------------------------------------------------------

! date: 23 Feb 2021
!> author: Vikas Sharma, Ph. D.
! summary: A data type to represent a bounding box;
!
!{!pages/BoundingBox_.md!}

TYPE :: BoundingBox_
  INTEGER(I4B) :: nsd = 0
    !! Number of spatial dimension
    !! NSD = 1, 2, 3 for 1D, 2D, 3D box
  REAL(DFP) :: box(2, 3) = 0.0
    !! Box contains the xmin, ymin, ...
    !! `Box(1:2, 1:3)`  an array containing box coordinates.
    !!- `Box(1:2, 1:3)`  an array containing box coordinates.
    !!- `Box(1, 1)` is x_min
    !!- `Box(2, 1)` is x_max
    !!- `Box(1, 2)` is y_min
    !!- `Box(2, 2)` is y_max
    !!- `Box(1, 3)` is z_min
    !!- `Box(2, 3)` is z_max
  REAL(DFP) :: l(3) = 0.0_DFP
  !! l(1) length in x
  !! l(2) length in y
  !! l(3) length in z
END TYPE BoundingBox_

TYPE(BoundingBox_), PARAMETER :: TypeBoundingBox = BoundingBox_()
!! A Type Instance of Boundingbox

!> author: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: Contains the pointer to the [[BoundingBox_]] data type.

TYPE :: BoundingBoxPointer_
  CLASS(BoundingBoxPointer_), POINTER :: ptr => NULL()
END TYPE BoundingBoxPointer_

!----------------------------------------------------------------------------
!                                                                    Matrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: A data type for an Array of rank 2 of real numbers
!
!{!pages/docs-api/RealMatrix/RealMatrix_.md!}

TYPE :: RealMatrix_
  INTEGER(I4B) :: tDimension = 0_I4B
  CHARACTER(5) :: MatrixProp = 'UNSYM'
  REAL(DFP), ALLOCATABLE :: Val(:, :)
END TYPE RealMatrix_

TYPE(RealMatrix_), PARAMETER :: TypeRealMatrix = RealMatrix_(Val=NULL())

TYPE :: RealMatrixPointer_
  CLASS(RealMatrix_), POINTER :: ptr => NULL()
END TYPE RealMatrixPointer_

!----------------------------------------------------------------------------
!                                                             IntVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         24 Feb 2021
! summary:         A data type to contain fortran vector of integer numbers
!
!{!pages/IntVector_.md!}

TYPE :: IntVector_
  INTEGER(I4B) :: tDimension = 1_I4B
  INTEGER(I4B), ALLOCATABLE :: Val(:)
END TYPE IntVector_

TYPE(IntVector_), PARAMETER :: TypeIntVector = IntVector_(Val=NULL())

TYPE :: IntVectorPointer_
  CLASS(IntVector_), POINTER :: ptr => NULL()
END TYPE IntVectorPointer_

!----------------------------------------------------------------------------
!                                                             RealVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 Feb 2021
! summary: A data type to contain fortran vector of real numbers
!
!{!pages/RealVector_.md!}

TYPE :: RealVector_
  INTEGER(I4B) :: tDimension = 1_I4B
  REAL(DFP), ALLOCATABLE :: Val(:)
END TYPE RealVector_

TYPE(RealVector_), PARAMETER :: TypeRealVector = RealVector_(Val=NULL())

TYPE :: RealVectorPointer_
  CLASS(RealVector_), POINTER :: ptr => NULL()
END TYPE RealVectorPointer_

!----------------------------------------------------------------------------
!                                                                 Vector3D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! summary: Data type for 3D vectors
! date: 24 Feb 2021

TYPE :: Vector3D_
  INTEGER(I4B) :: tDimension = 1_I4B
  REAL(DFP) :: Val(3) = 0.0_DFP
END TYPE Vector3D_

TYPE(Vector3D_), PARAMETER :: TypeVector3D = Vector3D_()

!----------------------------------------------------------------------------
!                                                            Vector3DPointer_
!----------------------------------------------------------------------------

TYPE :: Vector3DPointer_
  CLASS(Vector3D_), POINTER :: ptr => NULL()
END TYPE Vector3DPointer_

!----------------------------------------------------------------------------
!                                                              IndexValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 June 2022
! summary: Index value keymap; useful for defining nodal boundary conditions

TYPE :: IndexValue_
  INTEGER(I4B) :: Indx
  REAL(DFP) :: Val
END TYPE

TYPE(IndexValue_), PUBLIC, PARAMETER :: TypeIndexValue = &
  & IndexValue_(Indx=0, Val=0.0_DFP)

TYPE :: IndexValuePointer_
  CLASS(IndexValue_), POINTER :: ptr => NULL()
END TYPE IndexValuePointer_

!----------------------------------------------------------------------------
!                                                                     DOF_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: Degree of freedom object type

TYPE :: DOF_
  INTEGER(I4B), ALLOCATABLE :: map(:, :)
    !! Encapsulation of information of DOF
  INTEGER(I4B), ALLOCATABLE :: valMap(:)
    !! Val map
  INTEGER(I4B) :: storageFMT = FMT_NODES
    !! Storage format
END TYPE DOF_

TYPE(DOF_), PARAMETER :: TypeDOF = DOF_(MAP=NULL(), ValMap=NULL())

TYPE :: DOFPointer_
  CLASS(DOF_), POINTER :: ptr => NULL()
END TYPE DOFPointer_

!----------------------------------------------------------------------------
!                                                            SparseOrdering
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 June 2022
! summary: SparseMatrix reordering scheme

TYPE :: SparseMatrixReOrdering_
  CHARACTER(10) :: name
  INTEGER(I4B), ALLOCATABLE :: PERM(:)
  INTEGER(I4B), ALLOCATABLE :: IPERM(:)
END TYPE SparseMatrixReOrdering_

TYPE(SparseMatrixReOrdering_), PARAMETER :: TypeSparseMatrixReOrdering = &
  & SparseMatrixReOrdering_(name='', PERM=NULL(), IPERM=NULL())

!----------------------------------------------------------------------------
!                                                               CSRSparsity_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 June 2021
! summary: User data type for handling the sparsity pattern
!
!{!pages/CSRSparsity_.md!}

TYPE :: CSRSparsity_
  INTEGER(I4B) :: nnz = 0
  INTEGER(I4B) :: ncol = 0
  INTEGER(I4B) :: nrow = 0
  LOGICAL(LGT) :: isSorted = .FALSE.
  LOGICAL(LGT) :: isInitiated = .FALSE.
  LOGICAL(LGT) :: isSparsityLock = .FALSE.
  LOGICAL(LGT) :: isDiagStored = .FALSE.
  INTEGER(I4B), ALLOCATABLE :: IA(:)
  INTEGER(I4B), ALLOCATABLE :: JA(:)
  INTEGER(I4B), ALLOCATABLE :: idiag(:)
  TYPE(IntVector_), ALLOCATABLE :: row(:)
  TYPE(DOF_) :: idof
  !! DOF for row
  TYPE(DOF_) :: jdof
  !! DOF for columns
END TYPE CSRSparsity_

TYPE(CSRSparsity_), PARAMETER :: TypeCSRSparsity = &
  & CSRSparsity_(IA=NULL(), JA=NULL(), Row=NULL())

TYPE :: CSRSparsityPointer_
  CLASS(CSRSparsity_), POINTER :: ptr => NULL()
END TYPE CSRSparsityPointer_

!----------------------------------------------------------------------------
!                                                                 SuperLU_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  23-01-25
! summary: SuperLU data structure

#ifdef USE_SuperLU
TYPE :: SuperLU_
  TYPE(SuperMatrix) :: A
  TYPE(SuperMatrix) :: B
  TYPE(SuperMatrix) :: X
  TYPE(SuperMatrix) :: L
  TYPE(SuperMatrix) :: U
  TYPE(GlobalLU_t) :: Glu
  TYPE(superlu_options_t) :: options
  TYPE(SuperLUStat_t) :: stat
  TYPE(mem_usage_t) :: mem_usage
  TYPE(C_PTR) :: Work
  ! TYPE(C_PTR), POINTER :: Work
  !! work-space for superlu, the size is decided by superlu
  INTEGER(I4B), ALLOCATABLE :: ia(:)
  !! starting index of row, size(m+1)
  INTEGER(I4B), ALLOCATABLE :: ja(:)
  !! column indices, size(nnz)
  INTEGER(I4B), ALLOCATABLE :: perm_c(:)
  !! col permutation, size(n)
  INTEGER(I4B), ALLOCATABLE :: perm_r(:)
  !! row permutation, size(m)
  INTEGER(I4B), ALLOCATABLE :: etree(:)
  !! elimination tree, size(n)
  REAL(DFP), ALLOCATABLE :: nzval(:)
  !! nonzero values, size(nnz)
  REAL(DFP), ALLOCATABLE :: sol(:, :)
  !! solution, size(n, nrhs)
  REAL(DFP), ALLOCATABLE :: rhs(:, :)
  !! right hand side, size(m, nrhs)
  REAL(DFP), ALLOCATABLE :: R(:)
  !! row digonal scaling, size(m)
  REAL(DFP), ALLOCATABLE :: C(:)
  !! column diagonal scaling, size(n)
  REAL(DFP), ALLOCATABLE :: ferr(:)
  !! size(nrhs)
  REAL(DFP), ALLOCATABLE :: berr(:)
  !! size(nrhs)
  CHARACTER(1, kind=C_CHAR) :: equed(2)
  INTEGER(C_SIZE_T) :: lwork = 0
  INTEGER(C_SIZE_T) :: info = 0
  REAL(DFP) :: recip_pivot_growth = 0.0_DFP
  REAL(DFP) :: rcond = 0.0_DFP
  LOGICAL(LGT) :: isAInitiated = .FALSE.
  LOGICAL(LGT) :: isBInitiated = .FALSE.
  LOGICAL(LGT) :: isXInitiated = .FALSE.
  LOGICAL(LGT) :: isLInitiated = .FALSE.
  LOGICAL(LGT) :: isUInitiated = .FALSE.
  LOGICAL(LGT) :: isGluInitiated = .FALSE.
  LOGICAL(LGT) :: isStatInitiated = .FALSE.
END TYPE SuperLU_
#endif

!----------------------------------------------------------------------------
!                                                             CSRMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: User data type for handling CSR matrices

TYPE :: CSRMatrix_
  LOGICAL(LGT) :: csrOwnership = .TRUE.
  !! This variable, if true, denotes that csr is allocated inside the obj
  INTEGER(I4B) :: tDimension = 2_I4B
  CHARACTER(20) :: matrixProp = 'UNSYM'
  REAL(DFP), ALLOCATABLE :: A(:)
  TYPE(CSRSparsity_) :: csr
#ifdef USE_SuperLU
  TYPE(SuperLU_), POINTER :: slu => NULL()
#endif
END TYPE CSRMatrix_

TYPE(CSRMatrix_), PARAMETER :: TypeCSRMatrix = CSRMatrix_(&
  & A=NULL(), slu=NULL())

TYPE :: CSRMatrixPointer_
  CLASS(CSRMatrix_), POINTER :: ptr => NULL()
END TYPE CSRMatrixPointer_

!----------------------------------------------------------------------------
!                                                            IterationData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 June 2022
! summary: Iteration data

TYPE :: IterationData_
  INTEGER(I4B) :: maxIter = 100
    !! Maximum number of iterations allowed
  INTEGER(I4B) :: iterationNumber = 1
    !! Iteration number
  REAL(DFP) :: residualError0 = 0.0
    !! Initial Residual error
  REAL(DFP) :: residualError = 0.0
    !! Current residual error
  REAL(DFP) :: residualTolerance = 1.0E-5
    !! Tolerance for checking convergence in residual
  REAL(DFP) :: solutionError0 = 0.0
    !! Initial solution error
  REAL(DFP) :: solutionError = 0.0
    !! Current solution error
  REAL(DFP) :: solutionTolerance = 1.0E-5
    !! Tolerance for checking convergence in solution
  INTEGER(I4B) :: convergenceType = RelativeConvergence
    !! Type of convergence
  INTEGER(I4B) :: convergenceIn = ConvergenceInRes
    !! Check Convergence in solution and/or residual
  INTEGER(I4B) :: normType = NormL2
    !! Error norm type
  LOGICAL(LGT) :: converged = .FALSE.
    !! Status of convergence
  REAL(DFP) :: timeAtStart = 0.0
    !! Starting time
  REAL(DFP) :: timeAtEnd = 0.0
    !! Present time
  REAL(DFP), ALLOCATABLE :: convergenceData(:, :)
    !! history of convergence data
    !! each column corresponding to a iteration
  TYPE(String), ALLOCATABLE :: header(:)
    !! header for convergenceData
END TYPE IterationData_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE(IterationData_), PARAMETER :: TypeIterationData = &
  & IterationData_(header=NULL())

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: IterationDataPointer_
  CLASS(IterationData_), POINTER :: ptr => NULL()
END TYPE IterationDataPointer_

!----------------------------------------------------------------------------
!                                                       VoigtRank2Tensor_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 June 2022
! summary: Voigt representation of rank2 tensor

TYPE :: VoigtRank2Tensor_
  REAL(DFP) :: V(6) = 0.0_DFP
  REAL(DFP) :: Scale = 1.0_DFP
  INTEGER(I4B) :: VoigtType = StressTypeVoigt
END TYPE VoigtRank2Tensor_

TYPE(VoigtRank2Tensor_), PARAMETER :: TypeVoigtRank2Tensor  &
  & = VoigtRank2Tensor_()

TYPE :: VoigtRank2TensorPointer
  CLASS(VoigtRank2Tensor_), POINTER :: ptr => NULL()
END TYPE VoigtRank2TensorPointer

!----------------------------------------------------------------------------
!                                                                  Tensor_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-05
! summary:  General type for Tensor

TYPE :: Tensor_
END TYPE Tensor_

!----------------------------------------------------------------------------
!                                                              Rank2Tensor_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary:  Rank 2 tensor

TYPE, EXTENDS(Tensor_) :: Rank2Tensor_
  REAL(DFP) :: T(3, 3) = 0.0_DFP
  LOGICAL(LGT) :: isSym = .FALSE.
END TYPE Rank2Tensor_

TYPE(Rank2Tensor_), PARAMETER :: TypeRank2Tensor = Rank2Tensor_()

TYPE :: Rank2TensorPointer_
  CLASS(Rank2Tensor_), POINTER :: ptr => NULL()
END TYPE Rank2TensorPointer_

!----------------------------------------------------------------------------
!                                                       DeformationGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-05
! summary:  Deformation gradient tensor

TYPE, EXTENDS(Rank2Tensor_) :: DeformationGradient_
END TYPE DeformationGradient_

TYPE(DeformationGradient_), PARAMETER :: TypeDeformationGradient  &
  & = DeformationGradient_()

TYPE :: DeformationGradientPointer_
  CLASS(DeformationGradient_), POINTER :: ptr => NULL()
END TYPE DeformationGradientPointer_

!----------------------------------------------------------------------------
!                                                           LeftCauchyGreen_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Left Cauchy Green Deformation tensor
!
!# Introduction
! This data tyoe defines Left Cauchy Green Deformation tensor, which
! is an Eulerian tensor. It is symmetric and given by
!
! $$b = F F^{T}=V^2$$
!
!{!pages/docs-api/LeftCauchyGreen/LeftCauchyGreen_.md!}

TYPE, EXTENDS(Rank2Tensor_) :: LeftCauchyGreen_
END TYPE LeftCauchyGreen_

TYPE(LeftCauchyGreen_), PARAMETER :: TypeLeftCauchyGreen  &
  & = LeftCauchyGreen_()

TYPE :: LeftCauchyGreenPointer_
  CLASS(LeftCauchyGreen_), POINTER :: ptr => NULL()
END TYPE LeftCauchyGreenPointer_

!----------------------------------------------------------------------------
!                                                          RightCauchyGreen_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Right Cauchy Green Deformation tensor
!
!# Introduction
! This data tyoe defines Right Cauchy Green Deformation tensor, which is an
! Eulerian tensor. It is symmetric and given by
!
! $$b = F F^{T}=V^2$$
!
!{!pages/RightCauchyGreen.md}

TYPE, EXTENDS(Rank2Tensor_) :: RightCauchyGreen_
END TYPE RightCauchyGreen_

TYPE(RightCauchyGreen_), PARAMETER :: TypeRightCauchyGreen  &
  & = RightCauchyGreen_()

TYPE :: RightCauchyGreenPointer_
  CLASS(RightCauchyGreen_), POINTER :: ptr => NULL()
END TYPE RightCauchyGreenPointer_

!----------------------------------------------------------------------------
!                                                                    Strain_
!----------------------------------------------------------------------------

TYPE, EXTENDS(Rank2Tensor_) :: Strain_
END TYPE Strain_

TYPE(Strain_), PARAMETER :: TypeStrain = Strain_()

TYPE :: StrainPointer_
  CLASS(Strain_), POINTER :: ptr => NULL()
END TYPE StrainPointer_

!----------------------------------------------------------------------------
!                                                             AlmansiStrain_
!----------------------------------------------------------------------------

TYPE, EXTENDS(Strain_) :: AlmansiStrain_
END TYPE AlmansiStrain_

TYPE(AlmansiStrain_), PARAMETER :: TypeAlmansiStrain = AlmansiStrain_()

TYPE :: AlmansiStrainPointer_
  CLASS(AlmansiStrain_), POINTER :: ptr => NULL()
END TYPE AlmansiStrainPointer_

!----------------------------------------------------------------------------
!                                                             GreenStrain_
!----------------------------------------------------------------------------

TYPE, EXTENDS(Strain_) :: GreenStrain_
END TYPE GreenStrain_

TYPE(GreenStrain_), PARAMETER :: TypeGreenStrain = GreenStrain_()

TYPE :: GreenStrainPointer_
  CLASS(GreenStrain_), POINTER :: ptr => NULL()
END TYPE GreenStrainPointer_

!----------------------------------------------------------------------------
!                                                             SmallStrain_
!----------------------------------------------------------------------------

TYPE, EXTENDS(Strain_) :: SmallStrain_
END TYPE SmallStrain_

TYPE(SmallStrain_), PARAMETER :: TypeSmallStrain = SmallStrain_()

TYPE :: SmallStrainPointer_
  CLASS(SmallStrain_), POINTER :: ptr => NULL()
END TYPE SmallStrainPointer_

!----------------------------------------------------------------------------
!                                                       ReferenceTopology_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  1 March 2021
! summary: This data type is defined to handle reference topology
!
!{!pages/ReferenceElement_.md}

TYPE :: ReferenceTopology_
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  INTEGER(I4B) :: name = 0
  INTEGER(I4B) :: xiDimension = 0
END TYPE ReferenceTopology_

TYPE :: ReferenceTopologyPointer_
  CLASS(ReferenceTopology_), POINTER :: ptr => NULL()
END TYPE ReferenceTopologyPointer_

!----------------------------------------------------------------------------
!                                                          ReferenceElement_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 March 2021
! summary: An abstract data type for Reference Element
!
!{!pages/docs-api/ReferenceElement/ReferenceElement_.md!}

TYPE :: ReferenceElement_
  CHARACTER(10) :: domainName = "GENERAL"
    !! UNIT, BIUNIT, GENERAL
  INTEGER(I4B) :: entityCounts(4) = 0
    !! Number of 0D, 1D, 2D, 3D entities
    !! entityCounts(1) = total number of points
    !! entityCounts(2) = total number of edges
    !! entityCounts(3) = total number of faces
    !! entityCounts(4) = total number of cells
  INTEGER(I4B) :: xiDimension = 0
    !! Xidimension
  INTEGER(I4B) :: name = 0
    !! name of the element
  INTEGER(I4B) :: order = 0
    !! Order of element
  INTEGER(I4B) :: nsd = 0
    !! Number of spatial dimensions
  INTEGER(I4B) :: interpolationPointType = Equidistance
    !! Interpolation point
    !! Equidistance
    !! GaussLegendre
    !! GaussLobatto
    !! Chebyshev
  TYPE(ReferenceTopology_), ALLOCATABLE :: topology(:)
    !! Topology information of 0D, 1, 2, 3D entities
  REAL(DFP), ALLOCATABLE :: xiJ(:, :)
    !! Node coord
    !! Rows represents the spatial components
    !! Columns represents the node number
  PROCEDURE(highorder_refelem), POINTER, PASS(obj) ::  &
    & highOrderElement => NULL()
    !! Routine to generate hgher order LagrangeElement
END TYPE ReferenceElement_

TYPE :: ReferenceElementPointer_
  CLASS(ReferenceElement_), POINTER :: ptr => NULL()
END TYPE ReferenceElementPointer_

INTERFACE
  SUBROUTINE highorder_refelem(obj, order, highOrderobj, ipType)
    IMPORT :: ReferenceElement_, I4B
    CLASS(ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: order
    CLASS(ReferenceElement_), INTENT(INOUT) :: highOrderobj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE highorder_refelem
END INTERFACE

!----------------------------------------------------------------------------
!                                                          ReferencePoint_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2021
! summary: This data type defines a reference point element
!
!{!pages/docs-api/ReferencePoint/ReferencePoint_.md!}

TYPE, EXTENDS(ReferenceElement_) :: ReferencePoint_
END TYPE ReferencePoint_

TYPE(ReferencePoint_), PARAMETER :: &
  & TypeReferencePoint = ReferencePoint_( &
  & XiJ=NULL(), EntityCounts=[0, 0, 0, 0],  &
  & xiDimension=0, name=0, &
  & Topology=NULL(), Order=0, NSD=0)

!----------------------------------------------------------------------------
!                                                          ReferenceLine_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2021
! summary: This data type defines a reference line
!
!{!pages/docs-api/ReferenceLine/ReferenceLine_.md!}

TYPE, EXTENDS(ReferenceElement_) :: ReferenceLine_
END TYPE ReferenceLine_

TYPE(ReferenceLine_), PARAMETER :: &
  & TypeReferenceLine = ReferenceLine_( &
  & XiJ=NULL(), EntityCounts=[0, 0, 0, 0],  &
  & xiDimension=0, name=0, &
  & Topology=NULL(), Order=0, NSD=0)

!----------------------------------------------------------------------------
!                                                      ReferenceTriangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2021
! summary: This data type defines a reference triangle
!
!{!pages/ReferenceTriangle.md}
TYPE, EXTENDS(ReferenceElement_) :: ReferenceTriangle_
END TYPE ReferenceTriangle_

TYPE(ReferenceTriangle_), PARAMETER :: &
  & TypeReferenceTriangle = ReferenceTriangle_( &
  & XiJ=NULL(), EntityCounts=[0, 0, 0, 0],  &
  & xiDimension=0, name=0, &
  & Topology=NULL(), Order=0, NSD=0)

!----------------------------------------------------------------------------
!                                                       ReferenceQuadrangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2021
! summary: This data type defines a reference quadrangle
!
!{!pages/ReferenceQuadrangle/ReferenceQuadrangle_.md!}

TYPE, EXTENDS(ReferenceElement_) :: ReferenceQuadrangle_
END TYPE ReferenceQuadrangle_

TYPE(ReferenceQuadrangle_), PARAMETER :: &
  & TypeReferenceQuadrangle &
  & = ReferenceQuadrangle_( &
  & XiJ=NULL(), EntityCounts=[0, 0, 0, 0],  &
  & xiDimension=0, name=0, &
  & Topology=NULL(), Order=0, NSD=0)

!----------------------------------------------------------------------------
!                                                    ReferenceTetrahedron_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2021
! summary: This data type defines a reference tetrahedron
!
!{!pages/ReferenceTetrahedron/ReferenceTetrahedron_.md!}

TYPE, EXTENDS(ReferenceElement_) :: ReferenceTetrahedron_
END TYPE ReferenceTetrahedron_

TYPE(ReferenceTetrahedron_), PARAMETER :: &
  & TypeReferenceTetrahedron &
  & = ReferenceTetrahedron_( &
  & XiJ=NULL(), EntityCounts=[0, 0, 0, 0], xiDimension=0, name=0, &
  & Topology=NULL(), Order=0, NSD=0)

!----------------------------------------------------------------------------
!                                                     ReferenceHexahedron_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2021
! summary: This data type defines a reference hexahedron
!
!{!pages/docs-api/ReferenceHexahedron/ReferenceHexahedron_.md}

TYPE, EXTENDS(ReferenceElement_) :: ReferenceHexahedron_
END TYPE ReferenceHexahedron_

TYPE(ReferenceHexahedron_), PARAMETER :: &
  & TypeReferenceHexahedron &
  & = ReferenceHexahedron_( &
  & XiJ=NULL(), &
  & EntityCounts=[0, 0, 0, 0], &
  & xiDimension=0, &
  & name=0, &
  & Topology=NULL(), &
  & Order=0, &
  & NSD=0)

!----------------------------------------------------------------------------
!                                                            ReferencePrism_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2021
! summary: This data type defines a reference prism
!
!{!pages/ReferencePrism.md}

TYPE, EXTENDS(ReferenceElement_) :: ReferencePrism_
END TYPE ReferencePrism_

TYPE(ReferencePrism_), PARAMETER :: TypeReferencePrism &
  & = ReferencePrism_( &
  & XiJ=NULL(), &
  & EntityCounts=[0, 0, 0, 0], &
  & xiDimension=0, &
  & name=0, &
  & Topology=NULL(), &
  & Order=0, &
  & NSD=0)

!----------------------------------------------------------------------------
!                                                          ReferencePyramid_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2021
! summary: This data type defines a reference pyramid
!
!{!pages/ReferencePyramid.md}

TYPE, EXTENDS(ReferenceElement_) :: ReferencePyramid_
END TYPE ReferencePyramid_

TYPE(ReferencePyramid_), PARAMETER :: TypeReferencePyramid &
  & = ReferencePyramid_( &
  & XiJ=NULL(), &
  & EntityCounts=[0, 0, 0, 0], &
  & xiDimension=0, &
  & name=0, &
  & Topology=NULL(), &
  & Order=0, &
  & NSD=0)

!----------------------------------------------------------------------------
!                                                                 KeyValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 June 2022
! summary: [[keyvalue_]] is a poor implementation of dict

TYPE :: KeyValue_
  INTEGER(I4B) :: DataType = 0
  TYPE(String) :: Key
  REAL(DFP), ALLOCATABLE :: VALUE(:, :)
END TYPE KeyValue_

TYPE(KeyValue_), PARAMETER :: TypeKeyValue = KeyValue_(VALUE=NULL())

!----------------------------------------------------------------------------
!                                                                FEVariable_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 June 2022
! summary: Finite element variable
!
! {!pages/FEVariable_.md!}

TYPE :: FEVariable_
  REAL(DFP), ALLOCATABLE :: val(:)
  !! values
  INTEGER(I4B) :: s(MAX_RANK_FEVARIABLE) = 0
  !! shape of the data
  INTEGER(I4B) :: defineOn = 0
  !! Nodal: nodal values
  !! Quadrature: quadrature values
  INTEGER(I4B) :: varType = 0
  !! Space
  !! Time
  !! SpaceTime
  !! Constant
  INTEGER(I4B) :: rank = 0
  !! Scalar
  !! Vector
  !! Matrix
  INTEGER(I4B) :: len = 0_I4B
  !! current total size
  INTEGER(I4B) :: capacity = 0_I4B
  !! capacity of the val
END TYPE FEVariable_

TYPE(FEVariable_), PARAMETER :: TypeFEVariable = FEVariable_(val=NULL())

!----------------------------------------------------------------------------
!                                                         FEVariableConstant_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: FEVariable Constant

TYPE :: FEVariableConstant_
!! INTEGER(I4B):: Val = 1
END TYPE FEVariableConstant_

TYPE(FEVariableConstant_), PARAMETER :: TypeFEVariableConstant = &
  & FEVariableConstant_()

TYPE(FEVariableConstant_), PARAMETER :: TypeVariableConstant = &
  & FEVariableConstant_()

!----------------------------------------------------------------------------
!                                                           FEVariableSpace_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: FEVariable Space
!
TYPE :: FEVariableSpace_
!! INTEGER(I4B):: Val = 2
END TYPE FEVariableSpace_

TYPE(FEVariableSpace_), PARAMETER :: TypeFEVariableSpace = &
  & FEVariableSpace_()
TYPE(FEVariableSpace_), PARAMETER :: TypeVariableSpace = &
  & FEVariableSpace_()

!----------------------------------------------------------------------------
!                                                       FEVariableSpaceTime_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: FEVariable Space time

TYPE :: FEVariableSpaceTime_
!! INTEGER(I4B):: Val = 3
END TYPE FEVariableSpaceTime_

TYPE(FEVariableSpaceTime_), PARAMETER :: TypeFEVariableSpaceTime  &
  & = FEVariableSpaceTime_()
TYPE(FEVariableSpaceTime_), PARAMETER :: TypeVariableSpaceTime  &
  & = FEVariableSpaceTime_()

!----------------------------------------------------------------------------
!                                                            FEVariableTime_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: FEVariable time

TYPE :: FEVariableTime_
!! INTEGER(I4B):: Val = 4
END TYPE FEVariableTime_

TYPE(FEVariableTime_), PARAMETER :: TypeFEVariableTime = FEVariableTime_()
TYPE(FEVariableTime_), PARAMETER :: TypeVariableTime = FEVariableTime_()

!----------------------------------------------------------------------------
!                                                          FEVariableScalar_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: FEVariable scalar

TYPE :: FEVariableScalar_
!!  INTEGER(I4B):: Val = 0
END TYPE FEVariableScalar_

TYPE(FEVariableScalar_), PARAMETER :: TypeFEVariableScalar  &
  & = FEVariableScalar_()

TYPE(FEVariableScalar_), PARAMETER :: TypeVariableScalar  &
  & = FEVariableScalar_()

!----------------------------------------------------------------------------
!                                                          FEVariableVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: FEVariable vector

TYPE :: FEVariableVector_
!!  INTEGER(I4B):: Val = 1
END TYPE FEVariableVector_

TYPE(FEVariableVector_), PARAMETER :: TypeFEVariableVector  &
  & = FEVariableVector_()

TYPE(FEVariableVector_), PARAMETER :: TypeVariableVector  &
  & = FEVariableVector_()

!----------------------------------------------------------------------------
!                                                          FEVariableMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: FEVariable matrix

TYPE :: FEVariableMatrix_
!!  INTEGER(I4B):: Val = 2
END TYPE FEVariableMatrix_

TYPE(FEVariableMatrix_), PARAMETER :: TypeFEVariableMatrix  &
  & = FEVariableMatrix_()
TYPE(FEVariableMatrix_), PARAMETER :: TypeVariableMatrix  &
  & = FEVariableMatrix_()

!----------------------------------------------------------------------------
!                                                           QuadraturePoint_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Quadrature points for numerical integration
!
!{!pages/docs-api/QuadraturePoint/QuadraturePoint_.md!}

TYPE :: QuadraturePoint_
  REAL(DFP), ALLOCATABLE :: points(:, :)
  INTEGER(I4B) :: txi = 0
END TYPE QuadraturePoint_

TYPE(QuadraturePoint_), PARAMETER :: TypeQuadraturePoint  &
  & = QuadraturePoint_(points=NULL())

TYPE :: QuadraturePointPointer_
  CLASS(QuadraturePoint_), POINTER :: ptr => NULL()
END TYPE QuadraturePointPointer_

!----------------------------------------------------------------------------
!                                                         BasisInterpolation_
!----------------------------------------------------------------------------

TYPE :: BaseInterpolation_
END TYPE BaseInterpolation_

!----------------------------------------------------------------------------
!                                                     LagrangeInterpolation_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary: Lagrange basis functions

TYPE, EXTENDS(BaseInterpolation_) :: LagrangeInterpolation_
END TYPE LagrangeInterpolation_

TYPE(LagrangeInterpolation_), PARAMETER :: TypeLagrangeInterpolation  &
  & = LagrangeInterpolation_()

!----------------------------------------------------------------------------
!                                                       HermitInterpolation_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary: Hermit basis functions

TYPE, EXTENDS(BaseInterpolation_) :: HermitInterpolation_
END TYPE HermitInterpolation_

TYPE(HermitInterpolation_), PARAMETER :: TypeHermitInterpolation  &
  & = HermitInterpolation_()

!----------------------------------------------------------------------------
!                                                  SerendipityInterpolation_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary: Serendipity basis functions

TYPE, EXTENDS(BaseInterpolation_) :: SerendipityInterpolation_
END TYPE SerendipityInterpolation_

TYPE(SerendipityInterpolation_), PARAMETER :: TypeSerendipityInterpolation  &
  & = SerendipityInterpolation_()

!----------------------------------------------------------------------------
!                                                    HierarchyInterpolation_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary: Hierarchical basis functions

TYPE, EXTENDS(BaseInterpolation_) :: HierarchyInterpolation_
END TYPE HierarchyInterpolation_

TYPE(HierarchyInterpolation_), PARAMETER :: TypeHierarchyInterpolation  &
  & = HierarchyInterpolation_()

!----------------------------------------------------------------------------
!                                                    OrthogonalInterpolation_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary: Orthogonal basis functions

TYPE, EXTENDS(BaseInterpolation_) :: OrthogonalInterpolation_
END TYPE OrthogonalInterpolation_

TYPE(OrthogonalInterpolation_), PARAMETER :: TypeOrthogonalInterpolation  &
  & = OrthogonalInterpolation_()

!----------------------------------------------------------------------------
!                                                          BaseContinuity_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-03
! summary:  Continuity of basis functions
!
!# Introduction
!
! `BaseContinuity_` denotes the Continuity or conformity of basis functions.
! Following values are allowed:
!
! - H1_
! - HDIV_
! - HCURL_
! - DG_

TYPE :: BaseContinuity_
END TYPE BaseContinuity_

TYPE(BaseContinuity_), PARAMETER :: TypeBaseContinuity = BaseContinuity_()

!----------------------------------------------------------------------------
!                                                                     H1_
!----------------------------------------------------------------------------

TYPE, EXTENDS(BaseContinuity_) :: H1_
END TYPE H1_

TYPE(H1_), PARAMETER :: TypeH1 = H1_()

!----------------------------------------------------------------------------
!                                                                   H1DIV_
!----------------------------------------------------------------------------

TYPE, EXTENDS(BaseContinuity_) :: HDIV_
END TYPE HDIV_

TYPE(HDIV_), PARAMETER :: TypeHDIV = HDIV_()

!----------------------------------------------------------------------------
!                                                                   HCURL_
!----------------------------------------------------------------------------

TYPE, EXTENDS(BaseContinuity_) :: HCURL_
END TYPE HCURL_

TYPE(HCURL_), PARAMETER :: TypeHCURL = HCURL_()

!----------------------------------------------------------------------------
!                                                                      DG_
!----------------------------------------------------------------------------

TYPE, EXTENDS(BaseContinuity_) :: DG_
END TYPE DG_

TYPE(DG_), PARAMETER :: TypeDG = DG_()

!----------------------------------------------------------------------------
!                                                                 Derivative
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-06
! update: 2021-11-06
! summary: Derivative class contains symbols for derivatives

INTEGER(I4B), PARAMETER :: DEL_NONE = 0
INTEGER(I4B), PARAMETER :: DEL_X = 1
INTEGER(I4B), PARAMETER :: DEL_Y = 2
INTEGER(I4B), PARAMETER :: DEL_Z = 3
INTEGER(I4B), PARAMETER :: DEL_X_ALL = 4
INTEGER(I4B), PARAMETER :: DEL_t = -1

!----------------------------------------------------------------------------
!                                                            ElementData_
!----------------------------------------------------------------------------

TYPE :: ElementData_
  INTEGER(I4B) :: NSD = -1
  INTEGER(I4B) :: NNE = -1
  INTEGER(I4B) :: NNS = -1
  INTEGER(I4B) :: NNT = -1
  INTEGER(I4B) :: xiDimension = -1
  INTEGER(I4B) :: ElemTopology = -1
  INTEGER(I4B) :: SpaceElemTopo = -1
  INTEGER(I4B) :: TimeElemTopo = -1
  INTEGER(I4B) :: ElemType = -1
  INTEGER(I4B) :: MAT_Type = -1
END TYPE ElementData_

TYPE(ElementData_), PARAMETER :: TypeElementData = ElementData_()

TYPE :: ElementDataPointer_
  CLASS(ElementData_), POINTER :: ptr => NULL()
END TYPE ElementDataPointer_

!----------------------------------------------------------------------------
!                                                              ShapeData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 June 2022
! summary: shape function data (deprecated)

TYPE :: ShapeData_
  REAL(DFP) :: Ws = 0.0_DFP
  REAL(DFP) :: Js = 0.0_DFP
  REAL(DFP) :: Thickness = 1.0_DFP
  REAL(DFP) :: Xi(3) = 0.0_DFP
  REAL(DFP) :: XBar(3) = 0.0_DFP
  REAL(DFP) :: Normal(3) = 0.0_DFP
  INTEGER(I4B) :: ElemTopology = 0
  INTEGER(I4B) :: NSD = 0
  REAL(DFP), ALLOCATABLE :: N(:)
  REAL(DFP), ALLOCATABLE :: dNdXi(:, :)
  REAL(DFP), ALLOCATABLE :: dNdXt(:, :)
  REAL(DFP), ALLOCATABLE :: Jacobian(:, :)
END TYPE ShapeData_

TYPE(ShapeData_), PARAMETER :: &
  & TypeShapeData = ShapeData_( &
    & N=NULL(), &
    & dNdXi=NULL(), &
    & dNdXt=NULL(), &
    & Jacobian=NULL())

TYPE :: ShapeDataPointer_
  CLASS(ShapeDataPointer_), POINTER :: ptr => NULL()
END TYPE ShapeDataPointer_

!----------------------------------------------------------------------------
!                                                               STShapeData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 June 2022
! summary: Datatype for space-time shape data

TYPE, EXTENDS(ShapeData_) :: STShapeData_
  REAL(DFP) :: Theta = 0.0
  REAL(DFP) :: Wt = 0.0
  REAL(DFP) :: Jt = 0.0
  INTEGER(I4B) :: SpaceElemTopo = 0
  INTEGER(I4B) :: TimeElemTopo = 0
  REAL(DFP), ALLOCATABLE :: T(:)
    !! values of shape function at different time nodes
  REAL(DFP), ALLOCATABLE :: dTdTheta(:)
    !! Value of local time derivative of T at time gauss point
  REAL(DFP), ALLOCATABLE :: dNTdt(:, :)
    !! Value of global time derivative of T at time gauss points
  REAL(DFP), ALLOCATABLE :: dNTdXt(:, :, :)
    !! Spatial gradient of the shape functions at space-time gauss points
END TYPE STShapeData_

TYPE :: STShapeDataPointer_
  CLASS(STShapeData_), POINTER :: ptr => NULL()
END TYPE STShapeDataPointer_

!----------------------------------------------------------------------------
!                                                           ElemShapeData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: Datatype for data defined at all gauss points of an elements
!
!{!pages/docs-api/ElemShapeData/ElemshapeData_.md!}
!
TYPE :: ElemShapeData_
  REAL(DFP), ALLOCATABLE :: N(:, :)
    !! Shape function value `N(I, ips)`
  REAL(DFP), ALLOCATABLE :: dNdXi(:, :, :)
    !! Local derivative of a shape function
  REAL(DFP), ALLOCATABLE :: jacobian(:, :, :)
    !! Jacobian of mapping `J(:,:,ips)` also $\mathbf{F}_{\Xi x}$
  REAL(DFP), ALLOCATABLE :: js(:)
    !! Determinant of Jacobian at ips
  REAL(DFP), ALLOCATABLE :: ws(:)
    !! Weighting functions
  REAL(DFP), ALLOCATABLE :: dNdXt(:, :, :)
    !! Spatial derivative of shape function
  REAL(DFP), ALLOCATABLE :: thickness(:)
    !! Thickness of element
  REAL(DFP), ALLOCATABLE :: coord(:, :)
    !! Barycentric coordinate
  REAL(DFP), ALLOCATABLE :: normal(:, :)
    !! Normal in case of facet element
  TYPE(ReferenceElement_) :: refelem
    !! Refererece element
  TYPE(QuadraturePoint_) :: quad
    !! Quadrature points
END TYPE ElemShapeData_

TYPE(ElemShapeData_), PARAMETER :: &
  & TypeElemShapeData = ElemShapeData_( &
  & N=NULL(), &
  & dNdXi=NULL(), &
  & Jacobian=NULL(), &
  & Js=NULL(), &
  & Ws=NULL(), &
  & dNdXt=NULL(), &
  & Thickness=NULL(), &
  & Coord=NULL(), &
  & Normal=NULL())

TYPE :: ElemShapeDataPointer_
  CLASS(ShapeDataPointer_), POINTER :: ptr => NULL()
END TYPE ElemShapeDataPointer_

!----------------------------------------------------------------------------
!                                                           STElemShapeData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-08
! update: 2021-12-08
! summary: Space-time shape function data

TYPE, EXTENDS(ElemShapeData_) :: STElemShapeData_
  REAL(DFP) :: wt = 0.0
    !! Weight of gauss point in time domain
  REAL(DFP) :: theta = 0.0
    !! Gauss point in time domain
  REAL(DFP) :: jt = 0.0
    !! Jacobian $\frac{dt}{d\theta}$
  REAL(DFP), ALLOCATABLE :: T(:)
    !! Shape function in time domain
  REAL(DFP), ALLOCATABLE :: dTdTheta(:)
    !! Local shape function derivative in time domain
  REAL(DFP), ALLOCATABLE :: dNTdt(:, :, :)
  REAL(DFP), ALLOCATABLE :: dNTdXt(:, :, :, :)
    !! (I, a, i, ips)
END TYPE STElemShapeData_

TYPE(STElemShapeData_), PARAMETER :: &
  & TypeSTElemShapeData = STElemShapeData_( &
  & N=NULL(), &
  & dNdXi=NULL(), &
  & Jacobian=NULL(), &
  & Js=NULL(), &
  & Ws=NULL(), &
  & dNdXt=NULL(), &
  & Thickness=NULL(), &
  & Coord=NULL(), &
  & Normal=NULL(), &
  & T=NULL(), &
  & dTdTheta=NULL(), &
  & dNTdt=NULL(), &
  & dNTdXt=NULL())

!----------------------------------------------------------------------------
!                                                              Meshquality_
!----------------------------------------------------------------------------

TYPE :: QualityMeasure_
  INTEGER(I4B), PUBLIC :: area = 100
  INTEGER(I4B), PUBLIC :: maxAngle = 101
  INTEGER(I4B), PUBLIC :: minAngle = 102
  INTEGER(I4B), PUBLIC :: AngleRatio = 103
  INTEGER(I4B), PUBLIC :: RadiusRatio = 104
  INTEGER(I4B), PUBLIC :: EdgeRatio = 105
  INTEGER(I4B), PUBLIC :: AspectRatio = 106
  INTEGER(I4B), PUBLIC :: ScaledJacobian = 107
  INTEGER(I4B), PUBLIC :: Default = 106
END TYPE QualityMeasure_

TYPE(QualityMeasure_), PARAMETER :: QualityMeasure = QualityMeasure_()

!----------------------------------------------------------------------------
!                                                                    Random_
!----------------------------------------------------------------------------

TYPE :: Random_
  INTEGER(I4B) :: random_int = 100
  INTEGER(I4B), ALLOCATABLE :: random_int_seed(:)
  INTEGER(I4B), ALLOCATABLE :: random_int_vec(:)
  REAL(DFP) :: random_real = 0.0_DFP
  REAL(DFP), ALLOCATABLE :: random_real_vec(:)
END TYPE

TYPE(Random_), PARAMETER :: &
  & TypeRandom = Random_(random_int_seed=NULL(), &
  & random_int_vec=NULL(), &
  & random_real_vec=NULL())

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 March 2021
! summary: OpenMP and EASIFEM
TYPE :: OpenMP_
  INTEGER(I4B) :: Rank = 0
  INTEGER(I4B) :: NUM_THREADS = 1
  INTEGER(I4B) :: MAX_THREADS = 1
  INTEGER(I4B) :: STATE = OMP_THREADS_JOINED
  LOGICAL(LGT) :: IS_INIT = .FALSE.
  LOGICAL(LGT) :: DID_I_INIT = .FALSE.
END TYPE OpenMP_

TYPE(OpenMP_), PARAMETER :: TypeOpenMP = OpenMP_()
TYPE(OpenMP_) :: OMP
!$OMP THREADPRIVATE( OMP )

!----------------------------------------------------------------------------
!                                                              MultiIndices_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Multi-indices object is definedstringclass

TYPE :: MultiIndices_
  INTEGER(I4B) :: d
  !! dimension of simplex
  INTEGER(I4B) :: n
  !! order
END TYPE MultiIndices_

!----------------------------------------------------------------------------
!                                                                   Range_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-09
! summary:  Range denotes the triplet index

TYPE :: Range_
  INTEGER(I4B) :: is = 0_I4B
  !! istart
  INTEGER(I4B) :: ie = 0_I4B
  !! iend
  INTEGER(I4B) :: ic = 1_I4B
  !! increment
END TYPE Range_

!----------------------------------------------------------------------------
!                                                                Interval1D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-09
! summary:  Interval1D_ denotes the 1d interval

TYPE :: Interval1D_
  REAL(DFP) :: lower
!! lower limit
  REAL(DFP) :: upper
!! upper limit
END TYPE Interval1D_

!----------------------------------------------------------------------------
!                                                         SpaceTimeFunction
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_SpaceTimeFunction(x, t) RESULT(ans)
    IMPORT :: DFP
    ! CLASS( DirichletBC_ ), INTENT( IN ):: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: t
    REAL(DFP) :: ans
  END FUNCTION iface_SpaceTimeFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                              SpaceFunction
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_SpaceFunction(x) RESULT(ans)
    IMPORT :: DFP
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans
  END FUNCTION iface_SpaceFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                              TimeFunction
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_TimeFunction(t) RESULT(ans)
    IMPORT :: DFP
    REAL(DFP), INTENT(IN) :: t
    REAL(DFP) :: ans
  END FUNCTION iface_TimeFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                               1DFunction
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_1DFunction(x) RESULT(ans)
    IMPORT :: DFP
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION iface_1DFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                               2DFunction
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_2DFunction(x, y) RESULT(ans)
    IMPORT :: DFP
    REAL(DFP), INTENT(IN) :: x, y
    REAL(DFP) :: ans
  END FUNCTION iface_2DFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                               3DFunction
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_3DFunction(x, y, z) RESULT(ans)
    IMPORT :: DFP
    REAL(DFP), INTENT(IN) :: x, y, z
    REAL(DFP) :: ans
  END FUNCTION iface_3DFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                             ScalarFunction
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_ScalarFunction(x) RESULT(ans)
    IMPORT :: DFP
    REAL(DFP), OPTIONAL, INTENT(IN) :: x(:)
    REAL(DFP) :: ans
  END FUNCTION iface_ScalarFunction
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_VectorFunction(x) RESULT(ans)
    IMPORT :: DFP
    REAL(DFP), OPTIONAL, INTENT(IN) :: x(:)
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION iface_VectorFunction
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_MatrixFunction(x) RESULT(ans)
    IMPORT :: DFP
    REAL(DFP), OPTIONAL, INTENT(IN) :: x(:)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION iface_MatrixFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                              TypePreconOpt
!----------------------------------------------------------------------------

TYPE :: PrecondOpt_
  INTEGER(I4B) :: NONE = NO_PRECONDITION
  INTEGER(I4B) :: left = LEFT_PRECONDITION
  INTEGER(I4B) :: right = RIGHT_PRECONDITION
  INTEGER(I4B) :: both = LEFT_RIGHT_PRECONDITION
  INTEGER(I4B) :: jacobi = PRECOND_JACOBI
  INTEGER(I4B) :: ilu = PRECOND_ILU
  INTEGER(I4B) :: ssor = PRECOND_SSOR
  INTEGER(I4B) :: hybrid = PRECOND_HYBRID
  INTEGER(I4B) :: is = PRECOND_IS
  INTEGER(I4B) :: sainv = PRECOND_SAINV
  INTEGER(I4B) :: saamg = PRECOND_SAAMG
  INTEGER(I4B) :: iluc = PRECOND_ILUC
  INTEGER(I4B) :: adds = PRECOND_ADDS
  INTEGER(I4B) :: ilutp = PRECOND_ILUTP
  INTEGER(I4B) :: ilud = PRECOND_ILUD
  INTEGER(I4B) :: iludp = PRECOND_ILUDP
  INTEGER(I4B) :: ilu0 = PRECOND_ILU0
  INTEGER(I4B) :: iluk = PRECOND_ILUK
  INTEGER(I4B) :: ilut = PRECOND_ILUT
END TYPE PrecondOpt_

TYPE(PrecondOpt_), PARAMETER :: TypePrecondOpt = PrecondOpt_()

!----------------------------------------------------------------------------
!                                                              TypePreconOpt
!----------------------------------------------------------------------------

TYPE :: ConvergenceOpt_
  INTEGER(I4B) :: res = convergenceInRes
  INTEGER(I4B) :: sol = convergenceInSol
  INTEGER(I4B) :: both = convergenceInResSol
  INTEGER(I4B) :: relative = relativeConvergence
  INTEGER(I4B) :: absolute = absoluteConvergence
END TYPE ConvergenceOpt_

TYPE(ConvergenceOpt_), PARAMETER :: TypeConvergenceOpt = ConvergenceOpt_()

!----------------------------------------------------------------------------
!                                                           SolverNameOpt_
!----------------------------------------------------------------------------

TYPE SolverNameOpt_
  INTEGER(I4B) :: cg = LIS_CG
  INTEGER(I4B) :: bcg = LIS_BCG
  INTEGER(I4B) :: bicg = LIS_BICG
  INTEGER(I4B) :: cgs = LIS_CGS
  INTEGER(I4B) :: bcgstab = LIS_BCGSTAB
  INTEGER(I4B) :: bicgstab = LIS_BICGSTAB
  INTEGER(I4B) :: bicgstabl = LIS_BICGSTABL
  INTEGER(I4B) :: gpbicg = LIS_GPBICG
  INTEGER(I4B) :: tfqmr = LIS_TFQMR
  INTEGER(I4B) :: omn = LIS_OMN
  INTEGER(I4B) :: fom = LIS_FOM
  INTEGER(I4B) :: orthomin = LIS_ORTHOMIN
  INTEGER(I4B) :: gmres = LIS_GMRES
  INTEGER(I4B) :: gmr = LIS_GMR
  INTEGER(I4B) :: jacobi = LIS_JACOBI
  INTEGER(I4B) :: gs = LIS_GS
  INTEGER(I4B) :: sor = LIS_SOR
  INTEGER(I4B) :: bicgsafe = LIS_BICGSAFE
  INTEGER(I4B) :: cr = LIS_CR
  INTEGER(I4B) :: bicr = LIS_BICR
  INTEGER(I4B) :: crs = LIS_CRS
  INTEGER(I4B) :: bicrstab = LIS_BICRSTAB
  INTEGER(I4B) :: gpbicr = LIS_GPBICR
  INTEGER(I4B) :: bicrsafe = LIS_BICRSAFE
  INTEGER(I4B) :: fgmres = LIS_FGMRES
  INTEGER(I4B) :: idrs = LIS_IDRS
  INTEGER(I4B) :: idr1 = LIS_IDR1
  INTEGER(I4B) :: minres = LIS_MINRES
  INTEGER(I4B) :: cocg = LIS_COCG
  INTEGER(I4B) :: cocr = LIS_COCR
  INTEGER(I4B) :: cgnr = LIS_CGNR
  INTEGER(I4B) :: cgn = LIS_CGN
  INTEGER(I4B) :: dbcg = LIS_DBCG
  INTEGER(I4B) :: dbicg = LIS_DBICG
  INTEGER(I4B) :: dqgmres = LIS_DQGMRES
  INTEGER(I4B) :: superlu = LIS_SUPERLU
END TYPE SolverNameOpt_

TYPE(SolverNameOpt_), PARAMETER :: TypeSolverNameOpt = &
                                   SolverNameOpt_()

END MODULE BaseType
