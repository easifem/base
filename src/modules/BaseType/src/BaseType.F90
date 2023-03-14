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

TYPE(Math_), PARAMETER, PUBLIC :: Math = Math_()

!----------------------------------------------------------------------------
!                                                               BoundingBox_
!----------------------------------------------------------------------------

! date: 23 Feb 2021
!> author: Vikas Sharma, Ph. D.
! summary: A data type to represent a bounding box;
!
!{!pages/BoundingBox_.md!}

TYPE :: BoundingBox_
  INTEGER(I4B) :: NSD
    !! Number of spatial dimension
    !! NSD = 1, 2, 3 for 1D, 2D, 3D box
  REAL(DFP) :: Box(2, 3)
    !! Box contains the xmin, ymin, ...
    !! `Box(1:2, 1:3)`  an array containing box coordinates.
    !!- `Box(1:2, 1:3)`  an array containing box coordinates.
    !!- `Box(1, 1)` is x_min
    !!- `Box(2, 1)` is x_max
    !!- `Box(1, 2)` is y_min
    !!- `Box(2, 2)` is y_max
    !!- `Box(1, 3)` is z_min
    !!- `Box(2, 3)` is z_max
END TYPE BoundingBox_

PUBLIC :: BoundingBox_

TYPE(BoundingBox_), PUBLIC, PARAMETER :: TypeBoundingBox = &
  & BoundingBox_(NSD=0, Box=0)
!! A Type Instance of Boundingbox

!> author: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: Contains the pointer to the [[BoundingBox_]] data type.

TYPE :: BoundingBoxPointer_
  CLASS(BoundingBoxPointer_), POINTER :: ptr => NULL()
END TYPE BoundingBoxPointer_

PUBLIC :: BoundingBoxPointer_

!----------------------------------------------------------------------------
!                                                                    Matrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: A data type for an Array of rank 2 of real numbers
!
!{!pages/RealMatrix_.md!}

TYPE :: RealMatrix_
  INTEGER(I4B) :: tDimension = 0_I4B
  CHARACTER(5) :: MatrixProp = 'UNSYM'
  REAL(DFP), ALLOCATABLE :: Val(:, :)
END TYPE RealMatrix_

PUBLIC :: RealMatrix_

TYPE(RealMatrix_), PUBLIC, PARAMETER :: &
  & TypeRealMatrix = RealMatrix_( &
    & Val=NULL())

TYPE :: RealMatrixPointer_
  CLASS(RealMatrix_), POINTER :: ptr => NULL()
END TYPE RealMatrixPointer_

PUBLIC :: RealMatrixPointer_

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

PUBLIC :: IntVector_

TYPE(IntVector_), PUBLIC, PARAMETER :: TypeIntVector = IntVector_(Val=NULL())

TYPE :: IntVectorPointer_
  CLASS(IntVector_), POINTER :: ptr => NULL()
END TYPE IntVectorPointer_

PUBLIC :: IntVectorPointer_

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

PUBLIC :: RealVector_

TYPE(RealVector_), PUBLIC, PARAMETER :: TypeRealVector = RealVector_( &
  & tDimension=1_I4B, Val=NULL())

TYPE :: RealVectorPointer_
  CLASS(RealVector_), POINTER :: ptr => NULL()
END TYPE RealVectorPointer_

PUBLIC :: RealVectorPointer_

!----------------------------------------------------------------------------
!                                                                 Vector3D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! summary: Data type for 3D vectors
! date: 24 Feb 2021

TYPE :: Vector3D_
  INTEGER(I4B) :: tDimension = 1_I4B
  REAL(DFP) :: Val(3)
END TYPE Vector3D_

PUBLIC :: Vector3D_

TYPE(Vector3D_), PUBLIC, PARAMETER :: TypeVector3D = Vector3D_( &
  & tDimension=1_I4B, Val=[0.0_DFP, 0.0_DFP, 0.0_DFP])

!----------------------------------------------------------------------------
!                                                            Vector3DPointer_
!----------------------------------------------------------------------------

TYPE :: Vector3DPointer_
  CLASS(Vector3D_), POINTER :: ptr => NULL()
END TYPE Vector3DPointer_

PUBLIC :: Vector3DPointer_

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

PUBLIC :: IndexValue_

TYPE(IndexValue_), PUBLIC, PARAMETER :: TypeIndexValue = &
  & IndexValue_(Indx=0, Val=0.0_DFP)

TYPE :: IndexValuePointer_
  CLASS(IndexValue_), POINTER :: ptr => NULL()
END TYPE IndexValuePointer_

PUBLIC :: IndexValuePointer_

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

PUBLIC :: DOF_

TYPE(DOF_), PUBLIC, PARAMETER :: TypeDOF = DOF_(MAP=NULL(), ValMap=NULL())

TYPE :: DOFPointer_
  CLASS(DOF_), POINTER :: ptr => NULL()
END TYPE DOFPointer_

PUBLIC :: DOFPointer_

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

PUBLIC :: SparseMatrixReOrdering_

TYPE(SparseMatrixReOrdering_), PUBLIC, PARAMETER :: &
  & TypeSparseMatrixReOrdering = &
  & SparseMatrixReOrdering_(name='', PERM=NULL(), &
  & IPERM=NULL())

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

PUBLIC :: CSRSparsity_

TYPE(CSRSparsity_), PUBLIC, PARAMETER :: TypeCSRSparsity = &
  & CSRSparsity_(IA=NULL(), JA=NULL(), Row=NULL())

TYPE :: CSRSparsityPointer_
  CLASS(CSRSparsity_), POINTER :: ptr => NULL()
END TYPE CSRSparsityPointer_

PUBLIC :: CSRSparsityPointer_

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

PUBLIC :: CSRMatrix_

TYPE(CSRMatrix_), PUBLIC, PARAMETER :: TypeCSRMatrix = CSRMatrix_(&
  & A=NULL(), slu=NULL())

TYPE :: CSRMatrixPointer_
  CLASS(CSRMatrix_), POINTER :: ptr => NULL()
END TYPE CSRMatrixPointer_

PUBLIC :: CSRMatrixPointer_

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

PUBLIC :: IterationData_

TYPE(IterationData_), PUBLIC, PARAMETER :: TypeIterationData = &
  & IterationData_(header=NULL())

TYPE :: IterationDataPointer_
  CLASS(IterationData_), POINTER :: ptr => NULL()
END TYPE IterationDataPointer_

PUBLIC :: IterationDataPointer_

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

PUBLIC :: VoigtRank2Tensor_

TYPE(VoigtRank2Tensor_), PARAMETER, PUBLIC :: &
  & TypeVoigtRank2Tensor = VoigtRank2Tensor_()

TYPE :: VoigtRank2TensorPointer
  CLASS(VoigtRank2Tensor_), POINTER :: ptr => NULL()
END TYPE VoigtRank2TensorPointer

PUBLIC :: VoigtRank2TensorPointer

!----------------------------------------------------------------------------
!                                                                  Tensor_
!----------------------------------------------------------------------------

TYPE :: Tensor_
END TYPE Tensor_

!----------------------------------------------------------------------------
!                                                              Rank2Tensor_
!----------------------------------------------------------------------------

TYPE, EXTENDS(Tensor_) :: Rank2Tensor_
  REAL(DFP) :: T(3, 3) = 0.0_DFP
  LOGICAL(LGT) :: isSym = .FALSE.
END TYPE Rank2Tensor_

PUBLIC :: Rank2Tensor_

TYPE(Rank2Tensor_), PARAMETER, PUBLIC :: &
  & TypeRank2Tensor = Rank2Tensor_()

TYPE :: Rank2TensorPointer_
  CLASS(Rank2Tensor_), POINTER :: ptr => NULL()
END TYPE Rank2TensorPointer_

PUBLIC :: Rank2TensorPointer_

!----------------------------------------------------------------------------
!                                                       DeformationGradient_
!----------------------------------------------------------------------------
TYPE, EXTENDS(Rank2Tensor_) :: DeformationGradient_
END TYPE DeformationGradient_

PUBLIC :: DeformationGradient_

TYPE(DeformationGradient_), PUBLIC, PARAMETER :: &
  & TypeDeformationGradient = DeformationGradient_()

TYPE :: DeformationGradientPointer_
  CLASS(DeformationGradient_), POINTER :: ptr => NULL()
END TYPE DeformationGradientPointer_

PUBLIC :: DeformationGradientPointer_

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
!{!pages/LeftCauchyGreen.md}

TYPE, EXTENDS(Rank2Tensor_) :: LeftCauchyGreen_
END TYPE LeftCauchyGreen_

PUBLIC :: LeftCauchyGreen_

TYPE(LeftCauchyGreen_), PUBLIC, PARAMETER :: &
  & TypeLeftCauchyGreen = LeftCauchyGreen_()

TYPE :: LeftCauchyGreenPointer_
  CLASS(LeftCauchyGreen_), POINTER :: ptr => NULL()
END TYPE LeftCauchyGreenPointer_

PUBLIC :: LeftCauchyGreenPointer_

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

PUBLIC :: RightCauchyGreen_

TYPE(RightCauchyGreen_), PUBLIC, PARAMETER :: &
  & TypeRightCauchyGreen = RightCauchyGreen_()

TYPE :: RightCauchyGreenPointer_
  CLASS(RightCauchyGreen_), POINTER :: ptr => NULL()
END TYPE RightCauchyGreenPointer_

PUBLIC :: RightCauchyGreenPointer_

!----------------------------------------------------------------------------
!                                                                    Strain_
!----------------------------------------------------------------------------

TYPE, EXTENDS(Rank2Tensor_) :: Strain_
END TYPE Strain_

PUBLIC :: Strain_

TYPE(Strain_), PUBLIC, PARAMETER :: &
  & TypeStrain = Strain_()

TYPE :: StrainPointer_
  CLASS(Strain_), POINTER :: ptr => NULL()
END TYPE StrainPointer_

PUBLIC :: StrainPointer_

!----------------------------------------------------------------------------
!                                                             AlmansiStrain_
!----------------------------------------------------------------------------

TYPE, EXTENDS(Strain_) :: AlmansiStrain_
END TYPE AlmansiStrain_

PUBLIC :: AlmansiStrain_

TYPE(AlmansiStrain_), PUBLIC, PARAMETER :: &
  & TypeAlmansiStrain = AlmansiStrain_()

TYPE :: AlmansiStrainPointer_
  CLASS(AlmansiStrain_), POINTER :: ptr => NULL()
END TYPE AlmansiStrainPointer_

PUBLIC :: AlmansiStrainPointer_

!----------------------------------------------------------------------------
!                                                             GreenStrain_
!----------------------------------------------------------------------------

TYPE, EXTENDS(Strain_) :: GreenStrain_
END TYPE GreenStrain_

PUBLIC :: GreenStrain_

TYPE(GreenStrain_), PUBLIC, PARAMETER :: &
  & TypeGreenStrain = GreenStrain_()

TYPE :: GreenStrainPointer_
  CLASS(GreenStrain_), POINTER :: ptr => NULL()
END TYPE GreenStrainPointer_

PUBLIC :: GreenStrainPointer_

!----------------------------------------------------------------------------
!                                                             SmallStrain_
!----------------------------------------------------------------------------

TYPE, EXTENDS(Strain_) :: SmallStrain_
END TYPE SmallStrain_

PUBLIC :: SmallStrain_

TYPE(SmallStrain_), PUBLIC, PARAMETER :: &
  & TypeSmallStrain = SmallStrain_()

TYPE :: SmallStrainPointer_
  CLASS(SmallStrain_), POINTER :: ptr => NULL()
END TYPE SmallStrainPointer_

PUBLIC :: SmallStrainPointer_

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

PUBLIC :: ReferenceTopology_

TYPE :: ReferenceTopologyPointer_
  CLASS(ReferenceTopology_), POINTER :: ptr => NULL()
END TYPE ReferenceTopologyPointer_

PUBLIC :: ReferenceTopologyPointer_

!----------------------------------------------------------------------------
!                                                          ReferenceElement_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 March 2021
! summary: An abstract data type for Reference Element
!
!{!pages/ReferenceElement_.md}

TYPE :: ReferenceElement_
  REAL(DFP), ALLOCATABLE :: xiJ(:, :)
    !! Node coord
  INTEGER(I4B) :: entityCounts(4) = 0
    !! Number of 0D, 1D, 2D, 3D entities
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
    !! Equidistance, GaussLegendre, GaussLobatto, Chebyshev
  TYPE(ReferenceTopology_), ALLOCATABLE :: topology(:)
    !! Topology information of 0D, 1, 2, 3D entities
PROCEDURE(highorder_refelem), POINTER, PASS(obj) :: highOrderElement => NULL()
    !! Routine to generate hgher order LagrangeElement
END TYPE ReferenceElement_

PUBLIC :: ReferenceElement_

TYPE :: ReferenceElementPointer_
  CLASS(ReferenceElement_), POINTER :: ptr => NULL()
END TYPE ReferenceElementPointer_

PUBLIC :: ReferenceElementPointer_

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
!{!pages/ReferencePoint.md}

TYPE, EXTENDS(ReferenceElement_) :: ReferencePoint_
END TYPE ReferencePoint_

PUBLIC :: ReferencePoint_

TYPE(ReferencePoint_), PARAMETER, PUBLIC :: &
  & TypeReferencePoint = ReferencePoint_( &
  & XiJ=NULL(), EntityCounts=[0, 0, 0, 0], xiDimension=0, name=0, &
  & Topology=NULL(), Order=0, NSD=0)

!----------------------------------------------------------------------------
!                                                          ReferenceLine_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2021
! summary: This data type defines a reference line
!
!{!pages/ReferenceLine.md}

TYPE, EXTENDS(ReferenceElement_) :: ReferenceLine_
END TYPE ReferenceLine_

PUBLIC :: ReferenceLine_

TYPE(ReferenceLine_), PARAMETER, PUBLIC :: &
  & TypeReferenceLine = ReferenceLine_( &
  & XiJ=NULL(), EntityCounts=[0, 0, 0, 0], xiDimension=0, name=0, &
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

PUBLIC :: ReferenceTriangle_

TYPE(ReferenceTriangle_), PARAMETER, PUBLIC :: &
  & TypeReferenceTriangle = ReferenceTriangle_( &
  & XiJ=NULL(), EntityCounts=[0, 0, 0, 0], xiDimension=0, name=0, &
  & Topology=NULL(), Order=0, NSD=0)

!----------------------------------------------------------------------------
!                                                       ReferenceQuadrangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2021
! summary: This data type defines a reference quadrangle
!
!{!pages/ReferenceQuadrangle.md}

TYPE, EXTENDS(ReferenceElement_) :: ReferenceQuadrangle_
END TYPE ReferenceQuadrangle_

PUBLIC :: ReferenceQuadrangle_

TYPE(ReferenceQuadrangle_), PARAMETER, PUBLIC :: &
  & TypeReferenceQuadrangle &
  & = ReferenceQuadrangle_( &
  & XiJ=NULL(), EntityCounts=[0, 0, 0, 0], xiDimension=0, name=0, &
  & Topology=NULL(), Order=0, NSD=0)

!----------------------------------------------------------------------------
!                                                    ReferenceTetrahedron_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2021
! summary: This data type defines a reference tetrahedron
!
!{!pages/ReferenceTetrahedron.md}

TYPE, EXTENDS(ReferenceElement_) :: ReferenceTetrahedron_
END TYPE ReferenceTetrahedron_

PUBLIC :: ReferenceTetrahedron_

TYPE(ReferenceTetrahedron_), PARAMETER, PUBLIC :: &
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
!{!pages/ReferenceHexahedron.md}

TYPE, EXTENDS(ReferenceElement_) :: ReferenceHexahedron_
END TYPE ReferenceHexahedron_

PUBLIC :: ReferenceHexahedron_

TYPE(ReferenceHexahedron_), PARAMETER, PUBLIC :: &
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

PUBLIC :: ReferencePrism_

TYPE(ReferencePrism_), PARAMETER, PUBLIC :: TypeReferencePrism &
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

PUBLIC :: ReferencePyramid_

TYPE(ReferencePyramid_), PARAMETER, PUBLIC :: TypeReferencePyramid &
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

PUBLIC :: KeyValue_

TYPE(KeyValue_), PARAMETER, PUBLIC :: TypeKeyValue = &
  & KeyValue_(VALUE=NULL())

!----------------------------------------------------------------------------
!                                                                FEVariable_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 June 2022
! summary: Finite element variable
!
! {!pages/FEVariable_.md!}

INTEGER(I4B), PARAMETER, PUBLIC :: MAX_RANK_FEVARIABLE = 6

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
END TYPE FEVariable_

PUBLIC :: FEVariable_

TYPE(FEVariable_), PARAMETER, PUBLIC:: TypeFEVariable = FEVariable_(val = NULL())

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

PUBLIC :: FEVariableConstant_

TYPE(FEVariableConstant_), PARAMETER, PUBLIC :: TypeFEVariableConstant = &
  & FEVariableConstant_()

TYPE(FEVariableConstant_), PARAMETER, PUBLIC :: TypeVariableConstant = &
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

PUBLIC :: FEVariableSpace_

TYPE(FEVariableSpace_), PARAMETER, PUBLIC :: TypeFEVariableSpace = &
  & FEVariableSpace_()
TYPE(FEVariableSpace_), PARAMETER, PUBLIC :: TypeVariableSpace = &
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

PUBLIC :: FEVariableSpaceTime_

TYPE(FEVariableSpaceTime_), PARAMETER, PUBLIC :: &
  & TypeFEVariableSpaceTime = FEVariableSpaceTime_()
TYPE(FEVariableSpaceTime_), PARAMETER, PUBLIC :: &
  & TypeVariableSpaceTime = FEVariableSpaceTime_()

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

PUBLIC :: FEVariableTime_

TYPE(FEVariableTime_), PARAMETER, PUBLIC :: TypeFEVariableTime = &
  & FEVariableTime_()

TYPE(FEVariableTime_), PARAMETER, PUBLIC :: TypeVariableTime = &
  & FEVariableTime_()

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

PUBLIC :: FEVariableScalar_

TYPE(FEVariableScalar_), PARAMETER, PUBLIC :: &
  & TypeFEVariableScalar = FEVariableScalar_()

TYPE(FEVariableScalar_), PARAMETER, PUBLIC :: &
  & TypeVariableScalar = FEVariableScalar_()

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

PUBLIC :: FEVariableVector_

TYPE(FEVariableVector_), PARAMETER, PUBLIC :: &
  & TypeFEVariableVector = FEVariableVector_()

TYPE(FEVariableVector_), PARAMETER, PUBLIC :: &
  & TypeVariableVector = FEVariableVector_()

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

PUBLIC :: FEVariableMatrix_

TYPE(FEVariableMatrix_), PARAMETER, PUBLIC :: &
  & TypeFEVariableMatrix = FEVariableMatrix_()
TYPE(FEVariableMatrix_), PARAMETER, PUBLIC :: &
  & TypeVariableMatrix = FEVariableMatrix_()

!----------------------------------------------------------------------------
!                                                           QuadraturePoint_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! update: 2021-12-10
! summary: Quadrature points for numerical integration
!
!{!pages/QuadraturePoint_.md!}

TYPE :: QuadraturePoint_
  REAL(DFP), ALLOCATABLE :: points(:, :)
  INTEGER(I4B) :: txi = 0
END TYPE QuadraturePoint_

PUBLIC :: QuadraturePoint_

TYPE(QuadraturePoint_), PUBLIC, PARAMETER :: &
  & TypeQuadraturePoint = QuadraturePoint_(points=NULL())

TYPE :: QuadraturePointPointer_
  CLASS(QuadraturePoint_), POINTER :: ptr => NULL()
END TYPE QuadraturePointPointer_

PUBLIC :: QuadraturePointPointer_

!----------------------------------------------------------------------------
!                                                         BasisInterpolation_
!----------------------------------------------------------------------------

TYPE :: BaseInterpolation_
END TYPE BaseInterpolation_

PUBLIC :: BaseInterpolation_

!----------------------------------------------------------------------------
!                                                     LagrangeInterpolation_
!----------------------------------------------------------------------------

TYPE, EXTENDS(BaseInterpolation_) :: LagrangeInterpolation_
END TYPE LagrangeInterpolation_

PUBLIC :: LagrangeInterpolation_

TYPE(LagrangeInterpolation_), PARAMETER, PUBLIC :: &
  & TypeLagrangeInterpolation = LagrangeInterpolation_()

!----------------------------------------------------------------------------
!                                                       HermitInterpolation_
!----------------------------------------------------------------------------

TYPE, EXTENDS(BaseInterpolation_) :: HermitInterpolation_
END TYPE HermitInterpolation_

PUBLIC :: HermitInterpolation_

TYPE(HermitInterpolation_), PARAMETER, PUBLIC :: &
  & TypeHermitInterpolation = HermitInterpolation_()

!----------------------------------------------------------------------------
!                                                  SerendipityInterpolation_
!----------------------------------------------------------------------------

TYPE, EXTENDS(BaseInterpolation_) :: SerendipityInterpolation_
END TYPE SerendipityInterpolation_

PUBLIC :: SerendipityInterpolation_

TYPE(SerendipityInterpolation_), PARAMETER, PUBLIC :: &
  & TypeSerendipityInterpolation = SerendipityInterpolation_()

!----------------------------------------------------------------------------
!                                                    HierarchyInterpolation_
!----------------------------------------------------------------------------

TYPE, EXTENDS(BaseInterpolation_) :: HierarchyInterpolation_
END TYPE HierarchyInterpolation_

PUBLIC :: HierarchyInterpolation_

TYPE(HierarchyInterpolation_), PARAMETER, PUBLIC :: &
  & TypeHierarchyInterpolation = HierarchyInterpolation_()

!----------------------------------------------------------------------------
!                                                          BaseContinuity_
!----------------------------------------------------------------------------

TYPE :: BaseContinuity_
END TYPE BaseContinuity_

PUBLIC :: BaseContinuity_

TYPE(BaseContinuity_), PARAMETER, PUBLIC :: &
  & TypeBaseContinuity = BaseContinuity_()

!----------------------------------------------------------------------------
!                                                                     H1_
!----------------------------------------------------------------------------

TYPE, EXTENDS(BaseContinuity_) :: H1_
END TYPE H1_

PUBLIC :: H1_

TYPE(H1_), PARAMETER, PUBLIC :: &
  & TypeH1 = H1_()

!----------------------------------------------------------------------------
!                                                                   H1DIV_
!----------------------------------------------------------------------------

TYPE, EXTENDS(BaseContinuity_) :: H1DIV_
END TYPE H1DIV_

PUBLIC :: H1DIV_

TYPE(H1DIV_), PARAMETER, PUBLIC :: &
  & TypeH1DIV = H1DIV_()

!----------------------------------------------------------------------------
!                                                                   H1CURL_
!----------------------------------------------------------------------------

TYPE, EXTENDS(BaseContinuity_) :: H1CURL_
END TYPE H1CURL_

PUBLIC :: H1CURL_

TYPE(H1CURL_), PARAMETER, PUBLIC :: &
  & TypeH1CURL = H1CURL_()

!----------------------------------------------------------------------------
!                                                                      DG_
!----------------------------------------------------------------------------

TYPE, EXTENDS(BaseContinuity_) :: DG_
END TYPE DG_

PUBLIC :: DG_

TYPE(DG_), PARAMETER, PUBLIC :: &
  & TypeDG = DG_()

!----------------------------------------------------------------------------
!                                                                 Derivative
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-06
! update: 2021-11-06
! summary: Derivative class contains symbols for derivatives

INTEGER(I4B), PARAMETER, PUBLIC :: DEL_NONE = 0
INTEGER(I4B), PARAMETER, PUBLIC :: DEL_X = 1
INTEGER(I4B), PARAMETER, PUBLIC :: DEL_Y = 2
INTEGER(I4B), PARAMETER, PUBLIC :: DEL_Z = 3
INTEGER(I4B), PARAMETER, PUBLIC :: DEL_X_ALL = 4
!!
INTEGER(I4B), PARAMETER, PUBLIC :: DEL_t = -1

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

PUBLIC :: ElementData_

TYPE(ElementData_), PARAMETER, PUBLIC :: TypeElementData = ElementData_()

TYPE :: ElementDataPointer_
  CLASS(ElementData_), POINTER :: ptr => NULL()
END TYPE ElementDataPointer_

PUBLIC :: ElementDataPointer_

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

PUBLIC :: ShapeData_

TYPE(ShapeData_), PARAMETER, PUBLIC :: &
  & TypeShapeData = ShapeData_( &
    & N=NULL(), &
    & dNdXi=NULL(), &
    & dNdXt=NULL(), &
    & Jacobian=NULL())

TYPE :: ShapeDataPointer_
  CLASS(ShapeDataPointer_), POINTER :: ptr => NULL()
END TYPE ShapeDataPointer_

PUBLIC :: ShapeDataPointer_

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

PUBLIC :: STShapeData_

TYPE :: STShapeDataPointer_
  CLASS(STShapeData_), POINTER :: ptr => NULL()
END TYPE STShapeDataPointer_

PUBLIC :: STShapeDataPointer_

!----------------------------------------------------------------------------
!                                                           ElemShapeData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: Datatype for data defined at all gauss points of an elements
!
!{!pages/ElemshapeData_.md!}
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

PUBLIC :: ElemShapeData_

TYPE(ElemShapeData_), PUBLIC, PARAMETER :: &
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

PUBLIC :: ElemShapeDataPointer_

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

PUBLIC :: STElemShapeData_
TYPE(STElemShapeData_), PUBLIC, PARAMETER :: &
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

TYPE(QualityMeasure_), PARAMETER, PUBLIC :: &
  & QualityMeasure = QualityMeasure_()

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

PUBLIC :: Random_

TYPE(Random_), PARAMETER, PUBLIC :: &
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

TYPE(OpenMP_), PARAMETER, PUBLIC :: TypeOpenMP = OpenMP_()
TYPE(OpenMP_), PUBLIC :: OMP
!$OMP THREADPRIVATE( OMP )

!----------------------------------------------------------------------------
!                                                       Function Inerfaces
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

PUBLIC :: iface_SpaceTimeFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_SpaceFunction(x) RESULT(ans)
    IMPORT :: DFP
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans
  END FUNCTION iface_SpaceFunction
END INTERFACE

PUBLIC :: iface_SpaceFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_TimeFunction(t) RESULT(ans)
    IMPORT :: DFP
    REAL(DFP), INTENT(IN) :: t
    REAL(DFP) :: ans
  END FUNCTION iface_TimeFunction
END INTERFACE

PUBLIC :: iface_TimeFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_1DFunction(x) RESULT(ans)
    IMPORT :: DFP
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION iface_1DFunction
END INTERFACE

PUBLIC :: iface_1DFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_2DFunction(x, y) RESULT(ans)
    IMPORT :: DFP
    REAL(DFP), INTENT(IN) :: x, y
    REAL(DFP) :: ans
  END FUNCTION iface_2DFunction
END INTERFACE

PUBLIC :: iface_2DFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION iface_3DFunction(x, y, z) RESULT(ans)
    IMPORT :: DFP
    REAL(DFP), INTENT(IN) :: x, y, z
    REAL(DFP) :: ans
  END FUNCTION iface_3DFunction
END INTERFACE

PUBLIC :: iface_3DFunction

!----------------------------------------------------------------------------
!                                                              MultiIndices_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Multi-indices object is defined

TYPE :: MultiIndices_
  INTEGER(I4B) :: d
  !! dimension of simplex
  INTEGER(I4B) :: n
  !! order
END TYPE MultiIndices_

PUBLIC :: MultiIndices_

END MODULE BaseType
