MODULE BaseType
USE GlobalData
USE StringiFor, ONLY: string, adjustl, adjustr, count, index, len_trim, &
  & repeat, scan, trim, verify, read_file, read_lines, write_file, &
  & write_lines

USE PENF, ONLY: STR

IMPLICIT NONE
PRIVATE

PUBLIC :: string, adjustl, adjustr, count, index, len_trim, &
  & repeat, scan, trim, verify, read_file, read_lines, write_file, &
  & write_lines

PUBLIC :: STR

TYPE( String ), PARAMETER, PUBLIC :: TypeString = String( raw = NULL() )

!----------------------------------------------------------------------------
!                                                              StringPointer_
!----------------------------------------------------------------------------

TYPE :: StringPointer_
  TYPE( String ), POINTER :: Ptr => NULL ()
END TYPE StringPointer_

PUBLIC :: StringPointer_

!----------------------------------------------------------------------------
!                                                                   Buffer_
!----------------------------------------------------------------------------

TYPE :: Buffer_
  TYPE( StringPointer_ ), ALLOCATABLE :: Line( : )
  INTEGER( I4B ) :: tLine = 0
END TYPE Buffer_

PUBLIC :: Buffer_

!----------------------------------------------------------------------------
!                                                                      File_
!----------------------------------------------------------------------------

TYPE :: File_
  TYPE( String ) :: FileName, Path, Extension, ACTION, STATUS, ACCESS
  INTEGER( I4B ) :: UnitNo, WriteNo, IOSTAT
  LOGICAL( LGT ) :: isOpen
END TYPE File_

PUBLIC :: File_

TYPE( File_ ), PUBLIC, PARAMETER :: TypeFile = &
  & File_( FileName = TypeString, Path = TypeString, Extension = TypeString, &
  & ACTION = TypeString, STATUS = TypeString, ACCESS = TypeString, &
  & UnitNo = -1, WriteNo = -1, IOSTAT = -1, isOpen = .FALSE. )

TYPE :: FilePointer_
  CLASS( File_ ), POINTER :: Ptr => NULL( )
END TYPE FilePointer_

PUBLIC :: FilePointer_

!----------------------------------------------------------------------------
!                                                               BoundingBox_
!----------------------------------------------------------------------------

TYPE :: BoundingBox_
  INTEGER( I4B ) :: NSD
  REAL( DFP ) :: Box( 2, 3 )
END TYPE BoundingBox_

PUBLIC :: BoundingBox_

TYPE( BoundingBox_ ), PUBLIC, PARAMETER :: TypeBoundingBox = &
  & BoundingBox_( NSD = 0, Box = 0 )

TYPE :: BoundingBoxPointer_
  CLASS( BoundingBoxPointer_ ), POINTER :: Ptr => NULL( )
END TYPE BoundingBoxPointer_

PUBLIC :: BoundingBoxPointer_

!----------------------------------------------------------------------------
!                                                           AbstractArray_
!----------------------------------------------------------------------------

PUBLIC :: AbstractArray_

TYPE :: AbstractArray_
  INTEGER( I4B ) :: tDimension = 0_I4B
END TYPE AbstractArray_

TYPE :: AbstractArrayPointer_
  CLASS( AbstractArray_ ), POINTER :: Ptr => NULL( )
END TYPE AbstractArrayPointer_

PUBLIC :: AbstractArrayPointer_

!----------------------------------------------------------------------------
!                                                          AbstractMatrix_
!----------------------------------------------------------------------------

TYPE, EXTENDS( AbstractArray_ ) :: AbstractMatrix_
END TYPE AbstractMatrix_

PUBLIC :: AbstractMatrix_

TYPE :: AbstractMatrixPointer_
  CLASS( AbstractMatrix_ ), POINTER :: Ptr => NULL( )
END TYPE AbstractMatrixPointer_

PUBLIC :: AbstractMatrixPointer_

!----------------------------------------------------------------------------
!                                                                    Matrix_
!----------------------------------------------------------------------------

TYPE, EXTENDS( AbstractMatrix_ ) :: RealMatrix_
  REAL( DFP ), ALLOCATABLE :: Val( :, : )
  CHARACTER( LEN = 5 ) :: MatrixProp = 'UNSYM'
END TYPE RealMatrix_

PUBLIC :: RealMatrix_

TYPE( RealMatrix_ ), PUBLIC, PARAMETER :: &
  & TypeRealMatrix = RealMatrix_( &
    & Val=NULL() )

TYPE :: RealMatrixPointer_
  CLASS( RealMatrix_ ), POINTER :: Ptr => NULL( )
END TYPE RealMatrixPointer_

PUBLIC :: RealMatrixPointer_

! for matrix conversion ( dense to dense )
! element matrix storage may differ from global matrix storage format
INTEGER( I4B ), PARAMETER, PUBLIC :: DofToNodes = 0, NONE = -1, NodesToDOF = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: DOF_FMT = 0
INTEGER( I4B ), PARAMETER, PUBLIC :: NODES_FMT = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: FMT_DOF = 0
INTEGER( I4B ), PARAMETER, PUBLIC :: FMT_NODES = 1

!----------------------------------------------------------------------------
!                                                              SparseMatrix_
!----------------------------------------------------------------------------

TYPE, EXTENDS( AbstractMatrix_ ) :: SparseMatrix_
  TYPE( IntVector_ ), ALLOCATABLE :: Row( : )
  INTEGER( I4B ), ALLOCATABLE :: IA( : )
  INTEGER( I4B ), ALLOCATABLE :: JA( : )
  INTEGER( I4B ), ALLOCATABLE :: ColSize( : )
  INTEGER( I4B ), ALLOCATABLE :: RowSize( : )
  INTEGER( I4B ), ALLOCATABLE :: DiagIndx( : )
  REAL( DFP ), ALLOCATABLE :: A( : ), Diag( : )
  INTEGER( I4B ) :: tDOF = 1
  INTEGER( I4B ) :: tNodes=0
  INTEGER( I4B ) :: nnz = 0
  INTEGER( I4B ) :: ncol = 0
  INTEGER( I4B ) :: nrow=0
  CHARACTER( LEN = 5 ) :: MatrixProp='UNSYM'
  INTEGER( I4B ) :: StorageFMT = Nodes_FMT
END TYPE SparseMatrix_

PUBLIC :: SparseMatrix_

TYPE( SparseMatrix_ ), PUBLIC, PARAMETER :: &
  & TypeSparseMatrix = SparseMatrix_( &
    & Row = NULL( ), &
    & IA = NULL( ), &
    & JA = NULL( ), &
    & ColSize = NULL( ), &
    & RowSize = NULL( ), &
    & DiagIndx = NULL( ), &
    & A = NULL( ), &
    & Diag = NULL( ) )

TYPE :: SparseMatrixPointer_
  CLASS( SparseMatrix_ ), POINTER :: Ptr => NULL( )
END TYPE SparseMatrixPointer_

PUBLIC :: SparseMatrixPointer_

!----------------------------------------------------------------------------
!                                                          AbstractVector_
!----------------------------------------------------------------------------

TYPE, EXTENDS( AbstractArray_ ) :: AbstractVector_
END TYPE AbstractVector_

PUBLIC :: AbstractVector_

TYPE :: AbstractVectorPointer_
  CLASS( AbstractVector_ ), POINTER :: Ptr => NULL( )
END TYPE AbstractVectorPointer_

!----------------------------------------------------------------------------
!                                                             IntVector_
!----------------------------------------------------------------------------

TYPE, EXTENDS( AbstractVector_ ) :: IntVector_
  INTEGER( I4B ), ALLOCATABLE :: Val( : )
END TYPE IntVector_

PUBLIC :: IntVector_

TYPE(IntVector_), PUBLIC, PARAMETER :: TypeIntVector = IntVector_( &
tDimension = 1_I4B, Val = NULL( ) )

TYPE :: IntVectorPointer_
  CLASS( IntVector_ ), POINTER :: Ptr => NULL( )
END TYPE IntVectorPointer_

PUBLIC :: IntVectorPointer_

!----------------------------------------------------------------------------
!                                                             RealVector_
!----------------------------------------------------------------------------

TYPE, EXTENDS( AbstractVector_ ) :: RealVector_
  REAL( DFP ), ALLOCATABLE :: Val( : )
END TYPE RealVector_

PUBLIC :: RealVector_

TYPE(RealVector_), PUBLIC, PARAMETER :: TypeRealVector = RealVector_( &
tDimension = 1_I4B, Val = NULL( ) )

TYPE :: RealVectorPointer_
  CLASS( RealVector_ ), POINTER :: Ptr => NULL( )
END TYPE RealVectorPointer_

PUBLIC :: RealVectorPointer_

!----------------------------------------------------------------------------
!                                                              IndexValue_
!----------------------------------------------------------------------------

TYPE :: IndexValue_
  INTEGER( I4B ) :: Indx
  REAL( DFP ) :: Val
END TYPE

PUBLIC :: IndexValue_

TYPE( IndexValue_ ), PUBLIC, PARAMETER :: TypeIndexValue = &
  & IndexValue_( Indx = 0, Val = 0.0_DFP )

TYPE :: IndexValuePointer_
  CLASS( IndexValue_ ), POINTER :: Ptr => NULL( )
END TYPE IndexValuePointer_

PUBLIC :: IndexValuePointer_

!----------------------------------------------------------------------------
!                                                                     DOF_
!----------------------------------------------------------------------------

TYPE :: DOF_
  INTEGER( I4B ), ALLOCATABLE :: MAP( :, : )
  INTEGER( I4B ), ALLOCATABLE :: ValMap( : )
  INTEGER( I4B ) :: StorageFMT = Nodes_FMT
END TYPE DOF_

PUBLIC :: DOF_

TYPE( DOF_ ), PUBLIC, PARAMETER :: TypeDOF = DOF_( MAP=NULL(), &
  ValMap=NULL(),  StorageFMT = Nodes_FMT )

TYPE :: DOFPointer_
  CLASS( DOF_ ), POINTER :: Ptr => NULL( )
END TYPE DOFPointer_

PUBLIC :: DOFPointer_

!----------------------------------------------------------------------------
!                                                             IterationData_
!----------------------------------------------------------------------------

TYPE :: IterationData_
  INTEGER( I4B ) :: MaxIter = 100, IterationNumber = 0
  REAL( DFP ) :: Tolerance = 1.0E-5
  REAL( DFP ) :: ErrorAtStart = 0.0, ErrorAtEnd = 0.0
  REAL( DFP ) :: TimeAtStart = 0.0 , TimeAtEnd = 0.0
  INTEGER( I4B ) :: ConvergenceType = RelativeConvergence
  INTEGER( I4B ) :: ConvergenceIn = ConvergenceInRes
  INTEGER( I4B ) :: NormType = NormL2
  LOGICAL( LGT ) :: Converged = .FALSE.
END TYPE IterationData_

PUBLIC :: IterationData_

TYPE( IterationData_ ), PUBLIC, PARAMETER :: TypeIterationData = &
  & IterationData_( )

TYPE :: IterationDataPointer_
  CLASS( IterationData_ ), POINTER :: Ptr => NULL( )
END TYPE IterationDataPointer_

PUBLIC :: IterationDataPointer_

!----------------------------------------------------------------------------
!                                                  TensorRelatedParameters
!----------------------------------------------------------------------------

  INTEGER( I4B ), PARAMETER, PUBLIC :: SymTensor = 1, SkewSymTensor = -1, &
    & GeneralTensor = 0, StressTypeVoigt = 1, StrainTypeVoigt = -1, &
    & WithSpectral = 1, WithoutSpectral = -1, SineLode = 1, CosineLode = 0

!----------------------------------------------------------------------------
!                                                       VoigtRank2Tensor_
!----------------------------------------------------------------------------

TYPE :: VoigtRank2Tensor_
  REAL( DFP ) :: V( 9 )
END TYPE VoigtRank2Tensor_

PUBLIC :: VoigtRank2Tensor_

TYPE( VoigtRank2Tensor_ ), PARAMETER, PUBLIC :: &
  & TypeVoigtRank2Tensor = VoigtRank2Tensor_( V = 0.0_DFP )

TYPE :: VoigtRank2TensorPointer
  CLASS( VoigtRank2Tensor_ ), POINTER :: Ptr => NULL( )
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

TYPE, EXTENDS( Tensor_ ) :: Rank2Tensor_
  REAL( DFP ) :: T( 3, 3 )
END TYPE Rank2Tensor_

PUBLIC :: Rank2Tensor_

TYPE( Rank2Tensor_ ), PARAMETER, PUBLIC :: &
  & TypeRank2Tensor = Rank2Tensor_( T = 0.0_DFP )

TYPE :: Rank2TensorPointer_
  CLASS( Rank2Tensor_ ), POINTER :: Ptr => NULL( )
END TYPE Rank2TensorPointer_

PUBLIC :: Rank2TensorPointer_

!----------------------------------------------------------------------------
!                                                       ReferenceTopology_
!----------------------------------------------------------------------------

TYPE :: ReferenceTopology_
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
  INTEGER( I4B ) :: Name, XiDimension
END TYPE ReferenceTopology_

PUBLIC :: ReferenceTopology_

TYPE :: ReferenceTopologyPointer_
  CLASS( ReferenceTopology_ ), POINTER :: Ptr => NULL( )
END TYPE ReferenceTopologyPointer_

PUBLIC :: ReferenceTopologyPointer_

!----------------------------------------------------------------------------
!                                                        ReferenceElement_
!----------------------------------------------------------------------------

TYPE :: ReferenceElement_
  REAL( DFP ), ALLOCATABLE :: XiJ( :, : )
  INTEGER( I4B ) :: EntityCounts( 4 )
  INTEGER( I4B ) :: XiDimension, Name, Order, NSD
  TYPE( ReferenceTopology_ ), ALLOCATABLE :: Topology( : )
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangePoints => lp_refelem
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangeElement => lag_elem_refelem
END TYPE ReferenceElement_

PUBLIC :: ReferenceElement_

TYPE :: ReferenceElementPointer_
  CLASS( ReferenceElement_ ), POINTER :: Ptr => NULL( )
END TYPE ReferenceElementPointer_

PUBLIC :: ReferenceElementPointer_

INTERFACE
  MODULE PURE FUNCTION lp_refelem( Obj, Order ) RESULT( Ans )
    CLASS( ReferenceElement_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    REAL( DFP ), ALLOCATABLE :: Ans( :, : )
  END FUNCTION lp_refelem

  MODULE FUNCTION lag_elem_refelem(Obj, Order) RESULT( Ans )
    CLASS( ReferenceElement_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    CLASS( ReferenceElement_ ), POINTER :: Ans
  END FUNCTION lag_elem_refelem
END INTERFACE

!----------------------------------------------------------------------------
!                                                          ReferenceLine_
!----------------------------------------------------------------------------

TYPE, EXTENDS( ReferenceElement_ ) :: ReferenceLine_
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangePoints => lp_refelem_line
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangeElement => lag_elem_refLine
END TYPE ReferenceLine_

PUBLIC :: ReferenceLine_

TYPE( ReferenceLine_ ), PARAMETER, PUBLIC :: &
  & TypeReferenceLine = ReferenceLine_( &
  & XiJ=NULL( ), EntityCounts = [0,0,0,0], XiDimension = 0, Name = 0, &
  & Topology = NULL( ), Order = 0, NSD=0  )

INTERFACE
  MODULE PURE FUNCTION lp_refelem_line( Obj, Order ) RESULT( Ans )
    CLASS( ReferenceLine_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    REAL( DFP ), ALLOCATABLE :: Ans( :, : )
  END FUNCTION lp_refelem_line

  MODULE FUNCTION lag_elem_refline(Obj, Order) RESULT( Ans )
    CLASS( ReferenceLine_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    CLASS( ReferenceElement_ ), POINTER :: Ans
  END FUNCTION lag_elem_refline
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ReferenceTriangle_
!----------------------------------------------------------------------------

TYPE, EXTENDS( ReferenceElement_ ) :: ReferenceTriangle_
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangePoints => lp_refelem_Triangle
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangeElement => lag_elem_refTriangle
END TYPE ReferenceTriangle_

PUBLIC :: ReferenceTriangle_

TYPE( ReferenceTriangle_ ), PARAMETER, PUBLIC :: &
  & TypeReferenceTriangle = ReferenceTriangle_( &
  & XiJ=NULL( ), EntityCounts = [0,0,0,0], XiDimension = 0, Name = 0, &
  & Topology = NULL( ), Order = 0, NSD = 0 )

INTERFACE
  MODULE PURE FUNCTION lp_refelem_Triangle( Obj, Order ) RESULT( Ans )
    CLASS( ReferenceTriangle_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    REAL( DFP ), ALLOCATABLE :: Ans( :, : )
  END FUNCTION lp_refelem_Triangle

  MODULE FUNCTION lag_elem_refTriangle(Obj, Order) RESULT( Ans )
    CLASS( ReferenceTriangle_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    CLASS( ReferenceElement_ ), POINTER :: Ans
  END FUNCTION lag_elem_refTriangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ReferenceQuadrangle_
!----------------------------------------------------------------------------

TYPE, EXTENDS( ReferenceElement_ ) :: ReferenceQuadrangle_
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangePoints => lp_refelem_Quadrangle
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangeElement => lag_elem_refQuadrangle
END TYPE ReferenceQuadrangle_

PUBLIC :: ReferenceQuadrangle_

TYPE( ReferenceQuadrangle_ ), PARAMETER, PUBLIC :: &
  & TypeReferenceQuadrangle &
  & = ReferenceQuadrangle_( &
  & XiJ=NULL( ), EntityCounts = [0,0,0,0], XiDimension = 0, Name = 0, &
  & Topology = NULL( ), Order = 0, NSD = 0 )

INTERFACE
  MODULE PURE FUNCTION lp_refelem_Quadrangle( Obj, Order ) RESULT( Ans )
    CLASS( ReferenceQuadrangle_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    REAL( DFP ), ALLOCATABLE :: Ans( :, : )
  END FUNCTION lp_refelem_Quadrangle

  MODULE FUNCTION lag_elem_refQuadrangle(Obj, Order) RESULT( Ans )
    CLASS( ReferenceQuadrangle_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    CLASS( ReferenceElement_ ), POINTER :: Ans
  END FUNCTION lag_elem_refQuadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ReferenceTetrahedron_
!----------------------------------------------------------------------------

TYPE, EXTENDS( ReferenceElement_ ) :: ReferenceTetrahedron_
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangePoints => lp_refelem_Tetrahedron
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangeElement => lag_elem_refTetrahedron
END TYPE ReferenceTetrahedron_

PUBLIC :: ReferenceTetrahedron_

TYPE( ReferenceTetrahedron_ ), PARAMETER, PUBLIC :: &
  & TypeReferenceTetrahedron &
  & = ReferenceTetrahedron_( &
  & XiJ=NULL( ), EntityCounts = [0,0,0,0], XiDimension = 0, Name = 0, &
  & Topology = NULL( ), Order = 0, NSD = 0 )

INTERFACE
  MODULE PURE FUNCTION lp_refelem_Tetrahedron( Obj, Order ) RESULT( Ans )
    CLASS( ReferenceTetrahedron_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    REAL( DFP ), ALLOCATABLE :: Ans( :, : )
  END FUNCTION lp_refelem_Tetrahedron

  MODULE FUNCTION lag_elem_refTetrahedron(Obj, Order) RESULT( Ans )
    CLASS( ReferenceTetrahedron_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    CLASS( ReferenceElement_ ), POINTER :: Ans
  END FUNCTION lag_elem_refTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ReferenceHexahedron_
!----------------------------------------------------------------------------

TYPE, EXTENDS( ReferenceElement_ ) :: ReferenceHexahedron_
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangePoints => lp_refelem_Hexahedron
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangeElement => lag_elem_refHexahedron
END TYPE ReferenceHexahedron_

PUBLIC :: ReferenceHexahedron_

TYPE( ReferenceHexahedron_ ), PARAMETER, PUBLIC :: &
  & TypeReferenceHexahedron &
  & = ReferenceHexahedron_( &
  & XiJ=NULL( ), EntityCounts = [0,0,0,0], XiDimension = 0, Name = 0, &
  & Topology = NULL( ), Order = 0, NSD = 0 )

INTERFACE
  MODULE PURE FUNCTION lp_refelem_Hexahedron( Obj, Order ) RESULT( Ans )
    CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    REAL( DFP ), ALLOCATABLE :: Ans( :, : )
  END FUNCTION lp_refelem_Hexahedron

  MODULE FUNCTION lag_elem_refHexahedron(Obj, Order) RESULT( Ans )
    CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    CLASS( ReferenceElement_ ), POINTER :: Ans
  END FUNCTION lag_elem_refHexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                            ReferencePrism_
!----------------------------------------------------------------------------

TYPE, EXTENDS( ReferenceElement_ ) :: ReferencePrism_
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangePoints => lp_refelem_Prism
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangeElement => lag_elem_refPrism
END TYPE ReferencePrism_

PUBLIC :: ReferencePrism_

TYPE( ReferencePrism_ ), PARAMETER, PUBLIC :: TypeReferencePrism &
  & = ReferencePrism_( &
  & XiJ=NULL( ), EntityCounts = [0,0,0,0], XiDimension = 0, Name = 0, &
  & Topology = NULL( ), Order = 0, NSD = 0 )

INTERFACE
  MODULE PURE FUNCTION lp_refelem_Prism( Obj, Order ) RESULT( Ans )
    CLASS( ReferencePrism_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    REAL( DFP ), ALLOCATABLE :: Ans( :, : )
  END FUNCTION lp_refelem_Prism

  MODULE FUNCTION lag_elem_refPrism(Obj, Order) RESULT( Ans )
    CLASS( ReferencePrism_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    CLASS( ReferenceElement_ ), POINTER :: Ans
  END FUNCTION lag_elem_refPrism
END INTERFACE

!----------------------------------------------------------------------------
!                                                          ReferencePyramid_
!----------------------------------------------------------------------------

TYPE, EXTENDS( ReferenceElement_ ) :: ReferencePyramid_
  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangePoints => lp_refelem_Pyramid
  PROCEDURE, PUBLIC, PASS( Obj ) :: LagrangeElement => lag_elem_refPyramid
END TYPE ReferencePyramid_

PUBLIC :: ReferencePyramid_

TYPE( ReferencePyramid_ ), PARAMETER, PUBLIC :: TypeReferencePyramid &
  & = ReferencePyramid_( &
  & XiJ=NULL( ), EntityCounts = [0,0,0,0], &
  & XiDimension = 0, Name = 0, &
  & Topology = NULL( ), Order = 0, NSD = 0 )

INTERFACE
  MODULE PURE FUNCTION lp_refelem_Pyramid( Obj, Order ) RESULT( Ans )
    CLASS( ReferencePyramid_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    REAL( DFP ), ALLOCATABLE :: Ans( :, : )
  END FUNCTION lp_refelem_Pyramid

  MODULE FUNCTION lag_elem_refPyramid(Obj, Order) RESULT( Ans )
    CLASS( ReferencePyramid_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Order
    CLASS( ReferenceElement_ ), POINTER :: Ans
  END FUNCTION lag_elem_refPyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 KeyValue_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! [[keyvalue_]] is a poor implementation of dict
TYPE :: KeyValue_
  INTEGER( I4B ) :: DataType = 0
  TYPE( String ) :: Key
  REAL( DFP ), ALLOCATABLE :: Value( :, : )
END TYPE KeyValue_

PUBLIC :: KeyValue_

TYPE( KeyValue_ ), PARAMETER, PUBLIC :: TypeKeyValue = &
  & KeyValue_( Value = NULL( ), Key = TypeString )

!----------------------------------------------------------------------------
!                                                                FEVariable_
!----------------------------------------------------------------------------

TYPE :: FEVariable_
  REAL( DFP ), ALLOCATABLE :: R1( : ), R2( :, : ), R3( :, :, : ), &
    & R4( :, :, :, :)
  REAL( DFP ) :: R0 = 0.
  INTEGER( I4B ) :: DefineOn = 0 !Nodal, Quadrature
  INTEGER( I4B ) :: VarType = 0 !Space, SpaceTime, Constant
  INTEGER( I4B ) :: Rank = 0 ! Scalar, Vector, Matrix
  INTEGER( I4B ) :: CaseType = 0
END TYPE FEVariable_

PUBLIC :: FEVariable_

TYPE( FEVariable_ ), PARAMETER, PUBLIC :: TypeFEVariable = &
  & FEVariable_( R1 = NULL( ), R2 = NULL( ), R3 = NULL( ), &
  & R4 = NULL( ) )

INTEGER( I4B ), PARAMETER, PUBLIC :: Constant = 0
INTEGER( I4B ), PARAMETER, PUBLIC :: Space = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: SpaceTime = 2
INTEGER( I4B ), PARAMETER, PUBLIC :: Time= 3
INTEGER( I4B ), PARAMETER, PUBLIC :: Scalar = 0, Vector = 1, Matrix = 2
INTEGER( I4B ), PARAMETER, PUBLIC :: Nodal = 1, Quadrature = 2

!----------------------------------------------------------------------------
!                                                         FEVariableConstant_
!----------------------------------------------------------------------------

TYPE :: FEVariableConstant_
  INTEGER( I4B ) :: Val = 1
END TYPE FEVariableConstant_

PUBLIC :: FEVariableConstant_

TYPE( FEVariableConstant_ ), PARAMETER, PUBLIC :: TypeFEVariableConstant = &
  & FEVariableConstant_( 1_I4B )
TYPE( FEVariableConstant_ ), PARAMETER, PUBLIC :: TypeVariableConstant = &
  & FEVariableConstant_( 1_I4B )

!----------------------------------------------------------------------------
!                                                           FEVariableSpace_
!----------------------------------------------------------------------------

TYPE :: FEVariableSpace_
  INTEGER( I4B ) :: Val = 2
END TYPE FEVariableSpace_

PUBLIC :: FEVariableSpace_

TYPE( FEVariableSpace_ ), PARAMETER, PUBLIC :: TypeFEVariableSpace = &
  & FEVariableSpace_( 2_I4B )
TYPE( FEVariableSpace_ ), PARAMETER, PUBLIC :: TypeVariableSpace = &
  & FEVariableSpace_( 2_I4B )

!----------------------------------------------------------------------------
!                                                       FEVariableSpaceTime_
!----------------------------------------------------------------------------

TYPE :: FEVariableSpaceTime_
  INTEGER( I4B ) :: Val = 3
END TYPE FEVariableSpaceTime_

PUBLIC :: FEVariableSpaceTime_

TYPE( FEVariableSpaceTime_ ), PARAMETER, PUBLIC :: &
  & TypeFEVariableSpaceTime = FEVariableSpaceTime_( 3_I4B )
TYPE( FEVariableSpaceTime_ ), PARAMETER, PUBLIC :: &
  & TypeVariableSpaceTime = FEVariableSpaceTime_( 3_I4B )

!----------------------------------------------------------------------------
!                                                            FEVariableTime_
!----------------------------------------------------------------------------

TYPE :: FEVariableTime_
  INTEGER( I4B ) :: Val = 4
END TYPE FEVariableTime_

PUBLIC :: FEVariableTime_

TYPE( FEVariableTime_ ), PARAMETER, PUBLIC :: TypeFEVariableTime = &
  & FEVariableTime_( 4_I4B )
TYPE( FEVariableTime_ ), PARAMETER, PUBLIC :: TypeVariableTime = &
  & FEVariableTime_( 4_I4B )

!----------------------------------------------------------------------------
!                                                          FEVariableScalar_
!----------------------------------------------------------------------------

TYPE :: FEVariableScalar_
  INTEGER( I4B ) :: Val = 0
END TYPE FEVariableScalar_

PUBLIC :: FEVariableScalar_

TYPE( FEVariableScalar_ ), PARAMETER, PUBLIC :: &
  & TypeFEVariableScalar = FEVariableScalar_( 0_I4B )
TYPE( FEVariableScalar_ ), PARAMETER, PUBLIC :: &
  & TypeVariableScalar = FEVariableScalar_( 0_I4B )

!----------------------------------------------------------------------------
!                                                          FEVariableVector_
!----------------------------------------------------------------------------

TYPE :: FEVariableVector_
  INTEGER( I4B ) :: Val = 1
END TYPE FEVariableVector_

PUBLIC :: FEVariableVector_

TYPE( FEVariableVector_ ), PARAMETER, PUBLIC :: &
  & TypeFEVariableVector = FEVariableVector_( 1_I4B )
TYPE( FEVariableVector_ ), PARAMETER, PUBLIC :: &
  & TypeVariableVector = FEVariableVector_( 1_I4B )

!----------------------------------------------------------------------------
!                                                          FEVariableMatrix_
!----------------------------------------------------------------------------

TYPE :: FEVariableMatrix_
  INTEGER( I4B ) :: Val = 2
END TYPE FEVariableMatrix_

PUBLIC :: FEVariableMatrix_

TYPE( FEVariableMatrix_ ), PARAMETER, PUBLIC :: &
  & TypeFEVariableMatrix = FEVariableMatrix_( 2_I4B )
TYPE( FEVariableMatrix_ ), PARAMETER, PUBLIC :: &
  & TypeVariableMatrix = FEVariableMatrix_( 2_I4B )

!----------------------------------------------------------------------------
!                                                           QuadraturePoint_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! [[QuadraturePoint_]] data type contains the quadrature point information
!
! - Each of column of `Points` corresponds to a quadrature point
! - `Points(1:tXi, :)` contains information of quarature points
! - The last row contains the information about the weighths
TYPE :: QuadraturePoint_
  REAL(DFP), ALLOCATABLE :: Points( :, : )
  INTEGER( I4B ) :: tXi = 0
END TYPE QuadraturePoint_

PUBLIC :: QuadraturePoint_
TYPE( QuadraturePoint_ ), PUBLIC, PARAMETER :: &
  & TypeQuadraturePoint = QuadraturePoint_( Points = NULL( ) )
TYPE :: QuadraturePointPointer_
  CLASS( QuadraturePoint_ ), POINTER :: Ptr => NULL( )
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

TYPE, EXTENDS( BaseInterpolation_ ) :: LagrangeInterpolation_
END TYPE LagrangeInterpolation_

PUBLIC :: LagrangeInterpolation_

TYPE( LagrangeInterpolation_ ), PARAMETER, PUBLIC :: &
  & TypeLagrangeInterpolation = LagrangeInterpolation_()

!----------------------------------------------------------------------------
!                                                       HermitInterpolation_
!----------------------------------------------------------------------------

TYPE, EXTENDS( BaseInterpolation_ ) :: HermitInterpolation_
END TYPE HermitInterpolation_

PUBLIC :: HermitInterpolation_

TYPE( HermitInterpolation_ ), PARAMETER, PUBLIC :: &
  & TypeHermitInterpolation = HermitInterpolation_()

!----------------------------------------------------------------------------
!                                                  SerendipityInterpolation_
!----------------------------------------------------------------------------

TYPE, EXTENDS( BaseInterpolation_ ) :: SerendipityInterpolation_
END TYPE SerendipityInterpolation_

PUBLIC :: SerendipityInterpolation_

TYPE( SerendipityInterpolation_ ), PARAMETER, PUBLIC :: &
  & TypeSerendipityInterpolation = SerendipityInterpolation_()

!----------------------------------------------------------------------------
!                                                    HierarchyInterpolation_
!----------------------------------------------------------------------------

TYPE, EXTENDS( BaseInterpolation_ ) :: HierarchyInterpolation_
END TYPE HierarchyInterpolation_

PUBLIC :: HierarchyInterpolation_

TYPE( HierarchyInterpolation_ ), PARAMETER, PUBLIC :: &
  & TypeHierarchyInterpolation = HierarchyInterpolation_()

!----------------------------------------------------------------------------
!                                                          BasisContinuity_
!----------------------------------------------------------------------------

TYPE :: BasisContinuity_
END TYPE BasisContinuity_

PUBLIC :: BasisContinuity_

TYPE( BasisContinuity_ ), PARAMETER, PUBLIC :: &
  & TypeBasisContinuity = BasisContinuity_( )

!----------------------------------------------------------------------------
!                                                                     H1_
!----------------------------------------------------------------------------

TYPE, EXTENDS( BasisContinuity_ ) :: H1_
END TYPE H1_

PUBLIC :: H1_

TYPE( H1_ ), PARAMETER, PUBLIC :: &
  & TypeH1 = H1_()

!----------------------------------------------------------------------------
!                                                                   H1DIV_
!----------------------------------------------------------------------------

TYPE, EXTENDS( BasisContinuity_ ) :: H1DIV_
END TYPE H1DIV_

PUBLIC :: H1DIV_

TYPE( H1DIV_ ), PARAMETER, PUBLIC :: &
  & TypeH1DIV = H1DIV_()

!----------------------------------------------------------------------------
!                                                                   H1CURL_
!----------------------------------------------------------------------------

TYPE, EXTENDS( BasisContinuity_ ) :: H1CURL_
END TYPE H1CURL_

PUBLIC :: H1CURL_

TYPE( H1CURL_ ), PARAMETER, PUBLIC :: &
  & TypeH1CURL = H1CURL_()

!----------------------------------------------------------------------------
!                                                                      DG_
!----------------------------------------------------------------------------

TYPE, EXTENDS( BasisContinuity_ ) :: DG_
END TYPE DG_

PUBLIC :: DG_

TYPE( DG_ ), PARAMETER, PUBLIC :: &
  & TypeDG = DG_()

!----------------------------------------------------------------------------
!                                                            ElementData_
!----------------------------------------------------------------------------

TYPE :: ElementData_
  INTEGER(I4B) ::  NSD, NNE, NNS, NNT, XiDimension, ElemTopology, &
    & SpaceElemTopo, TimeElemTopo, ElemType, MAT_Type
END TYPE ElementData_

PUBLIC :: ElementData_

TYPE( ElementData_ ), PARAMETER, PUBLIC :: TypeElementData &
  & = ElementData_( NSD = -1, NNE = -1, NNS = -1, NNT = -1, &
  & XiDimension = -1, ElemTopology = -1, SpaceElemTopo = -1, &
  & TimeElemTopo = -1, ElemType = -1, MAT_Type = -1 )

TYPE :: ElementDataPointer_
  CLASS( ElementData_ ), POINTER :: Ptr => NULL( )
END TYPE ElementDataPointer_

PUBLIC :: ElementDataPointer_

!----------------------------------------------------------------------------
!                                                              ShapeData_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This Class bundles all required variables(related to shapefunctions)
! at a Gauss Point
TYPE :: ShapeData_
  REAL( DFP ) :: Ws = 0.0_DFP
  REAL( DFP ) :: Js = 0.0_DFP
  REAL( DFP ) :: Thickness = 1.0_DFP
  REAL( DFP ) :: Xi( 3 ) = 0.0_DFP
  REAL( DFP ) :: XBar( 3 ) = 0.0_DFP
  REAL( DFP ) :: Normal( 3 ) = 0.0_DFP
  INTEGER( I4B ) :: ElemTopology = 0
  INTEGER( I4B ) :: NSD = 0
  REAL( DFP ), ALLOCATABLE :: N( : )
  REAL( DFP ), ALLOCATABLE :: dNdXi( :, : )
  REAL( DFP ), ALLOCATABLE :: dNdXt( :, : )
  REAL( DFP ), ALLOCATABLE :: Jacobian( :, : )
END TYPE ShapeData_

PUBLIC :: ShapeData_

TYPE( ShapeData_ ), PARAMETER, PUBLIC :: &
  & TypeShapeData = ShapeData_( &
    & N = NULL( ), &
    & dNdXi = NULL( ), &
    & dNdXt = NULL( ), &
    & Jacobian = NULL( ) )

TYPE :: ShapeDataPointer_
  CLASS( ShapeDataPointer_ ), POINTER :: Ptr => NULL( )
END TYPE ShapeDataPointer_

PUBLIC :: ShapeDataPointer_

!----------------------------------------------------------------------------
!                                                               STShapeData_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This class extends [[shapedata_]] class to space-time FEM applcation
TYPE, EXTENDS( ShapeData_ ):: STShapeData_
  REAL( DFP ) :: Theta = 0.0
  REAL( DFP ) :: Wt = 0.0
  REAL( DFP ) :: Jt = 0.0
  INTEGER( I4B ) :: SpaceElemTopo = 0
  INTEGER( I4B ) :: TimeElemTopo = 0
  REAL( DFP ), ALLOCATABLE :: T(:)
    !! values of shape function at different time nodes
  REAL( DFP ), ALLOCATABLE :: dTdTheta( : )
    !! Value of local time derivative of T at time gauss point
  REAL( DFP ), ALLOCATABLE :: dNTdt( :, : )
    !! Value of global time derivative of T at time gauss points
  REAL( DFP ), ALLOCATABLE :: dNTdXt( :, :, : )
    !! Spatial gradient of the shape functions at space-time gauss points
END TYPE STShapeData_

PUBLIC :: STShapeData_

TYPE :: STShapeDataPointer_
  CLASS( STShapeData_ ), POINTER :: Ptr => NULL( )
END TYPE STShapeDataPointer_

PUBLIC :: STShapeDataPointer_

!----------------------------------------------------------------------------
!                                                           ElemShapeData_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This data type contains shapefunction related data defined
! at all gauss points of an elements
TYPE :: ElemShapeData_

  REAL( DFP ), ALLOCATABLE :: N( :, : )
    !! Shape function value `N(I,ips)`
  REAL( DFP ), ALLOCATABLE :: dNdXi( :, :, : )
    !! Local derivative of a shape function
  REAL( DFP ), ALLOCATABLE :: Jacobian( :, :, : )
    !! Jacobian of mapping `J(:,:,ips)` also $\mathbf{F}_{\Xi x}$
  REAL( DFP ), ALLOCATABLE :: Js( : )
    !! Determinant of Jacobian at ips
  REAL( DFP ), ALLOCATABLE :: Ws( : )
    !! Weighting functions
  REAL( DFP ), ALLOCATABLE :: dNdXt( :, :, : )
    !! Spatial derivative of shape function
  REAL( DFP ), ALLOCATABLE :: Thickness( : )
    !! Thickness of element
  REAL( DFP ), ALLOCATABLE :: Coord( :, : )
    !! Barycentric coordinate
  REAL( DFP ), ALLOCATABLE :: Normal( :, : )
    !! Normal in case of facet element
  TYPE( ReferenceElement_ ) :: RefElem
    !! Refererece element
  TYPE( QuadraturePoint_ ) :: Quad
    !! Quadrature points
END TYPE ElemShapeData_

PUBLIC :: ElemShapeData_

TYPE( ElemShapeData_ ), PUBLIC, PARAMETER :: &
  & TypeElemShapeData = ElemShapeData_( &
  & N = NULL( ), &
  & dNdXi = NULL( ), &
  & Jacobian = NULL( ), &
  & Js = NULL( ), &
  & Ws = NULL( ), &
  & dNdXt = NULL( ), &
  & Thickness = NULL( ), &
  & Coord = NULL( ), &
  & Normal = NULL( ) )

TYPE :: ElemShapeDataPointer_
  CLASS( ShapeDataPointer_ ), POINTER :: Ptr => NULL( )
END TYPE ElemShapeDataPointer_

PUBLIC :: ElemShapeDataPointer_

!----------------------------------------------------------------------------
!                                                           STElemShapeData_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This data type contains shapefunction related data defined
! at all gauss points of an elements
!
! ## How to initiate local shape data
!
! ```fortran
! program main
!   !! This program shows how to use space-time element shape data
! use easifem
! implicit none

!   !line order 1
!   type( stelemshapedata_ ), allocatable :: obj( : )
!   type( elemshapedata_ ) :: elemsd
!   type( quadraturepoint_ ) :: quad
!   class( referenceElement_ ), pointer :: refelem
!   integer( i4b ) :: orderInTime, ii
!   real( dfp ) :: xiJ( 1, 2 )

!   orderInTime = 2
!   ALLOCATE( ReferenceLine_ :: refelem )
!   refelem = ReferenceLine( nsd = 1 )

!   quad = GaussLegendreQuadrature(refelem=refelem,order=orderInTime )

!   ! higher order lagrange element
!   SELECT TYPE( refelem )
!   TYPE IS ( ReferenceLine_  )
!     refelem = LagrangeElement( refelem = refelem, order = 1 )
!   END SELECT

!   call initiate( &
!     & obj = elemsd, quad = quad, refelem = refelem, &
!     & ContinuityType= typeH1, InterpolType = TypeLagrangeInterpolation )

!   call initiate( obj = obj, elemsd = elemsd )

!   !! Generating shape functions for space element
!   ALLOCATE( ReferenceTriangle_ :: refelem )
!   refelem = ReferenceTriangle( nsd = 2 )

!   quad = GaussLegendreQuadrature( refelem=refelem, order = 2 )

!   do ii = 1, size( obj )
!     call initiate( &
!       & obj = obj( ii ), quad = quad, refelem = refelem, &
!       & ContinuityType= typeH1, InterpolType = TypeLagrangeInterpolation )
!     call display( obj( ii ), "ii :: "// str( ii ) )
!   end do
! end program
!```
TYPE, EXTENDS( ElemShapeData_ ) :: STElemShapeData_
  REAL( DFP ) :: Wt = 0.0
    !! Weight of gauss point in time domain
  REAL( DFP ) :: Theta = 0.0
    !! Gauss point in time domain
  REAL( DFP ) :: Jt = 0.0
    !! Jacobian $\frac{dt}{d\theta}$
  REAL( DFP ), ALLOCATABLE :: T( : )
    !! Shape function in time domain
  REAL( DFP ), ALLOCATABLE :: dTdTheta( : )
    !! Local shape function derivative in time domain
  REAL( DFP ), ALLOCATABLE :: dNTdt( :, :, : )
  REAL( DFP ), ALLOCATABLE :: dNTdXt( :, :, :, : )
END TYPE STElemShapeData_

PUBLIC :: STElemShapeData_
TYPE( STElemShapeData_ ), PUBLIC, PARAMETER :: &
  & TypeSTElemShapeData = STElemShapeData_( &
    & N = NULL( ), &
    & dNdXi = NULL( ), &
    & Jacobian = NULL( ), &
    & Js = NULL( ), &
    & Ws = NULL( ), &
    & dNdXt = NULL( ), &
    & Thickness = NULL( ), &
    & Coord = NULL( ), &
    & Normal = NULL( ), &
    & T = NULL( ), &
    & dTdTheta = NULL( ), &
    & dNTdt = NULL( ), &
    & dNTdXt = NULL( ) )

!----------------------------------------------------------------------------
!                                                              Meshquality_
!----------------------------------------------------------------------------

TYPE :: QualityMeasure_
  INTEGER( I4B ), PUBLIC :: area=100
  INTEGER( I4B ), PUBLIC :: maxAngle=101
  INTEGER( I4B ), PUBLIC :: minAngle=102
  INTEGER( I4B ), PUBLIC :: AngleRatio=103
  INTEGER( I4B ), PUBLIC :: RadiusRatio=104
  INTEGER( I4B ), PUBLIC :: EdgeRatio=105
  INTEGER( I4B ), PUBLIC :: AspectRatio=106
  INTEGER( I4B ), PUBLIC :: ScaledJacobian=107
  INTEGER( I4B ), PUBLIC :: Default=106
END TYPE QualityMeasure_

TYPE(QualityMeasure_), PARAMETER, PUBLIC :: &
  & QualityMeasure = QualityMeasure_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE BaseType
