MODULE FEVariable_Method
USE GlobalData
USE BaseType

IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                         Display@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE Display_Obj( Obj, Msg, UnitNo )
  TYPE( FEVariable_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE Display_obj
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE Display_Obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE fe_deallocate( Obj )
  TYPE( FEVariable_ ), INTENT( INOUT) :: Obj
END SUBROUTINE fe_deallocate
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE fe_deallocate
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                  NodalVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE MODULE PURE FUNCTION Nodal_Scalar_Constant( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val
  CLASS( FEVariableScalar_ ), INTENT( IN ) :: Rank
  CLASS( FEVariableConstant_ ), INTENT( IN ) :: VarType
END FUNCTION Nodal_Scalar_Constant
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Scalar_Constant
END INTERFACE NodalVariable

PUBLIC :: NodalVariable

!----------------------------------------------------------------------------
!                                                  NodalVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Nodal_Scalar_Space( Val, Rank, VarType ) RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  TYPE( FEVariableScalar_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpace_ ), INTENT( IN ) :: VarType
END FUNCTION Nodal_Scalar_Space
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Scalar_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                                   NodalVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Nodal_Scalar_SpaceTime( Val, Rank, VarType ) RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  TYPE( FEVariableScalar_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpaceTime_ ), INTENT( IN ) :: VarType
END FUNCTION Nodal_Scalar_SpaceTime
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Scalar_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                                  NodalVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE MODULE PURE FUNCTION Nodal_Vector_Constant( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableConstant_ ), INTENT( IN ) :: VarType
END FUNCTION Nodal_Vector_Constant
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Vector_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                                  NodalVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Nodal_Vector_Space( Val, Rank, VarType ) RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpace_ ), INTENT( IN ) :: VarType
END FUNCTION Nodal_Vector_Space
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Vector_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                                  NodalVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Nodal_Vector_SpaceTime( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpaceTime_ ), INTENT( IN ) :: VarType
END FUNCTION Nodal_Vector_SpaceTime
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Vector_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                                  NodalVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE MODULE PURE FUNCTION Nodal_Matrix_Constant( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  TYPE( FEVariableMatrix_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableConstant_ ), INTENT( IN ) :: VarType
END FUNCTION Nodal_Matrix_Constant
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Matrix_Constant
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                                  NodalVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Nodal_Matrix_Space( Val, Rank, VarType ) RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
  TYPE( FEVariableMatrix_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpace_ ), INTENT( IN ) :: VarType
END FUNCTION Nodal_Matrix_Space
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Matrix_Space
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                                  NodalVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Nodal_Matrix_SpaceTime( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, :, :, : )
  TYPE( FEVariableMatrix_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpaceTime_ ), INTENT( IN ) :: VarType
END FUNCTION Nodal_Matrix_SpaceTime
END INTERFACE

INTERFACE NodalVariable
  MODULE PROCEDURE Nodal_Matrix_SpaceTime
END INTERFACE NodalVariable

!----------------------------------------------------------------------------
!                                                  NodalVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE MODULE PURE FUNCTION Quadrature_Scalar_Constant( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val
  TYPE( FEVariableScalar_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableConstant_ ), INTENT( IN ) :: VarType
END FUNCTION Quadrature_Scalar_Constant
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Scalar_Constant
END INTERFACE QuadratureVariable

PUBLIC :: QuadratureVariable

!----------------------------------------------------------------------------
!                                             QuadratureVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Quadrature_Scalar_Space( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  TYPE( FEVariableScalar_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpace_ ), INTENT( IN ) :: VarType
END FUNCTION Quadrature_Scalar_Space
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Scalar_Space
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                             QuadratureVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Quadrature_Scalar_SpaceTime( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  TYPE( FEVariableScalar_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpaceTime_ ), INTENT( IN ) :: VarType
END FUNCTION Quadrature_Scalar_SpaceTime
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Scalar_SpaceTime
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                                  NodalVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE MODULE PURE FUNCTION Quadrature_Vector_Constant( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableConstant_ ), INTENT( IN ) :: VarType
END FUNCTION Quadrature_Vector_Constant
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Vector_Constant
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                                  QuadratureVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Quadrature_Vector_Space( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpace_ ), INTENT( IN ) :: VarType
END FUNCTION Quadrature_Vector_Space
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Vector_Space
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                              QuadratureVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Quadrature_Vector_SpaceTime( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpaceTime_ ), INTENT( IN ) :: VarType
END FUNCTION Quadrature_Vector_SpaceTime
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Vector_SpaceTime
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                             QuadratureVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE MODULE PURE FUNCTION Quadrature_Matrix_Constant( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  TYPE( FEVariableMatrix_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableConstant_ ), INTENT( IN ) :: VarType
END FUNCTION Quadrature_Matrix_Constant
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Matrix_Constant
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                             QuadratureVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Quadrature_Matrix_Space( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
  TYPE( FEVariableMatrix_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpace_ ), INTENT( IN ) :: VarType
END FUNCTION Quadrature_Matrix_Space
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Matrix_Space
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                             QuadratureVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Quadrature_Matrix_SpaceTime( Val, Rank, VarType ) &
  & RESULT( Obj )
  TYPE( FEVariable_ ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, :, :, : )
  TYPE( FEVariableMatrix_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpaceTime_ ), INTENT( IN ) :: VarType
END FUNCTION Quadrature_Matrix_SpaceTime
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Matrix_SpaceTime
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                                             SIZE@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Size_obj( Obj, Dim ) RESULT( Ans )
  CLASS( FEVariable_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Dim
  INTEGER( I4B ) :: Ans
END FUNCTION Size_Obj
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE Size_Obj
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                                             SHAPE@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Shape_obj( Obj ) RESULT( Ans )
  CLASS( FEVariable_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION Shape_Obj
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE Shape_Obj
END INTERFACE Shape

PUBLIC :: Shape

!----------------------------------------------------------------------------
!                                                       getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Scalar_Constant( Obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: Obj
  TYPE( FEVariableScalar_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableConstant_ ), INTENT( IN ) :: VarType
  REAL( DFP ) :: Val
END FUNCTION Scalar_Constant
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Scalar_Constant
END INTERFACE getValues

PUBLIC :: getValues

!----------------------------------------------------------------------------
!                                                   getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Scalar_Space( Obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: Obj
  TYPE( FEVariableScalar_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpace_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( : )
END FUNCTION Scalar_Space
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Scalar_Space
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                   getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Scalar_SpaceTime( Obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: Obj
  TYPE( FEVariableScalar_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpaceTime_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( :, : )
END FUNCTION Scalar_SpaceTime
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Scalar_SpaceTime
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                   getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Vector_Constant( Obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: Obj
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableConstant_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( : )
END FUNCTION Vector_Constant
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Vector_Constant
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                   getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Vector_Space( Obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: Obj
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpace_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( :, :  )
END FUNCTION Vector_Space
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Vector_Space
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                   getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Vector_SpaceTime( Obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: Obj
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpaceTime_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( :, :, :  )
END FUNCTION Vector_SpaceTime
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Vector_SpaceTime
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                   getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Matrix_Constant( Obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: Obj
  TYPE( FEVariableMatrix_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableConstant_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( :, : )
END FUNCTION Matrix_Constant
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Matrix_Constant
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                   getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Matrix_Space( Obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: Obj
  TYPE( FEVariableMatrix_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpace_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( :, :, :  )
END FUNCTION Matrix_Space
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Matrix_Space
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                   getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Matrix_SpaceTime( Obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: Obj
  TYPE( FEVariableMatrix_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpaceTime_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( :, :, :, : )
END FUNCTION Matrix_SpaceTime
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Matrix_SpaceTime
END INTERFACE getValues

END MODULE FEVariable_Method
