! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
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
! along with this program.  If not, see <https: //www.gnu.org/licenses/>

MODULE FEVariable_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE Display_obj( obj, Msg, UnitNo )
  TYPE( FEVariable_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE Display_obj
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE Display_obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE fe_deallocate( obj )
  TYPE( FEVariable_ ), INTENT( INOUT) :: obj
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
MODULE PURE FUNCTION Nodal_Scalar_Constant( Val, Rank, VarType ) &
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
MODULE PURE FUNCTION Nodal_Scalar_Space( Val, Rank, VarType ) RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
MODULE PURE FUNCTION Nodal_Scalar_SpaceTime( Val, Rank, VarType ) RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
MODULE PURE FUNCTION Nodal_Vector_Constant( Val, Rank, VarType ) &
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
MODULE PURE FUNCTION Nodal_Vector_Space( Val, Rank, VarType ) RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
MODULE PURE FUNCTION Nodal_Matrix_Constant( Val, Rank, VarType ) &
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
MODULE PURE FUNCTION Nodal_Matrix_Space( Val, Rank, VarType ) RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
MODULE PURE FUNCTION Quadrature_Scalar_Constant( Val, Rank, VarType ) &
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
MODULE PURE FUNCTION Quadrature_Vector_Constant( Val, Rank, VarType ) &
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableConstant_ ), INTENT( IN ) :: VarType
END FUNCTION Quadrature_Vector_Constant
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Vector_Constant
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                             QuadratureVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Quadrature_Vector_Space( Val, Rank, VarType ) &
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpace_ ), INTENT( IN ) :: VarType
END FUNCTION Quadrature_Vector_Space
END INTERFACE

INTERFACE QuadratureVariable
  MODULE PROCEDURE Quadrature_Vector_Space
END INTERFACE QuadratureVariable

!----------------------------------------------------------------------------
!                                             QuadratureVariable@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Quadrature_Vector_SpaceTime( Val, Rank, VarType ) &
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
MODULE PURE FUNCTION Quadrature_Matrix_Constant( Val, Rank, VarType ) &
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
  & RESULT( obj )
  TYPE( FEVariable_ ) :: obj
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
MODULE PURE FUNCTION Size_obj( obj, Dim ) RESULT( Ans )
  CLASS( FEVariable_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Dim
  INTEGER( I4B ) :: Ans
END FUNCTION Size_obj
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE Size_obj
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                                            SHAPE@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Shape_obj( obj ) RESULT( Ans )
  CLASS( FEVariable_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION Shape_obj
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE Shape_obj
END INTERFACE Shape

PUBLIC :: Shape

!----------------------------------------------------------------------------
!                                                       getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Scalar_Constant( obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: obj
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
!                                                       getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Scalar_Space( obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: obj
  TYPE( FEVariableScalar_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpace_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( : )
END FUNCTION Scalar_Space
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Scalar_Space
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                       getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Scalar_SpaceTime( obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: obj
  TYPE( FEVariableScalar_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpaceTime_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( :, : )
END FUNCTION Scalar_SpaceTime
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Scalar_SpaceTime
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                       getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Vector_Constant( obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: obj
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableConstant_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( : )
END FUNCTION Vector_Constant
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Vector_Constant
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                        getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Vector_Space( obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: obj
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpace_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( :, :  )
END FUNCTION Vector_Space
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Vector_Space
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                       getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Vector_SpaceTime( obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: obj
  TYPE( FEVariableVector_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpaceTime_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( :, :, :  )
END FUNCTION Vector_SpaceTime
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Vector_SpaceTime
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                       getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Matrix_Constant( obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: obj
  TYPE( FEVariableMatrix_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableConstant_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( :, : )
END FUNCTION Matrix_Constant
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Matrix_Constant
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                       getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Matrix_Space( obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: obj
  TYPE( FEVariableMatrix_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpace_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( :, :, :  )
END FUNCTION Matrix_Space
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Matrix_Space
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                       getValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Matrix_SpaceTime( obj, Rank, VarType ) RESULT( Val )
  CLASS( FEVariable_ ), INTENT( IN ) :: obj
  TYPE( FEVariableMatrix_ ), INTENT( IN ) :: Rank
  TYPE( FEVariableSpaceTime_ ), INTENT( IN ) :: VarType
  REAL( DFP ), ALLOCATABLE :: Val( :, :, :, : )
END FUNCTION Matrix_SpaceTime
END INTERFACE

INTERFACE getValues
  MODULE PROCEDURE Matrix_SpaceTime
END INTERFACE getValues

END MODULE FEVariable_Method
