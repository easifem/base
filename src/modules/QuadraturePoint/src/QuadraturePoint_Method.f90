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

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: This module contains the methods for data type [[QuadraturePoint_]]

MODULE QuadraturePoint_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE initiate_obj( obj, Points )
  CLASS( QuadraturePoint_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Points( :, : )
END SUBROUTINE initiate_obj
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE initiate_obj_txi( obj, tXi, tPoints )
  CLASS( QuadraturePoint_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tXi, tPoints
END SUBROUTINE initiate_obj_txi
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_obj, initiate_obj_txi
END INTERFACE

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                              QuadraturePoint@Constructure
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Constructor1( Points ) RESULT( obj )
  TYPE( QuadraturePoint_ ) :: obj
  REAL( DFP ), INTENT( IN ) :: Points( :, : )
END FUNCTION Constructor1
END INTERFACE

INTERFACE QuadraturePoint
  MODULE PROCEDURE Constructor1
END INTERFACE QuadraturePoint


PUBLIC :: QuadraturePoint

!----------------------------------------------------------------------------
!                                        QuadraturePoint_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Constructor_1( Points ) RESULT( obj )
  CLASS( QuadraturePoint_ ), POINTER :: obj
  REAL( DFP ), INTENT( IN ) :: Points( :, : )
END FUNCTION Constructor_1
END INTERFACE

INTERFACE QuadraturePoint_Pointer
  MODULE PROCEDURE Constructor_1
END INTERFACE QuadraturePoint_Pointer

PUBLIC :: QuadraturePoint_Pointer

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE deallocate_data(obj)
  CLASS( QuadraturePoint_ ), INTENT( INOUT ) :: obj
END SUBROUTINE deallocate_data
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE deallocate_data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                           SIZE@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION size_obj( obj, dims ) RESULT( Ans )
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dims
  INTEGER( I4B ) :: Ans
END FUNCTION size_obj
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE size_obj
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                             GetQuadraturePoint@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE getQP1( obj, Point, Weight, Num )
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: Point( 3 )
  REAL( DFP ), INTENT( INOUT ) :: Weight
  INTEGER( I4B ), INTENT( IN ) :: Num
END SUBROUTINE getQP1
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetQuadraturePoint@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE getQP2( obj, Point, Weight )
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Point( :, : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Weight( : )
END SUBROUTINE getQP2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetQuadraturePoint@Constructor
!----------------------------------------------------------------------------

INTERFACE GetQuadraturePoints
  MODULE PROCEDURE getQP1, getQP2
END INTERFACE

PUBLIC :: GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                               Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE display_obj( obj, msg, unitno )
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitno
END SUBROUTINE display_obj
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE display_obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                      GaussLegendreQuadrature@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQP1( RefElem, Order ) RESULT( obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQP1
END INTERFACE

!----------------------------------------------------------------------------
!                                      GaussLegendreQuadrature@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQP2( RefElem, NIPS ) RESULT( obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQP2
END INTERFACE

!----------------------------------------------------------------------------
!                                      GaussLegendreQuadrature@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE GaussLegendreQuadrature
  MODULE PROCEDURE getGaussLegendreQP1, getGaussLegendreQP2
END INTERFACE GaussLegendreQuadrature

PUBLIC :: GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPQuadrangle1( RefElem, Order)RESULT(obj)
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceQuadrangle_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPQuadrangle1

MODULE PURE FUNCTION getGaussLegendreQPQuadrangle2( RefElem, NIPS)RESULT(obj)
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferenceQuadrangle_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPQuadrangle2
END INTERFACE

END MODULE QuadraturePoint_Method
