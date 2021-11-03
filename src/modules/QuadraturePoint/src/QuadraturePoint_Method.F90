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

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiates the quadrature points

INTERFACE
MODULE PURE SUBROUTINE quad_initiate1( obj, Points )
  CLASS( QuadraturePoint_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Points( :, : )
    !! Points contains the quadrature points and weights
    !! Points( :, ipoint ) contains quadrature points and weights of ipoint
    !! quadrature point. The last row contains the weight. The rest of the
    !! rows contains the coordinates of quadrature.
END SUBROUTINE quad_initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiates the quadrature points

INTERFACE
MODULE PURE SUBROUTINE quad_initiate2( obj, tXi, tPoints )
  CLASS( QuadraturePoint_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tXi
    !! Total number of xidimension
  INTEGER( I4B ), INTENT( IN ) :: tPoints
    !! Total number quadrature points
END SUBROUTINE quad_initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiates the quadrature points

INTERFACE
MODULE PURE SUBROUTINE quad_initiate3( obj, refElem, order, QuadratureType )
  TYPE( QuadraturePoint_ ), INTENT( INOUT ) :: obj
    !! Total number of xidimension
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refElem
    !! Reference element
  INTEGER( I4B ), INTENT( IN ) :: order
    !! Order of integrand
  CHARACTER( LEN = * ), INTENT( IN ) :: quadratureType
    !! Total number quadrature points
END SUBROUTINE quad_initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiates the quadrature points

INTERFACE
MODULE PURE SUBROUTINE quad_initiate4( obj, refElem, NIPS, QuadratureType )
  TYPE( QuadraturePoint_ ), INTENT( INOUT ) :: obj
    !! Total number of xidimension
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refElem
    !! Reference element
  INTEGER( I4B ), INTENT( IN ) :: NIPS(1)
    !! Order of integrand
  CHARACTER( LEN = * ), INTENT( IN ) :: quadratureType
    !! Total number quadrature points
END SUBROUTINE quad_initiate4
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE quad_initiate1, quad_initiate2, quad_initiate3, &
    & quad_initiate4
END INTERFACE

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                              QuadraturePoint@Constructure
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate an instance of quadrature points

INTERFACE
MODULE PURE FUNCTION quad_Constructor1( Points ) RESULT( obj )
  TYPE( QuadraturePoint_ ) :: obj
  REAL( DFP ), INTENT( IN ) :: Points( :, : )
END FUNCTION quad_Constructor1
END INTERFACE

INTERFACE QuadraturePoint
  MODULE PROCEDURE quad_Constructor1
END INTERFACE QuadraturePoint


PUBLIC :: QuadraturePoint

!----------------------------------------------------------------------------
!                                        QuadraturePoint_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns a pointer to a newly created instance of quadrature points

INTERFACE
MODULE PURE FUNCTION quad_Constructor_1( Points ) RESULT( obj )
  CLASS( QuadraturePoint_ ), POINTER :: obj
  REAL( DFP ), INTENT( IN ) :: Points( :, : )
END FUNCTION quad_Constructor_1
END INTERFACE

INTERFACE QuadraturePoint_Pointer
  MODULE PROCEDURE quad_Constructor_1
END INTERFACE QuadraturePoint_Pointer

PUBLIC :: QuadraturePoint_Pointer

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Deallocates the data stored inside the quadrature point

INTERFACE
MODULE PURE SUBROUTINE quad_DeallocateData(obj)
  CLASS( QuadraturePoint_ ), INTENT( INOUT ) :: obj
END SUBROUTINE quad_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE quad_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                           SIZE@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns the size of obj%points,

INTERFACE
MODULE PURE FUNCTION quad_Size( obj, dims ) RESULT( Ans )
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dims
  INTEGER( I4B ) :: Ans
END FUNCTION quad_Size
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE quad_Size
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                       getTotalQuadraturePoints@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns total number of quadrature points

INTERFACE
MODULE PURE FUNCTION quad_getTotalQuadraturePoints( obj, dims ) RESULT( Ans )
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dims
  INTEGER( I4B ) :: Ans
END FUNCTION quad_getTotalQuadraturePoints
END INTERFACE

INTERFACE getTotalQuadraturePoints
  MODULE PROCEDURE quad_getTotalQuadraturePoints
END INTERFACE getTotalQuadraturePoints

PUBLIC :: getTotalQuadraturePoints

!----------------------------------------------------------------------------
!                                             GetQuadraturePoint@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE quad_GetQuadraturePoints1( obj, Point, Weight, Num )
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: Point( 3 )
  REAL( DFP ), INTENT( INOUT ) :: Weight
  INTEGER( I4B ), INTENT( IN ) :: Num
END SUBROUTINE quad_GetQuadraturePoints1
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetQuadraturePoint@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE quad_GetQuadraturePoints2( obj, Point, Weight )
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Point( :, : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Weight( : )
END SUBROUTINE quad_GetQuadraturePoints2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetQuadraturePoint@Constructor
!----------------------------------------------------------------------------

INTERFACE GetQuadraturePoints
  MODULE PROCEDURE quad_GetQuadraturePoints1, quad_GetQuadraturePoints2
END INTERFACE

PUBLIC :: GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                               Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary:  Display the content of quadrature point

INTERFACE
MODULE SUBROUTINE quad_Display( obj, msg, unitno )
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitno
END SUBROUTINE quad_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE quad_Display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                      GaussLegendreQuadrature@GaussLegendre
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: 	Returns the Gauss Legendre Quadrature points based on given order

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

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: 	Returns the Gauss-Legendre Quadrature points

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQP2( RefElem, NIPS ) RESULT( obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
    !! number of integration points
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

END MODULE QuadraturePoint_Method
