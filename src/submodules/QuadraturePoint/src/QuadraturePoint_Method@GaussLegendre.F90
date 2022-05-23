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
!

SUBMODULE(QuadraturePoint_Method) GaussLegendre
USE BaseMethod
IMPLICIT NONE

#include "./GaussLegendre/QuadDataLine.inc"
#include "./GaussLegendre/QuadDataTriangle.inc"

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPLine1( refelem, order )  &
  & RESULT( obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: order
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPLine1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPLine2( refelem, nips )  &
  & RESULT( obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: nips( 1 )
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPLine2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTriangle1( refelem, order ) &
  & RESULT(obj)
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: order
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPTriangle1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPQuadrangle1( refelem, order) &
  & RESULT(obj)
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: order
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPQuadrangle1

MODULE PURE FUNCTION getGaussLegendreQPQuadrangle2( refelem, nips) &
  & RESULT(obj)
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: nips( 1 )
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPQuadrangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTriangle2( refelem, nips ) &
  &  RESULT(obj)
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: nips( 1 )
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPTriangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTetrahedron1( refelem, order) &
  & RESULT(obj)
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: order
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPTetrahedron1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTetrahedron2( refelem, nips) &
  & RESULT(obj)
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: nips( 1 )
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPTetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPyramid1( refelem, order) &
  & RESULT(obj)
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: order
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPPyramid1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPyramid2( refelem, nips) &
  & RESULT(obj)
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: nips( 1 )
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPPyramid2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPrism1( refelem, order ) &
  & RESULT( obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: order
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPPrism1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPrism2( refelem, nips ) &
  & RESULT( obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: nips( 1 )
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPPrism2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPHexahedron1( refelem, order ) &
    & RESULT( obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: order
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPHexahedron1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPHexahedron2( refelem, nips ) &
    & RESULT( obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  INTEGER( I4B ), INTENT( IN ) :: nips( 1 )
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPHexahedron2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                    GaussLegendreQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQP1
  !!
  SELECT TYPE( refelem )
  !!
  TYPE IS ( ReferenceLine_ )
    obj = getGaussLegendreQPLine1( refelem = refelem, order = order )
  !!
  TYPE IS ( ReferenceTriangle_ )
    obj = getGaussLegendreQPTriangle1( refelem = refelem, order = order )
  !!
  TYPE IS ( ReferenceQuadrangle_ )
    obj = getGaussLegendreQPQuadrangle1( refelem = refelem, order = order )
  !!
  TYPE IS ( ReferenceTetrahedron_ )
    obj = getGaussLegendreQPTetrahedron1( refelem = refelem, order = order )
  !!
  TYPE IS ( ReferenceHexahedron_ )
    obj = getGaussLegendreQPHexahedron1( refelem = refelem, order = order )
  !!
  TYPE IS ( ReferencePrism_ )
    obj = getGaussLegendreQPPrism1( refelem = refelem, order = order )
  !!
  TYPE IS ( ReferencePyramid_ )
    obj = getGaussLegendreQPPyramid1( refelem = refelem, order = order )
  !!
  TYPE IS ( ReferenceElement_ )
    !!
    IF( isLine( refelem%name ) ) THEN
      obj = getGaussLegendreQPLine1( refelem=refelem, order=order )
      RETURN
    END IF
    !!
    IF( isTriangle( refelem%name ) ) THEN
      obj = getGaussLegendreQPTriangle1( refelem=refelem, order=order )
      RETURN
    END IF
    !!
    IF( isQuadrangle( refelem%name ) ) THEN
      obj = getGaussLegendreQPQuadrangle1( refelem=refelem, order=order )
      RETURN
    END IF
    !!
    IF( isTetrahedron( refelem%name ) ) THEN
      obj = getGaussLegendreQPTetrahedron1( refelem=refelem, order=order )
      RETURN
    END IF
    !!
    IF( isHexahedron( refelem%name ) ) THEN
      obj = getGaussLegendreQPHexahedron1( refelem=refelem, order=order )
      RETURN
    END IF
    !!
    IF( isPrism( refelem%name ) ) THEN
      obj = getGaussLegendreQPPrism1( refelem=refelem, order=order )
      RETURN
    END IF
    !!
    IF( isPyramid( refelem%name ) ) THEN
      obj = getGaussLegendreQPPyramid1( refelem=refelem, order=order )
      RETURN
    END IF
  !!
  END SELECT
  !!
END PROCEDURE getGaussLegendreQP1

!----------------------------------------------------------------------------
!                                                    GaussLegendreQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQP2
  !!
  SELECT TYPE( refelem )
  !!
  TYPE IS ( ReferenceLine_ )
    obj = getGaussLegendreQPLine2( refelem = refelem, nips = nips )
  !!
  TYPE IS ( ReferenceTriangle_ )
    obj = getGaussLegendreQPTriangle2( refelem = refelem, nips = nips )
  !!
  TYPE IS ( ReferenceQuadrangle_ )
    obj = getGaussLegendreQPQuadrangle2( refelem = refelem, nips = nips )
  !!
  TYPE IS ( ReferenceTetrahedron_ )
    obj = getGaussLegendreQPTetrahedron2( refelem = refelem, nips = nips )
  !!
  TYPE IS ( ReferenceHexahedron_ )
    obj = getGaussLegendreQPHexahedron2( refelem = refelem, nips = nips )
  !!
  TYPE IS ( ReferencePrism_ )
    obj = getGaussLegendreQPPrism2( refelem = refelem, nips = nips )
  !!
  TYPE IS ( ReferencePyramid_ )
    obj = getGaussLegendreQPPyramid2( refelem = refelem, nips = nips )
  !!
  TYPE IS ( ReferenceElement_ )
    !!
    IF( isLine( refelem%name ) ) THEN
      obj = getGaussLegendreQPLine2( refelem=refelem, nips=nips )
      RETURN
    END IF
    !!
    IF( isTriangle( refelem%name ) ) THEN
      obj = getGaussLegendreQPTriangle2( refelem=refelem, nips=nips )
      RETURN
    END IF
    !!
    IF( isQuadrangle( refelem%name ) ) THEN
      obj = getGaussLegendreQPQuadrangle2( refelem=refelem, nips=nips )
      RETURN
    END IF
    !!
    IF( isTetrahedron( refelem%name ) ) THEN
      obj = getGaussLegendreQPTetrahedron2( refelem=refelem, nips=nips )
      RETURN
    END IF
    !!
    IF( isHexahedron( refelem%name ) ) THEN
      obj = getGaussLegendreQPHexahedron2( refelem=refelem, nips=nips )
      RETURN
    END IF
    !!
    IF( isPrism( refelem%name ) ) THEN
      obj = getGaussLegendreQPPrism2( refelem=refelem, nips=nips )
      RETURN
    END IF
    !!
    IF( isPyramid( refelem%name ) ) THEN
      obj = getGaussLegendreQPPyramid2( refelem=refelem, nips=nips )
      RETURN
    END IF
  !!
  END SELECT
  !!
END PROCEDURE getGaussLegendreQP2

END SUBMODULE GaussLegendre