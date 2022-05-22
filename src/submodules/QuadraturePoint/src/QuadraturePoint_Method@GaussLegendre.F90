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
MODULE PURE FUNCTION getGaussLegendreQPLine1( refelem, Order ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceLine_ ), INTENT( IN ) :: refelem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPLine1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPLine2( refelem, NIPS ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferenceLine_ ), INTENT( IN ) :: refelem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPLine2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTriangle1( refelem, Order ) RESULT(obj)
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPTriangle1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPQuadrangle1( refelem, Order)RESULT(obj)
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceQuadrangle_ ), INTENT( IN ) :: refelem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPQuadrangle1

MODULE PURE FUNCTION getGaussLegendreQPQuadrangle2( refelem, NIPS)RESULT(obj)
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferenceQuadrangle_ ), INTENT( IN ) :: refelem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPQuadrangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTriangle2( refelem, NIPS ) RESULT(obj)
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPTriangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTetrahedron1( refelem, Order)RESULT(obj)
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceTetrahedron_ ), INTENT( IN ) :: refelem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPTetrahedron1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTetrahedron2( refelem, NIPS)RESULT(obj)
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferenceTetrahedron_ ), INTENT( IN ) :: refelem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPTetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPyramid1( refelem, Order)RESULT(obj)
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferencePyramid_ ), INTENT( IN ) :: refelem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPPyramid1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPyramid2( refelem, NIPS)RESULT(obj)
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferencePyramid_ ), INTENT( IN ) :: refelem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPPyramid2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPrism1( refelem, Order ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferencePrism_ ), INTENT( IN ) :: refelem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPPrism1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPrism2( refelem, NIPS ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferencePrism_ ), INTENT( IN ) :: refelem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPPrism2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPHexahedron1( refelem, Order ) &
    & RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceHexahedron_ ), INTENT( IN ) :: refelem
  TYPE( QuadraturePoint_ ) :: obj
END FUNCTION getGaussLegendreQPHexahedron1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPHexahedron2( refelem, NIPS ) &
    & RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferenceHexahedron_ ), INTENT( IN ) :: refelem
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
  SELECT TYPE( refelem )
  TYPE IS ( ReferenceLine_ )
    obj = getGaussLegendreQPLine1( refelem = refelem, Order = Order )

  TYPE IS ( ReferenceTriangle_ )
    obj = getGaussLegendreQPTriangle1( refelem = refelem, Order = Order )

  TYPE IS ( ReferenceQuadrangle_ )
    obj = getGaussLegendreQPQuadrangle1( refelem = refelem, Order = Order )

  TYPE IS ( ReferenceTetrahedron_ )
    obj = getGaussLegendreQPTetrahedron1( refelem = refelem, Order = Order )

  TYPE IS ( ReferenceHexahedron_ )
    obj = getGaussLegendreQPHexahedron1( refelem = refelem, Order = Order )

  TYPE IS ( ReferencePrism_ )
    obj = getGaussLegendreQPPrism1( refelem = refelem, Order = Order )

  TYPE IS ( ReferencePyramid_ )
    obj = getGaussLegendreQPPyramid1( refelem = refelem, Order = Order )
  END SELECT
END PROCEDURE getGaussLegendreQP1

!----------------------------------------------------------------------------
!                                                    GaussLegendreQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQP2
  SELECT TYPE( refelem )
  TYPE IS ( ReferenceLine_ )
    obj = getGaussLegendreQPLine2( refelem = refelem, NIPS = NIPS )

  TYPE IS ( ReferenceTriangle_ )
    obj = getGaussLegendreQPTriangle2( refelem = refelem, NIPS = NIPS )

  TYPE IS ( ReferenceQuadrangle_ )
    obj = getGaussLegendreQPQuadrangle2( refelem = refelem, NIPS = NIPS )

  TYPE IS ( ReferenceTetrahedron_ )
    obj = getGaussLegendreQPTetrahedron2( refelem = refelem, NIPS = NIPS )

  TYPE IS ( ReferenceHexahedron_ )
    obj = getGaussLegendreQPHexahedron2( refelem = refelem, NIPS = NIPS )

  TYPE IS ( ReferencePrism_ )
    obj = getGaussLegendreQPPrism2( refelem = refelem, NIPS = NIPS )

  TYPE IS ( ReferencePyramid_ )
    obj = getGaussLegendreQPPyramid2( refelem = refelem, NIPS = NIPS )
  END SELECT
END PROCEDURE getGaussLegendreQP2

END SUBMODULE GaussLegendre