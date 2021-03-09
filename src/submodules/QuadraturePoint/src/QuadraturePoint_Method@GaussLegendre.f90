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

SUBMODULE( QuadraturePoint_Method ) GaussLegendre
USE BaseMethod
IMPLICIT NONE

#include "./GaussLegendre/QuadDataLine.inc"
#include "./GaussLegendre/QuadDataTriangle.inc"

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPLine1( RefElem, Order ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceLine_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPLine1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPLine2( RefElem, NIPS ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferenceLine_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPLine2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTriangle1( RefElem, Order ) RESULT(Obj)
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceTriangle_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPTriangle1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTriangle2( RefElem, NIPS ) RESULT(Obj)
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferenceTriangle_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPTriangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTetrahedron1( RefElem, Order)RESULT(Obj)
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceTetrahedron_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPTetrahedron1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTetrahedron2( RefElem, NIPS)RESULT(Obj)
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferenceTetrahedron_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPTetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPyramid1( RefElem, Order)RESULT(Obj)
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferencePyramid_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPPyramid1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPyramid2( RefElem, NIPS)RESULT(Obj)
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferencePyramid_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPPyramid2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPrism1( RefElem, Order ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferencePrism_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPPrism1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPrism2( RefElem, NIPS ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferencePrism_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPPrism2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPHexahedron1( RefElem, Order ) &
    & RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: Order
  TYPE( ReferenceHexahedron_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPHexahedron1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPHexahedron2( RefElem, NIPS ) &
    & RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
  TYPE( ReferenceHexahedron_ ), INTENT( IN ) :: RefElem
  TYPE( QuadraturePoint_ ) :: Obj
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
    Obj = getGaussLegendreQPLine1( RefElem = RefElem, Order = Order )

  TYPE IS ( ReferenceTriangle_ )
    Obj = getGaussLegendreQPTriangle1( RefElem = RefElem, Order = Order )

  TYPE IS ( ReferenceQuadrangle_ )
    Obj = getGaussLegendreQPQuadrangle1( RefElem = RefElem, Order = Order )

  TYPE IS ( ReferenceTetrahedron_ )
    Obj = getGaussLegendreQPTetrahedron1( RefElem = RefElem, Order = Order )

  TYPE IS ( ReferenceHexahedron_ )
    Obj = getGaussLegendreQPHexahedron1( RefElem = RefElem, Order = Order )

  TYPE IS ( ReferencePrism_ )
    Obj = getGaussLegendreQPPrism1( RefElem = RefElem, Order = Order )

  TYPE IS ( ReferencePyramid_ )
    Obj = getGaussLegendreQPPyramid1( RefElem = RefElem, Order = Order )
  END SELECT
END PROCEDURE getGaussLegendreQP1

!----------------------------------------------------------------------------
!                                                    GaussLegendreQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQP2
  SELECT TYPE( refelem )
  TYPE IS ( ReferenceLine_ )
    Obj = getGaussLegendreQPLine2( RefElem = RefElem, NIPS = NIPS )

  TYPE IS ( ReferenceTriangle_ )
    Obj = getGaussLegendreQPTriangle2( RefElem = RefElem, NIPS = NIPS )

  TYPE IS ( ReferenceQuadrangle_ )
    Obj = getGaussLegendreQPQuadrangle2( RefElem = RefElem, NIPS = NIPS )

  TYPE IS ( ReferenceTetrahedron_ )
    Obj = getGaussLegendreQPTetrahedron2( RefElem = RefElem, NIPS = NIPS )

  TYPE IS ( ReferenceHexahedron_ )
    Obj = getGaussLegendreQPHexahedron2( RefElem = RefElem, NIPS = NIPS )

  TYPE IS ( ReferencePrism_ )
    Obj = getGaussLegendreQPPrism2( RefElem = RefElem, NIPS = NIPS )

  TYPE IS ( ReferencePyramid_ )
    Obj = getGaussLegendreQPPyramid2( RefElem = RefElem, NIPS = NIPS )
  END SELECT
END PROCEDURE getGaussLegendreQP2

END SUBMODULE GaussLegendre