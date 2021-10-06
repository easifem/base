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

SUBMODULE(ElemShapeData_Method) H1Lagrange
IMPLICIT NONE

!----------------------------------------------------------------------------
!                                                    Initiate@LineH1Lagrange
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	5 March 2021
! summary: This subroutine computes the shape functions given by Lagrange polynomials over reference line element
!
!# Introduction
!
! * This subroutine computes shape functions, lagrange polynomials, over reference line element
! * The interpolation functions are defined inside the reference element itself
! * The order of shape functions are also included inside the refelem

INTERFACE
MODULE PURE SUBROUTINE Line_H1_Lagrange( obj, Quad, RefElem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
    !! Quadrature points where shapefunctions will be evaluated
  CLASS( ReferenceLine_ ), INTENT( IN ) :: RefElem
    !! Reference element where shape functions will be defined
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
    !! H1 (nodal) Continuity type
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
    !! Lagrange polynomial will be used for interpolation
END SUBROUTINE Line_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@TriangleH1Lagrange
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	5 March 2021
! summary: This subroutine computes the shape functions given by Lagrange polynomials over reference triangle element

INTERFACE
MODULE PURE SUBROUTINE Triangle_H1_Lagrange( obj, Quad, RefElem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: RefElem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Triangle_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@QuadrangleH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Quadrangle_H1_Lagrange( obj, Quad, RefElem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceQuadrangle_ ), INTENT( IN ) :: RefElem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Quadrangle_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@TetrahedronH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Tetrahedron_H1_Lagrange( obj, Quad, RefElem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceTetrahedron_ ), INTENT( IN ) :: RefElem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Tetrahedron_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@HexahedronH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Hexahedron_H1_Lagrange( obj, Quad, RefElem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: RefElem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Hexahedron_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@PrismH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Prism_H1_Lagrange( obj, Quad, RefElem, &
  & ContinuityType, InterpolType)
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferencePrism_ ), INTENT( IN ) :: RefElem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Prism_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@PyramidH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Pyramid_H1_Lagrange( obj, Quad, RefElem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferencePyramid_ ), INTENT( IN ) :: RefElem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Pyramid_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE H1_Lagrange
  SELECT TYPE( refelem )
  TYPE IS ( ReferenceLine_ )
    CALL Line_H1_Lagrange( obj=obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  TYPE IS ( ReferenceTriangle_ )
    CALL Triangle_H1_Lagrange( obj=obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  TYPE IS ( ReferenceQuadrangle_ )
    CALL Quadrangle_H1_Lagrange( obj=obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  TYPE IS ( ReferenceTetrahedron_ )
    CALL Tetrahedron_H1_Lagrange( obj=obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  TYPE IS ( ReferenceHexahedron_ )
    CALL Hexahedron_H1_Lagrange( obj=obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  TYPE IS ( ReferencePrism_ )
    CALL Prism_H1_Lagrange( obj=obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  TYPE IS ( ReferencePyramid_ )
    CALL Pyramid_H1_Lagrange( obj=obj, Quad=Quad, RefElem=RefElem, &
      & ContinuityType=ContinuityType, InterpolType=InterpolType )
  END SELECT
END PROCEDURE H1_Lagrange

END SUBMODULE H1Lagrange