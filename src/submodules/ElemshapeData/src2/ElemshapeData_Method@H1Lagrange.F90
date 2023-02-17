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
USE BaseMethod
IMPLICIT NONE

!----------------------------------------------------------------------------
!                                                    Initiate@LineH1Lagrange
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	5 March 2021
! summary: returns the shape functions given by Lagrange polynomials
!
!# Introduction
!
! This subroutine computes shape functions, lagrange polynomials, over
! reference line element. The interpolation functions are defined inside the
! reference element itself. The order of shape functions are also included
! inside the refelem

INTERFACE
MODULE PURE SUBROUTINE Line_H1_Lagrange( obj, quad, refelem, &
  & continuityType, interpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: quad
    !! Quadrature points where shapefunctions will be evaluated
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
    !! Reference element where shape functions will be defined
  CLASS( H1_ ), INTENT( IN ) :: continuityType
    !! H1 (nodal) Continuity type
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: interpolType
    !! Lagrange polynomial will be used for interpolation
END SUBROUTINE Line_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@TriangleH1Lagrange
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	5 March 2021
! summary: Returns the shape functions given by Lagrange polynomials

INTERFACE
MODULE PURE SUBROUTINE Triangle_H1_Lagrange( obj, quad, refelem, &
  & continuityType, interpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  CLASS( H1_ ), INTENT( IN ) :: continuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: interpolType
END SUBROUTINE Triangle_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@QuadrangleH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Quadrangle_H1_Lagrange( obj, quad, refelem, &
  & continuityType, interpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  CLASS( H1_ ), INTENT( IN ) :: continuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: interpolType
END SUBROUTINE Quadrangle_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@TetrahedronH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Tetrahedron_H1_Lagrange( obj, quad, refelem, &
  & continuityType, interpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  CLASS( H1_ ), INTENT( IN ) :: continuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: interpolType
END SUBROUTINE Tetrahedron_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@HexahedronH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Hexahedron_H1_Lagrange( obj, quad, refelem, &
  & continuityType, interpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  CLASS( H1_ ), INTENT( IN ) :: continuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: interpolType
END SUBROUTINE Hexahedron_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@PrismH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Prism_H1_Lagrange( obj, quad, refelem, &
  & continuityType, interpolType)
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  CLASS( H1_ ), INTENT( IN ) :: continuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: interpolType
END SUBROUTINE Prism_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@PyramidH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Pyramid_H1_Lagrange( obj, quad, refelem, &
  & continuityType, interpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  CLASS( H1_ ), INTENT( IN ) :: continuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: interpolType
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
  !!
  !!
  !!
  SELECT TYPE( refelem )
  !!
  !! Line
  !!
  TYPE IS ( ReferenceLine_ )
    CALL Line_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType )
  !!
  !! Triangle
  !!
  TYPE IS ( ReferenceTriangle_ )
    CALL Triangle_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType )
  !!
  !! Quadrangle
  !!
  TYPE IS ( ReferenceQuadrangle_ )
    CALL Quadrangle_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType )
  !!
  !! Tetrahedron
  !!
  TYPE IS ( ReferenceTetrahedron_ )
    CALL Tetrahedron_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType )
  !!
  !! Hexahedron
  !!
  TYPE IS ( ReferenceHexahedron_ )
    CALL Hexahedron_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType )
  !!
  !! Prism
  !!
  TYPE IS ( ReferencePrism_ )
    CALL Prism_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType )
  !!
  !! Pyramid
  !!
  TYPE IS ( ReferencePyramid_ )
    CALL Pyramid_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType )
  !!
  !! ReferenceElement
  !!
  TYPE IS ( ReferenceElement_ )
  !!
  !! Line
  !!
    IF( isLine( refelem%name ) ) THEN
      CALL Line_H1_Lagrange( &
        & obj=obj, &
        & quad=quad, &
        & refelem=refelem, &
        & continuityType=continuityType, &
        & interpolType=interpolType )
      RETURN
    END IF
  !!
  !! Triangle
  !!
    IF( isTriangle( refelem%name ) ) THEN
      CALL Triangle_H1_Lagrange( &
        & obj=obj, &
        & quad=quad, &
        & refelem=refelem, &
        & continuityType=continuityType, &
        & interpolType=interpolType )
      RETURN
    END IF
  !!
  !! Quadrangle
  !!
    IF( isQuadrangle( refelem%name ) ) THEN
      CALL Quadrangle_H1_Lagrange( &
        & obj=obj, &
        & quad=quad, &
        & refelem=refelem, &
        & continuityType=continuityType, &
        & interpolType=interpolType )
      RETURN
    END IF
  !!
  !! Tetrahedron
  !!
    IF( isTetrahedron( refelem%name ) ) THEN
      CALL Tetrahedron_H1_Lagrange( &
        & obj=obj, &
        & quad=quad, &
        & refelem=refelem, &
        & continuityType=continuityType, &
        & interpolType=interpolType )
      RETURN
    END IF
  !!
  !! Hexahedron
  !!
    IF( isHexahedron( refelem%name ) ) THEN
      CALL Hexahedron_H1_Lagrange( &
        & obj=obj, &
        & quad=quad, &
        & refelem=refelem, &
        & continuityType=continuityType, &
        & interpolType=interpolType )
      RETURN
    END IF
  !!
  !! Prism
  !!
    IF( isPrism( refelem%name ) ) THEN
      CALL Prism_H1_Lagrange( &
        & obj=obj, &
        & quad=quad, &
        & refelem=refelem, &
        & continuityType=continuityType, &
        & interpolType=interpolType )
      RETURN
    END IF
  !!
  !! Pyramid
  !!
    IF( isPyramid( refelem%name ) ) THEN
      CALL Pyramid_H1_Lagrange( &
        & obj=obj, &
        & quad=quad, &
        & refelem=refelem, &
        & continuityType=continuityType, &
        & interpolType=interpolType )
      RETURN
    END IF
  !!
  !!
  !!
  END SELECT
  !!
  !!
  !!
END PROCEDURE H1_Lagrange

END SUBMODULE H1Lagrange