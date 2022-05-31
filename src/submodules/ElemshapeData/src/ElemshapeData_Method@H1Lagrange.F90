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

!> authors: Vikas Sharma, Ph. D.
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
MODULE PURE SUBROUTINE Line_H1_Lagrange( obj, Quad, refelem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
    !! Quadrature points where shapefunctions will be evaluated
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
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
! summary: Returns the shape functions given by Lagrange polynomials

INTERFACE
MODULE PURE SUBROUTINE Triangle_H1_Lagrange( obj, Quad, refelem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Triangle_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@QuadrangleH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Quadrangle_H1_Lagrange( obj, Quad, refelem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Quadrangle_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@TetrahedronH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Tetrahedron_H1_Lagrange( obj, Quad, refelem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Tetrahedron_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@HexahedronH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Hexahedron_H1_Lagrange( obj, Quad, refelem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Hexahedron_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@PrismH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Prism_H1_Lagrange( obj, Quad, refelem, &
  & ContinuityType, InterpolType)
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Prism_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@PyramidH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Pyramid_H1_Lagrange( obj, Quad, refelem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
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
  !!
  !!
  !!
  SELECT TYPE( refelem )
  !!
  !! Line
  !!
  TYPE IS ( ReferenceLine_ )
    ALLOCATE( ReferenceLine_ :: obj%refelem )
    CALL Line_H1_Lagrange( &
      & obj=obj, &
      & Quad=Quad, &
      & refelem=refelem, &
      & ContinuityType=ContinuityType, &
      & InterpolType=InterpolType )
  !!
  !! Triangle
  !!
  TYPE IS ( ReferenceTriangle_ )
    ALLOCATE( ReferenceTriangle_ :: obj%refelem )
    CALL Triangle_H1_Lagrange( &
      & obj=obj, &
      & Quad=Quad, &
      & refelem=refelem, &
      & ContinuityType=ContinuityType, &
      & InterpolType=InterpolType )
  !!
  !! Quadrangle
  !!
  TYPE IS ( ReferenceQuadrangle_ )
    ALLOCATE( ReferenceQuadrangle_ :: obj%refelem )
    CALL Quadrangle_H1_Lagrange( &
      & obj=obj, &
      & Quad=Quad, &
      & refelem=refelem, &
      & ContinuityType=ContinuityType, &
      & InterpolType=InterpolType )
  !!
  !! Tetrahedron
  !!
  TYPE IS ( ReferenceTetrahedron_ )
    ALLOCATE( ReferenceTetrahedron_ :: obj%refelem )
    CALL Tetrahedron_H1_Lagrange( &
      & obj=obj, &
      & Quad=Quad, &
      & refelem=refelem, &
      & ContinuityType=ContinuityType, &
      & InterpolType=InterpolType )
  !!
  !! Hexahedron
  !!
  TYPE IS ( ReferenceHexahedron_ )
    ALLOCATE( ReferenceHexahedron_ :: obj%refelem )
    CALL Hexahedron_H1_Lagrange( &
      & obj=obj, &
      & Quad=Quad, &
      & refelem=refelem, &
      & ContinuityType=ContinuityType, &
      & InterpolType=InterpolType )
  !!
  !! Prism
  !!
  TYPE IS ( ReferencePrism_ )
    ALLOCATE( ReferencePrism_ :: obj%refelem )
    CALL Prism_H1_Lagrange( &
      & obj=obj, &
      & Quad=Quad, &
      & refelem=refelem, &
      & ContinuityType=ContinuityType, &
      & InterpolType=InterpolType )
  !!
  !! Pyramid
  !!
  TYPE IS ( ReferencePyramid_ )
    ALLOCATE( ReferencePyramid_ :: obj%refelem )
    CALL Pyramid_H1_Lagrange( &
      & obj=obj, &
      & Quad=Quad, &
      & refelem=refelem, &
      & ContinuityType=ContinuityType, &
      & InterpolType=InterpolType )
  !!
  !! ReferenceElement
  !!
  TYPE IS ( ReferenceElement_ )
  !!
  !! Line
  !!
    IF( isLine( refelem%name ) ) THEN
      ALLOCATE( ReferenceLine_ :: obj%refelem )
      CALL Line_H1_Lagrange( &
        & obj=obj, &
        & Quad=Quad, &
        & refelem=refelem, &
        & ContinuityType=ContinuityType, &
        & InterpolType=InterpolType )
      RETURN
    END IF
  !!
  !! Triangle
  !!
    IF( isTriangle( refelem%name ) ) THEN
      ALLOCATE( ReferenceTriangle_ :: obj%refelem )
      CALL Triangle_H1_Lagrange( &
        & obj=obj, &
        & Quad=Quad, &
        & refelem=refelem, &
        & ContinuityType=ContinuityType, &
        & InterpolType=InterpolType )
      RETURN
    END IF
  !!
  !! Quadrangle
  !!
    IF( isQuadrangle( refelem%name ) ) THEN
      ALLOCATE( ReferenceQuadrangle_ :: obj%refelem )
      CALL Quadrangle_H1_Lagrange( &
        & obj=obj, &
        & Quad=Quad, &
        & refelem=refelem, &
        & ContinuityType=ContinuityType, &
        & InterpolType=InterpolType )
      RETURN
    END IF
  !!
  !! Tetrahedron
  !!
    IF( isTetrahedron( refelem%name ) ) THEN
      ALLOCATE( ReferenceTetrahedron_ :: obj%refelem )
      CALL Tetrahedron_H1_Lagrange( &
        & obj=obj, &
        & Quad=Quad, &
        & refelem=refelem, &
        & ContinuityType=ContinuityType, &
        & InterpolType=InterpolType )
      RETURN
    END IF
  !!
  !! Hexahedron
  !!
    IF( isHexahedron( refelem%name ) ) THEN
      ALLOCATE( ReferenceHexahedron_ :: obj%refelem )
      CALL Hexahedron_H1_Lagrange( &
        & obj=obj, &
        & Quad=Quad, &
        & refelem=refelem, &
        & ContinuityType=ContinuityType, &
        & InterpolType=InterpolType )
      RETURN
    END IF
  !!
  !! Prism
  !!
    IF( isPrism( refelem%name ) ) THEN
      ALLOCATE( ReferencePrism_ :: obj%refelem )
      CALL Prism_H1_Lagrange( &
        & obj=obj, &
        & Quad=Quad, &
        & refelem=refelem, &
        & ContinuityType=ContinuityType, &
        & InterpolType=InterpolType )
      RETURN
    END IF
  !!
  !! Pyramid
  !!
    IF( isPyramid( refelem%name ) ) THEN
      ALLOCATE( ReferencePyramid_ :: obj%refelem )
      CALL Pyramid_H1_Lagrange( &
        & obj=obj, &
        & Quad=Quad, &
        & refelem=refelem, &
        & ContinuityType=ContinuityType, &
        & InterpolType=InterpolType )
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