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

!> author: Vikas Sharma, Ph. D.
! date: 5 March 2021
! summary: This module contains methods for [[ReferenceQuadrangle_]]

MODULE ReferenceQuadrangle_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
PUBLIC :: Initiate
PUBLIC :: ReferenceQuadrangle
PUBLIC :: ReferenceQuadrangle_Pointer
PUBLIC :: HighorderElement_Quadrangle
PUBLIC :: Measure_Simplex_Quadrangle
PUBLIC :: Quadrangle_Quality
PUBLIC :: Quality_Quadrangle
PUBLIC :: QuadArea3D, QuadrangleArea3D
PUBLIC :: QuadArea2D, QuadrangleArea2D
PUBLIC :: QuadrangleName
PUBLIC :: GetEdgeConnectivity_Quadrangle
PUBLIC :: RefQuadrangleCoord
PUBLIC :: RefCoord_Quadrangle

!----------------------------------------------------------------------------
!                                                       QuadrangleName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-17
! summary:  Returns integer name of quadragle from order

INTERFACE QuadrangleName
  MODULE PURE FUNCTION QuadrangleName1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION QuadrangleName1
END INTERFACE QuadrangleName

!----------------------------------------------------------------------------
!                                                       Initiate@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns linear quadrangle element

INTERFACE Initiate
  MODULE PURE SUBROUTINE initiate_ref_Quadrangle(obj, NSD, xij, domainName)
    CLASS(ReferenceQuadrangle_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
  END SUBROUTINE initiate_ref_Quadrangle
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferenceQuadrangle@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns Lienar Quadrangle element

INTERFACE ReferenceQuadrangle
  MODULE PURE FUNCTION reference_Quadrangle(NSD, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    TYPE(ReferenceQuadrangle_) :: obj
  END FUNCTION reference_Quadrangle
END INTERFACE ReferenceQuadrangle

!----------------------------------------------------------------------------
!                                     ReferenceQuadrangle_Pointer@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns linear Quadrangle element

INTERFACE ReferenceQuadrangle_Pointer
  MODULE FUNCTION reference_Quadrangle_Pointer(NSD, xij, domainName)  &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    CLASS(ReferenceQuadrangle_), POINTER :: obj
  END FUNCTION reference_Quadrangle_Pointer
END INTERFACE ReferenceQuadrangle_Pointer

!----------------------------------------------------------------------------
!                                                LagrangeElement@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary:         Higher order lagrange elements
!
!### Usage
!
!```fortran
! subroutine test4
!   class( ReferenceElement_ ), pointer :: obj_ptr => null()
!   type( ReferenceQuadrangle_ ) :: obj
!   obj_ptr => referenceQuadrangle_pointer( nsd = 2 )
!   call obj_ptr%LagrangeElement( order = 2, Highorderobj = obj )
!   call display( obj, "higher order obj : ")
!   call obj_ptr%LagrangeElement( order = 3, Highorderobj = obj )
!   call display( obj, "3rd order obj : ")
! end
!```

INTERFACE
  MODULE SUBROUTINE HighorderElement_Quadrangle(refelem, order, obj, &
    & ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), INTENT(IN) :: order
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE HighorderElement_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Quadrangle(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Quadrangle_quality
!----------------------------------------------------------------------------

INTERFACE Quality_Quadrangle
  MODULE FUNCTION Quadrangle_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Quadrangle_Quality
END INTERFACE Quality_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Aug 2022
! summary:         Area of quadrangle in 3D
!
!# Introduction
!
!- QUADAREA3D computes the area of a quadrilateral in 3D.
!- A quadrilateral is a polygon defined by 4 vertices.
! It is assumed that the four vertices of the quadrilateral
! are coplanar.
!- This algorithm computes the area of the related Varignon parallelogram
! first.

INTERFACE QuadrangleArea3D
  MODULE PURE SUBROUTINE QuadArea3D(q, ans)
    REAL(DFP), INTENT(IN) :: q(3, 4)
    REAL(DFP), INTENT(OUT) :: ans
  END SUBROUTINE QuadArea3D
END INTERFACE QuadrangleArea3D

!----------------------------------------------------------------------------
!                                                          QuadrangleArea2D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Aug 2022
! summary:         QuadArea2D
!
!# Introduction
!
!- QUADAREA2D computes the area of a quadrilateral in 2D.
!- A quadrilateral is a polygon defined by 4 vertices.
! This algorithm should be able to handle nonconvex quadrilaterals.
! The vertices of the quadrilateral should be listed in counter clockwise
! order, so that the area is positive.

INTERFACE QuadrangleArea2D
  MODULE PURE SUBROUTINE QuadArea2D(q, ans)
    REAL(DFP), INTENT(IN) :: q(2, 4)
    REAL(DFP), INTENT(OUT) :: ans
  END SUBROUTINE QuadArea2D
END INTERFACE QuadrangleArea2D

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-08
! summary:  Returns number of edges in the element

INTERFACE
  MODULE SUBROUTINE GetEdgeConnectivity_Quadrangle(con, opt)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the edge number
    !! The row represents a edge
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt =2, then edge connectivity for Lagrangian approximation
    !! opt=1 is default
  END SUBROUTINE GetEdgeConnectivity_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   RefQuadrangleCoord
!----------------------------------------------------------------------------

INTERFACE RefCoord_Quadrangle
  MODULE PURE FUNCTION RefQuadrangleCoord(refQuadrangle) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    REAL(DFP) :: ans(2, 4)
  END FUNCTION RefQuadrangleCoord
END INTERFACE RefCoord_Quadrangle

END MODULE ReferenceQuadrangle_Method
