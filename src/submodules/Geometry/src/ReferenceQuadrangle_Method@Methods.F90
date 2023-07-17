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
! date: 2 March 2021
! summary: This submodule contains method for [[ReferenceQuadrangle_]]

SUBMODULE(ReferenceQuadrangle_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             QuadrangleName1
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadrangleName1
SELECT CASE (order)
CASE (1)
  ans = Quadrangle4
CASE (2)
  ans = Quadrangle9
CASE (3)
  ans = Quadrangle16
CASE (4:)
  ans = Quadrangle16 + order - 3_I4B
END SELECT
END PROCEDURE QuadrangleName1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Quadrangle
CALL Reallocate(obj%xij, 3, 4)
obj%xij = InterpolationPoint_Quadrangle(  &
  & xij=xij, &
  & order=1, &
  & ipType=Equidistance, &
  & layout="VEFC")
obj%EntityCounts = [4, 4, 1, 0]
obj%XiDimension = 2
obj%Name = Quadrangle4
obj%order = 1
obj%NSD = NSD
ALLOCATE (obj%Topology(9))
obj%Topology(1) = ReferenceTopology([1], Point)
obj%Topology(2) = ReferenceTopology([2], Point)
obj%Topology(3) = ReferenceTopology([3], Point)
obj%Topology(4) = ReferenceTopology([4], Point)
obj%Topology(5) = ReferenceTopology([1, 2], Line2)
obj%Topology(6) = ReferenceTopology([2, 3], Line2)
obj%Topology(7) = ReferenceTopology([3, 4], Line2)
obj%Topology(8) = ReferenceTopology([4, 1], Line2)
obj%Topology(9) = ReferenceTopology([1, 2, 3, 4], Quadrangle4)
obj%highorderElement => highorderElement_Quadrangle
END PROCEDURE Initiate_ref_Quadrangle

!----------------------------------------------------------------------------
!                                                       ReferenceQuadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Quadrangle
CALL Initiate(obj=obj, nsd=NSD, xij=xij)
END PROCEDURE reference_Quadrangle

!----------------------------------------------------------------------------
!                                               ReferenceQuadrangle_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Quadrangle_Pointer
ALLOCATE (obj)
CALL Initiate(obj=obj, nsd=NSD, xij=xij)
END PROCEDURE reference_Quadrangle_Pointer

!----------------------------------------------------------------------------
!                                                            LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE highorderElement_Quadrangle
INTEGER(I4B) :: NNS, I
INTEGER(I4B), ALLOCATABLE :: aintvec(:)

CALL DEALLOCATE (obj)
SELECT CASE (order)
CASE (1)
  CALL Initiate(obj=obj, Anotherobj=refelem)
! CASE (2)
!   obj%xij = InterpolationPoint_Quadrangle( &
!     & xij=refelem%xij(1:3, 1:4), &
!     & order=order,  &
!     & ipType=ipType, &
!     & layout="VEFC")
!   NNS = 9
!   obj%EntityCounts = [NNS, 4, 1, 0]
!   obj%XiDimension = 2
!   obj%Name = Quadrangle9
!   obj%order = order
!   obj%NSD = refelem%NSD
!   ALLOCATE (obj%Topology(SUM(obj%EntityCounts)))
!   DO I = 1, NNS
!     obj%Topology(I) = ReferenceTopology([I], Point)
!   END DO
!   obj%Topology(NNS + 1) = ReferenceTopology([1, 2, 5], Line3)
!   obj%Topology(NNS + 2) = ReferenceTopology([2, 3, 6], Line3)
!   obj%Topology(NNS + 3) = ReferenceTopology([3, 4, 7], Line3)
!   obj%Topology(NNS + 4) = ReferenceTopology([4, 1, 8], Line3)
!   obj%Topology(NNS + 5) = ReferenceTopology([1, 2, 3, 4, 5, 6, 7, 8, 9], &
!     & obj%Name)
!   obj%highOrderElement => refelem%highOrderElement
! CASE (3)
!   obj%xij = InterpolationPoint_Quadrangle( &
!     & xij=refelem%xij(1:3, 1:4), &
!     & order=order, &
!     & ipType=ipType, layout="VEFC")
!   NNS = 16
!   obj%EntityCounts = [NNS, 4, 1, 0]
!   obj%XiDimension = 2
!   obj%Name = Quadrangle16
!   obj%order = order
!   obj%NSD = refelem%NSD
!   ALLOCATE (obj%Topology(SUM(obj%EntityCounts)))
!   DO I = 1, NNS
!     obj%Topology(I) = ReferenceTopology([I], Point)
!   END DO
!   obj%Topology(NNS + 1) = ReferenceTopology([1, 2, 5, 6], Line4)
!   obj%Topology(NNS + 2) = ReferenceTopology([2, 3, 7, 8], Line4)
!   obj%Topology(NNS + 3) = ReferenceTopology([3, 4, 9, 10], Line4)
!   obj%Topology(NNS + 4) = ReferenceTopology([4, 1, 11, 12], Line4)
!   obj%Topology(NNS + 5) = ReferenceTopology(arange(1, NNS, 1), obj%Name)
!   obj%highOrderElement => refelem%highOrderElement
CASE DEFAULT
  obj%xij = InterpolationPoint_Quadrangle( &
    & xij=refelem%xij(1:3, 1:4), &
    & order=order, &
    & ipType=ipType,  &
    & layout="VEFC")
  NNS = LagrangeDOF_Quadrangle(order=order)
  obj%EntityCounts = [NNS, 4, 1, 0]
  obj%XiDimension = 2
  obj%Name = QuadrangleName(order=order)
  obj%order = order
  obj%NSD = refelem%NSD
  ALLOCATE (obj%Topology(SUM(obj%EntityCounts)))
  DO I = 1, NNS
    obj%Topology(I) = ReferenceTopology([I], Point)
  END DO
  aintvec = [1, 2] .append.arange(5_I4B, 3_I4B + order)
  obj%Topology(NNS + 1) = ReferenceTopology(aintvec, LineName(order=order))

  aintvec = [2, 3] .append.arange( &
                                  & 3_I4B + order + 1, &
                                  & 3_I4B + order + order - 1_I4B)
  obj%Topology(NNS + 2) = ReferenceTopology(aintvec, LineName(order=order))

  aintvec = [3, 4] .append.arange(&
                                  & 2_I4B + 2_I4B * order + 1, &
                                  & 2_I4B + 2_I4B * order + order - 1_I4B)
  obj%Topology(NNS + 3) = ReferenceTopology(aintvec, LineName(order=order))

  aintvec = [4, 1] .append.arange( &
                            & 1_I4B + 3_I4B * order + 1,  &
                            & 1_I4B + 3_I4B * order + order - 1_I4B)
  obj%Topology(NNS + 4) = ReferenceTopology(aintvec, LineName(order=order))

  obj%Topology(NNS + 5) = ReferenceTopology( &
                            & arange(1_I4B, NNS, 1_I4B), obj%Name)
  obj%highOrderElement => refelem%highOrderElement
END SELECT
END PROCEDURE highorderElement_Quadrangle

!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Quadrangle
IF (refelem%nsd .EQ. 2) THEN
  CALL QuadArea2D(xij(1:2, 1:4), Ans)
ELSE
  CALL QuadArea3D(xij(1:3, 1:4), Ans)
END IF
END PROCEDURE Measure_Simplex_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrangle_quality
END PROCEDURE Quadrangle_quality

!----------------------------------------------------------------------------
!                                                                 QuadArea3D
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadArea3D
REAL(DFP) :: p(3, 4)
! Define a parallelogram by averaging consecutive vertices.
p(1:3, 1:3) = (q(1:3, 1:3) + q(1:3, 2:4)) / 2.0_DFP
p(1:3, 4) = (q(1:3, 4) + q(1:3, 1)) / 2.0_DFP
!  Compute the area.
CALL PARALLELOGRAMAREA3D(p, area)
! The quadrilateral's area is twice that of the parallelogram.
area = 2.0_DFP * area
END PROCEDURE QuadArea3D

!----------------------------------------------------------------------------
!                                                                 QuadArea2D
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadArea2D
INTEGER(I4B), PARAMETER :: dim_num = 2
REAL(DFP) :: area_triangle
REAL(DFP) :: t(dim_num, 3)
area = 0.0_DFP
t(1:dim_num, 1:3) = RESHAPE( &
  & [q(1:2, 1), q(1:2, 2), q(1:2, 3)], &
  & [dim_num, 3] &
  & )
CALL TRIANGLEAREA2D(t, area_triangle)
area = area + area_triangle
t(1:dim_num, 1:3) = RESHAPE( &
  & [q(1:2, 3), q(1:2, 4), q(1:2, 1)],  &
  & [dim_num, 3])
CALL TRIANGLEAREA2D(t, area_triangle)
area = area + area_triangle
END PROCEDURE QuadArea2D

!-----------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------

PURE FUNCTION R8MATDET4D(a)
  REAL(DFP), INTENT(IN) :: a(4, 4)
  REAL(DFP) :: R8MATDET4D
  R8MATDET4D = &
    a(1, 1) * ( &
    a(2, 2) * (a(3, 3) * a(4, 4) - a(3, 4) * a(4, 3)) &
    - a(2, 3) * (a(3, 2) * a(4, 4) - a(3, 4) * a(4, 2)) &
    + a(2, 4) * (a(3, 2) * a(4, 3) - a(3, 3) * a(4, 2))) &
    - a(1, 2) * ( &
    a(2, 1) * (a(3, 3) * a(4, 4) - a(3, 4) * a(4, 3)) &
    - a(2, 3) * (a(3, 1) * a(4, 4) - a(3, 4) * a(4, 1)) &
    + a(2, 4) * (a(3, 1) * a(4, 3) - a(3, 3) * a(4, 1))) &
    + a(1, 3) * ( &
    a(2, 1) * (a(3, 2) * a(4, 4) - a(3, 4) * a(4, 2)) &
    - a(2, 2) * (a(3, 1) * a(4, 4) - a(3, 4) * a(4, 1)) &
    + a(2, 4) * (a(3, 1) * a(4, 2) - a(3, 2) * a(4, 1))) &
    - a(1, 4) * ( &
    a(2, 1) * (a(3, 2) * a(4, 3) - a(3, 3) * a(4, 2)) &
    - a(2, 2) * (a(3, 1) * a(4, 3) - a(3, 3) * a(4, 1)) &
    + a(2, 3) * (a(3, 1) * a(4, 2) - a(3, 2) * a(4, 1)))
END FUNCTION R8MATDET4D

!-----------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-17
! summary: Computes the area of a parallelogram in 3D
!
!# Introduction
!
!    A parallelogram is a polygon having four sides, with the property
!    that each pair of opposite sides is paralell.
!    A parallelogram in 3D must have the property that it is "really"
!    a 2D object, that is, that the four vertices that define it lie
!    in some plane.
!    Given the first three vertices of the parallelogram (in 2D or 3D),
!    P1, P2, and P3, the fourth vertex must satisfy
!      P4 = P1 + ( P3 - P2 )
!    This routine uses the fact that the norm of the cross product
!    of two vectors is the area of the parallelogram they form:
!      Area = ( P3 - P2 ) x ( P1 - P2 ).
!
!        P4<-----P3
!        /       /
!       /       /
!      P1----->P2
!

PURE SUBROUTINE PARALLELOGRAMAREA3D(p, ans)
  REAL(DFP), INTENT(IN) :: p(3, 4)
  REAL(DFP), INTENT(OUT) :: ans
  REAL(DFP) :: cross(3)
  ! Compute the cross product vector.
  cross(1) = (p(2, 2) - p(2, 1)) * (p(3, 3) - p(3, 1)) &
          & - (p(3, 2) - p(3, 1)) * (p(2, 3) - p(2, 1))
  cross(2) = (p(3, 2) - p(3, 1)) * (p(1, 3) - p(1, 1)) &
          & - (p(1, 2) - p(1, 1)) * (p(3, 3) - p(3, 1))
  cross(3) = (p(1, 2) - p(1, 1)) * (p(2, 3) - p(2, 1)) &
          & - (p(2, 2) - p(2, 1)) * (p(1, 3) - p(1, 1))
  ans = SQRT(SUM(cross(1:3)**2))
END SUBROUTINE PARALLELOGRAMAREA3D

!-----------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-17
! summary:  Computes the area of a parallelogram in 2D
!
!# Introduction
!
!    A parallelogram is a polygon having four sides, with the property
!    that each pair of opposite sides is paralell.
!    Given the first three vertices of the parallelogram,
!    P1, P2, and P3, the fourth vertex must satisfy
!
!      P4 = P1 + ( P3 - P2 )
!
!    This routine uses the fact that the norm of the cross product
!    of two vectors is the area of the parallelogram they form:
!
!      Area = ( P3 - P2 ) x ( P1 - P2 ).
!
!        P4<-----P3
!        /       /
!       /       /
!      P1----->P2

PURE SUBROUTINE PARALLELOGRAMAREA2D(p, ans)
  REAL(DFP), INTENT(IN) :: p(2, 4)
  REAL(DFP), INTENT(OUT) :: ans
  ans = (p(1, 2) - p(1, 1)) * (p(2, 3) - p(2, 1)) &
    & - (p(2, 2) - p(2, 1)) * (p(1, 3) - p(1, 1))
END SUBROUTINE PARALLELOGRAMAREA2D

END SUBMODULE Methods
