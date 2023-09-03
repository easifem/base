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
!                                                             Quadranglename1
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadranglename1
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
END PROCEDURE Quadranglename1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Quadrangle
REAL(DFP) :: unit_xij(2, 4), biunit_xij(2, 4)
CALL DEALLOCATE (obj)

unit_xij = RefCoord_Quadrangle("UNIT")
biunit_xij = RefCoord_Quadrangle("BIUNIT")

IF (PRESENT(xij)) THEN
  obj%xij = xij(1:2, 1:4)
  IF (ALL(obj%xij(1:2, 1:4) .approxeq.unit_xij)) THEN
    obj%domainName = "UNIT"
  ELSE IF (ALL(obj%xij(1:2, 1:4) .approxeq.biunit_xij)) THEN
    obj%domainName = "BIUNIT"
  ELSE
    obj%domainName = "GENERAL"
  END IF

ELSE

  IF (PRESENT(domainName)) THEN
    obj%domainName = UpperCase(domainName)
    IF (obj%domainName .EQ. "UNIT" .OR. obj%domainName .EQ. "BIUNIT") THEN
      obj%xij = RefCoord_Quadrangle(obj%domainName)
    END IF
  ELSE
    obj%domainName = "BIUNIT"
    obj%xij = RefCoord_Quadrangle(obj%domainName)
  END IF

END IF

obj%entityCounts = [4, 4, 1, 0]
obj%xidimension = 2
obj%name = Quadrangle4
obj%order = 1
obj%NSD = NSD
ALLOCATE (obj%topology(9))
obj%topology(1) = ReferenceTopology([1], Point)
obj%topology(2) = ReferenceTopology([2], Point)
obj%topology(3) = ReferenceTopology([3], Point)
obj%topology(4) = ReferenceTopology([4], Point)
obj%topology(5) = ReferenceTopology([1, 2], Line2)
obj%topology(6) = ReferenceTopology([2, 3], Line2)
obj%topology(7) = ReferenceTopology([3, 4], Line2)
obj%topology(8) = ReferenceTopology([4, 1], Line2)
obj%topology(9) = ReferenceTopology([1, 2, 3, 4], Quadrangle4)
obj%highorderElement => highorderElement_Quadrangle
END PROCEDURE Initiate_ref_Quadrangle

!----------------------------------------------------------------------------
!                                                       ReferenceQuadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Quadrangle
CALL Initiate(obj=obj, nsd=NSD, xij=xij, domainName=domainName)
END PROCEDURE reference_Quadrangle

!----------------------------------------------------------------------------
!                                               ReferenceQuadrangle_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Quadrangle_Pointer
ALLOCATE (obj)
CALL Initiate(obj=obj, nsd=NSD, xij=xij, domainName=domainName)
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
CASE DEFAULT
  obj%xij = InterpolationPoint_Quadrangle( &
    & xij=refelem%xij(1:3, 1:4), &
    & order=order, &
    & ipType=ipType,  &
    & layout="VEFC")
  obj%domainName=refelem%domainName
  NNS = LagrangeDOF_Quadrangle(order=order)
  obj%entityCounts = [NNS, 4, 1, 0]
  obj%xidimension = 2
  obj%name = QuadrangleName(order=order)
  obj%order = order
  obj%NSD = refelem%NSD
  ALLOCATE (obj%topology(SUM(obj%entityCounts)))
  DO I = 1, NNS
    obj%topology(I) = ReferenceTopology([I], Point)
  END DO
  aintvec = [1, 2] .append.arange(5_I4B, 3_I4B + order)
  obj%topology(NNS + 1) = ReferenceTopology(aintvec, Linename(order=order))

  aintvec = [2, 3] .append.arange( &
                                  & 3_I4B + order + 1, &
                                  & 3_I4B + order + order - 1_I4B)
  obj%topology(NNS + 2) = ReferenceTopology(aintvec, Linename(order=order))

  aintvec = [3, 4] .append.arange(&
                                  & 2_I4B + 2_I4B * order + 1, &
                                  & 2_I4B + 2_I4B * order + order - 1_I4B)
  obj%topology(NNS + 3) = ReferenceTopology(aintvec, Linename(order=order))

  aintvec = [4, 1] .append.arange( &
                            & 1_I4B + 3_I4B * order + 1,  &
                            & 1_I4B + 3_I4B * order + order - 1_I4B)
  obj%topology(NNS + 4) = ReferenceTopology(aintvec, Linename(order=order))

  obj%topology(NNS + 5) = ReferenceTopology( &
                            & arange(1_I4B, NNS, 1_I4B), obj%name)
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
CALL PARALLELOGRAMAREA3D(p, ans)
! The quadrilateral's area is twice that of the parallelogram.
ans = 2.0_DFP * ans
END PROCEDURE QuadArea3D

!----------------------------------------------------------------------------
!                                                                 QuadArea2D
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadArea2D
INTEGER(I4B), PARAMETER :: dim_num = 2
REAL(DFP) :: area_triangle
REAL(DFP) :: t(dim_num, 3)
ans = 0.0_DFP
t(1:dim_num, 1:3) = RESHAPE( &
  & [q(1:2, 1), q(1:2, 2), q(1:2, 3)], &
  & [dim_num, 3] &
  & )
CALL TRIANGLEAREA2D(t, area_triangle)
ans = ans + area_triangle
t(1:dim_num, 1:3) = RESHAPE( &
  & [q(1:2, 3), q(1:2, 4), q(1:2, 1)],  &
  & [dim_num, 3])
CALL TRIANGLEAREA2D(t, area_triangle)
ans = ans + area_triangle
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
