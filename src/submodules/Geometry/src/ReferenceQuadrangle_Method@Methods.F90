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
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Quadrangle
  !!
CALL Reallocate(obj%xij, 3, 4)
  !!
obj%xij = InterpolationPoint_Quadrangle(  &
  & xij=xij, &
  & order=1, &
  & ipType=Equidistance, &
  & layout="VEFC")
  !!
obj%EntityCounts = [4, 4, 1, 0]
obj%XiDimension = 2
obj%Name = Quadrangle4
obj%order = 1
obj%NSD = NSD
  !!
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
  !!
obj%highorderElement => highorderElement_Quadrangle
  !!
END PROCEDURE Initiate_ref_Quadrangle

!----------------------------------------------------------------------------
!                                                       ReferenceQuadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Quadrangle
IF (PRESENT(xij)) THEN
  CALL Initiate(obj, NSD, xij)
ELSE
  CALL Initiate(obj, NSD)
END IF
END PROCEDURE reference_Quadrangle

!----------------------------------------------------------------------------
!                                               ReferenceQuadrangle_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Quadrangle_Pointer
ALLOCATE (obj)
IF (PRESENT(xij)) THEN
  CALL Initiate(obj, NSD, xij)
ELSE
  CALL Initiate(obj, NSD)
END IF
END PROCEDURE reference_Quadrangle_Pointer

!----------------------------------------------------------------------------
!                                                            LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE highorderElement_Quadrangle
INTEGER(I4B) :: NNS, I
  !!
CALL Deallocate (obj)
  !!
SELECT CASE (order)
    !!
CASE (1)
    !!
  CALL Initiate(obj=obj, Anotherobj=refelem)
    !!
CASE (2)
    !!
  obj%xij = InterpolationPoint_Quadrangle(xij=refelem%xij(1:3, 1:4), &
    & order=order, ipType=ipType, &
    & layout="VEFC")
    !!
  NNS = 9
  obj%EntityCounts = [NNS, 4, 1, 0]
  obj%XiDimension = 2
  obj%Name = Quadrangle9
  obj%order = order
  obj%NSD = refelem%NSD
    !!
  ALLOCATE (obj%Topology(SUM(obj%EntityCounts)))
  DO I = 1, NNS
    obj%Topology(I) = ReferenceTopology([I], Point)
  END DO
  obj%Topology(NNS + 1) = ReferenceTopology([1, 2, 5], Line3)
  obj%Topology(NNS + 2) = ReferenceTopology([2, 3, 6], Line3)
  obj%Topology(NNS + 3) = ReferenceTopology([3, 4, 7], Line3)
  obj%Topology(NNS + 4) = ReferenceTopology([4, 1, 8], Line3)
  obj%Topology(NNS + 5) = ReferenceTopology([1, 2, 3, 4, 5, 6, 7, 8, 9], &
    & obj%Name)
  obj%highOrderElement => refelem%highOrderElement
    !!
CASE (3)
    !!
  obj%xij = InterpolationPoint_Quadrangle( &
    & xij=refelem%xij(1:3, 1:4), &
    & order=order, &
    & ipType=ipType, layout="VEFC")
    !!
  NNS = 16
  obj%EntityCounts = [NNS, 4, 1, 0]
  obj%XiDimension = 2
  obj%Name = Quadrangle16
  obj%order = order
  obj%NSD = refelem%NSD
    !!
  ALLOCATE (obj%Topology(SUM(obj%EntityCounts)))
  DO I = 1, NNS
    obj%Topology(I) = ReferenceTopology([I], Point)
  END DO
    !!
  obj%Topology(NNS + 1) = ReferenceTopology([1, 2, 5, 6], Line4)
  obj%Topology(NNS + 2) = ReferenceTopology([2, 3, 7, 8], Line4)
  obj%Topology(NNS + 3) = ReferenceTopology([3, 4, 9, 10], Line4)
  obj%Topology(NNS + 4) = ReferenceTopology([4, 1, 11, 12], Line4)
  obj%Topology(NNS + 5) = ReferenceTopology(arange(1, NNS, 1), obj%Name)
    !!
  obj%highOrderElement => refelem%highOrderElement
    !!
END SELECT
  !!
END PROCEDURE highorderElement_Quadrangle

!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Quadrangle
IF (refelem%nsd .EQ. 2) THEN
  CALL QUADAREA2D(xij(1:2, 1:4), Ans)
ELSE
  CALL QUADAREA3D(xij(1:3, 1:4), Ans)
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
  !!
  !! Define a parallelogram by averaging consecutive vertices.
p(1:3, 1:3) = (q(1:3, 1:3) + q(1:3, 2:4)) / 2.0_DFP
p(1:3, 4) = (q(1:3, 4) + q(1:3, 1)) / 2.0_DFP
  !!
  !!  Compute the area.
CALL PARALLELOGRAMAREA3D(p, area)
  !! The quadrilateral's area is twice that of the parallelogram.
area = 2.0_DFP * area
END PROCEDURE QuadArea3D

!----------------------------------------------------------------------------
!                                                                 QuadArea2D
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadArea2D
INTEGER(I4B), PARAMETER :: dim_num = 2
  !!
REAL(DFP) :: area_triangle
REAL(DFP) :: t(dim_num, 3)
  !!
area = 0.0_DFP
  !!
t(1:dim_num, 1:3) = reshape((/ &
                            q(1:2, 1), q(1:2, 2), q(1:2, 3)/), (/dim_num, 3/))
  !!
CALL TRIANGLEAREA2D(t, area_triangle)
  !!
area = area + area_triangle
  !!
t(1:dim_num, 1:3) = RESHAPE((/ &
                            q(1:2, 3), q(1:2, 4), q(1:2, 1)/), (/dim_num, 3/))
  !!
CALL TRIANGLEAREA2D(t, area_triangle)
  !!
area = area + area_triangle
  !!
END PROCEDURE QuadArea2D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./modified_burkardt.inc"

END SUBMODULE Methods
