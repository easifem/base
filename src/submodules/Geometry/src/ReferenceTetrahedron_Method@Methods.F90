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
! summary:         This submodule contains methods for [[ReferenceTetrahedron_]]

SUBMODULE(ReferenceTetrahedron_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Tetrahedron
INTEGER(I4B) :: ii, jj
INTEGER(I4B), PARAMETER :: tNodes = 4, tFaces=4, tEdges=6
INTEGER(I4B) :: p1p2(2, tEdges), lloop(3, tFaces), vol(tNodes, 1)

p1p2 = EdgeConnectivity_Tetrahedron( &
  & baseInterpol="LAGRANGE",  &
  & baseContinuity="H1")

lloop = FacetConnectivity_Tetrahedron( &
  & baseInterpol="LAGRANGE",  &
  & baseContinuity="H1")

vol(:, 1) = arange(1_I4B, tNodes)

IF (PRESENT(xij)) THEN
  obj%xij = xij
ELSE
  obj%xij = RefCoord_Tetrahedron("UNIT")
END IF

obj%EntityCounts = [tNodes, tEdges, tFaces, 1_I4B]
obj%XiDimension = 3_I4B
obj%Name = Tetrahedron4
obj%order = 1_I4B
obj%nsd = nsd

ALLOCATE (obj%Topology(SUM(obj%EntityCounts)))
DO ii = 1, obj%EntityCounts(1)
  obj%Topology(ii) = ReferenceTopology([ii], Point)
END DO

jj = obj%EntityCounts(1)
DO ii = 1, obj%EntityCounts(2)
  obj%Topology(jj + ii) = ReferenceTopology(p1p2(:, ii), Line2)
END DO

jj = SUM(obj%EntityCounts(1:2))
DO ii = 1, obj%EntityCounts(3)
  obj%Topology(jj + ii) = ReferenceTopology(lloop(:, ii), Triangle3)
END DO

jj = SUM(obj%EntityCounts(1:3))
DO ii = 1, obj%EntityCounts(4)
  obj%Topology(jj + ii) = ReferenceTopology(vol(:, ii), Tetrahedron4)
END DO

obj%highorderElement => highorderElement_Tetrahedron
END PROCEDURE Initiate_ref_Tetrahedron

!----------------------------------------------------------------------------
!                                                      ReferenceTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Tetrahedron
IF (PRESENT(XiJ)) THEN
  CALL Initiate(obj, NSD, XiJ)
ELSE
  CALL Initiate(obj, NSD)
END IF
END PROCEDURE reference_Tetrahedron

!----------------------------------------------------------------------------
!                                              ReferenceTetrahedron_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Tetrahedron_Pointer
ALLOCATE (obj)
IF (PRESENT(XiJ)) THEN
  CALL Initiate(obj, NSD, XiJ)
ELSE
  CALL Initiate(obj, NSD)
END IF
END PROCEDURE reference_Tetrahedron_Pointer

!----------------------------------------------------------------------------
!                                                             LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE highOrderElement_Tetrahedron
! TODO Implement highOrderElement_Tetrahedron
END PROCEDURE highOrderElement_Tetrahedron

!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Tetrahedron
CALL TetrahedronVolume3D(XiJ(1:3, 1:4), ans)
END PROCEDURE Measure_Simplex_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Tetrahedron_quality
! TODO Implement Tetrahedron_quality 
END PROCEDURE Tetrahedron_quality

!----------------------------------------------------------------------------
!                                                       TetrahedronVolume3D
!----------------------------------------------------------------------------

MODULE PROCEDURE TetrahedronVolume3D
REAL(DFP) :: a(4, 4)
a(1:3, 1:4) = xij(1:3, 1:4)
a(4, 1:4) = 1.0_DFP
ans = ABS(Det(a)) / 6.0_DFP
END PROCEDURE TetrahedronVolume3D

END SUBMODULE Methods
