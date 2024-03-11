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
! summary:  This submodule contains methods for [[ReferenceTetrahedron_]]

SUBMODULE(ReferenceTetrahedron_Method) Methods
USE ReferenceElement_Method
USE ApproxUtility
USE InvUtility
USE InputUtility
USE StringUtility
USE ArangeUtility

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_ref_Tetrahedron
INTEGER(I4B) :: ii, jj
INTEGER(I4B), PARAMETER :: tNodes = 4, tFaces = 4, tEdges = 6
INTEGER(I4B) :: p1p2(2, tEdges), lloop(3, tFaces), vol(tNodes, 1)
REAL(DFP) :: unit_xij(3, 4), biunit_xij(3, 4)

CALL DEALLOCATE (obj)

! p1p2 = EdgeConnectivity_Tetrahedron( &
!   & baseInterpol="LAGRANGE",  &
!   & baseContinuity="H1")

CALL GetEdgeConnectivity_Tetrahedron(con=p1p2, opt=2_I4B)
CALL GetFaceConnectivity_Tetrahedron(con=lloop, opt=2_I4B)

! lloop = FacetConnectivity_Tetrahedron( &
!   & baseInterpol="LAGRANGE",  &
!   & baseContinuity="H1")

vol(:, 1) = arange(1_I4B, tNodes)

unit_xij = RefCoord_Tetrahedron("UNIT")
biunit_xij = RefCoord_Tetrahedron("BIUNIT")

IF (PRESENT(xij)) THEN
  obj%xij = xij(1:3, 1:4)

  IF (ALL(obj%xij(1:3, 1:4) .approxeq.unit_xij)) THEN
    obj%domainName = "UNIT"
  ELSE IF (ALL(obj%xij(1:3, 1:4) .approxeq.biunit_xij)) THEN
    obj%domainName = "BIUNIT"
  ELSE
    obj%domainName = "GENERAL"
  END IF

ELSE

  IF (PRESENT(domainName)) THEN
    obj%domainName = UpperCase(domainName)
    IF (obj%domainName .EQ. "UNIT" .OR. obj%domainName .EQ. "BIUNIT") THEN
      obj%xij = RefCoord_Tetrahedron(obj%domainName)
    END IF
  ELSE
    obj%domainName = "UNIT"
    obj%xij = RefCoord_Tetrahedron(obj%domainName)
  END IF

END IF

obj%entityCounts = [tNodes, tEdges, tFaces, 1_I4B]
obj%xidimension = 3_I4B
obj%name = Tetrahedron4
obj%order = 1_I4B
obj%nsd = nsd

ALLOCATE (obj%topology(SUM(obj%entityCounts)))
DO ii = 1, obj%entityCounts(1)
  obj%topology(ii) = ReferenceTopology([ii], Point)
END DO

jj = obj%entityCounts(1)
DO ii = 1, obj%entityCounts(2)
  obj%topology(jj + ii) = ReferenceTopology(p1p2(:, ii), Line2)
END DO

jj = SUM(obj%entityCounts(1:2))
DO ii = 1, obj%entityCounts(3)
  obj%topology(jj + ii) = ReferenceTopology(lloop(:, ii), Triangle3)
END DO

jj = SUM(obj%entityCounts(1:3))
DO ii = 1, obj%entityCounts(4)
  obj%topology(jj + ii) = ReferenceTopology(vol(:, ii), Tetrahedron4)
END DO

obj%highorderElement => highorderElement_Tetrahedron
END PROCEDURE Initiate_ref_Tetrahedron

!----------------------------------------------------------------------------
!                                                      ReferenceTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Tetrahedron
CALL initiate_ref_tetrahedron(obj=obj, nsd=nsd, xij=xij,  &
  & domainName=domainName)
END PROCEDURE reference_Tetrahedron

!----------------------------------------------------------------------------
!                                              ReferenceTetrahedron_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Tetrahedron_Pointer
ALLOCATE (obj)
CALL initiate_ref_tetrahedron(obj=obj, nsd=nsd, xij=xij,  &
  & domainName=domainName)
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

MODULE PROCEDURE Tetrahedron_Quality
ans = 0.0_DFP
! TODO Implement Tetrahedron_Quality
END PROCEDURE Tetrahedron_Quality

!----------------------------------------------------------------------------
!                                                       TetrahedronVolume3D
!----------------------------------------------------------------------------

MODULE PROCEDURE TetrahedronVolume3D
REAL(DFP) :: a(4, 4)
a(1:3, 1:4) = xij(1:3, 1:4)
a(4, 1:4) = 1.0_DFP
ans = ABS(Det(a)) / 6.0_DFP
END PROCEDURE TetrahedronVolume3D

!----------------------------------------------------------------------------
!                                                      RefCoord_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE RefCoord_Tetrahedron
CHARACTER(:), ALLOCATABLE :: layout
layout = UpperCase(refTetrahedron)
SELECT CASE (layout)
CASE ("BIUNIT")
  ans(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
  ans(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
  ans(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
  ans(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
CASE ("UNIT")
  ans(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
  ans(:, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
  ans(:, 3) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
  ans(:, 4) = [0.0_DFP, 0.0_DFP, 1.0_DFP]
END SELECT
layout = ""
END PROCEDURE RefCoord_Tetrahedron

!----------------------------------------------------------------------------
!                                           GetEdgeConnectivity_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeConnectivity_Tetrahedron
con(1:2, 1) = [1, 2]
con(1:2, 2) = [1, 3]
con(1:2, 3) = [1, 4]
con(1:2, 4) = [2, 3]
con(1:2, 5) = [2, 4]
con(1:2, 6) = [3, 4]
END PROCEDURE GetEdgeConnectivity_Tetrahedron

!----------------------------------------------------------------------------
!                                           GetFaceConnectivity_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceConnectivity_Tetrahedron
INTEGER(I4B) :: opt0
opt0 = input(option=opt, default=1_I4B)

SELECT CASE (opt0)
CASE (1_I4B)
  con(1:3, 1) = [1, 2, 3]
  con(1:3, 2) = [1, 2, 4]
  con(1:3, 3) = [1, 3, 4]
  con(1:3, 4) = [2, 3, 4]
CASE (2_I4B)
  con(1:3, 1) = [1, 3, 2]
  con(1:3, 2) = [1, 2, 4]
  con(1:3, 3) = [1, 4, 3]
  con(1:3, 4) = [2, 3, 4]
END SELECT
END PROCEDURE GetFaceConnectivity_Tetrahedron

!----------------------------------------------------------------------------
!                                                            GetFaceElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType_Tetrahedron
faceElemType(1:4) = Triangle3
IF (PRESENT(tFaceNodes)) tFaceNodes(1:4) = 3_I4B
END PROCEDURE GetFaceElemType_Tetrahedron

END SUBMODULE Methods
