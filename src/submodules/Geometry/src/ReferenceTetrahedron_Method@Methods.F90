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
USE Display_Method
USE ReallocateUtility
USE MiscUtility, ONLY: Int2STR

USE ReferenceLine_Method, ONLY: ElementType_Line

USE TriangleInterpolationUtility, ONLY: InterpolationPoint_Triangle

USE ReferenceTriangle_Method, ONLY: ElementOrder_Triangle,  &
  & TotalEntities_Triangle, FacetTopology_Triangle

USE TetrahedronInterpolationUtility, ONLY: LagrangeDOF_Tetrahedron,  &
  & InterpolationPoint_Tetrahedron

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     ElementName_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementName_Tetrahedron

SELECT CASE (elemType)
CASE (Tetrahedron4)
  ans = "Tetrahedron4"

CASE (Tetrahedron10)
  ans = "Tetrahedron10"

CASE (Tetrahedron20)
  ans = "Tetrahedron20"

CASE (Tetrahedron35)
  ans = "Tetrahedron35"

CASE (Tetrahedron56)
  ans = "Tetrahedron56"

CASE DEFAULT
  ans = "NONE"

END SELECT

END PROCEDURE ElementName_Tetrahedron

!----------------------------------------------------------------------------
!                                                 FacetTopology_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetTopology_Tetrahedron
INTEGER(I4B) :: ii, faceElemType(4), tFaceNodes(4)
INTEGER(I4B), ALLOCATABLE :: con(:, :)

CALL GetFaceElemType_Tetrahedron(faceElemType=faceElemType,  &
  & elemType=elemType, tFaceNodes=tFaceNodes)

CALL Reallocate(con, tFaceNodes(1), 4)

ii = ElementOrder_Tetrahedron(elemType=elemType)
CALL GetFaceConnectivity_Tetrahedron(con=con, order=ii)

DO ii = 1, 4
  ans(ii)%nptrs = nptrs(con(:, ii))
  ans(ii)%xiDimension = 2
  ans(ii)%name = faceElemType(ii)
END DO

IF (ALLOCATED(con)) DEALLOCATE (con)

END PROCEDURE FacetTopology_Tetrahedron

!----------------------------------------------------------------------------
!                                                   TotalEntities_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalEntities_Tetrahedron
ans(2:4) = [6, 4, 1]
ans(1) = TotalNodesInElement_Tetrahedron(elemType)
END PROCEDURE TotalEntities_Tetrahedron

!----------------------------------------------------------------------------
!                                            TotalNodesInElement_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalNodesInElement_Tetrahedron
SELECT CASE (elemType)
CASE (Tetrahedron4)
  ans = 4
CASE (Tetrahedron10)
  ans = 10
CASE (Tetrahedron20)
  ans = 20
CASE (Tetrahedron35)
  ans = 35
CASE (Tetrahedron56)
  ans = 56
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE TotalNodesInElement_Tetrahedron

!----------------------------------------------------------------------------
!                                                   ElementOrder_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementOrder_Tetrahedron
SELECT CASE (ElemType)
CASE (Tetrahedron4)
  ans = 1
CASE (Tetrahedron10)
  ans = 2
CASE (Tetrahedron20)
  ans = 3
CASE (Tetrahedron35)
  ans = 4
CASE (Tetrahedron56)
  ans = 5
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE ElementOrder_Tetrahedron

!----------------------------------------------------------------------------
!                                                     ElementType_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementType_Tetrahedron
SELECT CASE (elemName)
CASE ("Tetrahedron4", "Tetrahedron")
  ans = Tetrahedron4
CASE ("Tetrahedron10")
  ans = Tetrahedron10
CASE ("Tetrahedron20")
  ans = Tetrahedron20
CASE ("Tetrahedron35")
  ans = Tetrahedron35
CASE ("Tetrahedron56")
  ans = Tetrahedron56
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE ElementType_Tetrahedron

!----------------------------------------------------------------------------
!                                                 FacetElements_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Tetrahedron1
INTEGER(I4B) :: ii, istart, tsize, jj
TYPE(ReferenceTopology_) :: topo

istart = refelem%entityCounts(1) + refelem%entityCounts(2)
! tPoints + tEdges

ii = 1
ans(ii)%nsd = refelem%nsd
ans(ii)%interpolationPointType = refelem%interpolationPointType
ans(ii)%xij = InterpolationPoint_Triangle( &
  & order=refelem%order, &
  & ipType=refelem%interpolationPointType, &
  & layout="VEFC")

DO ii = 2, 4
  ans(ii)%nsd = ans(1)%nsd
  ans(ii)%interpolationPointType = ans(1)%interpolationPointType
  ans(ii)%xij = ans(1)%xij
END DO

DO ii = 1, 4

  topo = refelem%topology(istart + ii)
  ans(ii)%xidimension = topo%xidimension
  ans(ii)%name = topo%name

  ans(ii)%order = ElementOrder_Triangle(topo%name)
  ans(ii)%entityCounts = TotalEntities_Triangle(topo%name)

  tsize = SUM(ans(ii)%entityCounts)
  ! ALLOCATE (ans(ii)%topology(tsize))
  CALL RefTopoReallocate(ans(ii)%topology, tsize)

  ! points
  DO jj = 1, ans(ii)%entityCounts(1)
    ans(ii)%topology(jj) = ReferenceTopology(nptrs=topo%nptrs(jj:jj), &
      & name=Point)
  END DO

  ! lines
  jj = ans(ii)%entityCounts(1)
  CALL FacetTopology_Triangle(elemType=topo%name,  &
    & nptrs=topo%nptrs, ans=ans(ii)%topology(jj + 1:))

  ! surface
  tsize = jj + ans(ii)%entityCounts(2)
  ans(ii)%topology(tsize + 1) = ReferenceTopology(nptrs=topo%nptrs, &
    & name=topo%name)

END DO

CALL DEALLOCATE (topo)

END PROCEDURE FacetElements_Tetrahedron1

!----------------------------------------------------------------------------
!                                                 FacetElements_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Tetrahedron2
INTEGER(I4B) :: ii, jj, order, entityCounts(4), tsize
INTEGER(I4B), ALLOCATABLE :: edgeCon(:, :), faceCon(:, :)
INTEGER(I4B) :: faceElemType(4), tFaceNodes(4)

CALL GetFaceElemType_Tetrahedron(faceElemType=faceElemType,  &
  & tFaceNodes=tFaceNodes, elemType=elemType)

entityCounts = TotalEntities_Tetrahedron(elemType)
order = ElementOrder_Tetrahedron(elemType)

CALL Reallocate(edgeCon, order + 1, entityCounts(2))
CALL Reallocate(faceCon, tFaceNodes(1), entityCounts(3))

CALL GetEdgeConnectivity_Tetrahedron(con=edgeCon, order=order)
CALL GetFaceConnectivity_Tetrahedron(con=faceCon, order=order)

DO ii = 1, entityCounts(3)

  ans(ii)%xiDimension = 2
  ans(ii)%order = order
  ans(ii)%name = faceElemType(ii)
  ans(ii)%interpolationPointType = Equidistance

  ans(ii)%xij = InterpolationPoint_Triangle(  &
    & order=ans(ii)%order, &
    & ipType=ans(ii)%interpolationPointType, &
    & layout="VEFC")

  ans(ii)%nsd = nsd
  ans(ii)%entityCounts = TotalEntities_Triangle(ans(ii)%name)

  tsize = SUM(ans(ii)%entityCounts)
  ! ALLOCATE (ans(ii)%topology(tsize))
  CALL RefTopoReallocate(ans(ii)%topology, tsize)

  ! points
  DO jj = 1, ans(ii)%entityCounts(1)
    ans(ii)%topology(jj) = Referencetopology(nptrs=faceCon(jj:jj, ii),  &
      & name=Point)
  END DO

  ! lines
  jj = ans(ii)%entityCounts(1)
  CALL FacetTopology_Triangle(elemType=ans(ii)%name,  &
    & nptrs=faceCon(:, ii), ans=ans(ii)%topology(jj + 1:))

  ! surface
  tsize = jj + ans(ii)%entityCounts(2)
  ans(ii)%topology(tsize + 1) = ReferenceTopology(nptrs=faceCon(:, ii), &
    & name=ans(ii)%name)

END DO

IF (ALLOCATED(edgeCon)) DEALLOCATE (edgeCon)
IF (ALLOCATED(faceCon)) DEALLOCATE (faceCon)
END PROCEDURE FacetElements_Tetrahedron2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_Ref_Tetrahedron
INTEGER(I4B), PARAMETER :: tNodes = 4, tFaces = 4, tEdges = 6
INTEGER(I4B) :: ii, jj, p1p2(2, tEdges), lloop(3, tFaces)
REAL(DFP) :: unit_xij(nsd, tNodes), biunit_xij(nsd, tNodes)

CALL DEALLOCATE (obj)

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

CALL GetEdgeConnectivity_Tetrahedron(con=p1p2, order=1)
CALL GetFaceConnectivity_Tetrahedron(con=lloop, order=1)

obj%entityCounts = [tNodes, tEdges, tFaces, 1_I4B]
obj%xidimension = nsd
obj%name = Tetrahedron4
obj%order = 1_I4B
obj%nsd = nsd

ii = SUM(obj%entityCounts)
CALL RefTopoReallocate(obj%topology, ii)

! points
DO ii = 1, obj%entityCounts(1)
  obj%topology(ii) = ReferenceTopology([ii], Point)
END DO

! lines
jj = obj%entityCounts(1)
DO ii = 1, obj%entityCounts(2)
  obj%topology(jj + ii) = ReferenceTopology(p1p2(:, ii), Line2)
END DO

! faces
jj = jj + obj%entityCounts(2)
DO ii = 1, obj%entityCounts(3)
  obj%topology(jj + ii) = ReferenceTopology(lloop(:, ii), Triangle3)
END DO

! cell
jj = jj + obj%entityCounts(3)
obj%topology(jj + 1) = ReferenceTopology(arange(1_I4B, tNodes), Tetrahedron4)

obj%highorderElement => highorderElement_Tetrahedron
END PROCEDURE Initiate_Ref_Tetrahedron

!----------------------------------------------------------------------------
!                                                      ReferenceTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE Reference_Tetrahedron
CALL Initiate_Ref_tetrahedron(obj=obj, nsd=nsd, xij=xij,  &
  & domainName=domainName)
END PROCEDURE Reference_Tetrahedron

!----------------------------------------------------------------------------
!                                              ReferenceTetrahedron_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Reference_Tetrahedron_Pointer
ALLOCATE (obj)
CALL Initiate_Ref_tetrahedron(obj=obj, nsd=nsd, xij=xij,  &
  & domainName=domainName)
END PROCEDURE Reference_Tetrahedron_Pointer

!----------------------------------------------------------------------------
!                                                             LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE HighOrderElement_Tetrahedron
INTEGER(I4B), PARAMETER :: tNodes = 4
INTEGER(I4B) :: ii, tFaceNodes(4), faceElemType(4), jj,  &
  & edgetype
INTEGER(I4B), ALLOCATABLE :: edgecon(:, :), facecon(:, :)

CALL DEALLOCATE (obj)

obj%xij = InterpolationPoint_Tetrahedron( &
  & xij=refelem%xij(1:3, 1:tNodes), &
  & order=order, &
  & ipType=ipType, &
  & layout="VEFC")

obj%domainName = refelem%domainName
obj%nsd = refelem%nsd
obj%highOrderElement => refelem%highOrderElement
obj%order = order
obj%xiDimension = refelem%xiDimension

ii = LagrangeDOF_Tetrahedron(order=order)
obj%name = ElementType_Tetrahedron("Tetrahedron"//INT2STR(ii))
obj%entityCounts = TotalEntities_Tetrahedron(obj%name)

ii = SUM(obj%entityCounts)
CALL RefTopoReallocate(obj%topology, ii)

! points
DO ii = 1, obj%entityCounts(1)
  obj%topology(ii) = ReferenceTopology([ii], Point)
END DO

CALL Reallocate(edgecon, order + 1, obj%entityCounts(2))
CALL GetEdgeConnectivity_Tetrahedron(con=edgecon, order=order)
edgetype = ElementType_Line("Line"//Int2STR(order + 1))

! lines
jj = obj%entityCounts(1)
DO ii = 1, obj%entityCounts(2)
  obj%topology(jj + ii) = ReferenceTopology(nptrs=edgecon(:, ii),  &
    & name=edgetype)
END DO

CALL GetFaceElemType_Tetrahedron(faceElemType=faceElemType,  &
  & tFaceNodes=tFaceNodes, elemType=obj%name)
CALL Reallocate(facecon, tFaceNodes(1), obj%entityCounts(3))
CALL GetFaceConnectivity_Tetrahedron(con=facecon, order=order)

! faces
jj = jj + obj%entityCounts(2)
DO ii = 1, obj%entityCounts(3)
  obj%topology(jj + ii) = ReferenceTopology( &
    & nptrs=facecon(1:tFaceNodes(ii), ii),  &
    & name=faceElemType(ii))
END DO

! cell
jj = jj + obj%entityCounts(3)
obj%topology(jj + 1) = ReferenceTopology( &
  & arange(1_I4B, obj%entityCounts(1)), obj%name)

IF (ALLOCATED(edgecon)) DEALLOCATE (edgecon)
IF (ALLOCATED(facecon)) DEALLOCATE (facecon)

END PROCEDURE HighOrderElement_Tetrahedron

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
CHARACTER(1) :: layout

layout = refTetrahedron(1:1)

SELECT CASE (layout)
CASE ("B", "b")
  ans(1:3, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
  ans(1:3, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
  ans(1:3, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
  ans(1:3, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]

CASE ("U", "u")
  ans(1:3, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
  ans(1:3, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
  ans(1:3, 3) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
  ans(1:3, 4) = [0.0_DFP, 0.0_DFP, 1.0_DFP]

END SELECT
END PROCEDURE RefCoord_Tetrahedron

!----------------------------------------------------------------------------
!                                           GetEdgeConnectivity_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeConnectivity_Tetrahedron
INTEGER(I4B) :: order0, ii, jj, iface
con(1:2, 1) = [1, 2]
con(1:2, 2) = [1, 3]
con(1:2, 3) = [1, 4]
con(1:2, 4) = [2, 3]
con(1:2, 5) = [2, 4]
con(1:2, 6) = [3, 4]

order0 = Input(default=1_I4B, option=order)

IF (PRESENT(ncol)) ncol = 6
IF (PRESENT(nrow)) nrow = order0 + 1

jj = 4
DO iface = 1, 6
  DO ii = 1, order0 - 1
    con(2 + ii, iface) = jj + ii
  END DO
  jj = jj + order0 - 1
END DO

END PROCEDURE GetEdgeConnectivity_Tetrahedron

!----------------------------------------------------------------------------
!                                           GetFaceConnectivity_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceConnectivity_Tetrahedron
INTEGER(I4B) :: order0, jj
con(1:3, 1) = [1, 3, 2]
con(1:3, 2) = [1, 2, 4]
con(1:3, 3) = [1, 4, 3]
con(1:3, 4) = [2, 3, 4]

order0 = Input(default=1_I4B, option=order)
IF (PRESENT(ncol)) ncol = 4

jj = 4_I4B

SELECT CASE (order0)
CASE (2_I4B)
  con(jj:6, 1) = [6, 8, 5]
  con(jj:6, 2) = [5, 9, 7]
  con(jj:6, 3) = [7, 10, 6]
  con(jj:6, 4) = [8, 10, 9]
  jj = 7
END SELECT

IF (PRESENT(nrow)) nrow = jj - 1

END PROCEDURE GetFaceConnectivity_Tetrahedron

!----------------------------------------------------------------------------
!                                                            GetFaceElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType_Tetrahedron
INTEGER(I4B) :: elemType0
elemType0 = Input(default=Tetrahedron4, option=elemType)

SELECT CASE (elemType0)
CASE (Tetrahedron4)
  IF (PRESENT(faceElemType)) &
    faceElemType(1:4) = Triangle3

  IF (PRESENT(tFaceNodes)) tFaceNodes(1:4) = 3_I4B

CASE (Tetrahedron10)
  IF (PRESENT(faceElemType)) &
    faceElemType(1:4) = Triangle6

  IF (PRESENT(tFaceNodes)) tFaceNodes(1:4) = 6_I4B

CASE (Tetrahedron20)
  IF (PRESENT(faceElemType)) &
    faceElemType(1:4) = Triangle10

  IF (PRESENT(tFaceNodes)) tFaceNodes(1:4) = 10_I4B

CASE (Tetrahedron35)
  IF (PRESENT(faceElemType)) &
    faceElemType(1:4) = Triangle15

  IF (PRESENT(tFaceNodes)) tFaceNodes(1:4) = 15_I4B

CASE (Tetrahedron56)
  IF (PRESENT(faceElemType)) &
    faceElemType(1:4) = Triangle21

  IF (PRESENT(tFaceNodes)) tFaceNodes(1:4) = 21_I4B

END SELECT
END PROCEDURE GetFaceElemType_Tetrahedron

END SUBMODULE Methods
