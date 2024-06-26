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
! summary: This module contains method for [[ReferenceHexahedron_]]

SUBMODULE(ReferenceHexahedron_Method) Methods
USE ReferenceElement_Method
USE ApproxUtility
USE InvUtility
USE InputUtility
USE StringUtility
USE ArangeUtility
USE MiscUtility, ONLY: Int2STR

USE ReferenceLine_Method, ONLY: ElementType_Line

USE ReferenceQuadrangle_Method, ONLY: RefQuadrangleCoord,  &
  & ElementOrder_Quadrangle,  &
  & TotalEntities_Quadrangle,  &
  & FacetTopology_Quadrangle

USE QuadrangleInterpolationUtility, ONLY: InterpolationPoint_Quadrangle

USE HexahedronInterpolationUtility, ONLY: LagrangeDOF_Hexahedron,  &
  & InterpolationPoint_Hexahedron

USE ReferencePrism_Method, ONLY: PolyhedronVolume3d
USE ReallocateUtility

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   ElementName_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementName_Hexahedron
SELECT CASE (elemType)
CASE (Hexahedron8)
  ans = "Hexahedron8"

CASE (Hexahedron27)
  ans = "Hexahedron27"

CASE (Hexahedron20)
  ans = "Hexahedron20"

CASE (Hexahedron64)
  ans = "Hexahedron64"

CASE (Hexahedron125)
  ans = "Hexahedron125"

CASE DEFAULT
  ans = "NONE"
END SELECT

END PROCEDURE ElementName_Hexahedron

!----------------------------------------------------------------------------
!                                                   FacetElements_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Hexahedron1
INTEGER(I4B) :: ii, istart, tsize, jj
TYPE(ReferenceTopology_) :: topo

istart = refelem%entityCounts(1) + refelem%entityCounts(2)
! tPoints + tEdges

ii = 1
ans(ii)%nsd = refelem%nsd
ans(ii)%interpolationPointType = refelem%interpolationPointType
ans(ii)%xij = InterpolationPoint_Quadrangle( &
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

  ans(ii)%order = ElementOrder_Quadrangle(topo%name)
  ans(ii)%entityCounts = TotalEntities_Quadrangle(topo%name)

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
  CALL FacetTopology_Quadrangle(elemType=topo%name,  &
    & nptrs=topo%nptrs, ans=ans(ii)%topology(jj + 1:))

  ! surface
  tsize = jj + ans(ii)%entityCounts(2)
  ans(ii)%topology(tsize + 1) = ReferenceTopology(nptrs=topo%nptrs, &
    & name=topo%name)

END DO

CALL DEALLOCATE (topo)

END PROCEDURE FacetElements_Hexahedron1

!----------------------------------------------------------------------------
!                                                   FacetElements_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Hexahedron2
INTEGER(I4B), PARAMETER :: tface = 6
INTEGER(I4B) :: ii, jj, order, entityCounts(4), tsize
INTEGER(I4B), ALLOCATABLE :: edgeCon(:, :), faceCon(:, :)
INTEGER(I4B) :: faceElemType(tface), tFaceNodes(tface)

CALL GetFaceElemType_Hexahedron( &
  & faceElemType=faceElemType, &
  & tFaceNodes=tFaceNodes, &
  & elemType=elemType)

entityCounts = TotalEntities_Hexahedron(elemType)
order = ElementOrder_Hexahedron(elemType)

CALL Reallocate(edgeCon, order + 1, entityCounts(2))
CALL Reallocate(faceCon, tFaceNodes(1), tface)

CALL GetEdgeConnectivity_Hexahedron(con=edgeCon, order=order)
CALL GetFaceConnectivity_Hexahedron(con=faceCon, order=order)

DO ii = 1, tface

  ans(ii)%xiDimension = 2
  ans(ii)%order = order
  ans(ii)%name = faceElemType(ii)
  ans(ii)%interpolationPointType = Equidistance

  ans(ii)%xij = InterpolationPoint_Quadrangle( &
    & order=ans(ii)%order, &
    & ipType=ans(ii)%interpolationPointType, &
    & layout="VEFC")

  ans(ii)%nsd = nsd
  ans(ii)%entityCounts = TotalEntities_Quadrangle(ans(ii)%name)

  tsize = SUM(ans(ii)%entityCounts)
  CALL RefTopoReallocate(ans(ii)%topology, tsize)
  ! ALLOCATE (ans(ii)%topology(tsize))

  ! points
  DO jj = 1, ans(ii)%entityCounts(1)
    ans(ii)%topology(jj) = Referencetopology(nptrs=faceCon(jj:jj, ii),  &
      & name=Point)
  END DO

  ! lines
  jj = ans(ii)%entityCounts(1)
  CALL FacetTopology_Quadrangle(elemType=ans(ii)%name,  &
    & nptrs=faceCon(:, ii), ans=ans(ii)%topology(jj + 1:))

  ! surface
  tsize = jj + ans(ii)%entityCounts(2)
  ans(ii)%topology(tsize + 1) = ReferenceTopology(nptrs=faceCon(:, ii), &
    & name=ans(ii)%name)

END DO

IF (ALLOCATED(edgeCon)) DEALLOCATE (edgeCon)
IF (ALLOCATED(faceCon)) DEALLOCATE (faceCon)
END PROCEDURE FacetElements_Hexahedron2

!----------------------------------------------------------------------------
!                                                 FacetTopology_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetTopology_Hexahedron
INTEGER(I4B), PARAMETER :: tface = 6
INTEGER(I4B) :: ii, faceElemType(tface), tFaceNodes(tface)
INTEGER(I4B), ALLOCATABLE :: con(:, :)

CALL GetFaceElemType_Hexahedron(faceElemType=faceElemType,  &
  & elemType=elemType, tFaceNodes=tFaceNodes)

CALL Reallocate(con, tFaceNodes(1), tface)

ii = ElementOrder_Hexahedron(elemType=elemType)
CALL GetFaceConnectivity_Hexahedron(con=con, order=ii)

DO ii = 1, tface
  ans(ii)%nptrs = nptrs(con(1:tFaceNodes(ii), ii))
  ans(ii)%xiDimension = 2
  ans(ii)%name = faceElemType(ii)
END DO

IF (ALLOCATED(con)) DEALLOCATE (con)
END PROCEDURE FacetTopology_Hexahedron

!----------------------------------------------------------------------------
!                                                  TotalEntities_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalEntities_Hexahedron
ans(2:4) = [12, 6, 1]
ans(1) = TotalNodesInElement_Hexahedron(elemType)
END PROCEDURE TotalEntities_Hexahedron

!----------------------------------------------------------------------------
!                                            TotalNodesInElement_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalNodesInElement_Hexahedron
SELECT CASE (elemType)
CASE (Hexahedron8)
  ans = 8
CASE (Hexahedron20)
  ans = 20
CASE (Hexahedron27)
  ans = 27
CASE (Hexahedron64)
  ans = 64
CASE (Hexahedron125)
  ans = 125
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE TotalNodesInElement_Hexahedron

!----------------------------------------------------------------------------
!                                                     ElementType_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementType_Hexahedron
SELECT CASE (elemName)
CASE ("Hexahedron8", "Hexahedron")
  ans = Hexahedron8
CASE ("Hexahedron20")
  ans = Hexahedron20
CASE ("Hexahedron27")
  ans = Hexahedron27
CASE ("Hexahedron64")
  ans = Hexahedron64
CASE ("Hexahedron125")
  ans = Hexahedron125
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE ElementType_Hexahedron

!----------------------------------------------------------------------------
!                                                 ElementOrder_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementOrder_Hexahedron
SELECT CASE (ElemType)
CASE (Hexahedron)
  ans = 1
CASE (Hexahedron20)
  ans = 2
CASE (Hexahedron27)
  ans = 2
CASE (Hexahedron64)
  ans = 3
CASE (Hexahedron125)
  ans = 4
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE ElementOrder_Hexahedron

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_Ref_Hexahedron
INTEGER(I4B) :: ii, jj
INTEGER(I4B) :: p1p2(2, 12), lloop(4, 6), vol(8, 1)
REAL(DFP) :: unit_xij(3, 8), biunit_xij(3, 8)

CALL DEALLOCATE (obj)
unit_xij = RefCoord_Hexahedron("UNIT")
biunit_xij = RefCoord_Hexahedron("BIUNIT")

IF (PRESENT(xij)) THEN
  obj%xij = xij(1:3, 1:8)
  IF (ALL(obj%xij(1:3, 1:8) .approxeq.unit_xij)) THEN
    obj%domainName = "UNIT"
  ELSE IF (ALL(obj%xij(1:3, 1:8) .approxeq.biunit_xij)) THEN
    obj%domainName = "BIUNIT"
  ELSE
    obj%domainName = "GENERAL"
  END IF

ELSE

  IF (PRESENT(domainName)) THEN
    obj%domainName = UpperCase(domainName)
    IF (obj%domainName .EQ. "UNIT" .OR. obj%domainName .EQ. "BIUNIT") THEN
      obj%xij = RefCoord_Hexahedron(obj%domainName)
    END IF
  ELSE
    obj%domainName = "BIUNIT"
    obj%xij = RefCoord_Hexahedron(obj%domainName)
  END IF

END IF

CALL GetEdgeConnectivity_Hexahedron(con=p1p2, order=1_I4B)
CALL GetFaceConnectivity_Hexahedron(con=lloop, order=1_I4B)

vol(:, 1) = arange(1_I4B, 8_I4B)

obj%entityCounts = TotalEntities_Hexahedron(Hexahedron8)
obj%xidimension = 3
obj%name = Hexahedron8
obj%order = 1
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
  obj%topology(jj + ii) = ReferenceTopology(nptrs=p1p2(:, ii),  &
    & name=Line2)
END DO

! faces
jj = jj + obj%entityCounts(2)
DO ii = 1, obj%entityCounts(3)
  obj%topology(jj + ii) = ReferenceTopology(nptrs=lloop(:, ii),  &
  & name=Quadrangle4)
END DO

! cell
jj = jj + obj%entityCounts(3)
obj%topology(jj + 1) = ReferenceTopology(vol(:, 1), Hexahedron8)

obj%highorderElement => HighorderElement_Hexahedron
END PROCEDURE Initiate_Ref_Hexahedron

!----------------------------------------------------------------------------
!                                                       ReferenceHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE Reference_Hexahedron
CALL Initiate_Ref_Hexahedron(obj=obj, nsd=NSD, xij=xij,  &
  & domainName=domainName)
END PROCEDURE Reference_Hexahedron

!----------------------------------------------------------------------------
!                                                        ReferenceHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE Reference_Hexahedron_Pointer
ALLOCATE (obj)
CALL Initiate_Ref_Hexahedron(obj=obj, nsd=NSD, xij=xij,  &
  & domainName=domainName)
END PROCEDURE Reference_Hexahedron_Pointer

!----------------------------------------------------------------------------
!                                                          highOrderElement
!----------------------------------------------------------------------------

MODULE PROCEDURE HighorderElement_Hexahedron
INTEGER(I4B) :: ii, tFaceNodes(8), faceElemType(8), jj,  &
  & edgetype
INTEGER(I4B), ALLOCATABLE :: edgecon(:, :), facecon(:, :)

CALL DEALLOCATE (obj)

obj%xij = InterpolationPoint_Hexahedron( &
  & xij=refelem%xij(1:3, 1:8), &
  & order=order, &
  & ipType=ipType, &
  & layout="VEFC")

obj%domainName = refelem%domainName
obj%nsd = refelem%nsd
obj%highOrderElement => refelem%highOrderElement
obj%order = order
obj%xiDimension = refelem%xiDimension

ii = LagrangeDOF_Hexahedron(order=order)
obj%name = ElementType_Hexahedron("Hexahedron"//INT2STR(ii))
obj%entityCounts = TotalEntities_Hexahedron(obj%name)

ii = SUM(obj%entityCounts)
CALL RefTopoReallocate(obj%topology, ii)

! points
DO ii = 1, obj%entityCounts(1)
  obj%topology(ii) = ReferenceTopology([ii], Point)
END DO

CALL Reallocate(edgecon, order + 1, obj%entityCounts(2))
CALL GetEdgeConnectivity_Hexahedron(con=edgecon, order=order)
edgetype = ElementType_Line("Line"//Int2STR(order + 1))

! lines
jj = obj%entityCounts(1)
DO ii = 1, obj%entityCounts(2)
  obj%topology(jj + ii) = ReferenceTopology(nptrs=edgecon(:, ii),  &
    & name=edgetype)
END DO

CALL GetFaceElemType_Hexahedron(faceElemType=faceElemType,  &
  & tFaceNodes=tFaceNodes, elemType=obj%name)
CALL Reallocate(facecon, tFaceNodes(1), obj%entityCounts(3))
CALL GetFaceConnectivity_Hexahedron(con=facecon, order=order)

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

END PROCEDURE HighorderElement_Hexahedron

!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Hexahedron
INTEGER(I4B) :: Order0(6), Node0(6, 4), FM(6, 7), iFace, b
Order0 = [4, 4, 4, 4, 4, 4]
FM = FacetMatrix(RefElem)
DO iFace = 1, 6
  b = FM(iFace, 3) + 3
  Node0(iFace, 1:Order0(iFace)) = FM(iFace, 4:b)
END DO
CALL PolyhedronVolume3d(coord=XiJ(1:3, 1:8), &
  & order_max=4, face_num=6, node=node0, node_num=8, &
  & order=order0, ans=ans)
END PROCEDURE Measure_Simplex_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Hexahedron_Quality
ans = 0.0_I4B
! TODO Implement Hexahedron_quality
END PROCEDURE Hexahedron_quality

!----------------------------------------------------------------------------
!                                                       HexahedronVolume3D
!----------------------------------------------------------------------------

MODULE PROCEDURE HexahedronVolume3D
TYPE(ReferenceHexahedron_) :: refelem
CALL Initiate_Ref_Hexahedron(obj=refelem, nsd=3_I4B)
ans = Measure_Simplex_Hexahedron(refelem=refelem, xij=xij)
CALL DEALLOCATE (refelem)
END PROCEDURE HexahedronVolume3D

!----------------------------------------------------------------------------
!                                                       RefHexahedronCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE RefHexahedronCoord
REAL(DFP) :: one, mone
CHARACTER(:), ALLOCATABLE :: astr

astr = UpperCase(refHexahedron)

SELECT CASE (astr)
CASE ("UNIT")
  one = 1.0_DFP
  mone = 0.0_DFP
CASE ("BIUNIT")
  one = 1.0_DFP
  mone = -1.0_DFP
END SELECT

astr = ""

ans(3, 1:4) = mone
ans(3, 5:8) = one
ans(1:2, 1:4) = RefQuadrangleCoord(refHexahedron)
ans(1:2, 5:8) = ans(1:2, 1:4)
END PROCEDURE RefHexahedronCoord

!----------------------------------------------------------------------------
!                                            GetEdgeConnectivity_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeConnectivity_Hexahedron
INTEGER(I4B) :: order0, ii, jj, iface
con(1:2, 1) = [1, 2]
con(1:2, 2) = [1, 4]
con(1:2, 3) = [1, 5]
con(1:2, 4) = [2, 3]
con(1:2, 5) = [2, 6]
con(1:2, 6) = [3, 4]
con(1:2, 7) = [3, 7]
con(1:2, 8) = [4, 8]
con(1:2, 9) = [5, 6]
con(1:2, 10) = [5, 8]
con(1:2, 11) = [6, 7]
con(1:2, 12) = [7, 8]

order0 = Input(default=1_I4B, option=order)

IF (PRESENT(ncol)) ncol = 12
IF (PRESENT(nrow)) nrow = order0 + 1

jj = 8
DO iface = 1, 12
  DO ii = 1, order0 - 1
    con(2 + ii, iface) = jj + ii
  END DO
  jj = jj + order0 - 1
END DO

END PROCEDURE GetEdgeConnectivity_Hexahedron

!----------------------------------------------------------------------------
!                                            GetFaceConnectivity_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceConnectivity_Hexahedron
INTEGER(I4B) :: order0, ii
con(1:4, 1) = [1, 4, 3, 2] ! back
con(1:4, 2) = [5, 6, 7, 8] ! front
con(1:4, 3) = [1, 5, 8, 4] ! left
con(1:4, 4) = [2, 3, 7, 6] ! right
con(1:4, 5) = [1, 2, 6, 5] ! bottom
con(1:4, 6) = [3, 4, 8, 7] ! top

order0 = Input(default=1_I4B, option=order)

IF (PRESENT(ncol)) ncol = 6

ii = 5

SELECT CASE (order0)
CASE (2_I4B)
  con(ii:8, 1) = [10, 14, 12, 9, 21] ! back
  con(ii:8, 2) = [17, 19, 20, 18, 22] ! front
  con(ii:8, 3) = [11, 18, 16, 10, 23] ! left
  con(ii:8, 4) = [12, 15, 19, 13, 24] ! right
  con(ii:8, 5) = [9, 13, 17, 11, 25] ! bottom
  con(ii:8, 6) = [14, 16, 20, 15, 26] ! top
  ii = 9
END SELECT

IF (PRESENT(nrow)) nrow = ii - 1

END PROCEDURE GetFaceConnectivity_Hexahedron

!----------------------------------------------------------------------------
!                                               GetFaceElemType_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType_Hexahedron
INTEGER(I4B) :: elemType0
elemType0 = Input(default=Hexahedron8, option=elemType)

SELECT CASE (elemType0)
CASE (Hexahedron8)

  IF (PRESENT(faceElemType)) faceElemType(1:6) = Quadrangle4
  IF (PRESENT(tFaceNodes)) tFaceNodes(1:6) = 4_I4B

CASE (Hexahedron20)
  IF (PRESENT(faceElemType)) faceElemType(1:6) = Quadrangle8
  IF (PRESENT(tFaceNodes)) tFaceNodes(1:6) = 8_I4B

CASE (Hexahedron27)
  IF (PRESENT(faceElemType)) faceElemType(1:6) = Quadrangle9
  IF (PRESENT(tFaceNodes)) tFaceNodes(1:6) = 9_I4B

CASE (Hexahedron64)
  IF (PRESENT(faceElemType)) faceElemType(1:6) = Quadrangle16
  IF (PRESENT(tFaceNodes)) tFaceNodes(1:6) = 16_I4B

END SELECT
END PROCEDURE GetFaceElemType_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
