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

USE GlobalData, ONLY: Quadrangle, Quadrangle4, Quadrangle8, Quadrangle9, &
                      Quadrangle16, Point, Line2, Equidistance, INT8

USE ReferenceElement_Method, ONLY: ReferenceTopology, DEALLOCATE, &
                                   ReferenceElement_Initiate => Initiate

USE LineInterpolationUtility, ONLY: InterpolationPoint_Line

USE ReferenceLine_Method, ONLY: ElementOrder_Line, LineName

USE QuadrangleInterpolationUtility, ONLY: InterpolationPoint_Quadrangle, &
                                          LagrangeDOF_Quadrangle

USE ReferenceTriangle_Method, ONLY: TRIANGLEAREA2D

USE ReferenceLine_Method, ONLY: Linename, ElementType_Line

USE ApproxUtility, ONLY: OPERATOR(.approxeq.)

USE AppendUtility

USE StringUtility, ONLY: UpperCase

USE ArangeUtility, ONLY: Arange

USE InputUtility, ONLY: Input

USE SortUtility, ONLY: Sort

USE ReallocateUtility, ONLY: Reallocate

USE Display_Method, ONLY: ToString

USE MiscUtility, ONLY: Int2Str

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     ElementName_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementName_Quadrangle
SELECT CASE (elemType)
CASE (Quadrangle4)
  ans = "Quadrangle4"
CASE (Quadrangle8)
  ans = "Quadrangle8"
CASE (Quadrangle9)
  ans = "Quadrangle9"
CASE (Quadrangle16)
  ans = "Quadrangle16"
CASE default
  ans = ""
END SELECT
END PROCEDURE ElementName_Quadrangle

!----------------------------------------------------------------------------
!                                                   FacetTopology_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetTopology_Quadrangle
INTEGER(I4B) :: order, ii, lineType
INTEGER(I4B), ALLOCATABLE :: con(:, :)

order = ElementOrder_Quadrangle(elemType)
CALL Reallocate(con, order + 1, 4)
CALL GetEdgeConnectivity_Quadrangle(con=con, &
                             opt=DEFAULT_OPT_QUADRANGLE_EDGE_CON, order=order)
lineType = ElementType_Line("Line"//Int2Str(order + 1))

DO ii = 1, 4
  ans(ii)%nptrs = nptrs(con(:, ii))
  ans(ii)%xiDimension = 1
  ans(ii)%name = lineType
END DO

IF (ALLOCATED(con)) DEALLOCATE (con)

END PROCEDURE FacetTopology_Quadrangle

!----------------------------------------------------------------------------
!                                                    TotalEntities_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalEntities_Quadrangle
ans(2:4) = [4, 1, 0]
ans(1) = TotalNodesInElement_Quadrangle(elemType)
END PROCEDURE TotalEntities_Quadrangle

!----------------------------------------------------------------------------
!                                              TotalNodesInElement_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalNodesInElement_Quadrangle
SELECT CASE (elemType)
CASE (Quadrangle4)
  ans = 4
CASE (Quadrangle8)
  ans = 8
CASE (Quadrangle9)
  ans = 9
CASE (Quadrangle16)
  ans = 16
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE TotalNodesInElement_Quadrangle

!----------------------------------------------------------------------------
!                                                     ElementOrder_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementOrder_Quadrangle
SELECT CASE (elemType)
CASE (Quadrangle4)
  ans = 1
CASE (Quadrangle8)
  ans = 2
CASE (Quadrangle9)
  ans = 2
CASE (Quadrangle16)
  ans = 3
END SELECT
END PROCEDURE ElementOrder_Quadrangle

!----------------------------------------------------------------------------
!                                                     ElementType_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementType_Quadrangle
SELECT CASE (elemName)
CASE ("Quadrangle4", "Quadrangle")
  ans = Quadrangle4
CASE ("Quadrangle8")
  ans = Quadrangle8
CASE ("Quadrangle9")
  ans = Quadrangle9
CASE ("Quadrangle16")
  ans = Quadrangle16
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE ElementType_Quadrangle

!----------------------------------------------------------------------------
!                                                    FacetElements_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Quadrangle1
INTEGER(I4B) :: ii, istart, tsize, jj
TYPE(Referencetopology_) :: topo

istart = refelem%entityCounts(1)

ans(1)%xij = InterpolationPoint_Line( &
             order=refelem%order, &
             ipType=refelem%interpolationPointType, &
             layout="VEFC")

ans(1)%interpolationPointType = refelem%interpolationPointType
ans(1)%nsd = refelem%nsd
DO ii = 2, 4
  ans(ii)%xij = ans(1)%xij
  ans(ii)%interpolationPointType = ans(1)%interpolationPointType
  ans(ii)%nsd = ans(1)%nsd
END DO

DO ii = 1, 4
  topo = refelem%topology(istart + ii)
  tsize = SIZE(topo%nptrs)
  ans(ii)%xiDimension = topo%xiDimension
  ans(ii)%name = topo%name
  ans(ii)%order = ElementOrder_Line(elemType=topo%name)
  ans(ii)%entityCounts = [tsize, 1, 0, 0]

  ALLOCATE (ans(ii)%topology(tsize + 1))

  DO jj = 1, tsize
    ans(ii)%topology(jj) = Referencetopology( &
      & nptrs=topo%nptrs(jj:jj), name=Point)
  END DO

  ans(ii)%topology(tsize + 1) = Referencetopology( &
    & nptrs=topo%nptrs, name=topo%name)
END DO

CALL DEALLOCATE (topo)

END PROCEDURE FacetElements_Quadrangle1

!----------------------------------------------------------------------------
!                                                    FacetElements_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Quadrangle2
INTEGER(I4B) :: ii, jj, order
INTEGER(I4B), ALLOCATABLE :: edgeCon(:, :)

order = ElementOrder_Quadrangle(elemType)
CALL Reallocate(edgeCon, order + 1, 4)
CALL GetEdgeConnectivity_Quadrangle(con=edgeCon, &
                             opt=DEFAULT_OPT_QUADRANGLE_EDGE_CON, order=order)
!! The edges are accordign to gmsh
!! [1,2], [2,3], [3,4], [4,1]

DO ii = 1, 4

  ans(ii)%xiDimension = 1
  ans(ii)%order = order
  ans(ii)%name = ElementType_Line("Line"//tostring(order + 1))
  ans(ii)%interpolationPointType = Equidistance
  ans(ii)%xij = InterpolationPoint_Line(order=order, ipType=Equidistance, &
                                        layout="VEFC")

  ans(ii)%nsd = nsd
  ans(ii)%entityCounts = [order + 1, 1, 0, 0]
  ALLOCATE (ans(ii)%topology(order + 2))

  DO jj = 1, order + 1
    ans(ii)%topology(jj) = Referencetopology(nptrs=edgeCon(jj:jj, ii), &
                                             name=Point)
  END DO

  ans(ii)%topology(order + 2) = Referencetopology(nptrs=edgeCon(1:2, ii), &
                                                  name=ans(ii)%name)

END DO

IF (ALLOCATED(edgeCon)) DEALLOCATE (edgeCon)

END PROCEDURE FacetElements_Quadrangle2

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
CALL initiate_ref_quadrangle(obj=obj, nsd=NSD, xij=xij, domainName=domainName)
END PROCEDURE reference_Quadrangle

!----------------------------------------------------------------------------
!                                               ReferenceQuadrangle_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Quadrangle_Pointer
ALLOCATE (obj)
CALL initiate_ref_quadrangle(obj=obj, nsd=NSD, xij=xij, domainName=domainName)
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
  CALL ReferenceElement_Initiate(obj=obj, Anotherobj=refelem)
CASE DEFAULT
  obj%xij = InterpolationPoint_Quadrangle(xij=refelem%xij, order=order, &
                                          ipType=ipType, layout="VEFC")
  obj%domainName = refelem%domainName
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

MODULE PROCEDURE Quadrangle_Quality
ans = 0.0_DFP
END PROCEDURE Quadrangle_Quality

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!                                                        RefQuadrangleCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE RefQuadrangleCoord
CHARACTER(1) :: astr
astr = refQuadrangle(1:1)

SELECT CASE (astr)
CASE ("U", "u")
  ans(1, 1:4) = [0.0_DFP, 1.0_DFP, 1.0_DFP, 0.0_DFP]
  ans(2, 1:4) = [0.0_DFP, 0.0_DFP, 1.0_DFP, 1.0_DFP]

CASE ("B", "b")
  ans(1, 1:4) = [-1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP]
  ans(2, 1:4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP]
END SELECT
END PROCEDURE RefQuadrangleCoord

!----------------------------------------------------------------------------
!                                              GetEdgeConnectivity_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeConnectivity_Quadrangle
INTEGER(I4B) :: opt0, order0, ii, jj, iface

opt0 = Input(default=1_I4B, option=opt)
order0 = Input(default=1_I4B, option=order)

IF (PRESENT(ncol)) ncol = 4
IF (PRESENT(nrow)) nrow = 1 + order0

SELECT CASE (opt0)
CASE (1_I4B)
  con(1:2, 1) = [1, 2]
  con(1:2, 2) = [4, 3]
  con(1:2, 3) = [1, 4]
  con(1:2, 4) = [2, 3]
CASE (2_I4B)
  !! For Lagrangian polynomial
  con(1:2, 1) = [1, 2]
  con(1:2, 2) = [2, 3]
  con(1:2, 3) = [3, 4]
  con(1:2, 4) = [4, 1]
END SELECT

jj = 4
DO iface = 1, 4
  DO ii = 1, order0 - 1
    con(2 + ii, iface) = jj + ii
  END DO
  jj = jj + order0 - 1
END DO

END PROCEDURE GetEdgeConnectivity_Quadrangle

!----------------------------------------------------------------------------
!                                           GetFaceConnectivity_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceConnectivity_Quadrangle
CALL GetEdgeConnectivity_Quadrangle(con=con, opt=2_I4B, order=order, &
                                    nrow=nrow, ncol=ncol)
END PROCEDURE GetFaceConnectivity_Quadrangle

!----------------------------------------------------------------------------
!                                               FaceShapeMetaData_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FaceShapeMetaData_Quadrangle
INTEGER(I4B) :: a(4), localFaces0(4)

a(1) = MINLOC(face, 1)
a(2) = HelpFaceData_Quadrangle(1, a(1)) !b
a(3) = HelpFaceData_Quadrangle(2, a(1)) !c
a(4) = HelpFaceData_Quadrangle(3, a(1)) !d

localFaces0(1:4) = face(a)
IF (PRESENT(localFaces)) THEN
  localFaces(1:4) = localFaces0(1:4)
END IF

sorted_face(1) = localFaces0(1)
sorted_face(3) = localFaces0(3)

IF (localFaces0(2) .LT. localFaces0(4)) THEN
  sorted_face(2) = localFaces0(2)
  sorted_face(4) = localFaces0(4)

  IF (PRESENT(faceOrient)) THEN
    faceOrient(3) = 1_INT8
    faceOrient(1) = SIGN(1, localFaces0(2) - localFaces0(1))
    faceOrient(2) = SIGN(1, localFaces0(4) - localFaces0(1))
  END IF

ELSE
  sorted_face(2) = localFaces0(4)
  sorted_face(4) = localFaces0(2)

  IF (PRESENT(faceOrient)) THEN
    faceOrient(3) = -1_INT8
    faceOrient(1) = SIGN(1, localFaces0(4) - localFaces0(1))
    faceOrient(2) = SIGN(1, localFaces0(2) - localFaces0(1))
  END IF

END IF

END PROCEDURE FaceShapeMetaData_Quadrangle

!----------------------------------------------------------------------------
!                                             GetFaceElemType_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType_Quadrangle
INTEGER(I4B) :: order
order = ElementOrder_Quadrangle(Input(default=Quadrangle, option=elemType))
IF (PRESENT(faceElemType)) faceElemType(1:4) = LineName(order)
IF (PRESENT(tFaceNodes)) tFaceNodes(1:4) = order + 1
END PROCEDURE GetFaceElemType_Quadrangle

END SUBMODULE Methods
