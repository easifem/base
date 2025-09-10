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
! date:  2 March 2021
! summary: This submodule contains methods for [[ReferenceTriangle_]]

SUBMODULE(ReferenceTriangle_Method) Methods
USE BaseType, ONLY: QualityMeasure
USE ReferenceElement_Method
USE StringUtility
USE ApproxUtility
USE ArangeUtility
USE TriangleInterpolationUtility, ONLY: InterpolationPoint_Triangle,  &
  & LagrangeDOF_Triangle
USE Triangle_Method
USE InputUtility
USE ReferenceLine_Method, ONLY: ElementType_Line,  &
  & ElementOrder_Line
USE LineInterpolationUtility, ONLY: InterpolationPoint_Line
USE MiscUtility, ONLY: Int2Str
USE Display_Method
USE ReallocateUtility

! USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     ElementName_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementName_Triangle
SELECT CASE (elemType)
CASE (Triangle3)
  ans = "Triangle3"
CASE (Triangle6)
  ans = "Triangle6"
CASE (Triangle9)
  ans = "Triangle9"
CASE (Triangle10)
  ans = "Triangle10"
CASE (Triangle12)
  ans = "Triangle12"
CASE (Triangle15a)
  ans = "Triangle15a"
CASE (Triangle15b)
  ans = "Triangle15b"
CASE (Triangle21)
  ans = "Triangle21"
CASE DEFAULT
  ans = "NONE"
END SELECT
END PROCEDURE ElementName_Triangle

!----------------------------------------------------------------------------
!                                                     FacetTopology_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetTopology_Triangle
INTEGER(I4B) :: order, ii, lineType
INTEGER(I4B), ALLOCATABLE :: con(:, :)

order = ElementOrder_Triangle(elemType)
CALL Reallocate(con, order + 1, 3)
CALL GetFaceConnectivity_Triangle(con=con,  &
  & opt=DEFAULT_OPT_TRIANGLE_EDGE_CON, order=order)
lineType = ElementType_Line("Line"//Int2Str(order + 1))

DO ii = 1, 3
  ans(ii)%nptrs = nptrs(con(:, ii))
  ans(ii)%xiDimension = 1
  ans(ii)%name = lineType
END DO

IF (ALLOCATED(con)) DEALLOCATE (con)

END PROCEDURE FacetTopology_Triangle

!----------------------------------------------------------------------------
!                                                    TotalEntities_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalEntities_Triangle
ans(2:4) = [3, 1, 0]
ans(1) = TotalNodesInElement_Triangle(elemType)
END PROCEDURE TotalEntities_Triangle

!----------------------------------------------------------------------------
!                                              TotalNodesInElement_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalNodesInElement_Triangle
SELECT CASE (ElemType)
CASE (Triangle3)
  ans = 3
CASE (Triangle6)
  ans = 6
CASE (Triangle9)
  ans = 9
CASE (Triangle10)
  ans = 10
CASE (Triangle12)
  ans = 12
CASE (Triangle15a)
  ans = 15
CASE (Triangle15b)
  ans = 15
CASE (Triangle21)
  ans = 21
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE TotalNodesInElement_Triangle

!----------------------------------------------------------------------------
!                                                     ElementOrder_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementOrder_Triangle
SELECT CASE (ElemType)
CASE (Triangle3)
  ans = 1
CASE (Triangle6)
  ans = 2
CASE (Triangle9)
  ans = 3
CASE (Triangle10)
  ans = 3
CASE (Triangle12)
  ans = 4
CASE (Triangle15a)
  ans = 4
CASE (Triangle15b)
  ans = 5
CASE (Triangle21)
  ans = 5
END SELECT
END PROCEDURE ElementOrder_Triangle

!----------------------------------------------------------------------------
!                                                     ElementType_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementType_Triangle
SELECT CASE (elemName)
CASE ("Triangle3", "Triangle")
  ans = Triangle3
CASE ("Triangle6")
  ans = Triangle6
CASE ("Triangle9")
  ans = Triangle9
CASE ("Triangle10")
  ans = Triangle10
CASE ("Triangle12")
  ans = Triangle12
CASE ("Triangle15a")
  ans = Triangle15a
CASE ("Triangle15b")
  ans = Triangle15b
CASE ("Triangle21")
  ans = Triangle21
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE ElementType_Triangle

!----------------------------------------------------------------------------
!                                                    FacetElements_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Triangle1
INTEGER(I4B) :: ii, istart, tsize, jj
TYPE(ReferenceTopology_) :: topo

istart = refelem%entityCounts(1)

ans(1)%xij = InterpolationPoint_Line(  &
  & order=refelem%order, &
  & ipType=refelem%interpolationPointType, &
  & layout="VEFC")

ans(1)%interpolationPointType = refelem%interpolationPointType
ans(1)%nsd = refelem%nsd
DO ii = 2, 3
  ans(ii)%xij = ans(1)%xij
  ans(ii)%interpolationPointType = ans(1)%interpolationPointType
  ans(ii)%nsd = ans(1)%nsd
END DO

DO ii = 1, 3
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

END PROCEDURE FacetElements_Triangle1

!----------------------------------------------------------------------------
!                                                    FacetElements_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Triangle2
INTEGER(I4B) :: ii, jj, order
INTEGER(I4B), ALLOCATABLE :: facecon(:, :)

order = ElementOrder_Triangle(elemType)
CALL Reallocate(facecon, order + 1, 3)
CALL GetFaceConnectivity_Triangle(con=facecon,  &
  & opt=DEFAULT_OPT_TRIANGLE_EDGE_CON, order=order)
!! The edges are accordign to gmsh
!! [1,2], [2,3], [3,1]

DO ii = 1, 3

  ans(ii)%xiDimension = 1
  ans(ii)%order = order
  ans(ii)%name = ElementType_Line("Line"//tostring(order + 1))
  ans(ii)%interpolationPointType = Equidistance
  ans(ii)%xij = InterpolationPoint_Line(  &
    & order=ans(ii)%order, &
    & ipType=ans(ii)%interpolationPointType, &
    & layout="VEFC")

  ans(ii)%nsd = nsd
  ans(ii)%entityCounts = [order + 1, 1, 0, 0]
  ALLOCATE (ans(ii)%topology(order + 2))

  DO jj = 1, order + 1
    ans(ii)%topology(jj) = Referencetopology(nptrs=facecon(jj:jj, ii),  &
      & name=Point)
  END DO

  ans(ii)%topology(order + 2) = Referencetopology(nptrs=facecon(1:2, ii),  &
    & name=ans(ii)%name)

END DO

IF (ALLOCATED(facecon)) DEALLOCATE (facecon)

END PROCEDURE FacetElements_Triangle2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_ref_Triangle
REAL(DFP) :: unit_xij(2, 3), biunit_xij(2, 3)
INTEGER(I4B) :: facecon(2, 3), ii

CALL DEALLOCATE (obj)

unit_xij = RefCoord_Triangle("UNIT")
biunit_xij = RefCoord_Triangle("BIUNIT")

IF (PRESENT(xij)) THEN
  obj%xij = xij(1:2, 1:3)
  IF (ALL(obj%xij(1:2, 1:3) .approxeq.unit_xij)) THEN
    obj%domainName = "UNIT"
  ELSE IF (ALL(obj%xij(1:2, 1:3) .approxeq.biunit_xij)) THEN
    obj%domainName = "BIUNIT"
  ELSE
    obj%domainName = "GENERAL"
  END IF

ELSE

  IF (PRESENT(domainName)) THEN
    obj%domainName = UpperCase(domainName)
    IF (obj%domainName .EQ. "UNIT" .OR. obj%domainName .EQ. "BIUNIT") THEN
      obj%xij = RefCoord_Triangle(obj%domainName)
    END IF
  ELSE
    obj%domainName = "UNIT"
    obj%xij = RefCoord_Triangle(obj%domainName)
  END IF

END IF

obj%entityCounts = [3, 3, 1, 0]
obj%xiDimension = 2
obj%name = Triangle3
obj%order = 1
obj%nsd = nsd

ALLOCATE (obj%topology(7))
obj%topology(1) = Referencetopology([1], Point)
obj%topology(2) = Referencetopology([2], Point)
obj%topology(3) = Referencetopology([3], Point)

CALL GetFaceConnectivity_Triangle(con=facecon,  &
  & opt=DEFAULT_OPT_TRIANGLE_EDGE_CON,  &
  & order=1)

DO ii = 1, 3
  obj%topology(3 + ii) = Referencetopology(facecon(1:2, ii), Line2)
END DO

obj%topology(7) = Referencetopology([1, 2, 3], Triangle3)

obj%highorderElement => highorderElement_Triangle
END PROCEDURE initiate_ref_Triangle

!----------------------------------------------------------------------------
!                                                          ReferenceTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Triangle
CALL initiate_ref_triangle(obj=obj, nsd=nsd, xij=xij, domainName=domainName)
END PROCEDURE reference_Triangle

!----------------------------------------------------------------------------
!                                                          ReferenceTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Triangle_Pointer
ALLOCATE (obj)
CALL initiate_ref_triangle(obj=obj, nsd=nsd, xij=xij, domainName=domainName)
END PROCEDURE reference_Triangle_Pointer

!----------------------------------------------------------------------------
!                                                           LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE HighorderElement_Triangle
INTEGER(I4B) :: linetype, ii, nns
INTEGER(I4B), ALLOCATABLE :: facecon(:, :)

CALL DEALLOCATE (obj)

obj%xij = InterpolationPoint_Triangle( &
  & xij=refelem%xij(:, 1:3), &
  & order=order, &
  & ipType=ipType, &
  & layout="VEFC")

obj%domainName = refelem%domainName
obj%nsd = refelem%nsd
obj%highOrderElement => refelem%highOrderElement
obj%order = order
obj%xidimension = refelem%xidimension
nns = LagrangeDOF_Triangle(order=order)
obj%name = ElementType_Triangle("Triangle"//Int2Str(nns))
obj%entityCounts = TotalEntities_Triangle(obj%name)
ii = SUM(obj%entityCounts)
CALL RefTopoReallocate(obj%topology, ii)

DO ii = 1, obj%entityCounts(1)
  obj%topology(ii) = ReferenceTopology([ii], Point)
END DO

CALL Reallocate(facecon, order + 1, obj%entityCounts(2))
CALL GetFaceConnectivity_Triangle(con=facecon,  &
  & opt=DEFAULT_OPT_TRIANGLE_EDGE_CON, order=order)

linetype = ElementType_Line("Line"//Int2Str(order + 1))
ii = obj%entityCounts(1)
obj%topology(ii + 1) = ReferenceTopology(facecon(:, 1), linetype)
obj%topology(ii + 2) = ReferenceTopology(facecon(:, 2), linetype)
obj%topology(ii + 3) = ReferenceTopology(facecon(:, 3), linetype)
obj%topology(ii + 4) = ReferenceTopology(facecon(:, 4), linetype)

ii = ii + obj%entityCounts(2)
obj%topology(ii + 1) = ReferenceTopology(arange(1_I4B, nns), obj%name)

IF (ALLOCATED(facecon)) DEALLOCATE (facecon)
END PROCEDURE HighorderElement_Triangle

!----------------------------------------------------------------------------
!                                                            MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Triangle
ans = triangle_area(refelem, xij)
END PROCEDURE Measure_Simplex_Triangle

!----------------------------------------------------------------------------
!                                                           Triangle_Angles
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_angles
SELECT CASE (refelem%nsd)
CASE (2)
  ans = triangle_angles_2d(xij(1:2, 1:3))
CASE (3)
  ans = triangle_angles_3d(xij(1:3, 1:3))
END SELECT
END PROCEDURE triangle_angles

!----------------------------------------------------------------------------
!                                                             Triangle_Area
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_area
SELECT CASE (refelem%nsd)
CASE (2)
  ans = triangle_area_2d(xij(1:2, 1:3))
CASE (3)
  ans = TRIANGLE_AREA_3D(xij(1:3, 1:3))
END SELECT
END PROCEDURE triangle_area

!----------------------------------------------------------------------------
!                                                        Triangle_ArealVector
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_ArealVector
SELECT CASE (refelem%nsd)
CASE (2)
  ans(1:2) = 0.0_DFP
  ans(3) = triangle_area_2d(xij(1:2, 1:3))
CASE (3)
  ans = triangle_area_vector_3d(xij(1:3, 1:3))
END SELECT
END PROCEDURE triangle_ArealVector

!----------------------------------------------------------------------------
!                                                       Triangle_Barycentric
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_barycentric
ans = triangle_barycentric_2d(xij(1:2, 1:3), x(1:2))
END PROCEDURE triangle_barycentric

!----------------------------------------------------------------------------
!                                                         Triangle_Centroid
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_centroid
IF (refelem%nsd .EQ. 2) THEN
  Ans(3) = 0.0_DFP
  ans(1:2) = triangle_centroid_2d(xij)
ELSE
  ans = triangle_centroid_3d(xij)
END IF
END PROCEDURE triangle_centroid

!----------------------------------------------------------------------------
!                                                      triangle_circumcentre
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumcentre
IF (refelem%nsd .EQ. 2) THEN
  Ans(3) = 0.0_DFP
  ans(1:2) = triangle_circumcenter_2d(xij)
ELSE
  ans = triangle_circumcenter(3_I4B, xij)
END IF
END PROCEDURE triangle_circumcentre

!----------------------------------------------------------------------------
!                                                      triangle_circumcircle
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumcircle
Ans(4) = 0.0_DFP
CALL triangle_circumcircle_2d(xij, ans(1), ans(2:3))
END PROCEDURE triangle_circumcircle

!----------------------------------------------------------------------------
!                                                     triangle_circumradius
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumradius
ans = triangle_circumradius_2d(xij)
END PROCEDURE triangle_circumradius

!----------------------------------------------------------------------------
!                                                   triangle_contains_line
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_line
IF (parametricLine) THEN
  CALL triangle_contains_line_par_3d(xij, x1, x2, &
    & inside, xint)
ELSE
  CALL triangle_contains_line_exp_3d(xij, x1, x2, &
    & inside, xint)
END IF
END PROCEDURE triangle_contains_line

!----------------------------------------------------------------------------
!                                                    triangle_contains_point
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_point
ans = triangle_contains_point_2d_1(xij(1:2, 1:3), x(1:2))
END PROCEDURE triangle_contains_point

!----------------------------------------------------------------------------
!                                                         triangle_diameter
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_diameter
ans = triangle_diameter_2d(xij(1:2, 1:3))
END PROCEDURE triangle_diameter

!----------------------------------------------------------------------------
!                                                       triangle_edge_length
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_edge_length
ans = triangle_edge_length_2d(xij(1:2, 1:3))
END PROCEDURE triangle_edge_length

!----------------------------------------------------------------------------
!                                                        triangle_incenter
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_incenter
Ans(3) = 0.0_DFP
ans(1:2) = triangle_incenter_2d(xij(1:2, 1:3))
END PROCEDURE triangle_incenter

!----------------------------------------------------------------------------
!                                                         triangle_incircle
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_incircle
Ans(4) = 0.0_DFP
CALL triangle_incircle_2d(xij(1:2, 1:3), Ans(1), Ans(2:3))
END PROCEDURE triangle_incircle

!----------------------------------------------------------------------------
!                                                          triangle_inradius
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_inradius
ans = triangle_inradius_2d(xij(1:2, 1:3))
END PROCEDURE triangle_inradius

!----------------------------------------------------------------------------
!                                                      triangle_orthocenter
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_orthocenter
Ans(3) = 0.0_DFP
ans(1:2) = triangle_orthocenter_2d(xij(1:2, 1:3))
END PROCEDURE triangle_orthocenter

!----------------------------------------------------------------------------
!                                                       triangle_point_dist
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_point_dist
SELECT CASE (refelem%nsd)
CASE (2)
  ans = triangle_point_dist_2d(xij(1:2, 1:3), x(1:2))
CASE (3)
  ans = triangle_point_dist_3d(xij(1:3, 1:3), x(1:3))
END SELECT
END PROCEDURE triangle_point_dist

!----------------------------------------------------------------------------
!                                                     triangle_nearest_point
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_get_nearest_point
CALL triangle_point_near_2d(xij(1:2, 1:3), x(1:2), xn(1:2), dist)
END PROCEDURE triangle_get_nearest_point

!----------------------------------------------------------------------------
!                                                      triangle_random_point
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_random_point
Ans = 0.0_DFP
ans(1:2, 1:n) = triangle_sample(xij(1:2, 1:3), n, seed)
END PROCEDURE triangle_random_point

!----------------------------------------------------------------------------
!                                                          triangle_quality
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_quality
REAL(DFP) :: rvar(3)

SELECT CASE (measure)

CASE (QualityMeasure%area)
  Ans = Area(refelem=refelem, xij=xij)

CASE (QualityMeasure%maxangle)
  Ans = MAXVAL(Angles(refelem=refelem, xij=xij))

CASE (QualityMeasure%minangle)
  Ans = MINVAL(Angles(refelem=refelem, xij=xij))

CASE (QualityMeasure%angleratio)
  Ans = 3.0_DFP * MINVAL(Angles(refelem=refelem, xij=xij)) / Pi

CASE (QualityMeasure%radiusRatio)
  Ans = 2.0_DFP * InRadius(refelem=refelem, xij=xij) &
    & / CircumRadius(refelem=refelem, xij=xij)

CASE (QualityMeasure%edgeRatio)
  rvar = EdgeLength(refelem=refelem, xij=xij)
  Ans = MINVAL(rvar) / MAXVAL(rvar)

CASE (QualityMeasure%aspectRatio)
  rvar = EdgeLength(refelem=refelem, xij=xij)
  Ans = MAXVAL(rvar) * SUM(rvar) &
    & / (4.0_DFP * SQRT(3.0_DFP) * area(refelem=refelem, xij=xij))
END SELECT
END PROCEDURE triangle_quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE TriangleArea3D
INTEGER(I4B), PARAMETER :: dim_num = 3
REAL(DFP) :: cross(dim_num)

! Compute the cross product vector.
cross(1) = (t(2, 2) - t(2, 1)) * (t(3, 3) - t(3, 1)) &
           - (t(3, 2) - t(3, 1)) * (t(2, 3) - t(2, 1))
cross(2) = (t(3, 2) - t(3, 1)) * (t(1, 3) - t(1, 1)) &
           - (t(1, 2) - t(1, 1)) * (t(3, 3) - t(3, 1))
cross(3) = (t(1, 2) - t(1, 1)) * (t(2, 3) - t(2, 1)) &
           - (t(2, 2) - t(2, 1)) * (t(1, 3) - t(1, 1))
ans = 0.5_DFP * SQRT(SUM(cross(1:3)**2))
END PROCEDURE TriangleArea3D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE TriangleArea2D
ans = 0.5_DFP * ( &
      t(1, 1) * (t(2, 2) - t(2, 3)) &
      + t(1, 2) * (t(2, 3) - t(2, 1)) &
      + t(1, 3) * (t(2, 1) - t(2, 2)))
END PROCEDURE TriangleArea2D

!----------------------------------------------------------------------------
!                                              GetEdgeConnectivity_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeConnectivity_Triangle
INTEGER(I4B) :: opt0, order0, ii, jj, iface

opt0 = Input(default=1_I4B, option=opt)
order0 = Input(default=1_I4B, option=order)
jj = 3

IF (PRESENT(ncol)) ncol = 3
IF (PRESENT(nrow)) nrow = 1 + order0

SELECT CASE (opt0)
CASE (1_I4B)
  con(1:2, 1) = [1, 2]
  con(1:2, 2) = [1, 3]
  con(1:2, 3) = [2, 3]

  iface = 1
  DO ii = 1, order0 - 1
    con(2 + ii, iface) = jj + ii
    jj = jj + 1
  END DO

  iface = 3
  DO ii = 1, order0 - 1
    con(2 + ii, iface) = jj + ii
    jj = jj + 1
  END DO

  iface = 2
  DO ii = 1, order0 - 1
    con(2 + ii, iface) = jj + ii
    jj = jj + 1
  END DO

CASE (2_I4B)
  !! For Lagrangian polynomial
  con(1:2, 1) = [1, 2]
  con(1:2, 2) = [2, 3]
  con(1:2, 3) = [3, 1]

  iface = 1
  DO ii = 1, order0 - 1
    con(2 + ii, iface) = jj + ii
    jj = jj + 1
  END DO

  iface = 2
  DO ii = 1, order0 - 1
    con(2 + ii, iface) = jj + ii
    jj = jj + 1
  END DO

  iface = 3
  DO ii = 1, order0 - 1
    con(2 + ii, iface) = jj + ii
    jj = jj + 1
  END DO
END SELECT

END PROCEDURE GetEdgeConnectivity_Triangle

!----------------------------------------------------------------------------
!                                               GetFaceConnectivity_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceConnectivity_Triangle
CALL GetEdgeconnectivity_Triangle(con=con, opt=2_I4B, order=order, &
                                  nrow=nrow, ncol=ncol)
END PROCEDURE GetFaceConnectivity_Triangle

!----------------------------------------------------------------------------
!                                                         RefTriangleCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE RefTriangleCoord
CHARACTER(1) :: astr

astr = reftriangle(1:1)

SELECT CASE (astr)
CASE ("B", "b")
  ans(1:2, 1) = [-1.0_DFP, -1.0_DFP]
  ans(1:2, 2) = [1.0_DFP, -1.0_DFP]
  ans(1:2, 3) = [-1.0_DFP, 1.0_DFP]

CASE ("U", "u")
  ans(1:2, 1) = [0.0_DFP, 0.0_DFP]
  ans(1:2, 2) = [1.0_DFP, 0.0_DFP]
  ans(1:2, 3) = [0.0_DFP, 1.0_DFP]
END SELECT
END PROCEDURE RefTriangleCoord

!----------------------------------------------------------------------------
!                                               FaceShapeMetaData_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FaceShapeMetaData_Triangle
INTEGER(I4B), PARAMETER :: HelpFaceData_Triangle(2, 3) =  &
  & RESHAPE([ &
    & 2, 3, &
    & 3, 1, &
    & 1, 2  &
  & ], [2, 3])

INTEGER(I4B) :: a(3), localFaces0(3)

a(1) = MINLOC(face, 1)
a(2) = HelpFaceData_Triangle(1, a(1)) !b
a(3) = HelpFaceData_Triangle(2, a(1)) !c

localFaces0 = face(a)
IF (PRESENT(localFaces)) THEN
  localFaces(1:3) = localFaces0
END IF

sorted_face(1) = localFaces0(1)

IF (localFaces0(2) .LT. localFaces0(3)) THEN
  sorted_face(2) = localFaces0(2)
  sorted_face(3) = localFaces0(3)

  IF (PRESENT(faceOrient)) THEN
    faceOrient(1) = a(1) - 1_I4B
    faceOrient(2) = 1_INT8
  END IF

ELSE
  sorted_face(2) = localFaces0(3)
  sorted_face(3) = localFaces0(2)

  IF (PRESENT(faceOrient)) THEN
    faceOrient(1) = a(1) - 1_I4B
    faceOrient(2) = -1_INT8
  END IF

END IF

END PROCEDURE FaceShapeMetaData_Triangle

!----------------------------------------------------------------------------
!                                               GetFaceElemType_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType_Triangle1
INTEGER(I4B) :: elemType0

elemType0 = Input(default=Triangle, option=elemType)

SELECT CASE (elemType0)

CASE (Triangle3)

  IF (PRESENT(faceElemType)) faceElemType(1:3) = Line2
  IF (PRESENT(tFaceNodes)) tFaceNodes(1:3) = 2_I4B

CASE (Triangle6)
  IF (PRESENT(faceElemType)) faceElemType(1:3) = Line3
  IF (PRESENT(tFaceNodes)) tFaceNodes(1:3) = 3_I4B

CASE (Triangle9, Triangle10)
  IF (PRESENT(faceElemType)) faceElemType(1:3) = Line4
  IF (PRESENT(tFaceNodes)) tFaceNodes(1:3) = 4_I4B

CASE (Triangle15)
  IF (PRESENT(faceElemType)) faceElemType(1:3) = Line5
  IF (PRESENT(tFaceNodes)) tFaceNodes(1:3) = 5_I4B

CASE (Triangle21a, Triangle21b)
  IF (PRESENT(faceElemType)) faceElemType(1:3) = Line6
  IF (PRESENT(tFaceNodes)) tFaceNodes(1:3) = 6_I4B

CASE (Triangle18)
  IF (PRESENT(faceElemType)) faceElemType(1:3) = Line7
  IF (PRESENT(tFaceNodes)) tFaceNodes(1:3) = 7_I4B

END SELECT

END PROCEDURE GetFaceElemType_Triangle1

!----------------------------------------------------------------------------
!                                                  GetFaceElemType_Triangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType_Triangle2
SELECT CASE (elemType)

CASE (Triangle3)
  faceElemType = Line2
  tFaceNodes = 2_I4B

CASE (Triangle6)
  faceElemType = Line3
  tFaceNodes = 3_I4B

CASE (Triangle9, Triangle10)
  faceElemType = Line4
  tFaceNodes = 4_I4B

CASE (Triangle15)
  faceElemType = Line5
  tFaceNodes = 5_I4B

CASE (Triangle21a, Triangle21b)
  faceElemType = Line6
  tFaceNodes = 6_I4B

CASE (Triangle18)
  faceElemType = Line7
  tFaceNodes = 7_I4B

END SELECT
END PROCEDURE GetFaceElemType_Triangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
