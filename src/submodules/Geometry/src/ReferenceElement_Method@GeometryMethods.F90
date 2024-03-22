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

SUBMODULE(ReferenceElement_Method) GeometryMethods
USE ErrorHandling, ONLY: Errormsg
USE Display_Method

USE ReferencePoint_Method, ONLY: Measure_Simplex_Point, Point_quality
USE ReferenceLine_Method, ONLY: Measure_Simplex_Line, Line_quality

USE ReferenceTriangle_Method, ONLY: Measure_Simplex_Triangle,  &
  & Triangle_quality, triangle_contains_point,  &
  & GetEdgeConnectivity_Triangle

USE ReferenceQuadrangle_Method, ONLY: Measure_Simplex_Quadrangle,  &
  & Quadrangle_quality, GetEdgeConnectivity_Quadrangle

USE ReferenceTetrahedron_Method, ONLY: Measure_Simplex_Tetrahedron,  &
  & Tetrahedron_quality, GetEdgeConnectivity_Tetrahedron,  &
  & GetFaceConnectivity_Tetrahedron, GetFaceElemType_Tetrahedron

USE ReferenceHexahedron_Method, ONLY: Measure_Simplex_Hexahedron,  &
  & Hexahedron_quality, GetEdgeConnectivity_Hexahedron,  &
  & GetFaceConnectivity_Hexahedron, GetFaceElemType_Hexahedron

USE ReferencePrism_Method, ONLY: Measure_Simplex_Prism,  &
  & Prism_quality, GetEdgeConnectivity_Prism,  &
  & GetFaceConnectivity_Prism, GetFaceElemType_Prism

USE ReferencePyramid_Method, ONLY: Measure_Simplex_Pyramid,  &
  & Pyramid_quality, GetEdgeConnectivity_Pyramid,  &
  & GetFaceConnectivity_Pyramid, GetFaceElemType_Pyramid

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     GetElementIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE GetElementIndex
SELECT CASE (elemType)
CASE (Point)
  ans = 1
CASE (Line, Line3, Line4, Line5, Line6)
  ans = 2
CASE (Triangle, Triangle6, Triangle9, Triangle10, Triangle12,  &
& Triangle15, Triangle21, Triangle15a)
  ans = 3
CASE (Quadrangle, Quadrangle8, Quadrangle9, Quadrangle16)
  ans = 4
CASE (Tetrahedron, Tetrahedron10, Tetrahedron20, Tetrahedron35,  &
& Tetrahedron56)
  ans = 5
CASE (Hexahedron, Hexahedron27, Hexahedron20, Hexahedron64,  &
& Hexahedron125)
  ans = 6
CASE (Prism, Prism15, Prism18)
  ans = 7
CASE (Pyramid, Pyramid13, Pyramid14)
  ans = 8
END SELECT
END PROCEDURE GetElementIndex

!----------------------------------------------------------------------------
!                                                        RefElemGetGeoParam
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemGetGeoParam1
IF (PRESENT(tCells)) tCells = 1_I4B

SELECT CASE (elemType)
CASE (Point)
  IF (PRESENT(tNodes)) tNodes = 1_I4B
  IF (PRESENT(tEdges)) tEdges = 0_I4B
  IF (PRESENT(tFaces)) tFaces = 0_I4B

CASE (Line)
  IF (PRESENT(tNodes)) tNodes = 2_I4B
  IF (PRESENT(tEdges)) tEdges = 0_I4B
  IF (PRESENT(tFaces)) tFaces = 0_I4B

CASE (Line3)
  IF (PRESENT(tNodes)) tNodes = 3_I4B
  IF (PRESENT(tEdges)) tEdges = 0_I4B
  IF (PRESENT(tFaces)) tFaces = 0_I4B

CASE (Triangle)
  IF (PRESENT(tNodes)) tNodes = 3_I4B
  IF (PRESENT(tEdges)) tEdges = 3_I4B
  IF (PRESENT(tFaces)) tFaces = 0_I4B
  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Triangle(con=edgeCon,  &
    & opt=edgeOpt)

CASE (Triangle6)
  IF (PRESENT(tNodes)) tNodes = 6_I4B
  IF (PRESENT(tEdges)) tEdges = 3_I4B
  IF (PRESENT(tFaces)) tFaces = 0_I4B
  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Triangle(con=edgeCon,  &
    & opt=edgeOpt)

CASE (Quadrangle)
  IF (PRESENT(tNodes)) tNodes = 4_I4B
  IF (PRESENT(tEdges)) tEdges = 4_I4B
  IF (PRESENT(tFaces)) tFaces = 0_I4B
  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Quadrangle(con=edgeCon,  &
    & opt=edgeOpt)

CASE (Quadrangle8)
  IF (PRESENT(tNodes)) tNodes = 8_I4B
  IF (PRESENT(tEdges)) tEdges = 4_I4B
  IF (PRESENT(tFaces)) tFaces = 0_I4B
  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Quadrangle(con=edgeCon,  &
    & opt=edgeOpt)

CASE (Quadrangle9)
  IF (PRESENT(tNodes)) tNodes = 9_I4B
  IF (PRESENT(tEdges)) tEdges = 4_I4B
  IF (PRESENT(tFaces)) tFaces = 0_I4B
  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Quadrangle(con=edgeCon,  &
    & opt=edgeOpt)

CASE (Tetrahedron)
  IF (PRESENT(tNodes)) tNodes = 4_I4B
  IF (PRESENT(tEdges)) tEdges = 6_I4B
  IF (PRESENT(tFaces)) tFaces = 4_I4B
  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Tetrahedron(con=edgeCon,  &
    & opt=edgeOpt)
  IF (PRESENT(faceCon)) CALL GetFaceConnectivity_Tetrahedron(con=faceCon,  &
    & opt=faceOpt)
  IF (PRESENT(faceElemType)) CALL GetFaceElemType_Tetrahedron( &
    & faceElemType=faceElemType, tFaceNodes=tFaceNodes)

CASE (Hexahedron)
  IF (PRESENT(tNodes)) tNodes = 8_I4B
  IF (PRESENT(tEdges)) tEdges = 12_I4B
  IF (PRESENT(tFaces)) tFaces = 6_I4B
  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Hexahedron(con=edgeCon,  &
    & opt=edgeOpt)
  IF (PRESENT(faceCon)) CALL GetFaceConnectivity_Hexahedron(con=faceCon,  &
    & opt=faceOpt)
  IF (PRESENT(faceElemType)) CALL GetFaceElemType_Hexahedron( &
    & faceElemType=faceElemType, tFaceNodes=tFaceNodes)

CASE (Prism)
  IF (PRESENT(tNodes)) tNodes = 6_I4B
  IF (PRESENT(tEdges)) tEdges = 9_I4B
  IF (PRESENT(tFaces)) tFaces = 5_I4B
  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Prism(con=edgeCon,  &
    & opt=edgeOpt)
  IF (PRESENT(faceCon)) CALL GetFaceConnectivity_Prism(con=faceCon,  &
    & opt=faceOpt)
  IF (PRESENT(faceElemType)) CALL GetFaceElemType_Prism( &
    & faceElemType=faceElemType, tFaceNodes=tFaceNodes)

CASE (Pyramid)
  IF (PRESENT(tNodes)) tNodes = 5_I4B
  IF (PRESENT(tEdges)) tEdges = 8_I4B
  IF (PRESENT(tFaces)) tFaces = 5_I4B
  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Pyramid(con=edgeCon,  &
    & opt=edgeOpt)
  IF (PRESENT(faceCon)) CALL GetFaceConnectivity_Pyramid(con=faceCon,  &
    & opt=faceOpt)

  IF (PRESENT(faceElemType)) CALL GetFaceElemType_Tetrahedron( &
    & faceElemType=faceElemType, tFaceNodes=tFaceNodes)

CASE DEFAULT
  IF (PRESENT(tNodes)) tNodes = 0_I4B
  IF (PRESENT(tEdges)) tEdges = 0_I4B
  IF (PRESENT(tFaces)) tFaces = 0_I4B
END SELECT
END PROCEDURE RefElemGetGeoParam1

!----------------------------------------------------------------------------
!                                                             GetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalNodes1
CALL RefElemGetGeoParam(tNodes=ans, elemType=elemType)
END PROCEDURE GetTotalNodes1

!----------------------------------------------------------------------------
!                                                          GetTotalEdges
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalEdges1
CALL RefElemGetGeoParam(tEdges=ans, elemType=elemType)
END PROCEDURE GetTotalEdges1

!----------------------------------------------------------------------------
!                                                              GetTotalFaces
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalFaces1
CALL RefElemGetGeoParam(tFaces=ans, elemType=elemType)
END PROCEDURE GetTotalFaces1

!----------------------------------------------------------------------------
!                                                             GetTotalCells
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalCells1
CALL RefElemGetGeoParam(tCells=ans, elemType=elemType)
! SELECT CASE (elemType)
! CASE (Point, Line, Triangle, Quadrangle)
!   ans = 0_I4B
! CASE (Tetrahedron, Hexahedron, Prism, Pyramid)
!   ans = 1_I4B
! END SELECT
END PROCEDURE GetTotalCells1

!----------------------------------------------------------------------------
!                                                    GetEdgeConnectivity1
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeConnectivity1
SELECT CASE (elemType)
! CASE (Point, Line)
CASE (Triangle)
  CALL GetEdgeConnectivity_Triangle(con=con, opt=opt)
CASE (Quadrangle)
  CALL GetEdgeConnectivity_Quadrangle(con=con, opt=opt)
CASE (Tetrahedron)
  CALL GetEdgeConnectivity_Tetrahedron(con=con, opt=opt)
CASE (Hexahedron)
  CALL GetEdgeConnectivity_Hexahedron(con=con, opt=opt)
CASE (Prism)
  CALL GetEdgeConnectivity_Prism(con=con, opt=opt)
CASE (Pyramid)
  CALL GetEdgeConnectivity_Pyramid(con=con, opt=opt)
END SELECT
END PROCEDURE GetEdgeConnectivity1

!----------------------------------------------------------------------------
!                                                    GetFaceConnectivity2
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceConnectivity1
SELECT CASE (elemType)
! CASE (Point, Line, Triangle, Quadrangle)
CASE (Tetrahedron)
  CALL GetFaceConnectivity_Tetrahedron(con=con, opt=opt)
CASE (Hexahedron)
  CALL GetFaceConnectivity_Hexahedron(con=con, opt=opt)
CASE (Prism)
  CALL GetFaceConnectivity_Prism(con=con, opt=opt)
CASE (Pyramid)
  CALL GetFaceConnectivity_Pyramid(con=con, opt=opt)
END SELECT
END PROCEDURE GetFaceConnectivity1

!----------------------------------------------------------------------------
!                                                            GetFaceElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType1
SELECT CASE (elemType)
! CASE (Point, Line, Triangle, Quadrangle)
CASE (Tetrahedron)
  CALL GetFaceElemType_Tetrahedron(faceElemType=faceElemType, opt=opt,  &
  & tFaceNodes=tFaceNodes)
CASE (Hexahedron)
  CALL GetFaceElemType_Hexahedron(faceElemType=faceElemType, opt=opt,  &
  & tFaceNodes=tFaceNodes)
CASE (Prism)
  CALL GetFaceElemType_Prism(faceElemType=faceElemType, opt=opt,  &
  & tFaceNodes=tFaceNodes)
CASE (Pyramid)
  CALL GetFaceElemType_Pyramid(faceElemType=faceElemType, opt=opt,  &
  & tFaceNodes=tFaceNodes)
END SELECT
END PROCEDURE GetFaceElemType1

!----------------------------------------------------------------------------
!                                                            MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex
INTEGER(I4B) :: elemType
Ans = 0.0_DFP
SELECT TYPE (refelem)
TYPE IS (ReferencePoint_)
  Ans = Measure_Simplex_Point(refelem, xij)
TYPE IS (ReferenceLine_)
  Ans = Measure_Simplex_Line(refelem, xij)
TYPE IS (ReferenceTriangle_)
  Ans = Measure_Simplex_Triangle(refelem, xij)
TYPE IS (ReferenceQuadrangle_)
  Ans = Measure_Simplex_Quadrangle(refelem, xij)
TYPE IS (ReferenceTetrahedron_)
  Ans = Measure_Simplex_Tetrahedron(refelem, xij)
TYPE IS (ReferenceHexahedron_)
  Ans = Measure_Simplex_Hexahedron(refelem, xij)
TYPE IS (ReferencePrism_)
  Ans = Measure_Simplex_Prism(refelem, xij)
TYPE IS (ReferencePyramid_)
  Ans = Measure_Simplex_Pyramid(refelem, xij)
CLASS DEFAULT
  elemType = refelem%name
  IF (isPoint(elemType)) THEN
    Ans = Measure_Simplex_Point(refelem, xij)
  ELSEIF (isLine(elemType)) THEN
    Ans = Measure_Simplex_Line(refelem, xij)
  ELSEIF (isTriangle(elemType)) THEN
    Ans = Measure_Simplex_Triangle(refelem, xij)
  ELSEIF (isQuadrangle(elemType)) THEN
    Ans = Measure_Simplex_Quadrangle(refelem, xij)
  ELSEIF (isTetrahedron(elemType)) THEN
    Ans = Measure_Simplex_Tetrahedron(refelem, xij)
  ELSEIF (isHexahedron(elemType)) THEN
    Ans = Measure_Simplex_Hexahedron(refelem, xij)
  ELSEIF (isPrism(elemType)) THEN
    Ans = Measure_Simplex_Prism(refelem, xij)
  ELSEIF (isPyramid(elemType)) THEN
    Ans = Measure_Simplex_Pyramid(refelem, xij)
  END IF
END SELECT
END PROCEDURE Measure_Simplex

!----------------------------------------------------------------------------
!                                                             ElementQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE Element_Quality
SELECT TYPE (refelem)
CLASS IS (ReferencePoint_)
  Ans = Point_quality(refelem, xij, measure)
CLASS IS (ReferenceLine_)
  Ans = Line_quality(refelem, xij, measure)
CLASS IS (ReferenceTriangle_)
  Ans = Triangle_quality(refelem, xij, measure)
CLASS IS (ReferenceQuadrangle_)
  Ans = Quadrangle_quality(refelem, xij, measure)
CLASS IS (ReferenceTetrahedron_)
  Ans = Tetrahedron_quality(refelem, xij, measure)
CLASS IS (ReferencePrism_)
  Ans = Prism_quality(refelem, xij, measure)
CLASS IS (ReferenceHexahedron_)
  Ans = Hexahedron_quality(refelem, xij, measure)
CLASS IS (ReferencePyramid_)
  Ans = Pyramid_quality(refelem, xij, measure)
END SELECT
END PROCEDURE Element_Quality

!----------------------------------------------------------------------------
!                                                              ContainsPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE contains_point
SELECT TYPE (refelem)
CLASS IS (ReferenceLine_)
  CALL Display("ERROR:: ReferenceElement_Method@Geometry.F90")
  CALL Display("          Contains_point()")
  CALL Display("            No case found for ReferenceLine_")
  CALL Display("            Program stopped!!")
  STOP
CLASS IS (ReferenceTriangle_)
  Ans = triangle_contains_point(refelem, xij, x)
CLASS IS (ReferenceQuadrangle_)
  CALL Display("ERROR:: ReferenceElement_Method@Geometry.F90")
  CALL Display("          Contains_point()")
  CALL Display("            No case found for Quadrangle_")
  CALL Display("            Program stopped!!")
  STOP
CLASS DEFAULT
  CALL Display("ERROR:: ReferenceElement_Method@Geometry.F90")
  CALL Display("          Contains_point()")
  CALL Display("            No case found")
  CALL Display("            Program stopped!!")
  STOP
END SELECT
END PROCEDURE contains_point

!----------------------------------------------------------------------------
!                                                       TotalEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_TotalEntities
SELECT CASE (ElemType)
CASE (Point1)
  ans = 0
  ans(1) = 1
CASE (Line2)
  ans = [2, 1, 0, 0]
CASE (Triangle3)
  ans = [3, 3, 1, 0]
CASE (Quadrangle4)
  ans = [4, 4, 1, 0]
CASE (Tetrahedron4)
  ans = [4, 6, 4, 1]
CASE (Hexahedron8)
  ans = [8, 12, 6, 1]
CASE (Prism6)
  ans = [6, 9, 5, 1]
CASE (Pyramid5)
  ans = [5, 8, 5, 1]
  ! Order=2 elements
CASE (Line3)
  ans = [3, 1, 0, 0]
CASE (Triangle6)
  ans = [6, 3, 1, 0]
CASE (Quadrangle9)
  ans = [9, 4, 1, 0]
CASE (Quadrangle8)
  ans = [8, 4, 1, 0]
CASE (Tetrahedron10)
  ans = [10, 6, 4, 1]
CASE (Hexahedron20)
  ans = [20, 12, 6, 1]
CASE (Hexahedron27)
  ans = [27, 12, 6, 1]
CASE (Prism15)
  ans = [15, 9, 5, 1]
CASE (Prism18)
  ans = [18, 9, 5, 1]
CASE (Pyramid13)
  ans = [13, 8, 5, 1]
CASE (Pyramid14)
  ans = [14, 8, 5, 1]
CASE (Triangle9)
  ans = [9, 3, 1, 0]
CASE (Triangle10)
  ans = [10, 3, 1, 0]
CASE (Triangle12)
  ans = [12, 3, 1, 0]
CASE (Triangle15a)
  ans = [15, 3, 1, 0]
CASE (Triangle15b)
  ans = [15, 3, 1, 0]
CASE (Triangle21)
  ans = [21, 3, 1, 0]
CASE (Line4)
  ans = [4, 1, 0, 0]
CASE (Line5)
  ans = [5, 1, 0, 0]
CASE (Line6)
  ans = [6, 1, 0, 0]
CASE (Tetrahedron20)
  ans = [20, 6, 4, 1]
CASE (Tetrahedron35)
  ans = [35, 6, 4, 1]
CASE (Tetrahedron56)
  ans = [56, 6, 4, 1]
CASE (Hexahedron64)
  ans = [64, 6, 4, 1]
CASE (Hexahedron125)
  ans = [125, 6, 4, 1]
END SELECT
END PROCEDURE refelem_TotalEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GeometryMethods
