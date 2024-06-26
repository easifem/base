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

USE ReferencePoint_Method, ONLY: Measure_Simplex_Point, Point_quality, &
                                 TotalNodesInElement_Point

USE ReferenceLine_Method, ONLY: Measure_Simplex_Line, &
                                Line_quality, &
                                TotalNodesInElement_Line, &
                                TotalEntities_Line, &
                                GetFaceElemType_Line, &
                                GetEdgeConnectivity_Line, &
                                GetFaceConnectivity_Line

USE ReferenceTriangle_Method, ONLY: Measure_Simplex_Triangle, &
                                    Triangle_quality, &
                                    triangle_contains_point, &
                                    GetEdgeConnectivity_Triangle, &
                                    TotalNodesInElement_Triangle, &
                                    TotalEntities_Triangle, &
                                    GetFaceConnectivity_Triangle, &
                                    GetFaceElemType_Triangle

USE ReferenceQuadrangle_Method, ONLY: Measure_Simplex_Quadrangle, &
                                      Quadrangle_quality, &
                                      GetEdgeConnectivity_Quadrangle, &
                                      TotalNodesInElement_Quadrangle, &
                                      TotalEntities_Quadrangle, &
                                      GetFaceConnectivity_Quadrangle, &
                                      GetFaceElemType_Quadrangle

USE ReferenceTetrahedron_Method, ONLY: Measure_Simplex_Tetrahedron, &
                                       Tetrahedron_quality, &
                                       GetEdgeConnectivity_Tetrahedron, &
                                       GetFaceConnectivity_Tetrahedron, &
                                       GetFaceElemType_Tetrahedron, &
                                       TotalNodesInElement_Tetrahedron, &
                                       TotalEntities_Tetrahedron

USE ReferenceHexahedron_Method, ONLY: Measure_Simplex_Hexahedron, &
                                      Hexahedron_quality, &
                                      GetEdgeConnectivity_Hexahedron, &
                                      GetFaceConnectivity_Hexahedron, &
                                      GetFaceElemType_Hexahedron, &
                                      TotalNodesInElement_Hexahedron, &
                                      TotalEntities_Hexahedron

USE ReferencePrism_Method, ONLY: Measure_Simplex_Prism, &
                                 Prism_quality, &
                                 GetEdgeConnectivity_Prism, &
                                 GetFaceConnectivity_Prism, &
                                 GetFaceElemType_Prism, &
                                 TotalNodesInElement_Prism, &
                                 TotalEntities_Prism

USE ReferencePyramid_Method, ONLY: Measure_Simplex_Pyramid, &
                                   Pyramid_quality, &
                                   GetEdgeConnectivity_Pyramid, &
                                   GetFaceConnectivity_Pyramid, &
                                   GetFaceElemType_Pyramid, &
                                   TotalNodesInElement_Pyramid, &
                                   TotalEntities_Pyramid

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     GetElementIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE GetElementIndex
INTEGER(I4B) :: topo

topo = refelem_ElementTopology1(elemType)

SELECT CASE (topo)
CASE (Point)
  ans = 1

CASE (Line)
  ans = 2

CASE (Triangle)
  ans = 3

CASE (Quadrangle)
  ans = 4

CASE (Tetrahedron)
  ans = 5

CASE (Hexahedron)
  ans = 6

CASE (Prism)
  ans = 7

CASE (Pyramid)
  ans = 8

END SELECT
END PROCEDURE GetElementIndex

!----------------------------------------------------------------------------
!                                                        RefElemGetGeoParam
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemGetGeoParam1
INTEGER(I4B) :: topo

topo = refelem_ElementTopology1(elemType)

IF (PRESENT(tCells)) tCells = 1_I4B

SELECT CASE (topo)

CASE (Point, Line)

  IF (PRESENT(tNodes)) tNodes = TotalNodesInElement_Line(elemType)
  IF (PRESENT(tEdges)) tEdges = 0_I4B
  IF (PRESENT(tFaces)) tFaces = 2_I4B

  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Line(con=edgeCon, &
                                                     opt=edgeOpt, order=order)

  IF (PRESENT(faceCon)) CALL GetFaceConnectivity_Line(con=faceCon, &
                                                     opt=faceOpt, order=order)

  CALL GetFaceElemType_Line(faceElemType=faceElemType, &
                            tFaceNodes=tFaceNodes, elemType=elemType)

CASE (Triangle)

  IF (PRESENT(tNodes)) tNodes = TotalNodesInElement_Triangle(elemType)
  IF (PRESENT(tEdges)) tEdges = 3_I4B
  IF (PRESENT(tFaces)) tFaces = 3_I4B

  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Triangle(con=edgeCon, &
                                                     opt=edgeOpt, order=order)

  IF (PRESENT(faceCon)) CALL GetFaceConnectivity_Triangle(con=faceCon, &
                                                     opt=faceOpt, order=order)

  CALL GetFaceElemType_Triangle(faceElemType=faceElemType, &
                                tFaceNodes=tFaceNodes, elemType=elemType)

CASE (Quadrangle)

  IF (PRESENT(tNodes)) tNodes = TotalNodesInElement_Quadrangle(elemType)
  IF (PRESENT(tEdges)) tEdges = 4_I4B
  IF (PRESENT(tFaces)) tFaces = 4_I4B

  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Quadrangle(con=edgeCon, &
                                                     opt=edgeOpt, order=order)

  IF (PRESENT(faceCon)) CALL GetFaceConnectivity_Quadrangle(con=faceCon, &
                                                     opt=faceOpt, order=order)

  CALL GetFaceElemType_Quadrangle(faceElemType=faceElemType, &
                                  tFaceNodes=tFaceNodes, elemType=elemType)

CASE (Tetrahedron)

  IF (PRESENT(tNodes)) tNodes = TotalNodesInElement_Tetrahedron(elemType)
  IF (PRESENT(tEdges)) tEdges = 6_I4B
  IF (PRESENT(tFaces)) tFaces = 4_I4B
  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Tetrahedron(con=edgeCon, &
                                                     opt=edgeOpt, order=order)
  IF (PRESENT(faceCon)) CALL GetFaceConnectivity_Tetrahedron(con=faceCon, &
                                                     opt=faceOpt, order=order)

  CALL GetFaceElemType_Tetrahedron(faceElemType=faceElemType, &
                                   tFaceNodes=tFaceNodes, elemType=elemType)

CASE (Hexahedron)

  IF (PRESENT(tNodes)) tNodes = TotalNodesInElement_Hexahedron(elemType)
  IF (PRESENT(tEdges)) tEdges = 12_I4B
  IF (PRESENT(tFaces)) tFaces = 6_I4B
  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Hexahedron(con=edgeCon, &
                                                     opt=edgeOpt, order=order)
  IF (PRESENT(faceCon)) CALL GetFaceConnectivity_Hexahedron(con=faceCon, &
                                                     opt=faceOpt, order=order)

  CALL GetFaceElemType_Hexahedron(faceElemType=faceElemType, &
                                  tFaceNodes=tFaceNodes, elemType=elemType)

CASE (Prism)

  IF (PRESENT(tNodes)) tNodes = TotalNodesInElement_Prism(elemType)
  IF (PRESENT(tEdges)) tEdges = 9_I4B
  IF (PRESENT(tFaces)) tFaces = 5_I4B
  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Prism(con=edgeCon, &
                                                     opt=edgeOpt, order=order)
  IF (PRESENT(faceCon)) CALL GetFaceConnectivity_Prism(con=faceCon, &
                                                     opt=faceOpt, order=order)

  CALL GetFaceElemType_Prism(faceElemType=faceElemType, &
                             tFaceNodes=tFaceNodes, elemType=elemType)

CASE (Pyramid)

  IF (PRESENT(tNodes)) tNodes = TotalNodesInElement_Pyramid(elemType)
  IF (PRESENT(tEdges)) tEdges = 8_I4B
  IF (PRESENT(tFaces)) tFaces = 5_I4B
  IF (PRESENT(edgeCon)) CALL GetEdgeConnectivity_Pyramid(con=edgeCon, &
                                                     opt=edgeOpt, order=order)
  IF (PRESENT(faceCon)) CALL GetFaceConnectivity_Pyramid(con=faceCon, &
                                                     opt=faceOpt, order=order)

  CALL GetFaceElemType_Pyramid(faceElemType=faceElemType, &
                               tFaceNodes=tFaceNodes, elemType=elemType)

CASE DEFAULT
  IF (PRESENT(tNodes)) tNodes = 0_I4B
  IF (PRESENT(tEdges)) tEdges = 0_I4B
  IF (PRESENT(tFaces)) tFaces = 0_I4B
  IF (PRESENT(edgeCon)) edgeCon = 0_I4B
  IF (PRESENT(faceCon)) faceCon = 0_I4B
  IF (PRESENT(faceElemType)) faceElemType = 0_I4B
  IF (PRESENT(tFaceNodes)) tFaceNodes = 0_I4B
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
END PROCEDURE GetTotalCells1

!----------------------------------------------------------------------------
!                                                    GetEdgeConnectivity1
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeConnectivity1
INTEGER(I4B) :: topo

topo = refelem_ElementTopology1(elemType)

SELECT CASE (topo)

CASE (Line)
  CALL GetEdgeConnectivity_Line(con=con, opt=opt, order=order, &
                                nrow=nrow, ncol=ncol)

CASE (Triangle)

  CALL GetEdgeConnectivity_Triangle(con=con, opt=opt, order=order, &
                                    nrow=nrow, ncol=ncol)

CASE (Quadrangle)

  CALL GetEdgeConnectivity_Quadrangle(con=con, opt=opt, order=order, &
                                      nrow=nrow, ncol=ncol)

CASE (Tetrahedron)

  CALL GetEdgeConnectivity_Tetrahedron(con=con, opt=opt, order=order, &
                                       nrow=nrow, ncol=ncol)

CASE (Hexahedron)

  CALL GetEdgeConnectivity_Hexahedron(con=con, opt=opt, order=order, &
                                      nrow=nrow, ncol=ncol)

CASE (Prism)

  CALL GetEdgeConnectivity_Prism(con=con, opt=opt, order=order, &
                                 nrow=nrow, ncol=ncol)

CASE (Pyramid)

  CALL GetEdgeConnectivity_Pyramid(con=con, opt=opt, order=order, &
                                   nrow=nrow, ncol=ncol)

END SELECT
END PROCEDURE GetEdgeConnectivity1

!----------------------------------------------------------------------------
!                                                    GetFaceConnectivity2
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceConnectivity1
INTEGER(I4B) :: topo

topo = refelem_ElementTopology1(elemType)

SELECT CASE (topo)

CASE (Line)
  CALL GetFaceConnectivity_Line(con=con, opt=opt, order=order, &
                                nrow=nrow, ncol=ncol)

CASE (Triangle)

  CALL GetFaceConnectivity_Triangle(con=con, opt=opt, order=order, &
                                    nrow=nrow, ncol=ncol)

CASE (Quadrangle)

  CALL GetFaceConnectivity_Quadrangle(con=con, opt=opt, order=order, &
                                      nrow=nrow, ncol=ncol)

CASE (Tetrahedron)

  CALL GetFaceConnectivity_Tetrahedron(con=con, opt=opt, order=order, &
                                       nrow=nrow, ncol=ncol)

CASE (Hexahedron)

  CALL GetFaceConnectivity_Hexahedron(con=con, opt=opt, order=order, &
                                      nrow=nrow, ncol=ncol)

CASE (Prism)

  CALL GetFaceConnectivity_Prism(con=con, opt=opt, order=order, &
                                 nrow=nrow, ncol=ncol)

CASE (Pyramid)

  CALL GetFaceConnectivity_Pyramid(con=con, opt=opt, order=order, &
                                   nrow=nrow, ncol=ncol)

END SELECT
END PROCEDURE GetFaceConnectivity1

!----------------------------------------------------------------------------
!                                                            GetFaceElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType1
INTEGER(I4B) :: topo

topo = refelem_ElementTopology1(elemType)

SELECT CASE (topo)

CASE (Line)

  CALL GetFaceElemType_Line(faceElemType=faceElemType, opt=opt,  &
    & tFaceNodes=tFaceNodes, elemType=elemType)

CASE (Triangle)

  CALL GetFaceElemType_Triangle(faceElemType=faceElemType, opt=opt,  &
    & tFaceNodes=tFaceNodes, elemType=elemType)

CASE (Quadrangle)

  CALL GetFaceElemType_Quadrangle(faceElemType=faceElemType, opt=opt,  &
    & tFaceNodes=tFaceNodes, elemType=elemType)

CASE (Tetrahedron)

  CALL GetFaceElemType_Tetrahedron(faceElemType=faceElemType, opt=opt,  &
    & tFaceNodes=tFaceNodes, elemType=elemType)

CASE (Hexahedron)

  CALL GetFaceElemType_Hexahedron(faceElemType=faceElemType, opt=opt,  &
    & tFaceNodes=tFaceNodes, elemType=elemType)

CASE (Prism)

  CALL GetFaceElemType_Prism(faceElemType=faceElemType, opt=opt,  &
    & tFaceNodes=tFaceNodes, elemType=elemType)

CASE (Pyramid)

  CALL GetFaceElemType_Pyramid(faceElemType=faceElemType, opt=opt,  &
    & tFaceNodes=tFaceNodes, elemType=elemType)

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
INTEGER(I4B) :: topo

topo = refelem_ElementTopology1(elemType)

SELECT CASE (topo)
CASE (Point, Line)

  ans = TotalEntities_Line(elemType)

CASE (Triangle)

  ans = TotalEntities_Triangle(elemType)

CASE (Quadrangle)
  ans = TotalEntities_Quadrangle(elemType)

CASE (Tetrahedron)
  ans = TotalEntities_Tetrahedron(elemType)

CASE (Hexahedron)
  ans = TotalEntities_Hexahedron(elemType)

CASE (Prism)
  ans = TotalEntities_Prism(elemType)

CASE (Pyramid)
  ans = TotalEntities_Pyramid(elemType)

END SELECT
END PROCEDURE refelem_TotalEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GeometryMethods
