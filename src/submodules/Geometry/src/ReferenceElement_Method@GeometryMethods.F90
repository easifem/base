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
USE BaseType, ONLY: ReferenceHexahedron_
USE BaseType, ONLY: ReferenceLine_
USE BaseType, ONLY: ReferencePoint_
USE BaseType, ONLY: ReferencePrism_
USE BaseType, ONLY: ReferencePyramid_
USE BaseType, ONLY: ReferenceQuadrangle_
USE BaseType, ONLY: ReferenceTetrahedron_
USE BaseType, ONLY: ReferenceTriangle_
USE BaseType, ONLY: TypeElemNameOpt
USE Display_Method, ONLY: UtilityDisplay => Display
USE ErrorHandling, ONLY: Errormsg
USE ReferenceHexahedron_Method, ONLY: GetEdgeConnectivity_Hexahedron
USE ReferenceHexahedron_Method, ONLY: GetFaceConnectivity_Hexahedron
USE ReferenceHexahedron_Method, ONLY: GetFaceElemType_Hexahedron
USE ReferenceHexahedron_Method, ONLY: Hexahedron_quality
USE ReferenceHexahedron_Method, ONLY: Measure_Simplex_Hexahedron
USE ReferenceHexahedron_Method, ONLY: RefCoord_Hexahedron
USE ReferenceHexahedron_Method, ONLY: TotalEntities_Hexahedron
USE ReferenceHexahedron_Method, ONLY: TotalNodesInElement_Hexahedron
USE ReferenceLine_Method, ONLY: GetEdgeConnectivity_Line
USE ReferenceLine_Method, ONLY: GetFaceConnectivity_Line
USE ReferenceLine_Method, ONLY: GetFaceElemType_Line
USE ReferenceLine_Method, ONLY: Line_quality
USE ReferenceLine_Method, ONLY: Measure_Simplex_Line
USE ReferenceLine_Method, ONLY: RefCoord_Line
USE ReferenceLine_Method, ONLY: TotalEntities_Line
USE ReferenceLine_Method, ONLY: TotalNodesInElement_Line
USE ReferencePoint_Method, ONLY: Measure_Simplex_Point
USE ReferencePoint_Method, ONLY: Point_quality
USE ReferencePoint_Method, ONLY: TotalNodesInElement_Point
USE ReferencePrism_Method, ONLY: GetEdgeConnectivity_Prism
USE ReferencePrism_Method, ONLY: GetFaceConnectivity_Prism
USE ReferencePrism_Method, ONLY: GetFaceElemType_Prism
USE ReferencePrism_Method, ONLY: Measure_Simplex_Prism
USE ReferencePrism_Method, ONLY: Prism_quality
USE ReferencePrism_Method, ONLY: RefCoord_Prism
USE ReferencePrism_Method, ONLY: TotalEntities_Prism
USE ReferencePrism_Method, ONLY: TotalNodesInElement_Prism
USE ReferencePyramid_Method, ONLY: GetEdgeConnectivity_Pyramid
USE ReferencePyramid_Method, ONLY: GetFaceConnectivity_Pyramid
USE ReferencePyramid_Method, ONLY: GetFaceElemType_Pyramid
USE ReferencePyramid_Method, ONLY: Measure_Simplex_Pyramid
USE ReferencePyramid_Method, ONLY: Pyramid_quality
USE ReferencePyramid_Method, ONLY: RefCoord_Pyramid
USE ReferencePyramid_Method, ONLY: TotalEntities_Pyramid
USE ReferencePyramid_Method, ONLY: TotalNodesInElement_Pyramid
USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: GetFaceConnectivity_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: GetFaceElemType_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: Measure_Simplex_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: Quadrangle_quality
USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: TotalEntities_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: TotalNodesInElement_Quadrangle
USE ReferenceTetrahedron_Method, ONLY: GetEdgeConnectivity_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: GetFaceConnectivity_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: GetFaceElemType_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: Measure_Simplex_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: Tetrahedron_quality
USE ReferenceTetrahedron_Method, ONLY: TotalEntities_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: TotalNodesInElement_Tetrahedron
USE ReferenceTriangle_Method, ONLY: GetEdgeConnectivity_Triangle
USE ReferenceTriangle_Method, ONLY: GetFaceConnectivity_Triangle
USE ReferenceTriangle_Method, ONLY: GetFaceElemType_Triangle
USE ReferenceTriangle_Method, ONLY: Measure_Simplex_Triangle
USE ReferenceTriangle_Method, ONLY: RefCoord_Triangle
USE ReferenceTriangle_Method, ONLY: TotalEntities_Triangle
USE ReferenceTriangle_Method, ONLY: TotalNodesInElement_Triangle
USE ReferenceTriangle_Method, ONLY: Triangle_quality
USE ReferenceTriangle_Method, ONLY: triangle_contains_point

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   RefCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE RefCoord
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)

CASE (TypeElemNameOpt%point)
  ALLOCATE (ans(3, 1))
  ans = 0.0_DFP

CASE (TypeElemNameOpt%line)
  ans = RefCoord_Line(refElem)

CASE (TypeElemNameOpt%triangle)
  ans = RefCoord_Triangle(refElem)

CASE (TypeElemNameOpt%quadrangle)
  ans = RefCoord_Quadrangle(refElem)

CASE (TypeElemNameOpt%Tetrahedron)
  ans = RefCoord_Tetrahedron(refElem)

CASE (TypeElemNameOpt%Hexahedron)
  ans = RefCoord_Hexahedron(refElem)

CASE (TypeElemNameOpt%Prism)
  ans = RefCoord_Prism(refElem)

CASE (TypeElemNameOpt%Pyramid)
  ans = RefCoord_Pyramid(refElem)

CASE DEFAULT
END SELECT
END PROCEDURE RefCoord

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE RefCoord_
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)

CASE (TypeElemNameOpt%point)
  nrow = 3
  ncol = 1
  ans(1:nrow, 1:ncol) = 0.0_DFP

CASE (TypeElemNameOpt%line)
  nrow = 1
  ncol = 2
  ans(1:nrow, 1:ncol) = RefCoord_Line(refElem)

CASE (TypeElemNameOpt%triangle)
  nrow = 2
  ncol = 3
  ans(1:nrow, 1:ncol) = RefCoord_Triangle(refElem)

CASE (TypeElemNameOpt%quadrangle)
  nrow = 2
  ncol = 4
  ans(1:nrow, 1:ncol) = RefCoord_Quadrangle(refElem)

CASE (TypeElemNameOpt%tetrahedron)
  nrow = 3
  ncol = 4
  ans(1:nrow, 1:ncol) = RefCoord_Tetrahedron(refElem)

CASE (TypeElemNameOpt%hexahedron)
  nrow = 3
  ncol = 8
  ans(1:nrow, 1:ncol) = RefCoord_Hexahedron(refElem)

CASE (TypeElemNameOpt%prism)
  nrow = 3
  ncol = 6
  ans(1:nrow, 1:ncol) = RefCoord_Prism(refElem)

CASE (TypeElemNameOpt%pyramid)
  nrow = 3
  ncol = 5
  ans(1:nrow, 1:ncol) = RefCoord_Pyramid(refElem)

CASE DEFAULT
  nrow = 0
  ncol = 0

END SELECT
END PROCEDURE RefCoord_

!----------------------------------------------------------------------------
!                                                     GetElementIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE GetElementIndex
INTEGER(I4B) :: topo

topo = refelem_ElementTopology1(elemType)

SELECT CASE (topo)
CASE (TypeElemNameOpt%point)
  ans = 1

CASE (TypeElemNameOpt%line)
  ans = 2

CASE (TypeElemNameOpt%triangle)
  ans = 3

CASE (TypeElemNameOpt%quadrangle)
  ans = 4

CASE (TypeElemNameOpt%tetrahedron)
  ans = 5

CASE (TypeElemNameOpt%hexahedron)
  ans = 6

CASE (TypeElemNameOpt%prism)
  ans = 7

CASE (TypeElemNameOpt%pyramid)
  ans = 8

CASE DEFAULT
  ans = 0

END SELECT
END PROCEDURE GetElementIndex

!----------------------------------------------------------------------------
!                                                        RefElemGetGeoParam
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemGetGeoParam1
INTEGER(I4B) :: topo
LOGICAL(LGT) :: isok

topo = refelem_ElementTopology1(elemType)

isok = PRESENT(tCells)
IF (isok) tCells = 1_I4B

SELECT CASE (topo)

CASE (TypeElemNameOpt%point, TypeElemNameOpt%line)

  isok = PRESENT(tNodes)
  IF (isok) tNodes = TotalNodesInElement_Line(elemType)

  isok = PRESENT(tEdges)
  IF (isok) tEdges = 0_I4B

  isok = PRESENT(tFaces)
  IF (isok) tFaces = 2_I4B

  isok = PRESENT(edgeCon)
  IF (isok) CALL GetEdgeConnectivity_Line( &
    con=edgeCon, opt=edgeOpt, order=order)

  isok = PRESENT(faceCon)
  IF (isok) CALL GetFaceConnectivity_Line( &
    con=faceCon, opt=faceOpt, order=order)

  CALL GetFaceElemType_Line( &
    faceElemType=faceElemType, tFaceNodes=tFaceNodes, elemType=elemType)

CASE (TypeElemNameOpt%triangle)

  isok = PRESENT(tNodes)
  IF (isok) tNodes = TotalNodesInElement_Triangle(elemType)

  isok = PRESENT(tEdges)
  IF (isok) tEdges = 3_I4B

  isok = PRESENT(tFaces)
  IF (isok) tFaces = 3_I4B

  isok = PRESENT(edgeCon)
  IF (isok) CALL GetEdgeConnectivity_Triangle( &
    con=edgeCon, opt=edgeOpt, order=order)

  isok = PRESENT(faceCon)
  IF (isok) CALL GetFaceConnectivity_Triangle( &
    con=faceCon, opt=faceOpt, order=order)

  CALL GetFaceElemType_Triangle( &
    faceElemType=faceElemType, tFaceNodes=tFaceNodes, elemType=elemType)

CASE (TypeElemNameOpt%quadrangle)

  isok = PRESENT(tNodes)
  IF (isok) tNodes = TotalNodesInElement_Quadrangle(elemType)

  isok = PRESENT(tEdges)
  IF (isok) tEdges = 4_I4B

  isok = PRESENT(tFaces)
  IF (isok) tFaces = 4_I4B

  isok = PRESENT(edgeCon)
  IF (isok) CALL GetEdgeConnectivity_Quadrangle( &
    con=edgeCon, opt=edgeOpt, order=order)

  isok = PRESENT(faceCon)
  IF (isok) CALL GetFaceConnectivity_Quadrangle( &
    con=faceCon, opt=faceOpt, order=order)

  CALL GetFaceElemType_Quadrangle( &
    faceElemType=faceElemType, tFaceNodes=tFaceNodes, elemType=elemType)

CASE (TypeElemNameOpt%tetrahedron)

  isok = PRESENT(tNodes)
  IF (isok) tNodes = TotalNodesInElement_Tetrahedron(elemType)

  isok = PRESENT(tEdges)
  IF (isok) tEdges = 6_I4B

  isok = PRESENT(tFaces)
  IF (isok) tFaces = 4_I4B

  isok = PRESENT(edgeCon)
  IF (isok) CALL GetEdgeConnectivity_Tetrahedron( &
    con=edgeCon, opt=edgeOpt, order=order)

  isok = PRESENT(faceCon)
  IF (isok) CALL GetFaceConnectivity_Tetrahedron( &
    con=faceCon, opt=faceOpt, order=order)

  CALL GetFaceElemType_Tetrahedron( &
    faceElemType=faceElemType, tFaceNodes=tFaceNodes, elemType=elemType)

CASE (TypeElemNameOpt%hexahedron)

  isok = PRESENT(tNodes)
  IF (isok) tNodes = TotalNodesInElement_Hexahedron(elemType)

  isok = PRESENT(tEdges)
  IF (isok) tEdges = 12_I4B

  isok = PRESENT(tFaces)
  IF (isok) tFaces = 6_I4B

  isok = PRESENT(edgeCon)
  IF (isok) CALL GetEdgeConnectivity_Hexahedron( &
    con=edgeCon, opt=edgeOpt, order=order)

  isok = PRESENT(faceCon)
  IF (isok) CALL GetFaceConnectivity_Hexahedron( &
    con=faceCon, opt=faceOpt, order=order)

  CALL GetFaceElemType_Hexahedron( &
    faceElemType=faceElemType, tFaceNodes=tFaceNodes, elemType=elemType)

CASE (TypeElemNameOpt%prism)

  isok = PRESENT(tNodes)
  IF (isok) tNodes = TotalNodesInElement_Prism(elemType)

  isok = PRESENT(tEdges)
  IF (isok) tEdges = 9_I4B

  isok = PRESENT(tFaces)
  IF (isok) tFaces = 5_I4B

  isok = PRESENT(edgeCon)
  IF (isok) CALL GetEdgeConnectivity_Prism( &
    con=edgeCon, opt=edgeOpt, order=order)

  isok = PRESENT(faceCon)
  IF (isok) CALL GetFaceConnectivity_Prism( &
    con=faceCon, opt=faceOpt, order=order)

  CALL GetFaceElemType_Prism( &
    faceElemType=faceElemType, tFaceNodes=tFaceNodes, elemType=elemType)

CASE (TypeElemNameOpt%pyramid)

  isok = PRESENT(tNodes)
  IF (isok) tNodes = TotalNodesInElement_Pyramid(elemType)

  isok = PRESENT(tEdges)
  IF (isok) tEdges = 8_I4B

  isok = PRESENT(tFaces)
  IF (isok) tFaces = 5_I4B

  isok = PRESENT(edgeCon)
  IF (isok) CALL GetEdgeConnectivity_Pyramid( &
    con=edgeCon, opt=edgeOpt, order=order)

  isok = PRESENT(faceCon)
  IF (isok) CALL GetFaceConnectivity_Pyramid( &
    con=faceCon, opt=faceOpt, order=order)

  CALL GetFaceElemType_Pyramid( &
    faceElemType=faceElemType, tFaceNodes=tFaceNodes, elemType=elemType)

CASE DEFAULT
  isok = PRESENT(tNodes)
  IF (isok) tNodes = 0_I4B

  isok = PRESENT(tEdges)
  IF (isok) tEdges = 0_I4B

  isok = PRESENT(tFaces)
  IF (isok) tFaces = 0_I4B

  isok = PRESENT(edgeCon)
  IF (isok) edgeCon = 0_I4B

  isok = PRESENT(faceCon)
  IF (isok) faceCon = 0_I4B

  isok = PRESENT(faceElemType)
  IF (isok) faceElemType = 0_I4B

  isok = PRESENT(tFaceNodes)
  IF (isok) tFaceNodes = 0_I4B
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

CASE (TypeElemNameOpt%line)
  CALL GetEdgeConnectivity_Line( &
    con=con, opt=opt, order=order, nrow=nrow, ncol=ncol)

CASE (TypeElemNameOpt%triangle)

  CALL GetEdgeConnectivity_Triangle( &
    con=con, opt=opt, order=order, nrow=nrow, ncol=ncol)

CASE (TypeElemNameOpt%quadrangle)

  CALL GetEdgeConnectivity_Quadrangle( &
    con=con, opt=opt, order=order, nrow=nrow, ncol=ncol)

CASE (TypeElemNameOpt%tetrahedron)

  CALL GetEdgeConnectivity_Tetrahedron( &
    con=con, opt=opt, order=order, nrow=nrow, ncol=ncol)

CASE (TypeElemNameOpt%hexahedron)

  CALL GetEdgeConnectivity_Hexahedron( &
    con=con, opt=opt, order=order, nrow=nrow, ncol=ncol)

CASE (TypeElemNameOpt%prism)

  CALL GetEdgeConnectivity_Prism( &
    con=con, opt=opt, order=order, nrow=nrow, ncol=ncol)

CASE (TypeElemNameOpt%pyramid)

  CALL GetEdgeConnectivity_Pyramid( &
    con=con, opt=opt, order=order, nrow=nrow, ncol=ncol)

CASE DEFAULT
  nrow = 0
  ncol = 0

END SELECT
END PROCEDURE GetEdgeConnectivity1

!----------------------------------------------------------------------------
!                                                    GetFaceConnectivity2
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceConnectivity1
INTEGER(I4B) :: topo

topo = refelem_ElementTopology1(elemType)

SELECT CASE (topo)

CASE (TypeElemNameOpt%line)
  CALL GetFaceConnectivity_Line(con=con, opt=opt, order=order, &
                                nrow=nrow, ncol=ncol)

CASE (TypeElemNameOpt%triangle)

  CALL GetFaceConnectivity_Triangle(con=con, opt=opt, order=order, &
                                    nrow=nrow, ncol=ncol)

CASE (TypeElemNameOpt%quadrangle)

  CALL GetFaceConnectivity_Quadrangle(con=con, opt=opt, order=order, &
                                      nrow=nrow, ncol=ncol)

CASE (TypeElemNameOpt%tetrahedron)

  CALL GetFaceConnectivity_Tetrahedron(con=con, opt=opt, order=order, &
                                       nrow=nrow, ncol=ncol)

CASE (TypeElemNameOpt%hexahedron)

  CALL GetFaceConnectivity_Hexahedron(con=con, opt=opt, order=order, &
                                      nrow=nrow, ncol=ncol)

CASE (TypeElemNameOpt%prism)

  CALL GetFaceConnectivity_Prism(con=con, opt=opt, order=order, &
                                 nrow=nrow, ncol=ncol)

CASE (TypeElemNameOpt%pyramid)

  CALL GetFaceConnectivity_Pyramid(con=con, opt=opt, order=order, &
                                   nrow=nrow, ncol=ncol)

CASE DEFAULT
  nrow = 0
  ncol = 0

END SELECT
END PROCEDURE GetFaceConnectivity1

!----------------------------------------------------------------------------
!                                                            GetFaceElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType1
INTEGER(I4B) :: topo

topo = refelem_ElementTopology1(elemType)

SELECT CASE (topo)

CASE (TypeElemNameOpt%line)
  CALL GetFaceElemType_Line(faceElemType=faceElemType, opt=opt, &
                            tFaceNodes=tFaceNodes, elemType=elemType)

CASE (TypeElemNameOpt%triangle)
  CALL GetFaceElemType_Triangle(faceElemType=faceElemType, opt=opt, &
                                tFaceNodes=tFaceNodes, elemType=elemType)

CASE (TypeElemNameOpt%quadrangle)
  CALL GetFaceElemType_Quadrangle(faceElemType=faceElemType, opt=opt, &
                                  tFaceNodes=tFaceNodes, elemType=elemType)

CASE (TypeElemNameOpt%tetrahedron)
  CALL GetFaceElemType_Tetrahedron(faceElemType=faceElemType, opt=opt, &
                                   tFaceNodes=tFaceNodes, elemType=elemType)

CASE (TypeElemNameOpt%hexahedron)
  CALL GetFaceElemType_Hexahedron(faceElemType=faceElemType, opt=opt, &
                                  tFaceNodes=tFaceNodes, elemType=elemType)

CASE (TypeElemNameOpt%prism)
  CALL GetFaceElemType_Prism(faceElemType=faceElemType, opt=opt, &
                             tFaceNodes=tFaceNodes, elemType=elemType)

CASE (TypeElemNameOpt%pyramid)
  CALL GetFaceElemType_Pyramid(faceElemType=faceElemType, opt=opt, &
                               tFaceNodes=tFaceNodes, elemType=elemType)

CASE DEFAULT
  ! Do nothing - arrays are already passed by caller

END SELECT
END PROCEDURE GetFaceElemType1

!----------------------------------------------------------------------------
!                                                            GetFaceElemType
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType2
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)

CASE (TypeElemNameOpt%line)
  CALL GetFaceElemType_Line(faceElemType=faceElemType, opt=opt, &
                            tFaceNodes=tFaceNodes, elemType=elemType, &
                            localFaceNumber=localFaceNumber)

CASE (TypeElemNameOpt%triangle)
  CALL GetFaceElemType_Triangle(faceElemType=faceElemType, opt=opt, &
                                tFaceNodes=tFaceNodes, elemType=elemType, &
                                localFaceNumber=localFaceNumber)

CASE (TypeElemNameOpt%quadrangle)
  CALL GetFaceElemType_Quadrangle(faceElemType=faceElemType, opt=opt, &
                                  tFaceNodes=tFaceNodes, elemType=elemType, &
                                  localFaceNumber=localFaceNumber)

CASE (TypeElemNameOpt%tetrahedron)
  CALL GetFaceElemType_Tetrahedron(faceElemType=faceElemType, opt=opt, &
                                   tFaceNodes=tFaceNodes, elemType=elemType, &
                                   localFaceNumber=localFaceNumber)

CASE (TypeElemNameOpt%hexahedron)
  CALL GetFaceElemType_Hexahedron(faceElemType=faceElemType, opt=opt, &
                                  tFaceNodes=tFaceNodes, elemType=elemType, &
                                  localFaceNumber=localFaceNumber)

CASE (TypeElemNameOpt%prism)
  CALL GetFaceElemType_Prism(faceElemType=faceElemType, opt=opt, &
                             tFaceNodes=tFaceNodes, elemType=elemType, &
                             localFaceNumber=localFaceNumber)

CASE (TypeElemNameOpt%pyramid)
  CALL GetFaceElemType_Pyramid(faceElemType=faceElemType, opt=opt, &
                               tFaceNodes=tFaceNodes, elemType=elemType, &
                               localFaceNumber=localFaceNumber)

CASE DEFAULT
  faceElemType = 0_I4B
  tFaceNodes = 0_I4B

END SELECT
END PROCEDURE GetFaceElemType2

!----------------------------------------------------------------------------
!                                                            MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex
INTEGER(I4B) :: elemType
ans = 0.0_DFP
SELECT TYPE (refelem)
TYPE IS (ReferencePoint_)
  ans = Measure_Simplex_Point(refelem, xij)
TYPE IS (ReferenceLine_)
  ans = Measure_Simplex_Line(refelem, xij)
TYPE IS (ReferenceTriangle_)
  ans = Measure_Simplex_Triangle(refelem, xij)
TYPE IS (ReferenceQuadrangle_)
  ans = Measure_Simplex_Quadrangle(refelem, xij)
TYPE IS (ReferenceTetrahedron_)
  ans = Measure_Simplex_Tetrahedron(refelem, xij)
TYPE IS (ReferenceHexahedron_)
  ans = Measure_Simplex_Hexahedron(refelem, xij)
TYPE IS (ReferencePrism_)
  ans = Measure_Simplex_Prism(refelem, xij)
TYPE IS (ReferencePyramid_)
  ans = Measure_Simplex_Pyramid(refelem, xij)
CLASS DEFAULT
  elemType = refelem%name
  IF (isPoint(elemType)) THEN
    ans = Measure_Simplex_Point(refelem, xij)
  ELSEIF (isLine(elemType)) THEN
    ans = Measure_Simplex_Line(refelem, xij)
  ELSEIF (isTriangle(elemType)) THEN
    ans = Measure_Simplex_Triangle(refelem, xij)
  ELSEIF (isQuadrangle(elemType)) THEN
    ans = Measure_Simplex_Quadrangle(refelem, xij)
  ELSEIF (isTetrahedron(elemType)) THEN
    ans = Measure_Simplex_Tetrahedron(refelem, xij)
  ELSEIF (isHexahedron(elemType)) THEN
    ans = Measure_Simplex_Hexahedron(refelem, xij)
  ELSEIF (isPrism(elemType)) THEN
    ans = Measure_Simplex_Prism(refelem, xij)
  ELSEIF (isPyramid(elemType)) THEN
    ans = Measure_Simplex_Pyramid(refelem, xij)
  END IF
END SELECT
END PROCEDURE Measure_Simplex

!----------------------------------------------------------------------------
!                                                             ElementQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE Element_Quality
SELECT TYPE (refelem)
CLASS IS (ReferencePoint_)
  ans = Point_quality(refelem, xij, measure)
CLASS IS (ReferenceLine_)
  ans = Line_quality(refelem, xij, measure)
CLASS IS (ReferenceTriangle_)
  ans = Triangle_quality(refelem, xij, measure)
CLASS IS (ReferenceQuadrangle_)
  ans = Quadrangle_quality(refelem, xij, measure)
CLASS IS (ReferenceTetrahedron_)
  ans = Tetrahedron_quality(refelem, xij, measure)
CLASS IS (ReferencePrism_)
  ans = Prism_quality(refelem, xij, measure)
CLASS IS (ReferenceHexahedron_)
  ans = Hexahedron_quality(refelem, xij, measure)
CLASS IS (ReferencePyramid_)
  ans = Pyramid_quality(refelem, xij, measure)
END SELECT
END PROCEDURE Element_Quality

!----------------------------------------------------------------------------
!                                                              ContainsPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE contains_point
SELECT TYPE (refelem)
CLASS IS (ReferenceLine_)
  CALL UtilityDisplay("ERROR:: ReferenceElement_Method@Geometry.F90")
  CALL UtilityDisplay("          Contains_point()")
  CALL UtilityDisplay("            No case found for ReferenceLine_")
  CALL UtilityDisplay("            Program stopped!!")
  STOP
CLASS IS (ReferenceTriangle_)
  ans = triangle_contains_point(refelem, xij, x)
CLASS IS (ReferenceQuadrangle_)
  CALL UtilityDisplay("ERROR:: ReferenceElement_Method@Geometry.F90")
  CALL UtilityDisplay("          Contains_point()")
  CALL UtilityDisplay("            No case found for Quadrangle_")
  CALL UtilityDisplay("            Program stopped!!")
  STOP
CLASS DEFAULT
  CALL UtilityDisplay("ERROR:: ReferenceElement_Method@Geometry.F90")
  CALL UtilityDisplay("          Contains_point()")
  CALL UtilityDisplay("            No case found")
  CALL UtilityDisplay("            Program stopped!!")
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
CASE (TypeElemNameOpt%point, TypeElemNameOpt%line)

  ans = TotalEntities_Line(elemType)

CASE (TypeElemNameOpt%triangle)

  ans = TotalEntities_Triangle(elemType)

CASE (TypeElemNameOpt%quadrangle)
  ans = TotalEntities_Quadrangle(elemType)

CASE (TypeElemNameOpt%tetrahedron)
  ans = TotalEntities_Tetrahedron(elemType)

CASE (TypeElemNameOpt%hexahedron)
  ans = TotalEntities_Hexahedron(elemType)

CASE (TypeElemNameOpt%prism)
  ans = TotalEntities_Prism(elemType)

CASE (TypeElemNameOpt%pyramid)
  ans = TotalEntities_Pyramid(elemType)

CASE DEFAULT
  ans = 0_I4B

END SELECT
END PROCEDURE refelem_TotalEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GeometryMethods
