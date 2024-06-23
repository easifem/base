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

SUBMODULE(InterpolationUtility) Methods
USE GlobalData, ONLY: Point, Line, Triangle, Quadrangle, &
                      Tetrahedron, Hexahedron, Prism, Pyramid

USE ReferenceElement_Method, ONLY: ElementTopology

USE LineInterpolationUtility, ONLY: GetTotalDOF_Line, &
                                    GetTotalInDOF_Line, &
                                    RefElemDomain_Line

USE TriangleInterpolationUtility, ONLY: GetTotalDOF_Triangle, &
                                        GetTotalInDOF_Triangle, &
                                        RefElemDomain_Triangle

USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle, &
                                          GetTotalInDOF_Quadrangle, &
                                          RefElemDomain_Quadrangle

USE TetrahedronInterpolationUtility, ONLY: GetTotalDOF_Tetrahedron, &
                                           GetTotalInDOF_Tetrahedron, &
                                           RefElemDomain_Tetrahedron

USE HexahedronInterpolationUtility, ONLY: GetTotalDOF_Hexahedron, &
                                          GetTotalInDOF_Hexahedron, &
                                          RefElemDomain_Hexahedron

USE PrismInterpolationUtility, ONLY: GetTotalDOF_Prism, &
                                     GetTotalInDOF_Prism, &
                                     RefElemDomain_Prism

USE PyramidInterpolationUtility, ONLY: GetTotalDOF_Pyramid, &
                                       GetTotalInDOF_Pyramid, &
                                       RefElemDomain_Pyramid

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         VandermondeMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE VandermondeMatrix_Real32
INTEGER(I4B) :: ii
ans(:, 1) = 1.0_REAL32
DO ii = 2, order + 1
  ans(:, ii) = x**(ii - 1)
END DO
END PROCEDURE VandermondeMatrix_Real32

!----------------------------------------------------------------------------
!                                                         VandermondeMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE VandermondeMatrix_Real64
INTEGER(I4B) :: ii
ans(:, 1) = 1.0_REAL64
DO ii = 2, order + 1
  ans(:, ii) = x**(ii - 1)
END DO
END PROCEDURE VandermondeMatrix_Real64

!----------------------------------------------------------------------------
!                                                         VandermondeMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalDOF1
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (Point)
  ans = 0
CASE (Line)
  ans = GetTotalDOF_Line(order=order, baseContinuity=baseContinuity, &
                         baseInterpolation=baseInterpolation)

CASE (Triangle)
  ans = GetTotalDOF_Triangle(order=order, baseContinuity=baseContinuity, &
                             baseInterpolation=baseInterpolation)

CASE (Quadrangle)
  ans = GetTotalDOF_Quadrangle(order=order, baseContinuity=baseContinuity, &
                               baseInterpolation=baseInterpolation)

CASE (Tetrahedron)
  ans = GetTotalDOF_Tetrahedron(order=order, baseContinuity=baseContinuity, &
                                baseInterpolation=baseInterpolation)
CASE (Hexahedron)
  ans = GetTotalDOF_Hexahedron(order=order, baseContinuity=baseContinuity, &
                               baseInterpolation=baseInterpolation)

CASE (Prism)
  ans = GetTotalDOF_Prism(order=order, baseContinuity=baseContinuity, &
                          baseInterpolation=baseInterpolation)

CASE (Pyramid)
  ans = GetTotalDOF_Pyramid(order=order, baseContinuity=baseContinuity, &
                            baseInterpolation=baseInterpolation)
END SELECT

END PROCEDURE GetTotalDOF1

!----------------------------------------------------------------------------
!                                                         VandermondeMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalInDOF1
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (Point)
  ans = 0
CASE (Line)
  ans = GetTotalInDOF_Line(order=order, baseContinuity=baseContinuity, &
                           baseInterpolation=baseInterpolation)

CASE (Triangle)
  ans = GetTotalInDOF_Triangle(order=order, baseContinuity=baseContinuity, &
                               baseInterpolation=baseInterpolation)

CASE (Quadrangle)
  ans = GetTotalInDOF_Quadrangle(order=order, baseContinuity=baseContinuity, &
                                 baseInterpolation=baseInterpolation)

CASE (Tetrahedron)
 ans = GetTotalInDOF_Tetrahedron(order=order, baseContinuity=baseContinuity, &
                                  baseInterpolation=baseInterpolation)
CASE (Hexahedron)
  ans = GetTotalInDOF_Hexahedron(order=order, baseContinuity=baseContinuity, &
                                 baseInterpolation=baseInterpolation)

CASE (Prism)
  ans = GetTotalInDOF_Prism(order=order, baseContinuity=baseContinuity, &
                            baseInterpolation=baseInterpolation)

CASE (Pyramid)
  ans = GetTotalInDOF_Pyramid(order=order, baseContinuity=baseContinuity, &
                              baseInterpolation=baseInterpolation)
END SELECT

END PROCEDURE GetTotalInDOF1

!----------------------------------------------------------------------------
!                                                             RefElemDomain
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemDomain
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (Point)
  ans = ""

CASE (Line)
  ans = RefElemDomain_Line(baseContinuity, baseInterpol)

CASE (Triangle)
  ans = RefElemDomain_Triangle(baseContinuity, baseInterpol)

CASE (Quadrangle)
  ans = RefElemDomain_Quadrangle(baseContinuity, baseInterpol)

CASE (Tetrahedron)
  ans = RefElemDomain_Tetrahedron(baseContinuity, baseInterpol)

CASE (Hexahedron)
  ans = RefElemDomain_Hexahedron(baseContinuity, baseInterpol)

CASE (Prism)
  ans = RefElemDomain_Prism(baseContinuity, baseInterpol)

CASE (Pyramid)
  ans = RefElemDomain_Pyramid(baseContinuity, baseInterpol)
END SELECT

END PROCEDURE RefElemDomain

END SUBMODULE Methods
