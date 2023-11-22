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
! date: 1 March 2021
! summary: This sumodule contains method for geometry

SUBMODULE(ReferenceElement_Method) EnquireMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 isVolume
!----------------------------------------------------------------------------

MODULE PROCEDURE isVolume1
SELECT CASE (elemType)
CASE (Tetrahedron4, &
      Hexahedron8, &
      Prism6, &
      Pyramid5, &
      Tetrahedron10, &
      Hexahedron27, &
      Prism18, &
      Pyramid14, &
      Hexahedron20, &
      Prism15, &
      Pyramid13, &
      Tetrahedron20, &
      Tetrahedron35, &
      Tetrahedron56, &
      Hexahedron64, &
      Hexahedron125)
  ans = .TRUE.
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE isVolume1

!----------------------------------------------------------------------------
!                                                                 isVolume
!----------------------------------------------------------------------------

MODULE PROCEDURE isVolume2
ans = isVolume1(obj%name)
END PROCEDURE isVolume2

!----------------------------------------------------------------------------
!                                                                 isSurface
!----------------------------------------------------------------------------

MODULE PROCEDURE isSurface1
SELECT CASE (elemType)
CASE (Triangle3, &
      Triangle6, &
      Triangle9, &
      Triangle10, &
      Triangle12, &
      Triangle15a, &
      Triangle15b, &
      Triangle21, &
      Quadrangle4, &
      Quadrangle8, &
      Quadrangle9)
  ans = .TRUE.
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE isSurface1

!----------------------------------------------------------------------------
!                                                                 isSurface
!----------------------------------------------------------------------------

MODULE PROCEDURE isSurface2
ans = isSurface1(obj%name)
END PROCEDURE isSurface2

!----------------------------------------------------------------------------
!                                                                 isLine
!----------------------------------------------------------------------------

MODULE PROCEDURE isLine1
SELECT CASE (elemType)
CASE (Line2, &
  & Line3, &
  & Line4, &
  & Line5, &
  & Line6)
  ans = .TRUE.
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE isLine1

!----------------------------------------------------------------------------
!                                                                 isLine
!----------------------------------------------------------------------------

MODULE PROCEDURE isLine2
ans = isLine1(obj%name)
END PROCEDURE isLine2

!----------------------------------------------------------------------------
!                                                                 isPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE isPoint1
SELECT CASE (elemType)
CASE (Point1)
  ans = .TRUE.
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE isPoint1

!----------------------------------------------------------------------------
!                                                                 isPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE isPoint2
ans = isPoint1(obj%name)
END PROCEDURE isPoint2

!----------------------------------------------------------------------------
!                                                                 isTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE isTriangle1
SELECT CASE (elemType)
CASE (Triangle3, Triangle6, &
& Triangle9, Triangle10, Triangle12, Triangle15a, &
& Triangle15b, Triangle21)
  ans = .TRUE.
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE isTriangle1

!----------------------------------------------------------------------------
!                                                                 isTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE isTriangle2
ans = isTriangle1(obj%name)
END PROCEDURE isTriangle2

!----------------------------------------------------------------------------
!                                                              isQuadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE isQuadrangle1
SELECT CASE (elemType)
CASE (Quadrangle4, Quadrangle8, Quadrangle9)
  ans = .TRUE.
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE isQuadrangle1

!----------------------------------------------------------------------------
!                                                                 isQuadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE isQuadrangle2
ans = isQuadrangle1(obj%name)
END PROCEDURE isQuadrangle2

!----------------------------------------------------------------------------
!                                                            isTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE isTetrahedron1
SELECT CASE (elemType)
CASE (Tetrahedron4, Tetrahedron10, Tetrahedron20, Tetrahedron35,  &
  & Tetrahedron56)
  ans = .TRUE.
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE isTetrahedron1

!----------------------------------------------------------------------------
!                                                                 isTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE isTetrahedron2
ans = isTetrahedron1(obj%name)
END PROCEDURE isTetrahedron2

!----------------------------------------------------------------------------
!                                                               isHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE isHexahedron1
SELECT CASE (elemType)
CASE (Hexahedron8, Hexahedron27, &
  & Hexahedron20, Hexahedron64, Hexahedron125)
  ans = .TRUE.
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE isHexahedron1

!----------------------------------------------------------------------------
!                                                               isHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE isHexahedron2
ans = isHexahedron1(obj%name)
END PROCEDURE isHexahedron2

!----------------------------------------------------------------------------
!                                                                    isPrism
!----------------------------------------------------------------------------

MODULE PROCEDURE isPrism1
SELECT CASE (elemType)
CASE (Prism6, Prism18, Prism15)
  ans = .TRUE.
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE isPrism1

!----------------------------------------------------------------------------
!                                                                 isPrism
!----------------------------------------------------------------------------

MODULE PROCEDURE isPrism2
ans = isPrism1(obj%name)
END PROCEDURE isPrism2

!----------------------------------------------------------------------------
!                                                                  isPyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE isPyramid1
SELECT CASE (elemType)
CASE (Pyramid5, Pyramid13, Pyramid14)
  ans = .TRUE.
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE isPyramid1

!----------------------------------------------------------------------------
!                                                                 isPyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE isPyramid2
ans = isPyramid1(obj%name)
END PROCEDURE isPyramid2

!----------------------------------------------------------------------------
!                                                        isSerendipityElement
!----------------------------------------------------------------------------

MODULE PROCEDURE isSerendipityElement1
SELECT CASE (elemType)
CASE (Triangle9, Triangle12, Triangle15b, Quadrangle8)
  ans = .TRUE.
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE isSerendipityElement1

!----------------------------------------------------------------------------
!                                                     isSerendipityElement
!----------------------------------------------------------------------------

MODULE PROCEDURE isSerendipityElement2
ans = isSerendipityElement1(obj%name)
END PROCEDURE isSerendipityElement2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE EnquireMethods
