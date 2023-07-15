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

SUBMODULE(ElemShapeData_H1LagrangeMethods) Methods
USE BaseMethod
USE H1Lagrange_Line_Methods
USE H1Lagrange_Quadrangle_Methods
USE H1Lagrange_Triangle_Methods
USE H1Lagrange_Tetrahedron_Methods
USE H1Lagrange_Hexahedron_Methods
USE H1Lagrange_Prism_Methods
USE H1Lagrange_Pyramid_Methods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE H1_Lagrange
SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  CALL Line_H1_Lagrange( &
    & obj=obj, &
    & quad=quad, &
    & refelem=refelem, &
    & continuityType=continuityType, &
    & interpolType=interpolType)
TYPE IS (ReferenceTriangle_)
  CALL Triangle_H1_Lagrange( &
    & obj=obj, &
    & quad=quad, &
    & refelem=refelem, &
    & continuityType=continuityType, &
    & interpolType=interpolType)
TYPE IS (ReferenceQuadrangle_)
  CALL Quadrangle_H1_Lagrange( &
    & obj=obj, &
    & quad=quad, &
    & refelem=refelem, &
    & continuityType=continuityType, &
    & interpolType=interpolType)
TYPE IS (ReferenceTetrahedron_)
  CALL Tetrahedron_H1_Lagrange( &
    & obj=obj, &
    & quad=quad, &
    & refelem=refelem, &
    & continuityType=continuityType, &
    & interpolType=interpolType)
TYPE IS (ReferenceHexahedron_)
  CALL Hexahedron_H1_Lagrange( &
    & obj=obj, &
    & quad=quad, &
    & refelem=refelem, &
    & continuityType=continuityType, &
    & interpolType=interpolType)
TYPE IS (ReferencePrism_)
  CALL Prism_H1_Lagrange( &
    & obj=obj, &
    & quad=quad, &
    & refelem=refelem, &
    & continuityType=continuityType, &
    & interpolType=interpolType)
TYPE IS (ReferencePyramid_)
  CALL Pyramid_H1_Lagrange( &
    & obj=obj, &
    & quad=quad, &
    & refelem=refelem, &
    & continuityType=continuityType, &
    & interpolType=interpolType)
TYPE IS (ReferenceElement_)
  IF (isLine(refelem%name)) THEN
    CALL Line_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType)
    RETURN
  END IF
  IF (isTriangle(refelem%name)) THEN
    CALL Triangle_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType)
    RETURN
  END IF
  IF (isQuadrangle(refelem%name)) THEN
    CALL Quadrangle_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType)
    RETURN
  END IF
  IF (isTetrahedron(refelem%name)) THEN
    CALL Tetrahedron_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType)
    RETURN
  END IF
  IF (isHexahedron(refelem%name)) THEN
    CALL Hexahedron_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType)
    RETURN
  END IF
  IF (isPrism(refelem%name)) THEN
    CALL Prism_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType)
    RETURN
  END IF
  IF (isPyramid(refelem%name)) THEN
    CALL Pyramid_H1_Lagrange( &
      & obj=obj, &
      & quad=quad, &
      & refelem=refelem, &
      & continuityType=continuityType, &
      & interpolType=interpolType)
    RETURN
  END IF
END SELECT
END PROCEDURE H1_Lagrange

END SUBMODULE Methods
