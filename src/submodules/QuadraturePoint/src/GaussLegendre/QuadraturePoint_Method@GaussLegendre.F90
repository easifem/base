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

SUBMODULE(QuadraturePoint_Method) GaussLegendreMethods
USE BaseMethod
USE GaussLegendre_Line_Methods
USE GaussLegendre_Triangle_Methods
USE GaussLegendre_Quadrangle_Methods
USE GaussLegendre_Tetrahedron_Methods
USE GaussLegendre_Pyramid_Methods
USE GaussLegendre_Prism_Methods
USE GaussLegendre_Hexahedron_Methods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                    GaussLegendreQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE GetGaussLegendreQP1
SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  obj = getGaussLegendreQPLine1(order=order)
TYPE IS (ReferenceTriangle_)
  obj = getGaussLegendreQPTriangle1(order=order)
TYPE IS (ReferenceQuadrangle_)
  obj = getGaussLegendreQPQuadrangle1(order=order)
TYPE IS (ReferenceTetrahedron_)
  obj = getGaussLegendreQPTetrahedron1(order=order)
TYPE IS (ReferenceHexahedron_)
  obj = getGaussLegendreQPHexahedron1(order=order)
TYPE IS (ReferencePrism_)
  obj = getGaussLegendreQPPrism1(order=order)
TYPE IS (ReferencePyramid_)
  obj = getGaussLegendreQPPyramid1(order=order)
TYPE IS (ReferenceElement_)
  IF (isLine(refelem%name)) THEN
    obj = getGaussLegendreQPLine1(order=order)
    RETURN
  END IF
  IF (isTriangle(refelem%name)) THEN
    obj = getGaussLegendreQPTriangle1(order=order)
    RETURN
  END IF
  IF (isQuadrangle(refelem%name)) THEN
    obj = getGaussLegendreQPQuadrangle1(order=order)
    RETURN
  END IF
  IF (isTetrahedron(refelem%name)) THEN
    obj = getGaussLegendreQPTetrahedron1(order=order)
    RETURN
  END IF
  IF (isHexahedron(refelem%name)) THEN
    obj = getGaussLegendreQPHexahedron1(order=order)
    RETURN
  END IF
  IF (isPrism(refelem%name)) THEN
    obj = getGaussLegendreQPPrism1(order=order)
    RETURN
  END IF
  IF (isPyramid(refelem%name)) THEN
    obj = getGaussLegendreQPPyramid1(order=order)
    RETURN
  END IF
CLASS DEFAULT
  CALL ErrorMsg(&
    & msg="No case found",  &
    & file=__FILE__,  &
    & routine="getGaussLegendreQP1()", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT
END PROCEDURE GetGaussLegendreQP1

!----------------------------------------------------------------------------
!                                                    GaussLegendreQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE GetGaussLegendreQP2
SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  obj = getGaussLegendreQPLine2(nips=nips)
TYPE IS (ReferenceTriangle_)
  obj = getGaussLegendreQPTriangle2(nips=nips)
TYPE IS (ReferenceQuadrangle_)
  obj = getGaussLegendreQPQuadrangle2(nips=nips)
TYPE IS (ReferenceTetrahedron_)
  obj = getGaussLegendreQPTetrahedron2(nips=nips)
TYPE IS (ReferenceHexahedron_)
  obj = getGaussLegendreQPHexahedron2(nips=nips)
TYPE IS (ReferencePrism_)
  obj = getGaussLegendreQPPrism2(nips=nips)
TYPE IS (ReferencePyramid_)
  obj = getGaussLegendreQPPyramid2(nips=nips)
TYPE IS (ReferenceElement_)
  IF (isLine(refelem%name)) THEN
    obj = getGaussLegendreQPLine2(nips=nips)
    RETURN
  END IF
  IF (isTriangle(refelem%name)) THEN
    obj = getGaussLegendreQPTriangle2(nips=nips)
    RETURN
  END IF
  IF (isQuadrangle(refelem%name)) THEN
    obj = getGaussLegendreQPQuadrangle2(nips=nips)
    RETURN
  END IF
  IF (isTetrahedron(refelem%name)) THEN
    obj = getGaussLegendreQPTetrahedron2(nips=nips)
    RETURN
  END IF
  IF (isHexahedron(refelem%name)) THEN
    obj = getGaussLegendreQPHexahedron2(nips=nips)
    RETURN
  END IF
  IF (isPrism(refelem%name)) THEN
    obj = getGaussLegendreQPPrism2(nips=nips)
    RETURN
  END IF
  IF (isPyramid(refelem%name)) THEN
    obj = getGaussLegendreQPPyramid2(nips=nips)
    RETURN
  END IF
CLASS DEFAULT
  CALL ErrorMsg(&
    & msg="No case found",  &
    & file=__FILE__,  &
    & routine="getGaussLegendreQP1()", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT
END PROCEDURE GetGaussLegendreQP2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetGaussLegendreQP3
SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  obj = getGaussLegendreQPLine1(order=p)
TYPE IS (ReferenceTriangle_)
  obj = getGaussLegendreQPTriangle1(order=p)
TYPE IS (ReferenceQuadrangle_)
  obj = getGaussLegendreQPQuadrangle3(p=p, q=q)
TYPE IS (ReferenceTetrahedron_)
  obj = getGaussLegendreQPTetrahedron1(order=p)
TYPE IS (ReferenceHexahedron_)
  obj = getGaussLegendreQPHexahedron3(p=p, q=q, r=r)
TYPE IS (ReferencePrism_)
  obj = getGaussLegendreQPPrism1(order=p)
TYPE IS (ReferencePyramid_)
  obj = getGaussLegendreQPPyramid1(order=p)
TYPE IS (ReferenceElement_)
  IF (isLine(refelem%name)) THEN
    obj = getGaussLegendreQPLine1(order=p)
    RETURN
  END IF
  IF (isTriangle(refelem%name)) THEN
    obj = getGaussLegendreQPTriangle1(order=p)
    RETURN
  END IF
  IF (isQuadrangle(refelem%name)) THEN
    obj = getGaussLegendreQPQuadrangle3(p=p, q=q)
    RETURN
  END IF
  IF (isTetrahedron(refelem%name)) THEN
    obj = getGaussLegendreQPTetrahedron1(order=p)
    RETURN
  END IF
  IF (isHexahedron(refelem%name)) THEN
    obj = getGaussLegendreQPHexahedron3(p=p, q=q, r=r)
    RETURN
  END IF
  IF (isPrism(refelem%name)) THEN
    obj = getGaussLegendreQPPrism1(order=p)
    RETURN
  END IF
  IF (isPyramid(refelem%name)) THEN
    obj = getGaussLegendreQPPyramid1(order=p)
    RETURN
  END IF
CLASS DEFAULT
  CALL ErrorMsg(&
    & msg="No case found",  &
    & file=__FILE__,  &
    & routine="getGaussLegendreQP1()", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT
END PROCEDURE GetGaussLegendreQP3

END SUBMODULE GaussLegendreMethods
