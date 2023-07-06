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

SUBMODULE(QuadraturePoint_Method) GaussLegendre
USE BaseMethod
IMPLICIT NONE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPLine1(order)  &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPLine1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPLine2(nips)  &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPLine2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPQuadrangle1(order) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPQuadrangle1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPQuadrangle2(nips) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(:)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPQuadrangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPQuadrangle3(p, q) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPQuadrangle3
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPTriangle1(order) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPTriangle1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPTriangle2(nips) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(:)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPTriangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPTetrahedron1(order) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPTetrahedron1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPTetrahedron2(nips) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPTetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPPyramid1(order) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPPyramid1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPPyramid2(nips) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPPyramid2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPPrism1(order) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPPrism1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPPrism2(nips) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPPrism2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPHexahedron1(order) &
      & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPHexahedron1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPHexahedron2(nips) &
      & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(:)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPHexahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreQPHexahedron3(p, q, r) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B), INTENT(IN) :: r
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQPHexahedron3
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                    GaussLegendreQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQP1
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
END PROCEDURE getGaussLegendreQP1

!----------------------------------------------------------------------------
!                                                    GaussLegendreQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQP2
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
END PROCEDURE getGaussLegendreQP2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQP3
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
END PROCEDURE getGaussLegendreQP3

END SUBMODULE GaussLegendre
