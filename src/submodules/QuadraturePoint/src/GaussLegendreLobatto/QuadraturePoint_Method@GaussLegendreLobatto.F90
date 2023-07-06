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

SUBMODULE(QuadraturePoint_Method) GaussLegendreLobatto
USE BaseMethod
IMPLICIT NONE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPLine1(order)  &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPLine1
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPLine2(nips)  &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPLine2
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPQuadrangle1(order) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPQuadrangle1
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPQuadrangle2(nips) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(:)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPQuadrangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPQuadrangle3(p, q) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPQuadrangle3
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPTriangle1(order) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPTriangle1
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPTriangle2(nips) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(:)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPTriangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPTetrahedron1(order) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPTetrahedron1
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPTetrahedron2(nips) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPTetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPPyramid1(order) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPPyramid1
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPPyramid2(nips) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPPyramid2
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPPrism1(order) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPPrism1
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPPrism2(nips) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPPrism2
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPHexahedron1(order) &
      & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPHexahedron1
END INTERFACE

!----------------------------------------------------------------------------
!                                 GaussLegendreLobatto@GaussLegendreLobatto
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreLobattoQPHexahedron2(nips) &
      & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreLobattoQPHexahedron2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                            GaussLegendreLobattoQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreLobattoQP1
SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  obj = getGaussLegendreLobattoQPLine1(order=order)
TYPE IS (ReferenceTriangle_)
  obj = getGaussLegendreLobattoQPTriangle1(order=order)
TYPE IS (ReferenceQuadrangle_)
  obj = getGaussLegendreLobattoQPQuadrangle1(order=order)
TYPE IS (ReferenceTetrahedron_)
  obj = getGaussLegendreLobattoQPTetrahedron1(order=order)
TYPE IS (ReferenceHexahedron_)
  obj = getGaussLegendreLobattoQPHexahedron1(order=order)
TYPE IS (ReferencePrism_)
  obj = getGaussLegendreLobattoQPPrism1(order=order)
TYPE IS (ReferencePyramid_)
  obj = getGaussLegendreLobattoQPPyramid1(order=order)
TYPE IS (ReferenceElement_)
  IF (isLine(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPLine1(order=order)
    RETURN
  END IF
  IF (isTriangle(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPTriangle1(order=order)
    RETURN
  END IF
  IF (isQuadrangle(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPQuadrangle1(order=order)
    RETURN
  END IF
  IF (isTetrahedron(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPTetrahedron1(order=order)
    RETURN
  END IF
  IF (isHexahedron(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPHexahedron1(order=order)
    RETURN
  END IF
  IF (isPrism(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPPrism1(order=order)
    RETURN
  END IF
  IF (isPyramid(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPPyramid1(order=order)
    RETURN
  END IF
CLASS DEFAULT
  CALL ErrorMsg(&
    & msg="No case found",  &
    & file=__FILE__,  &
    & routine="getGaussLegendreLobattoQP1()", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT
END PROCEDURE getGaussLegendreLobattoQP1

!----------------------------------------------------------------------------
!                                           GaussLegendreLobattoQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreLobattoQP2
SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  obj = getGaussLegendreLobattoQPLine2(nips=nips)
TYPE IS (ReferenceTriangle_)
  obj = getGaussLegendreLobattoQPTriangle2(nips=nips)
TYPE IS (ReferenceQuadrangle_)
  obj = getGaussLegendreLobattoQPQuadrangle2(nips=nips)
TYPE IS (ReferenceTetrahedron_)
  obj = getGaussLegendreLobattoQPTetrahedron2(nips=nips)
TYPE IS (ReferenceHexahedron_)
  obj = getGaussLegendreLobattoQPHexahedron2(nips=nips)
TYPE IS (ReferencePrism_)
  obj = getGaussLegendreLobattoQPPrism2(nips=nips)
TYPE IS (ReferencePyramid_)
  obj = getGaussLegendreLobattoQPPyramid2(nips=nips)
TYPE IS (ReferenceElement_)
  IF (isLine(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPLine2(nips=nips)
    RETURN
  END IF
  IF (isTriangle(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPTriangle2(nips=nips)
    RETURN
  END IF
  IF (isQuadrangle(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPQuadrangle2(nips=nips)
    RETURN
  END IF
  IF (isTetrahedron(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPTetrahedron2(nips=nips)
    RETURN
  END IF
  IF (isHexahedron(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPHexahedron2(nips=nips)
    RETURN
  END IF
  IF (isPrism(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPPrism2(nips=nips)
    RETURN
  END IF
  IF (isPyramid(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPPyramid2(nips=nips)
    RETURN
  END IF
CLASS DEFAULT
  CALL ErrorMsg(&
    & msg="No case found",  &
    & file=__FILE__,  &
    & routine="getGaussLegendreLobattoQP1()", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT
END PROCEDURE getGaussLegendreLobattoQP2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreLobattoQP3
SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  obj = getGaussLegendreLobattoQPLine1(order=p)
TYPE IS (ReferenceTriangle_)
  obj = getGaussLegendreLobattoQPTriangle1(order=p)
TYPE IS (ReferenceQuadrangle_)
  obj = getGaussLegendreLobattoQPQuadrangle3(p=p, q=q)
TYPE IS (ReferenceTetrahedron_)
  obj = getGaussLegendreLobattoQPTetrahedron1(order=p)
TYPE IS (ReferenceHexahedron_)
  obj = getGaussLegendreLobattoQPHexahedron1(order=p)
TYPE IS (ReferencePrism_)
  obj = getGaussLegendreLobattoQPPrism1(order=p)
TYPE IS (ReferencePyramid_)
  obj = getGaussLegendreLobattoQPPyramid1(order=p)
TYPE IS (ReferenceElement_)
  IF (isLine(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPLine1(order=p)
    RETURN
  END IF
  IF (isTriangle(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPTriangle1(order=p)
    RETURN
  END IF
  IF (isQuadrangle(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPQuadrangle3(p=p, q=q)
    RETURN
  END IF
  IF (isTetrahedron(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPTetrahedron1(order=p)
    RETURN
  END IF
  IF (isHexahedron(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPHexahedron1(order=p)
    RETURN
  END IF
  IF (isPrism(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPPrism1(order=p)
    RETURN
  END IF
  IF (isPyramid(refelem%name)) THEN
    obj = getGaussLegendreLobattoQPPyramid1(order=p)
    RETURN
  END IF
CLASS DEFAULT
  CALL ErrorMsg(&
    & msg="No case found",  &
    & file=__FILE__,  &
    & routine="getGaussLegendreLobattoQP1()", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT
END PROCEDURE getGaussLegendreLobattoQP3

END SUBMODULE GaussLegendreLobatto
