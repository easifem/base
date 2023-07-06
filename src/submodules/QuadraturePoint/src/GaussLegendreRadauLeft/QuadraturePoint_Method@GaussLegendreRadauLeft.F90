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

SUBMODULE(QuadraturePoint_Method) GaussLegendreRadauLeft
USE BaseMethod
IMPLICIT NONE

!----------------------------------------------------------------------------
!                               GaussLegendreRadauLeft@GaussLegendreRadauLeft
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreRadauLeftQPLine1(order)  &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: order
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreRadauLeftQPLine1
END INTERFACE

!----------------------------------------------------------------------------
!                              GaussLegendreRadauLeft@GaussLegendreRadauLeft
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getGaussLegendreRadauLeftQPLine2(nips)  &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreRadauLeftQPLine2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                          GaussLegendreRadauLeftQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE GetGaussLegendreRadauLeftQP1
LOGICAL(LGT) :: isError
TYPE(String) :: amsg
isError = .FALSE.

SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  obj = getGaussLegendreRadauLeftQPLine1(order=order)
TYPE IS (ReferenceElement_)
  IF (isLine(refelem%name)) THEN
    obj = getGaussLegendreRadauLeftQPLine1(order=order)
    RETURN
  ELSE
    isError = .TRUE.
    amsg = "This routine is available for line element only."
  END IF
CLASS DEFAULT
  isError = .TRUE.
  amsg = "This routine is callable for line element only."
END SELECT

IF (isError) THEN
  CALL ErrorMsg(&
    & msg=amsg%chars(),  &
    & file=__FILE__,  &
    & routine="getGaussLegendreRadauLeftQP1()", &
    & line=__LINE__, &
    & unitno=stderr)
END IF

END PROCEDURE GetGaussLegendreRadauLeftQP1

!----------------------------------------------------------------------------
!                                           GaussLegendreRadauLeftQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE GetGaussLegendreRadauLeftQP2
LOGICAL(LGT) :: isError
TYPE(String) :: amsg
isError = .FALSE.

SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  obj = getGaussLegendreRadauLeftQPLine2(nips=nips)
TYPE IS (ReferenceElement_)
  IF (isLine(refelem%name)) THEN
    obj = getGaussLegendreRadauLeftQPLine2(nips=nips)
    RETURN
  ELSE
    isError = .TRUE.
    amsg = "This routine is available for line element only."
  END IF
CLASS DEFAULT
  isError = .TRUE.
  amsg = "This routine is callable for line element only."
END SELECT

IF (isError) THEN
  CALL ErrorMsg(&
    & msg=amsg%chars(),  &
    & file=__FILE__,  &
    & routine="getGaussLegendreRadauLeftQP2()", &
    & line=__LINE__, &
    & unitno=stderr)
END IF

END PROCEDURE GetGaussLegendreRadauLeftQP2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetGaussLegendreRadauLeftQP3
LOGICAL(LGT) :: isError
TYPE(String) :: amsg
isError = .FALSE.

SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  obj = getGaussLegendreRadauLeftQPLine1(order=p)
TYPE IS (ReferenceElement_)
  IF (isLine(refelem%name)) THEN
    obj = getGaussLegendreRadauLeftQPLine1(order=p)
    RETURN
  ELSE
    isError = .TRUE.
    amsg = "This routine is available for line element only."
  END IF
CLASS DEFAULT
  isError = .TRUE.
  amsg = "This routine is callable for line element only."
END SELECT

IF (isError) THEN
  CALL ErrorMsg(&
    & msg=amsg%chars(),  &
    & file=__FILE__,  &
    & routine="GetGaussLegendreRadauLeftQP3()", &
    & line=__LINE__, &
    & unitno=stderr)
END IF
END PROCEDURE GetGaussLegendreRadauLeftQP3

END SUBMODULE GaussLegendreRadauLeft
