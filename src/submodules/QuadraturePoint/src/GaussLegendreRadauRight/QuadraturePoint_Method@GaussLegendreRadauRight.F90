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

SUBMODULE(QuadraturePoint_Method) GaussLegendreRadauRight
USE BaseMethod
IMPLICIT NONE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                               GaussLegendreRadauRight@GaussLegendreRadauRight
!----------------------------------------------------------------------------

FUNCTION getGaussLegendreRadauRightQPLine1(order)  &
  & RESULT(obj)
  INTEGER(I4B), INTENT(IN) :: order
  TYPE(QuadraturePoint_) :: obj
  INTEGER(I4B) :: np
  REAL(DFP) :: pt(2 + INT(order / 2, KIND=I4B))
  REAL(DFP) :: wt(2 + INT(order / 2, KIND=I4B))
  np = SIZE(pt)
  CALL LegendreQuadrature(n=np, pt=pt, wt=wt, quadType=GaussRadauRight)
  CALL Initiate(obj=obj, points=pt.ROWCONCAT.wt)
END FUNCTION getGaussLegendreRadauRightQPLine1

!----------------------------------------------------------------------------
!                              GaussLegendreRadauRight@GaussLegendreRadauRight
!----------------------------------------------------------------------------

FUNCTION getGaussLegendreRadauRightQPLine2(nips)  &
  & RESULT(obj)
  INTEGER(I4B), INTENT(IN) :: nips(1)
  TYPE(QuadraturePoint_) :: obj
  REAL(DFP) :: pt(nips(1))
  REAL(DFP) :: wt(nips(1))
  CALL LegendreQuadrature(n=nips(1), pt=pt, wt=wt, quadType=GaussRadauRight)
  CALL Initiate(obj=obj, points=pt.ROWCONCAT.wt)
END FUNCTION getGaussLegendreRadauRightQPLine2

!----------------------------------------------------------------------------
!                                          GaussLegendreRadauRightQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE GetGaussLegendreRadauRightQP1
LOGICAL(LGT) :: isError
TYPE(String) :: amsg
isError = .FALSE.

SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  obj = getGaussLegendreRadauRightQPLine1(order=order)
TYPE IS (ReferenceElement_)
  IF (isLine(refelem%name)) THEN
    obj = getGaussLegendreRadauRightQPLine1(order=order)
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
    & routine="getGaussLegendreRadauRightQP1()", &
    & line=__LINE__, &
    & unitno=stderr)
END IF

END PROCEDURE GetGaussLegendreRadauRightQP1

!----------------------------------------------------------------------------
!                                           GaussLegendreRadauRightQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE GetGaussLegendreRadauRightQP2
LOGICAL(LGT) :: isError
TYPE(String) :: amsg
isError = .FALSE.

SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  obj = getGaussLegendreRadauRightQPLine2(nips=nips)
TYPE IS (ReferenceElement_)
  IF (isLine(refelem%name)) THEN
    obj = getGaussLegendreRadauRightQPLine2(nips=nips)
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
    & routine="getGaussLegendreRadauRightQP2()", &
    & line=__LINE__, &
    & unitno=stderr)
END IF

END PROCEDURE GetGaussLegendreRadauRightQP2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetGaussLegendreRadauRightQP3
LOGICAL(LGT) :: isError
TYPE(String) :: amsg
isError = .FALSE.

SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  obj = getGaussLegendreRadauRightQPLine1(order=p)
TYPE IS (ReferenceElement_)
  IF (isLine(refelem%name)) THEN
    obj = getGaussLegendreRadauRightQPLine1(order=p)
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
    & routine="GetGaussLegendreRadauRightQP3()", &
    & line=__LINE__, &
    & unitno=stderr)
END IF
END PROCEDURE GetGaussLegendreRadauRightQP3

END SUBMODULE GaussLegendreRadauRight
