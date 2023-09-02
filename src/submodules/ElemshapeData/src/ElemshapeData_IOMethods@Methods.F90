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
! date: 4 March 2021
! summary: Methods for IO of [[elemshapedata_]] and [[stelemshapedata_]]

SUBMODULE(ElemshapeData_IOMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                 ElemshapeData_ReactEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE ElemshapeData_ReactEncode

END PROCEDURE ElemshapeData_ReactEncode

!----------------------------------------------------------------------------
!                                                   ElemshapeData_MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE ElemshapeData_MdEncode
INTEGER(I4B) :: ii
TYPE(String), ALLOCATABLE :: rh(:), ch(:)

ans = MdEncode(obj%quad)//CHAR_LF2

IF (ALLOCATED(obj%N)) THEN
  CALL Reallocate(rh, SIZE(obj%N, 1))
  CALL Reallocate(ch, SIZE(obj%N, 2))
  DO ii = 1, SIZE(obj%N, 1)
    rh(ii) = "$N_{"//tostring(ii)//"}$"
  END DO
  DO ii = 1, SIZE(obj%N, 2)
    ch(ii) = "$ips_{"//tostring(ii)//"}$"
  END DO
  ans = ans//"**N**"//CHAR_LF2//MdEncode(val=obj%N, rh=rh, ch=ch)//CHAR_LF2
ELSE
  ans = ans//"**N Not ALLOCATED**"//CHAR_LF2
END IF

IF (ALLOCATED(obj%dNdXi)) THEN
  CALL Reallocate(rh, SIZE(obj%dNdXi, 1))
  CALL Reallocate(ch, SIZE(obj%dNdXi, 2))
  DO ii = 1, SIZE(obj%dNdXi, 1)
    rh(ii) = "$\frac{\partial N^{"//tostring(ii)//"}}{\partial \xi}$"
  END DO
  DO ii = 1, SIZE(obj%dNdXi, 2)
    ch(ii) = "$\frac{\partial N}{\partial \xi_{"//tostring(ii)//"}}$"
  END DO
  DO ii = 1, SIZE(obj%dNdXi, 3)
    ans = ans//"**dNdXi(:, :, "//tostring(ii)//" )**"//CHAR_LF2// &
      & MdEncode(val=obj%dNdXi(:, :, ii), rh=rh, ch=ch)//CHAR_LF2
  END DO
ELSE
  ans = ans//"**dNdXi Not ALLOCATED**"//CHAR_LF2
END IF

IF (ALLOCATED(obj%dNdXt)) THEN
  CALL Reallocate(rh, SIZE(obj%dNdXt, 1))
  CALL Reallocate(ch, SIZE(obj%dNdXt, 2))
  DO ii = 1, SIZE(obj%dNdXt, 1)
    rh(ii) = "$\frac{\partial N^{"//tostring(ii)//"}}{\partial x}$"
  END DO
  DO ii = 1, SIZE(obj%dNdXt, 2)
    ch(ii) = "$\frac{\partial N}{\partial {x}_{"//tostring(ii)//"}}$"
  END DO
  DO ii = 1, SIZE(obj%dNdXt, 3)
    ans = ans//"**dNdXt(:, :, "//tostring(ii)//" )**"//CHAR_LF2// &
      & MdEncode(val=obj%dNdXt(:, :, ii), rh=rh, ch=ch)//CHAR_LF2
  END DO
ELSE
  ans = ans//"**dNdXt Not ALLOCATED**"//CHAR_LF2
END IF

IF (ALLOCATED(obj%jacobian)) THEN
  CALL Reallocate(rh, SIZE(obj%jacobian, 1))
  CALL Reallocate(ch, SIZE(obj%jacobian, 2))
  DO ii = 1, SIZE(obj%jacobian, 1)
    rh(ii) = "row-"//tostring(ii)
  END DO
  DO ii = 1, SIZE(obj%jacobian, 2)
    ch(ii) = "col-"//tostring(ii)
  END DO
  DO ii = 1, SIZE(obj%jacobian, 3)
    ans = ans//"**jacobian(:, :, "//tostring(ii)//" )**"//CHAR_LF2// &
      & MdEncode(val=obj%jacobian(:, :, ii), rh=rh, ch=ch)//CHAR_LF2
  END DO
ELSE
  ans = ans//"**jacobian Not ALLOCATED**"//CHAR_LF2
END IF

IF (ALLOCATED(obj%js)) THEN
  CALL Reallocate(rh, 1)
  CALL Reallocate(ch, SIZE(obj%js, 1))
  rh(1) = "js"
  DO ii = 1, SIZE(obj%js, 1)
    ch(ii) = "$js_{"//tostring(ii)//"}$"
  END DO
  ans = ans//"**Js**"//CHAR_LF2//MdEncode(val=obj%js, rh=rh, ch=ch)//CHAR_LF2
ELSE
  ans = ans//"**js Not ALLOCATED**"//CHAR_LF2
END IF

IF (ALLOCATED(obj%thickness)) THEN
  CALL Reallocate(rh, 1)
  CALL Reallocate(ch, SIZE(obj%thickness, 1))
  rh(1) = "thickness"
  DO ii = 1, SIZE(obj%thickness, 1)
    ch(ii) = "thickness${}_{"//tostring(ii)//"}$"
  END DO
  ans = ans//"**thickness**"//CHAR_LF2// &
  & MdEncode(val=obj%thickness, rh=rh, ch=ch)//CHAR_LF2
ELSE
  ans = ans//"**thickness Not ALLOCATED**"//CHAR_LF2
END IF

IF (ALLOCATED(obj%normal)) THEN
  CALL Reallocate(rh, SIZE(obj%normal, 1))
  CALL Reallocate(ch, SIZE(obj%normal, 2))
  DO ii = 1, SIZE(obj%normal, 1)
    rh(ii) = "$n_{"//tostring(ii)//"}$"
  END DO
  DO ii = 1, SIZE(obj%normal, 2)
    ch(ii) = "$ips_{"//tostring(ii)//"}$"
  END DO
  ans = ans//"**normal**"//CHAR_LF2// &
    & MdEncode(val=obj%normal, rh=rh, ch=ch)//CHAR_LF2
ELSE
  ans = ans//"**normal not ALLOCATED**"//CHAR_LF2
END IF

! SELECT TYPE (obj); TYPE IS (STElemShapeData_)
!   CALL Display("# SHAPE FUNCTION IN TIME: ", unitno=unitno)
!   CALL Display(obj%jt, "# jt: ", unitno=unitno)
!   CALL Display(obj%theta, "# theta: ", unitno=unitno)
!   CALL Display(obj%wt, "# wt: ", unitno=unitno)
!   IF (ALLOCATED(obj%T)) THEN
!     CALL Display(obj%T, "# T: ", unitno=unitno)
!   ELSE
!     CALL Display("# T: NOT ALLOCATED", unitno=unitno)
!   END IF
!   IF (ALLOCATED(obj%dTdTheta)) THEN
!     CALL Display(obj%dTdTheta, "# dTdTheta: ", unitno=unitno)
!   ELSE
!     CALL Display("# dTdTheta: NOT ALLOCATED", unitno=unitno)
!   END IF
!   IF (ALLOCATED(obj%dNTdt)) THEN
!     CALL Display(obj%dNTdt, "# dNTdt: ", unitno=unitno)
!   ELSE
!     CALL Display("# dNTdt: NOT ALLOCATED", unitno=unitno)
!   END IF
!   IF (ALLOCATED(obj%dNTdXt)) THEN
!     CALL Display(obj%dNTdXt, "# dNTdXt: ", unitno=unitno)
!   ELSE
!     CALL Display("# dNTdXt: NOT ALLOCATED", unitno=unitno)
!   END IF
! END SELECT
END PROCEDURE ElemshapeData_MdEncode

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_display_1
CALL Display(msg, unitno=unitno)
CALL Display("# SHAPE FUNCTION IN SPACE: ", unitno=unitno)
CALL Display(obj%Quad, "# Quadrature Point: ", unitno=unitno)
IF (ALLOCATED(obj%N)) THEN
  CALL Display(obj%N, "# N: ", unitno)
ELSE
  CALL Display("# N: NOT ALLOCATED", unitno)
END IF
IF (ALLOCATED(obj%dNdXi)) THEN
  CALL Display(obj%dNdXi, "# dNdXi: ", unitno)
ELSE
  CALL Display("# dNdXi: NOT ALLOCATED", unitno)
END IF
IF (ALLOCATED(obj%dNdXt)) THEN
  CALL Display(obj%dNdXt, "# dNdXt: ", unitno)
ELSE
  CALL Display("# dNdXt: NOT ALLOCATED", unitno)
END IF
IF (ALLOCATED(obj%jacobian)) THEN
  CALL Display(obj%Jacobian, "# jacobian: ", unitno)
ELSE
  CALL Display("# jacobian: NOT ALLOCATED", unitno)
END IF
IF (ALLOCATED(obj%js)) THEN
  CALL Display(obj%js, "# js: ", unitno)
ELSE
  CALL Display("# js: NOT ALLOCATED", unitno)
END IF
IF (ALLOCATED(obj%thickness)) THEN
  CALL Display(obj%thickness, "# thickness: ", unitno)
ELSE
  CALL Display("# thickness: NOT ALLOCATED", unitno)
END IF
IF (ALLOCATED(obj%coord)) THEN
  CALL Display(obj%coord, "# coord: ", unitno)
ELSE
  CALL Display("# coord: NOT ALLOCATED", unitno)
END IF
IF (ALLOCATED(obj%normal)) THEN
  CALL Display(obj%normal, "# normal: ", unitno)
ELSE
  CALL Display("# normal: NOT ALLOCATED", unitno)
END IF
SELECT TYPE (obj); TYPE IS (STElemShapeData_)
  CALL Display("# SHAPE FUNCTION IN TIME: ", unitno=unitno)
  CALL Display(obj%jt, "# jt: ", unitno=unitno)
  CALL Display(obj%theta, "# theta: ", unitno=unitno)
  CALL Display(obj%wt, "# wt: ", unitno=unitno)
  IF (ALLOCATED(obj%T)) THEN
    CALL Display(obj%T, "# T: ", unitno=unitno)
  ELSE
    CALL Display("# T: NOT ALLOCATED", unitno=unitno)
  END IF
  IF (ALLOCATED(obj%dTdTheta)) THEN
    CALL Display(obj%dTdTheta, "# dTdTheta: ", unitno=unitno)
  ELSE
    CALL Display("# dTdTheta: NOT ALLOCATED", unitno=unitno)
  END IF
  IF (ALLOCATED(obj%dNTdt)) THEN
    CALL Display(obj%dNTdt, "# dNTdt: ", unitno=unitno)
  ELSE
    CALL Display("# dNTdt: NOT ALLOCATED", unitno=unitno)
  END IF
  IF (ALLOCATED(obj%dNTdXt)) THEN
    CALL Display(obj%dNTdXt, "# dNTdXt: ", unitno=unitno)
  ELSE
    CALL Display("# dNTdXt: NOT ALLOCATED", unitno=unitno)
  END IF
END SELECT
END PROCEDURE elemsd_display_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_display_2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(obj)
  CALL Display(obj=obj(ii), msg=TRIM(msg)//"("//tostring(ii)//"): ", &
    & unitno=unitno)
END DO
END PROCEDURE elemsd_display_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
