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
USE Display_Method, ONLY: Util_Display => Display, Tostring

USE MdEncode_Method, ONLY: Util_MdEncode => MdEncode

USE GlobalData, ONLY: CHAR_LF2

USE String_Class, ONLY: StringReallocate => Reallocate

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

IF (ALLOCATED(obj%N)) THEN
  CALL StringReallocate(rh, obj%nns)
  CALL StringReallocate(ch, obj%nips)

  DO ii = 1, obj%nns
    rh(ii) = "$N_{"//tostring(ii)//"}$"
  END DO

  DO ii = 1, obj%nips
    ch(ii) = "$ips_{"//tostring(ii)//"}$"
  END DO

ans = ans//"**N**"//CHAR_LF2//Util_MdEncode(val=obj%N, rh=rh, ch=ch)//CHAR_LF2

ELSE
  ans = ans//"**N Not ALLOCATED**"//CHAR_LF2
END IF

IF (ALLOCATED(obj%dNdXi)) THEN
  CALL StringReallocate(rh, obj%nns)
  CALL StringReallocate(ch, obj%xidim)

  DO ii = 1, obj%nns
    rh(ii) = "$\frac{\partial N^{"//tostring(ii)//"}}{\partial \xi}$"
  END DO

  DO ii = 1, obj%xidim
    ch(ii) = "$\frac{\partial N}{\partial \xi_{"//tostring(ii)//"}}$"
  END DO

  DO ii = 1, obj%nips
    ans = ans//"**dNdXi(:, :, "//tostring(ii)//" )**"//CHAR_LF2// &
          Util_MdEncode(val=obj%dNdXi(:, :, ii), rh=rh, ch=ch)//CHAR_LF2
  END DO

ELSE

  ans = ans//"**dNdXi Not ALLOCATED**"//CHAR_LF2

END IF

IF (ALLOCATED(obj%dNdXt)) THEN
  CALL StringReallocate(rh, obj%nns)
  CALL StringReallocate(ch, obj%nsd)

  DO ii = 1, obj%nns
    rh(ii) = "$\frac{\partial N^{"//tostring(ii)//"}}{\partial x}$"
  END DO

  DO ii = 1, obj%nsd
    ch(ii) = "$\frac{\partial N}{\partial {x}_{"//tostring(ii)//"}}$"
  END DO

  DO ii = 1, obj%nips
    ans = ans//"**dNdXt(:, :, "//tostring(ii)//" )**"//CHAR_LF2// &
          Util_MdEncode(val=obj%dNdXt(:, :, ii), rh=rh, ch=ch)//CHAR_LF2
  END DO

ELSE

  ans = ans//"**dNdXt Not ALLOCATED**"//CHAR_LF2

END IF

IF (ALLOCATED(obj%jacobian)) THEN
  CALL StringReallocate(rh, obj%nsd)
  CALL StringReallocate(ch, obj%xidim)

  DO ii = 1, obj%nsd
    rh(ii) = "row-"//tostring(ii)
  END DO

  DO ii = 1, obj%xidim
    ch(ii) = "col-"//tostring(ii)
  END DO

  DO ii = 1, obj%nips
    ans = ans//"**jacobian(:, :, "//tostring(ii)//" )**"//CHAR_LF2// &
          Util_MdEncode(val=obj%jacobian(:, :, ii), rh=rh, ch=ch)//CHAR_LF2
  END DO

ELSE
  ans = ans//"**jacobian Not ALLOCATED**"//CHAR_LF2
END IF

IF (ALLOCATED(obj%js)) THEN
  CALL StringReallocate(rh, 1)
  CALL StringReallocate(ch, obj%nips)
  rh(1) = "js"
  DO ii = 1, obj%nips
    ch(ii) = "$js_{"//tostring(ii)//"}$"
  END DO

  ans = ans//"**Js**"//CHAR_LF2//Util_MdEncode(val=obj%js, rh=rh, ch=ch)//CHAR_LF2

ELSE
  ans = ans//"**js Not ALLOCATED**"//CHAR_LF2
END IF

IF (ALLOCATED(obj%thickness)) THEN
  CALL StringReallocate(rh, 1)
  CALL StringReallocate(ch, obj%nips)

  rh(1) = "thickness"
  DO ii = 1, obj%nips
    ch(ii) = "thickness${}_{"//tostring(ii)//"}$"
  END DO

  ans = ans//"**thickness**"//CHAR_LF2// &
        Util_MdEncode(val=obj%thickness, rh=rh, ch=ch)//CHAR_LF2
ELSE
  ans = ans//"**thickness Not ALLOCATED**"//CHAR_LF2
END IF

IF (ALLOCATED(obj%normal)) THEN
  CALL StringReallocate(rh, SIZE(obj%normal, 1))
  CALL StringReallocate(ch, obj%nips)

  DO ii = 1, SIZE(obj%normal, 1)
    rh(ii) = "$n_{"//tostring(ii)//"}$"
  END DO

  DO ii = 1, obj%nips
    ch(ii) = "$ips_{"//tostring(ii)//"}$"
  END DO

  ans = ans//"**normal**"//CHAR_LF2// &
        Util_MdEncode(val=obj%normal, rh=rh, ch=ch)//CHAR_LF2
ELSE
  ans = ans//"**normal not ALLOCATED**"//CHAR_LF2
END IF

END PROCEDURE ElemshapeData_MdEncode

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_display_1
CALL Util_Display(msg, unitno=unitno)
CALL Util_Display(obj%nsd, "nsd: ", unitno)
CALL Util_Display(obj%xidim, "xidim: ", unitno)
CALL Util_Display(obj%nns, "nns: ", unitno)
CALL Util_Display(obj%nips, "nips: ", unitno)

IF (ALLOCATED(obj%N)) THEN
  CALL Util_Display(obj%N, "N: ", unitno)
ELSE
  CALL Util_Display("N: NOT ALLOCATED", unitno)
END IF
IF (ALLOCATED(obj%dNdXi)) THEN
  CALL Util_Display(obj%dNdXi, "dNdXi: ", unitno)
ELSE
  CALL Util_Display("dNdXi: NOT ALLOCATED", unitno)
END IF
IF (ALLOCATED(obj%dNdXt)) THEN
  CALL Util_Display(obj%dNdXt, "dNdXt: ", unitno)
ELSE
  CALL Util_Display("dNdXt: NOT ALLOCATED", unitno)
END IF
IF (ALLOCATED(obj%jacobian)) THEN
  CALL Util_Display(obj%Jacobian, "jacobian: ", unitno)
ELSE
  CALL Util_Display("jacobian: NOT ALLOCATED", unitno)
END IF

IF (ALLOCATED(obj%js)) THEN
  CALL Util_Display(obj%js, "js: ", unitno)
ELSE
  CALL Util_Display("js: NOT ALLOCATED", unitno)
END IF

IF (ALLOCATED(obj%ws)) THEN
  CALL Util_Display(obj%ws, "ws: ", unitno)
ELSE
  CALL Util_Display("ws: NOT ALLOCATED", unitno)
END IF

IF (ALLOCATED(obj%thickness)) THEN
  CALL Util_Display(obj%thickness, "thickness: ", unitno)
ELSE
  CALL Util_Display("thickness: NOT ALLOCATED", unitno)
END IF
IF (ALLOCATED(obj%coord)) THEN
  CALL Util_Display(obj%coord, "coord: ", unitno)
ELSE
  CALL Util_Display("coord: NOT ALLOCATED", unitno)
END IF
IF (ALLOCATED(obj%normal)) THEN
  CALL Util_Display(obj%normal, "normal: ", unitno)
ELSE
  CALL Util_Display("normal: NOT ALLOCATED", unitno)
END IF
SELECT TYPE (obj); TYPE IS (STElemShapeData_)
  CALL Util_Display("SHAPE FUNCTION IN TIME: ", unitno=unitno)
  CALL Util_Display(obj%jt, "jt: ", unitno=unitno)
  CALL Util_Display(obj%wt, "wt: ", unitno=unitno)
  IF (ALLOCATED(obj%T)) THEN
    CALL Util_Display(obj%T, "T: ", unitno=unitno)
  ELSE
    CALL Util_Display("T: NOT ALLOCATED", unitno=unitno)
  END IF
  IF (ALLOCATED(obj%dTdTheta)) THEN
    CALL Util_Display(obj%dTdTheta, "dTdTheta: ", unitno=unitno)
  ELSE
    CALL Util_Display("dTdTheta: NOT ALLOCATED", unitno=unitno)
  END IF
  IF (ALLOCATED(obj%dNTdt)) THEN
    CALL Util_Display(obj%dNTdt, "dNTdt: ", unitno=unitno)
  ELSE
    CALL Util_Display("dNTdt: NOT ALLOCATED", unitno=unitno)
  END IF
  IF (ALLOCATED(obj%dNTdXt)) THEN
    CALL Util_Display(obj%dNTdXt, "dNTdXt: ", unitno=unitno)
  ELSE
    CALL Util_Display("dNTdXt: NOT ALLOCATED", unitno=unitno)
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
               unitno=unitno)
END DO
END PROCEDURE elemsd_display_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
