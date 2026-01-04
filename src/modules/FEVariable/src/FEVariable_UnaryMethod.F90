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

MODULE FEVariable_UnaryMethod
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: ABS, ABS_
PUBLIC :: OPERATOR(**), POWER_
PUBLIC :: Sqrt, Sqrt_
PUBLIC :: Norm2, Norm2_
PUBLIC :: OPERATOR(.EQ.)
PUBLIC :: OPERATOR(.NE.)

!----------------------------------------------------------------------------
!                                                             Abs@AbsMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = ABS(obj)

INTERFACE ABS
  MODULE PURE FUNCTION obj_abs(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION obj_abs
END INTERFACE ABS

!----------------------------------------------------------------------------
!                                                             Abs@AbsMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = ABS(obj)

INTERFACE ABS_
  MODULE PURE SUBROUTINE obj_abs_(obj, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: ans
  END SUBROUTINE obj_abs_
END INTERFACE ABS_

!----------------------------------------------------------------------------
!                                                          Power@PowerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = obj ** n

INTERFACE OPERATOR(**)
  MODULE PURE FUNCTION obj_power(obj, n) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    TYPE(FEVariable_) :: ans
  END FUNCTION obj_power
END INTERFACE OPERATOR(**)

!----------------------------------------------------------------------------
!                                                          Power@PowerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = obj ** n

INTERFACE POWER_
  MODULE PURE SUBROUTINE obj_power_(obj, n, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    TYPE(FEVariable_), INTENT(INOUT) :: ans
  END SUBROUTINE obj_power_
END INTERFACE POWER_

!----------------------------------------------------------------------------
!                                                          Sqrt@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = SQRT(obj)

INTERFACE Sqrt
  MODULE PURE FUNCTION obj_sqrt(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION obj_sqrt
END INTERFACE Sqrt

!----------------------------------------------------------------------------
!                                                          SQRT@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-01
! summary:  ans = SQRT(obj)
!
!# Introduction
!  No allocation

INTERFACE Sqrt_
  MODULE PURE SUBROUTINE obj_sqrt_(obj, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: ans
  END SUBROUTINE obj_sqrt_
END INTERFACE Sqrt_

!----------------------------------------------------------------------------
!                                                         Norm2@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = NORM2(obj)

INTERFACE Norm2
  MODULE PURE FUNCTION obj_norm2(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION obj_norm2
END INTERFACE Norm2

!----------------------------------------------------------------------------
!                                                         Norm2@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = NORM2(obj)

INTERFACE Norm2_
  MODULE PURE SUBROUTINE obj_norm2_(obj, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: ans
  END SUBROUTINE obj_norm2_
END INTERFACE Norm2_

!----------------------------------------------------------------------------
!                                                              InquiryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = obj1 .eq. obj2

INTERFACE OPERATOR(.EQ.)
  MODULE PURE FUNCTION obj_isEqual(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isEqual
END INTERFACE OPERATOR(.EQ.)

!----------------------------------------------------------------------------
!                                                              InquiryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = obj1 .ne. obj2

INTERFACE OPERATOR(.NE.)
  MODULE PURE FUNCTION obj_notEqual(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION obj_notEqual
END INTERFACE OPERATOR(.NE.)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_UnaryMethod
