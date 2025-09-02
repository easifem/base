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
USE BaseType, ONLY: FEVariable_, &
                    FEVariableScalar_, &
                    FEVariableVector_, &
                    FEVariableMatrix_, &
                    FEVariableConstant_, &
                    FEVariableSpace_, &
                    FEVariableTime_, &
                    FEVariableSpaceTime_, &
                    TypeFEVariableOpt

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: ABS
PUBLIC :: OPERATOR(**)
PUBLIC :: Sqrt
PUBLIC :: OPERATOR(.EQ.)
PUBLIC :: OPERATOR(.NE.)
PUBLIC :: Norm2

!----------------------------------------------------------------------------
!                                                             Abs@AbsMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable

INTERFACE ABS
  MODULE PURE FUNCTION fevar_abs(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_abs
END INTERFACE ABS

!----------------------------------------------------------------------------
!                                                          Power@PowerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable

INTERFACE OPERATOR(**)
  MODULE PURE FUNCTION fevar_power(obj, n) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_power
END INTERFACE OPERATOR(**)

!----------------------------------------------------------------------------
!                                                          Sqrt@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable

INTERFACE Sqrt
  MODULE PURE FUNCTION fevar_sqrt(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_sqrt
END INTERFACE Sqrt

!----------------------------------------------------------------------------
!                                                         Norm2@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = NORM2(FEVariable)

INTERFACE Norm2
  MODULE PURE FUNCTION fevar_norm2(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_norm2
END INTERFACE Norm2

!----------------------------------------------------------------------------
!                                                              InquiryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = NORM2(FEVariable)

INTERFACE OPERATOR(.EQ.)
  MODULE PURE FUNCTION fevar_isEqual(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION fevar_isEqual
END INTERFACE OPERATOR(.EQ.)

!----------------------------------------------------------------------------
!                                                              InquiryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = NORM2(FEVariable)

INTERFACE OPERATOR(.NE.)
  MODULE PURE FUNCTION fevar_notEqual(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    CLASS(FEVariable_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION fevar_notEqual
END INTERFACE OPERATOR(.NE.)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_UnaryMethod
