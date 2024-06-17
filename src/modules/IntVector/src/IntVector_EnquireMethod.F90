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

MODULE IntVector_EnquireMethod
USE Basetype, ONLY: IntVector_
USE GlobalData, ONLY: DFP, I4B, LGT, INT8, INT16, INT32, INT64
PRIVATE

PUBLIC :: OPERATOR(.in.)
PUBLIC :: isPresent
PUBLIC :: isAllocated
PUBLIC :: isInitiated

!----------------------------------------------------------------------------
!                                                 isAllocated@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns true if the instance is allocated

INTERFACE isAllocated
  MODULE PURE FUNCTION intVec_isAllocated(obj) RESULT(Ans)
    CLASS(IntVector_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION intVec_isAllocated
END INTERFACE isAllocated

INTERFACE Allocated
  MODULE PROCEDURE intVec_isAllocated
END INTERFACE Allocated

INTERFACE isInitiated
  MODULE PROCEDURE intVec_isAllocated
END INTERFACE isInitiated

!----------------------------------------------------------------------------
!                                              Operator(.in.)@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns true if a integer set is inside another

INTERFACE OPERATOR(.in.)
  MODULE PURE FUNCTION intVec_in1(obj1, obj2) RESULT(Ans)
    CLASS(IntVector_), INTENT(IN) :: obj1
    CLASS(IntVector_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION intVec_in1
END INTERFACE

!----------------------------------------------------------------------------
!                                             Operator(.in.)@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns true if a integer set is inside another

INTERFACE OPERATOR(.in.)
  MODULE PURE FUNCTION intVec_in2a(a, obj) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: a
    CLASS(IntVector_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION intVec_in2a

  MODULE PURE FUNCTION intVec_in2b(a, obj) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: a
    CLASS(IntVector_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION intVec_in2b

  MODULE PURE FUNCTION intVec_in2c(a, obj) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: a
    CLASS(IntVector_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION intVec_in2c

  MODULE PURE FUNCTION intVec_in2d(a, obj) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: a
    CLASS(IntVector_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION intVec_in2d
END INTERFACE OPERATOR(.in.)

!----------------------------------------------------------------------------
!                                                   isPresent@EnquireMethods
!----------------------------------------------------------------------------

INTERFACE isPresent
  MODULE PURE FUNCTION intVec_isPresent1(obj, VALUE) RESULT(Ans)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: VALUE
    LOGICAL(LGT) :: Ans
  END FUNCTION intVec_isPresent1
END INTERFACE isPresent

!----------------------------------------------------------------------------
!                                                        isPresent@getMethod
!----------------------------------------------------------------------------

INTERFACE isPresent
  MODULE PURE FUNCTION intVec_isPresent2(obj, VALUE) RESULT(Ans)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: VALUE(:)
    LOGICAL(LGT), ALLOCATABLE :: Ans(:)
  END FUNCTION intVec_isPresent2
END INTERFACE isPresent

END MODULE IntVector_EnquireMethod
