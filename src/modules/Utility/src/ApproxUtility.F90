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

MODULE ApproxUtility
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: OPERATOR(.APPROXEQ.)
PUBLIC :: OPERATOR(.APPROXEQA.)
PUBLIC :: OPERATOR(.APPROXEQR.)
PUBLIC :: OPERATOR(.APPROXEQF.)
PUBLIC :: OPERATOR(.ARROXLE.)
PUBLIC :: OPERATOR(.ARROXGE.)
PUBLIC :: SOFTEQ
PUBLIC :: SOFTEQR
PUBLIC :: SOFTLE
PUBLIC :: SOFTLT
PUBLIC :: SOFTGE
PUBLIC :: SOFTGT
PUBLIC :: OPERATOR(==)
PUBLIC :: OPERATOR(/=)
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: isNumeric

!----------------------------------------------------------------------------
!                                                     APPROXEQ@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  3 Apr 2021
! summary: returns true if a and b are approximately equal
!
!# Introduction
!
! This routine just does a simple absolute comparison using an epsilon
! that is a compile time constant.
! It should be used whenever possible because it has
! the least overhead.
! However, it is not appropriate to use when a and b
! are either very large or very small.

INTERFACE
  MODULE ELEMENTAL FUNCTION approxeq_1(a, b) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: a, b
    LOGICAL(LGT) :: ans
  END FUNCTION approxeq_1
END INTERFACE

INTERFACE
  MODULE ELEMENTAL FUNCTION approxeq_2(a, b) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: a, b
    LOGICAL(LGT) :: ans
  END FUNCTION approxeq_2
END INTERFACE

INTERFACE OPERATOR(.APPROXEQ.)
  MODULE PROCEDURE approxeq_1, approxeq_2
END INTERFACE

INTERFACE OPERATOR(.APPROXEQA.)
  MODULE PROCEDURE approxeq_1, approxeq_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     APPROXR@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: returns bool logical indicating if a and b are approximately equal
!
!# Introduction
! This performs a relative comparison by scaling the default epsilon value to
! the size of the larger of the two. It should be used when c and b are of
! the same magnitude and very large or very small. If either c a or c b is
! zero (exactly) then this routine is equivalent to an absolute comparison.
!
! - TODO add support for Real32

INTERFACE OPERATOR(.APPROXEQR.)
  MODULE ELEMENTAL FUNCTION approxeqr_1(a, b) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: a, b
    LOGICAL(LGT) :: Ans
  END FUNCTION approxeqr_1

  MODULE ELEMENTAL FUNCTION approxeqr_2(a, b) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: a, b
    LOGICAL(LGT) :: Ans
  END FUNCTION approxeqr_2
END INTERFACE OPERATOR(.APPROXEQR.)

!----------------------------------------------------------------------------
!                                                    APPROXEQF@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Apr 2021
! summary: returns bool logical indicating if a and b are approximately equal
!
!# Introduction
! This performs a comparison of the binary representation of the two reals
! to compare the binary units in the last place (ULP). If the two reals differ
! on the floating point number line by 10 or less representable floating point
! reals then they are considered equal. In theory, this is the most
! appropriate comparison to use, but will break down near zero.
!
! - TODO change the name to approxeqf_1
! - TODO add support for the real32 and real64

INTERFACE
  MODULE ELEMENTAL FUNCTION approxeq_ulp_real(a, b) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: a, b
    LOGICAL(LGT) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE OPERATOR(.APPROXEQF.)
  MODULE PROCEDURE approxeq_ulp_real
END INTERFACE

!----------------------------------------------------------------------------
!                                                    APPROXLE@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Apr 2021
! summary: Defines the operator .APPROXLE.

INTERFACE
  MODULE ELEMENTAL FUNCTION approxle_1(r1, r2) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    LOGICAL(LGT) :: Ans
  END FUNCTION approxle_1

  MODULE ELEMENTAL FUNCTION approxle_2(r1, r2) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    LOGICAL(LGT) :: Ans
  END FUNCTION approxle_2
END INTERFACE

INTERFACE OPERATOR(.ARROXLE.)
  MODULE PROCEDURE approxle_1, approxle_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    APPROXGE@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary:  Defines the operation when comparing two single precision reals

INTERFACE
  MODULE ELEMENTAL FUNCTION approxge_1(r1, r2) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    LOGICAL(LGT) :: Ans
  END FUNCTION approxge_1

  MODULE ELEMENTAL FUNCTION approxge_2(r1, r2) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    LOGICAL(LGT) :: Ans
  END FUNCTION approxge_2
END INTERFACE

INTERFACE OPERATOR(.ARROXGE.)
  MODULE PROCEDURE approxge_1, approxge_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SOFTEQ@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Defines the operator SOFTEQ

INTERFACE
  MODULE ELEMENTAL FUNCTION softeq_1(r1, r2, tol) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    REAL(REAL32), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softeq_1

  MODULE ELEMENTAL FUNCTION softeq_2(r1, r2, tol) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    REAL(REAL64), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softeq_2
END INTERFACE

INTERFACE SOFTEQ
  MODULE PROCEDURE softeq_1, softeq_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SOFTEQR@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Defines the operator SOFTEQR

INTERFACE
  MODULE ELEMENTAL FUNCTION softeqr_1(r1, r2, tol) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    REAL(REAL32), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softeqr_1

  MODULE ELEMENTAL FUNCTION softeqr_2(r1, r2, tol) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    REAL(REAL64), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softeqr_2
END INTERFACE

INTERFACE SOFTEQR
  MODULE PROCEDURE softeqr_1, softeqr_2
END INTERFACE SOFTEQR

!----------------------------------------------------------------------------
!                                                       SOFTLE@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: SOFTLE

INTERFACE
  MODULE ELEMENTAL FUNCTION softle_1(r1, r2, tol) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    REAL(REAL32), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softle_1

  MODULE ELEMENTAL FUNCTION softle_2(r1, r2, tol) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    REAL(REAL64), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softle_2
END INTERFACE

INTERFACE SOFTLE
  MODULE PROCEDURE softle_1, softle_2
END INTERFACE SOFTLE

!----------------------------------------------------------------------------
!                                                      SOFTLT@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Defines the operation for SOFTLT

INTERFACE
  MODULE ELEMENTAL FUNCTION softlt_1(r1, r2, tol) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    REAL(REAL32), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softlt_1

  MODULE ELEMENTAL FUNCTION softlt_2(r1, r2, tol) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    REAL(REAL64), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softlt_2
END INTERFACE

INTERFACE SOFTLT
  MODULE PROCEDURE softlt_1, softlt_2
END INTERFACE SOFTLT

!----------------------------------------------------------------------------
!                                                       SOFTGE@ApproxMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION softge_1(r1, r2, tol) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    REAL(REAL32), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softge_1

  MODULE ELEMENTAL FUNCTION softge_2(r1, r2, tol) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    REAL(REAL64), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softge_2
END INTERFACE

INTERFACE SOFTGE
  MODULE PROCEDURE softge_1, softge_2
END INTERFACE SOFTGE

!----------------------------------------------------------------------------
!                                                      SOFTGT@ApproxMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION softgt_1(r1, r2, tol) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    REAL(REAL32), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softgt_1

  MODULE ELEMENTAL FUNCTION softgt_2(r1, r2, tol) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    REAL(REAL64), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softgt_2
END INTERFACE

INTERFACE SOFTGT
  MODULE PROCEDURE softgt_1, softgt_2
END INTERFACE SOFTGT

!----------------------------------------------------------------------------
!                                                           EQ@ApproxMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION equalto_logical(l1, l2) RESULT(Ans)
    LOGICAL(LGT), INTENT(IN) :: l1
    LOGICAL(LGT), INTENT(IN) :: l2
    LOGICAL(LGT) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE OPERATOR(==)
  MODULE PROCEDURE equalto_logical
END INTERFACE

!----------------------------------------------------------------------------
!                                                           EQ@ApproxMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION notequalto_logical(l1, l2) RESULT(Ans)
    LOGICAL(LGT), INTENT(IN) :: l1
    LOGICAL(LGT), INTENT(IN) :: l2
    LOGICAL(LGT) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE OPERATOR(/=)
  MODULE PROCEDURE notequalto_logical
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ASSIGN@ApproxMethods
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT(=)
  MODULE ELEMENTAL SUBROUTINE assign_char_to_int8(i, c)
    INTEGER(INT8), INTENT(OUT) :: i
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_int8

  MODULE ELEMENTAL SUBROUTINE assign_char_to_Int16(i, c)
    INTEGER(INT16), INTENT(OUT) :: i
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_Int16

  MODULE ELEMENTAL SUBROUTINE assign_char_to_Int32(i, c)
    INTEGER(INT32), INTENT(OUT) :: i
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_Int32

  MODULE ELEMENTAL SUBROUTINE assign_char_to_Int64(i, c)
    INTEGER(INT64), INTENT(OUT) :: i
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_Int64

  MODULE ELEMENTAL SUBROUTINE assign_char_to_Real32(s, c)
    REAL(REAL32), INTENT(OUT) :: s
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_Real32

  MODULE ELEMENTAL SUBROUTINE assign_char_to_Real64(s, c)
    REAL(REAL64), INTENT(OUT) :: s
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_Real64

  MODULE ELEMENTAL SUBROUTINE assign_char_to_bool(b, c)
    LOGICAL(LGT), INTENT(OUT) :: b
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_bool
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                           @ApproxMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION isNumeric(char_str) RESULT(bool)
    CHARACTER(*), INTENT(IN) :: char_str
    LOGICAL(LGT) :: bool
  END FUNCTION
END INTERFACE

END MODULE ApproxUtility
