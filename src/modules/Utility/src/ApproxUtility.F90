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
USE GlobalData, ONLY: DFP, I4B, LGT
USE GlobalData, ONLY: REAL32, REAL64
USE GlobalData, ONLY: INT8, INT16, INT32, INT64
IMPLICIT NONE
PRIVATE
PUBLIC :: OPERATOR(.APPROXEQ.)
PUBLIC :: OPERATOR(.APPROXEQA.)
PUBLIC :: OPERATOR(.APPROXEQR.)
PUBLIC :: OPERATOR(.APPROXEQF.)
PUBLIC :: OPERATOR(.APPROXLE.)
PUBLIC :: OPERATOR(.APPROXGE.)
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
PUBLIC :: CharToNumber

!----------------------------------------------------------------------------
!                                                     APPROXEQ@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns true if a and b are approximately equal
!
! This routine does a simple absolute comparison using an epsilon
! math%zero, that is a compile time constant.
! It should be used whenever possible because it has
! the least overhead.
! However, it is not appropriate to use when a and b
! are either very large or very small.
!
! Calling example:
!
!```fortran
! abool= a .APPROXEQ. b
! abool= a .APPROXEQA. b
!```
!
!$$
! (a-b) \le 1.0\times 10^{-15}
!$$

INTERFACE OPERATOR(.APPROXEQ.)
  MODULE PURE ELEMENTAL FUNCTION approxeq_1(a, b) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: a, b
    LOGICAL(LGT) :: ans
  END FUNCTION approxeq_1

  MODULE PURE ELEMENTAL FUNCTION approxeq_2(a, b) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: a, b
    LOGICAL(LGT) :: ans
  END FUNCTION approxeq_2
END INTERFACE OPERATOR(.APPROXEQ.)

INTERFACE OPERATOR(.APPROXEQA.)
  MODULE PROCEDURE approxeq_1, approxeq_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     APPROXR@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: returns bool logical indicating if a and b are approximately equal
!
! This performs a RELATIVE comparison by scaling the default
! epsilon-value to the size of the larger of the two.
!
! It should be used when a and b are of
! the same magnitude and very large or very small.
! If either a or b is zero (exactly) then this routine is
! equivalent to an absolute comparison APPROXEQ.
!
! Calling example:
!
!```fortran
! abool= a .APPROXEQR. b
!```
!
!$$
! (a-b) \le 1.0\times 10^{-15} \max{\vert a \vert, \vert b \vert}
!$$

INTERFACE OPERATOR(.APPROXEQR.)
  MODULE PURE ELEMENTAL FUNCTION approxeqr_1(a, b) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: a, b
    LOGICAL(LGT) :: Ans
  END FUNCTION approxeqr_1

  MODULE PURE ELEMENTAL FUNCTION approxeqr_2(a, b) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: a, b
    LOGICAL(LGT) :: Ans
  END FUNCTION approxeqr_2
END INTERFACE OPERATOR(.APPROXEQR.)

!----------------------------------------------------------------------------
!                                                    APPROXEQF@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns bool logical indicating if a and b are approximately equal
!
! This performs a comparison of the binary representation of the two reals
! to compare the binary units in the last place (ULP).
!
! If the two reals differ
! on the floating point number line by 10 or less representable floating point
! reals then they are considered equal.
!
! In theory, this is the most
! appropriate comparison to use, but will break down near zero.
!
! TODO:
! - Change the name to approxeqf_1
! - add support for the real32 and real64

INTERFACE OPERATOR(.APPROXEQF.)
  MODULE PURE ELEMENTAL FUNCTION approxeqf_1(a, b) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: a, b
    LOGICAL(LGT) :: Ans
  END FUNCTION approxeqf_1

  MODULE PURE ELEMENTAL FUNCTION approxeqf_2(a, b) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: a, b
    LOGICAL(LGT) :: Ans
  END FUNCTION approxeqf_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    APPROXLE@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Defines the operator .APPROXLE.
!
! Check Lesser than or equal to operator.
!
!```fortran
! abool = a .ApproxLE. b
!```
!
!$$
! a \le b + 1.0 \times 10^{-15}
!$$

INTERFACE OPERATOR(.APPROXLE.)
  MODULE PURE ELEMENTAL FUNCTION approxle_1(r1, r2) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    LOGICAL(LGT) :: Ans
  END FUNCTION approxle_1

  MODULE PURE ELEMENTAL FUNCTION approxle_2(r1, r2) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    LOGICAL(LGT) :: Ans
  END FUNCTION approxle_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    APPROXGE@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary:  Defines the operation when comparing two single precision reals
!
! Check Greater than or equal to operator.
!
!```fortran
! abool = a .ApproxGE. b
!```
!
!$$
! a \ge b + 1.0 \times 10^{-15}
!$$

INTERFACE OPERATOR(.APPROXGE.)
  MODULE PURE ELEMENTAL FUNCTION approxge_1(r1, r2) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    LOGICAL(LGT) :: Ans
  END FUNCTION approxge_1

  MODULE PURE ELEMENTAL FUNCTION approxge_2(r1, r2) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    LOGICAL(LGT) :: Ans
  END FUNCTION approxge_2
END INTERFACE OPERATOR(.APPROXGE.)

!----------------------------------------------------------------------------
!                                                      SOFTEQ@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Defines the operator SOFTEQ
!
! Check equality of two numbers.
!
! Calling example:
!
!```fortran
! abool = SOFTEQ(a, b, tol)
!```
!
!$$
! \vert a - b \vert \le tol
!$$

INTERFACE SOFTEQ
  MODULE PURE ELEMENTAL FUNCTION softeq_1(r1, r2, tol) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    REAL(REAL32), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softeq_1

  MODULE PURE ELEMENTAL FUNCTION softeq_2(r1, r2, tol) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    REAL(REAL64), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softeq_2
END INTERFACE SOFTEQ

!----------------------------------------------------------------------------
!                                                     SOFTEQR@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Defines the operator SOFTEQR
!
! Check equality of two numbers.
!
! Calling example:
!
!```fortran
!abool = SOFTEQ(a, b, tol)
!```
!
!$$
!\vert a - b \vert \le tol \times max(\vert a \vert, \vert b\vert)
!$$

INTERFACE SOFTEQR
  MODULE PURE ELEMENTAL FUNCTION softeqr_1(r1, r2, tol) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    REAL(REAL32), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softeqr_1

  MODULE PURE ELEMENTAL FUNCTION softeqr_2(r1, r2, tol) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    REAL(REAL64), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softeqr_2
END INTERFACE SOFTEQR

!----------------------------------------------------------------------------
!                                                       SOFTLE@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: SOFTLE
!
! Check lesser than or equal to inequality of two numbers.
!
! Calling example:
!
!```fortran
!abool = SOFTLE(a, b, tol)
!```
!
!$$
!a \le b + tol\
!$$

INTERFACE SOFTLE
  MODULE PURE ELEMENTAL FUNCTION softle_1(r1, r2, tol) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    REAL(REAL32), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softle_1

  MODULE PURE ELEMENTAL FUNCTION softle_2(r1, r2, tol) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    REAL(REAL64), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softle_2
END INTERFACE SOFTLE

!----------------------------------------------------------------------------
!                                                      SOFTLT@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Defines the operation for SOFTLT
!
! Check lesser than inequality of two numbers.
!
! Calling example:
!
! ```fortran
! abool = SOFTLT(a, b, tol)
! ```
!
! $$
! a < b + tol
! $$

INTERFACE SOFTLT
  MODULE PURE ELEMENTAL FUNCTION softlt_1(r1, r2, tol) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    REAL(REAL32), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softlt_1

  MODULE PURE ELEMENTAL FUNCTION softlt_2(r1, r2, tol) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    REAL(REAL64), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softlt_2
END INTERFACE SOFTLT

!----------------------------------------------------------------------------
!                                                       SOFTGE@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Defines the operation for SOFTGE
!
! Check greater than or equal to inequality of two numbers.
!
! Calling example:
!
!```fortran
!abool = SOFTGE(a, b, tol)
!```
!
!$$
!a \ge b + tol
!$$

INTERFACE SOFTGE
  MODULE PURE ELEMENTAL FUNCTION softge_1(r1, r2, tol) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    REAL(REAL32), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softge_1

  MODULE PURE ELEMENTAL FUNCTION softge_2(r1, r2, tol) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    REAL(REAL64), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softge_2
END INTERFACE SOFTGE

!----------------------------------------------------------------------------
!                                                      SOFTGT@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Defines the operation for SOFTGE
!
! Check greater than inequality of two numbers.
!
! Calling example:
!
!```fortran
!abool = SOFTGT(a, b, tol)
!```
!
!$$
!a > b + tol
!$$
INTERFACE SOFTGT
  MODULE PURE ELEMENTAL FUNCTION softgt_1(r1, r2, tol) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: r1
    REAL(REAL32), INTENT(IN) :: r2
    REAL(REAL32), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softgt_1

  MODULE PURE ELEMENTAL FUNCTION softgt_2(r1, r2, tol) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: r1
    REAL(REAL64), INTENT(IN) :: r2
    REAL(REAL64), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softgt_2
END INTERFACE SOFTGT

!----------------------------------------------------------------------------
!                                                           EQ@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Check equality of two logical values.
!
! To check equality of logical values:
!
! ```fortran
! abool = bool1 == bool2
! ```
INTERFACE OPERATOR(==)
  MODULE PURE ELEMENTAL FUNCTION equalto_logical(l1, l2) RESULT(Ans)
    LOGICAL(LGT), INTENT(IN) :: l1
    LOGICAL(LGT), INTENT(IN) :: l2
    LOGICAL(LGT) :: Ans
  END FUNCTION equalto_logical
END INTERFACE

!----------------------------------------------------------------------------
!                                                           EQ@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Check inequality of two logical values.
!
! To check inequality of logical values:
!
! ```fortran
! abool = bool1 /= bool2
! ```

INTERFACE OPERATOR(/=)
  MODULE PURE ELEMENTAL FUNCTION notequalto_logical(l1, l2) RESULT(Ans)
    LOGICAL(LGT), INTENT(IN) :: l1
    LOGICAL(LGT), INTENT(IN) :: l2
    LOGICAL(LGT) :: Ans
  END FUNCTION notequalto_logical
END INTERFACE OPERATOR(/=)

!----------------------------------------------------------------------------
!                                                       ASSIGN@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Check inequality of two logical values.
!
! To assign a string to a number.
!
!```fortran
! aint = "1"; aint = "128"
! areal = "1.0"
!```
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP, I4B, LGT
! USE ApproxUtility, ONLY: CharToNumber
! USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
! USE Display_Method, ONLY: Display
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: aint
! REAL(DFP) :: areal
!
! CALL CharToNumber(aint, "12")
! CALL OK(aint .EQ. 12_I4B, "test 1: ")
!
! CALL CharToNumber(areal, "12.23")
! CALL OK(areal.APPROXEQ.12.23_DFP, "test 2: ")
! END PROGRAM main
!```

INTERFACE ASSIGNMENT(=)
  MODULE PURE ELEMENTAL SUBROUTINE assign_char_to_int8(i, c)
    INTEGER(INT8), INTENT(OUT) :: i
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_int8

  MODULE PURE ELEMENTAL SUBROUTINE assign_char_to_Int16(i, c)
    INTEGER(INT16), INTENT(OUT) :: i
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_Int16

  MODULE PURE ELEMENTAL SUBROUTINE assign_char_to_Int32(i, c)
    INTEGER(INT32), INTENT(OUT) :: i
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_Int32

  MODULE PURE ELEMENTAL SUBROUTINE assign_char_to_Int64(i, c)
    INTEGER(INT64), INTENT(OUT) :: i
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_Int64

  MODULE PURE ELEMENTAL SUBROUTINE assign_char_to_Real32(s, c)
    REAL(REAL32), INTENT(OUT) :: s
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_Real32

  MODULE PURE ELEMENTAL SUBROUTINE assign_char_to_Real64(s, c)
    REAL(REAL64), INTENT(OUT) :: s
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_Real64

  MODULE PURE ELEMENTAL SUBROUTINE assign_char_to_bool(b, c)
    LOGICAL(LGT), INTENT(OUT) :: b
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE assign_char_to_bool
END INTERFACE

INTERFACE CharToNumber
  MODULE PROCEDURE assign_char_to_int8, assign_char_to_Int16, &
    assign_char_to_Int32, assign_char_to_Int64, &
    assign_char_to_Real32, assign_char_to_Real64, assign_char_to_bool
END INTERFACE CharToNumber

!----------------------------------------------------------------------------
!                                                    IsNumeric@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Check inequality of two logical values.
!
! To check if a string is number:
!
! ```fortran
! abool = isNumeric("1.2")
! ```
INTERFACE
  MODULE PURE FUNCTION isNumeric(char_str) RESULT(bool)
    CHARACTER(*), INTENT(IN) :: char_str
    LOGICAL(LGT) :: bool
  END FUNCTION isNumeric
END INTERFACE

END MODULE ApproxUtility
