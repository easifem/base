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

PUBLIC :: OPERATOR(.APPROXEQ.)

INTERFACE OPERATOR(.APPROXEQA.)
  MODULE PROCEDURE approxeq_1, approxeq_2
END INTERFACE

PUBLIC :: OPERATOR(.APPROXEQA.)

!----------------------------------------------------------------------------
!                                                     APPROXR@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: returns bool logical indicating if a and b are approximately equal
!
!# Introduction
! This performs a relative comparison by scaling the default epsilon value to
! the size of the larger of the two. It should be used when @c and @b are of
! the same magnitude and very large or very small. If either @c a or @c b is
! zero (exactly) then this routine is equivalent to an absolute comparison.
!
! - TODO add support for Real32

INTERFACE
  MODULE ELEMENTAL FUNCTION approxeqr_1(a, b) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: a, b
    LOGICAL(LGT) :: Ans
  END FUNCTION approxeqr_1
END INTERFACE

INTERFACE OPERATOR(.APPROXEQR.)
  MODULE PROCEDURE approxeqr_1
END INTERFACE

PUBLIC :: OPERATOR(.APPROXEQR.)

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

PUBLIC :: OPERATOR(.APPROXEQF.)

!----------------------------------------------------------------------------
!                                                    APPROXLE@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Apr 2021
! summary: Defines the operator .APPROXLE.
!
!# Introduction
!
! - TODO change the name to approxle_1
! - TODO add support for the real32 and real64

INTERFACE
  MODULE ELEMENTAL FUNCTION approxle_real(r1, r2) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: r1
    REAL(DFP), INTENT(IN) :: r2
    LOGICAL(LGT) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE OPERATOR(.ARROXLE.)
  MODULE PROCEDURE approxle_real
END INTERFACE

PUBLIC :: OPERATOR(.ARROXLE.)

!----------------------------------------------------------------------------
!                                                    APPROXGE@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary:  Defines the operation when comparing two single precision reals
!
! - TODO change the name to approxge_1
! - TODO add support for the real32 and real64

INTERFACE
  MODULE ELEMENTAL FUNCTION approxge_real(r1, r2) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: r1
    REAL(DFP), INTENT(IN) :: r2
    LOGICAL(LGT) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE OPERATOR(.ARROXGE.)
  MODULE PROCEDURE approxge_real
END INTERFACE

PUBLIC :: OPERATOR(.ARROXGE.)

!----------------------------------------------------------------------------
!                                                      SOFTEQ@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Defines the operator SOFTEQ
!
!# Introduction
! - TODO change the name to softeq_1
! - TODO add support for the real32 and real64

INTERFACE
  MODULE ELEMENTAL FUNCTION softeq_real(r1, r2, tol) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: r1
    REAL(DFP), INTENT(IN) :: r2
    REAL(DFP), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION
END INTERFACE
INTERFACE SOFTEQ
  MODULE PROCEDURE softeq_real
END INTERFACE

PUBLIC :: SOFTEQ

!----------------------------------------------------------------------------
!                                                     SOFTEQR@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Defines the operator SOFTEQR
!
! - TODO change the name to softeqr_1
! - TODO add support for the real32 and real64

INTERFACE
  MODULE ELEMENTAL FUNCTION softeqr_real(r1, r2, tol) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: r1
    REAL(DFP), INTENT(IN) :: r2
    REAL(DFP), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE SOFTEQR
  MODULE PROCEDURE softeqr_real
END INTERFACE SOFTEQR

PUBLIC :: SOFTEQR

!----------------------------------------------------------------------------
!                                                       SOFTLE@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: SOFTLE

INTERFACE
  MODULE ELEMENTAL FUNCTION softle_real(r1, r2, tol) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: r1
    REAL(DFP), INTENT(IN) :: r2
    REAL(DFP), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE SOFTLE
  MODULE PROCEDURE softle_real
END INTERFACE SOFTLE

PUBLIC :: SOFTLE

!----------------------------------------------------------------------------
!                                                      SOFTLT@ApproxMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Defines the operation for SOFTLT

INTERFACE
  MODULE ELEMENTAL FUNCTION softlt_real(r1, r2, tol) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: r1
    REAL(DFP), INTENT(IN) :: r2
    REAL(DFP), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE SOFTLT
  MODULE PROCEDURE softlt_real
END INTERFACE SOFTLT

PUBLIC :: SOFTLT

!----------------------------------------------------------------------------
!                                                       SOFTGE@ApproxMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION softge_real(r1, r2, tol) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: r1
    REAL(DFP), INTENT(IN) :: r2
    REAL(DFP), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION softge_real
END INTERFACE

INTERFACE SOFTGE
  MODULE PROCEDURE softge_real
END INTERFACE SOFTGE

PUBLIC :: SOFTGE

!----------------------------------------------------------------------------
!                                                      SOFTGT@ApproxMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION softgt_real(r1, r2, tol) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: r1
    REAL(DFP), INTENT(IN) :: r2
    REAL(DFP), INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE SOFTGT
  MODULE PROCEDURE softgt_real
END INTERFACE SOFTGT

PUBLIC :: SOFTGT

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

PUBLIC :: OPERATOR(==)

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

PUBLIC :: OPERATOR(/=)

!----------------------------------------------------------------------------
!                                                       ASSIGN@ApproxMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL SUBROUTINE assign_char_to_int(i, c)
    INTEGER(I4B), INTENT(OUT) :: i
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ASSIGN@ApproxMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL SUBROUTINE assign_char_to_bool(b, c)
    LOGICAL(LGT), INTENT(OUT) :: b
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ASSIGN@ApproxMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL SUBROUTINE assign_char_to_real(s, c)
    REAL(DFP), INTENT(OUT) :: s
    CHARACTER(*), INTENT(IN) :: c
  END SUBROUTINE
END INTERFACE

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE assign_char_to_int
  MODULE PROCEDURE assign_char_to_bool
  MODULE PROCEDURE assign_char_to_real
END INTERFACE

PUBLIC :: ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                           @ApproxMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION isNumeric(char_str) RESULT(bool)
    CHARACTER(*), INTENT(IN) :: char_str
    LOGICAL(LGT) :: bool
  END FUNCTION
END INTERFACE

PUBLIC :: isNumeric

END MODULE ApproxUtility
