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
! date: 2022-01-01
! summary: This module provides a method called `Binom` for calculating the
! binomial coefficients
!
! This module provides a method called `Binom` for calculating the
! binomial coefficients
!
! $$
! C_{k}^{n}
! $$
!
MODULE BinomUtility
USE GlobalData, ONLY: DFP, LGT, I4B
USE GlobalData, ONLY: INT8, INT16, INT32, INT64
USE GlobalData, ONLY: REAL32, REAL64
IMPLICIT NONE
PRIVATE
PUBLIC :: Binom

!----------------------------------------------------------------------------
!                                                                Binom
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-01
! summary: Compute the Binomial coefficient
!
! This routine calculates the bionomial coefficient $C_{k}^{n}$
!
! Usages is given below
!
!```fortran
! ans = Binom( n=10, k=2 )
!```
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE BinomUtility, ONLY: Binom
! USE Display_Method, ONLY: Display
! IMPLICIT NONE
! ! Here we are calculating ${}^{4}C_{m}$, for $m=0,1,2,3,4$
! CALL Display(Binom(4, 0, kind=1.0_DFP), "(4, 0) = ")
! CALL Display(Binom(4, 1, kind=1.0_DFP), "(4, 1) = ")
! CALL Display(Binom(4, 2, kind=1.0_DFP), "(4, 2) = ")
! CALL Display(Binom(4, 3, kind=1.0_DFP), "(4, 3) = ")
! CALL Display(Binom(4, 4, kind=1.0_DFP), "(4, 4) = ")
! ! ```txt title="Results"
! ! (4, 0) = 1.00000
! ! (4, 1) = 4.00000
! ! (4, 2) = 6.00000
! ! (4, 3) = 4.00000
! ! (4, 4) = 1.00000
! ! ```
! END PROGRAM main
!```

INTERFACE Binom
  MODULE RECURSIVE PURE FUNCTION Real32_Binom_Int8(n, k, kind) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: n
    !! n is integer, should be a positive number and greater or equal to k
    INTEGER(INT8), INTENT(IN) :: k
    REAL(REAL32), INTENT(IN) :: kind
    REAL(REAL32) :: ans
  END FUNCTION Real32_Binom_Int8

  MODULE RECURSIVE PURE FUNCTION Real32_Binom_Int16(n, k, kind) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: n
    INTEGER(INT16), INTENT(IN) :: k
    REAL(REAL32), INTENT(IN) :: kind
    REAL(REAL32) :: ans
  END FUNCTION Real32_Binom_Int16

  MODULE RECURSIVE PURE FUNCTION Real32_Binom_Int32(n, k, kind) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: n
    INTEGER(INT32), INTENT(IN) :: k
    REAL(REAL32), INTENT(IN) :: kind
    REAL(REAL32) :: ans
  END FUNCTION Real32_Binom_Int32

  MODULE RECURSIVE PURE FUNCTION Real32_Binom_Int64(n, k, kind) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: n
    INTEGER(INT64), INTENT(IN) :: k
    REAL(REAL32), INTENT(IN) :: kind
    REAL(REAL32) :: ans
  END FUNCTION Real32_Binom_Int64

  MODULE RECURSIVE PURE FUNCTION Real64_Binom_Int8(n, k, kind) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: n
    INTEGER(INT8), INTENT(IN) :: k
    REAL(REAL64), INTENT(IN) :: kind
    REAL(REAL64) :: ans
  END FUNCTION Real64_Binom_Int8

  MODULE RECURSIVE PURE FUNCTION Real64_Binom_Int16(n, k, kind) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: n
    INTEGER(INT16), INTENT(IN) :: k
    REAL(REAL64), INTENT(IN) :: kind
    REAL(REAL64) :: ans
  END FUNCTION Real64_Binom_Int16

  MODULE RECURSIVE PURE FUNCTION Real64_Binom_Int32(n, k, kind) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: n
    INTEGER(INT32), INTENT(IN) :: k
    REAL(REAL64), INTENT(IN) :: kind
    REAL(REAL64) :: ans
  END FUNCTION Real64_Binom_Int32

  MODULE RECURSIVE PURE FUNCTION Real64_Binom_Int64(n, k, kind) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: n
    INTEGER(INT64), INTENT(IN) :: k
    REAL(REAL64), INTENT(IN) :: kind
    REAL(REAL64) :: ans
  END FUNCTION Real64_Binom_Int64
END INTERFACE Binom

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BinomUtility
