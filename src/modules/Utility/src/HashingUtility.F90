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
! date: 2021-08-22
! summary: Hashing utilities, a unique number for a string.
!
! Description:
!   This module contains hashing utilities.
!
!   - `StringToUID` converts a string into an integer by using the
!     polynomial rolling hash.

MODULE HashingUtility
USE GlobalData, ONLY: I4B, LGT, DFP
IMPLICIT NONE

PRIVATE
PUBLIC :: StringToUID

!----------------------------------------------------------------------------
!                                                StringToUID@HashingMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-22
! summary: Returns an integer (a hash) for a given string.
!
! Description:
!   Returns an integer for the string `charVar` by using the polynomial
!   rolling hash. The same string always gives the same integer, and a
!   different order of the characters gives a different integer in
!   general. Different strings can, however, have the same value (a
!   collision), so the result should not be used as a guaranteed unique
!   identifier.
!
!   - Only `LEN_TRIM(charVar)` characters are used, so the trailing
!     blanks are ignored. The leading blanks are part of the string.
!   - An empty string (or a string of blanks) gives 0.
!   - The weight of a character `c` is `ICHAR(c) - ICHAR('A') + 1`.
!     Therefore `A` to `Z` have the weights 1 to 26, and `a` to `z`
!     have the weights 33 to 58.
!   - The characters with a code smaller than `ICHAR('A')`, such as
!     digits, `*`, `-` and blank, have a zero or negative weight. As
!     `MOD` keeps the sign of its first argument, the result is
!     negative for some strings, e.g., `x**2`.
!   - For a string of letters the result is in the range $[0, m)$.
!
!   The parameters are $p = 53$ and $m = 1000009$.
!
!   Reference: https://cp-algorithms.com/string/string-hashing.html
!
! Arguments:
!   charVar : CHARACTER(*) - the string to be hashed
!
! Returns:
!   ans : INTEGER(I4B) - hash of `charVar`
!
! Formula:
!   Let $n$ = `LEN_TRIM(charVar)` and let $c_i$ be the weight of the
!   $i$-th character. Then
!
!   $$\mathrm{ans} = \left( \sum_{i=1}^{n} c_i\, p^{\,i-1} \right)
!   \bmod m$$
!
!   The sum is evaluated term by term, and the remainder is taken after
!   each addition, so that no integer overflow occurs.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE HashingUtility, ONLY: StringToUID
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! The code of a character c is ICHAR(c) - ICHAR('A') + 1, so that
! ! A = 1, B = 2, ..., Z = 26, a = 33, ..., z = 58.
! CALL OK(StringToUID("A") .EQ. 1_I4B, "A = 1")
! CALL OK(StringToUID("B") .EQ. 2_I4B, "B = 2")
! CALL OK(StringToUID("Z") .EQ. 26_I4B, "Z = 26")
! CALL OK(StringToUID("a") .EQ. 33_I4B, "a = 33")
! CALL OK(StringToUID("z") .EQ. 58_I4B, "z = 58")
!
! ! The base of the polynomial is p = 53
! ! AB = 1 + 2 * 53, BA = 2 + 1 * 53, AAA = 1 + 53 + 53**2
! CALL OK(StringToUID("AB") .EQ. 107_I4B, "AB = 107")
! CALL OK(StringToUID("BA") .EQ. 55_I4B, "BA = 55")
! CALL OK(StringToUID("AAA") .EQ. 2863_I4B, "AAA = 2863")
!
! CALL OK(StringToUID("AB") .NE. StringToUID("BA"), "order matters")
! CALL OK(StringToUID("A") .NE. StringToUID("a"), "case matters")
!
! END PROGRAM main
!```

INTERFACE StringToUID
  MODULE PURE FUNCTION StringToUID_PolyRoll(charVar) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: charVar
    INTEGER(I4B) :: ans
  END FUNCTION StringToUID_PolyRoll
END INTERFACE StringToUID

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HashingUtility
