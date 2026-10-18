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
! date: 2022-08-05
! summary: Miscellaneous helper routines.
!
! Description:
!   This module collects small helper routines that do not belong to
!   another utility module:
!
!   - angle conversion: `Radian`, `Degrees`
!   - nearest point search: `Loc_NearestPoint`, `SearchNearestCoord`
!   - system helpers: `ExecuteCommand`, `GetUnitNo`
!   - integer helpers: `Factorial`, `ARTH`
!   - conversion to string: `Int2Str`, `Real2Str`
!   - array helpers: `OuterDiff`, `IMAXLOC`, `IMINLOC`
!   - complex numbers: `IMG`
!   - safe inverse trigonometric functions: `Safe_ACOS`, `Safe_ASIN`

MODULE MiscUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: SP
USE GlobalData, ONLY: DP
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
IMPLICIT NONE
PRIVATE

PUBLIC :: Radian
PUBLIC :: Degrees
PUBLIC :: SearchNearestCoord
PUBLIC :: ExecuteCommand
PUBLIC :: GetUnitNo
PUBLIC :: Factorial
PUBLIC :: Int2Str
PUBLIC :: Real2Str
PUBLIC :: ARTH
PUBLIC :: outerdiff
PUBLIC :: IMAXLOC
PUBLIC :: IMINLOC
PUBLIC :: IMG
PUBLIC :: LOC_NearestPoint
PUBLIC :: Safe_ACOS
PUBLIC :: Safe_ASIN

!----------------------------------------------------------------------------
!                                                                     Radian
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Converts an angle from degrees to radians.
!
! Description:
!   Returns `deg` in radians. The argument can be `REAL(DFP)` or
!   `INTEGER(I4B)`; the result is always `REAL(DFP)`.
!
!   The value of pi used is `3.1415926535`, so the result differs from
!   the exact value by a relative error of about 3e-11.
!
! Arguments:
!   deg : REAL(DFP) or INTEGER(I4B) - angle in degrees
!
! Returns:
!   Ans : REAL(DFP) - angle in radians
!
! Formula:
!   $$\mathrm{Ans} = \frac{\mathrm{deg}}{180}\,\pi$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MiscUtility, ONLY: Radian
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: piTol = 1.0E-8_DFP
! REAL(DFP) :: pi
!
! pi = ACOS(-1.0_DFP)
!
! ! real argument
! CALL OK(ABS(Radian(0.0_DFP)) .LT. tol, "Radian(0)")
! CALL OK(ABS(Radian(180.0_DFP) - 3.1415926535_DFP) .LT. tol, &
!         "Radian(180) uses pi = 3.1415926535")
! CALL OK(ABS(Radian(180.0_DFP) - pi) .LT. piTol, "Radian(180) ~ pi")
! CALL OK(ABS(Radian(90.0_DFP) - 0.5_DFP * pi) .LT. piTol, &
!         "Radian(90) ~ pi/2")
! CALL OK(ABS(Radian(-45.0_DFP) + 0.25_DFP * pi) .LT. piTol, &
!         "Radian(-45) ~ -pi/4")
!
! ! integer argument gives the same value as the real one
! CALL OK(ABS(Radian(90_I4B) - Radian(90.0_DFP)) .LT. tol, &
!         "Radian(90) integer = real")
! CALL OK(ABS(Radian(360_I4B) - 2.0_DFP * pi) .LT. piTol, &
!         "Radian(360) ~ 2 pi")
!
! END PROGRAM main
!```

INTERFACE Radian
  MODULE PURE FUNCTION radian_dfp(deg) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: deg
    REAL(DFP) :: Ans
  END FUNCTION radian_dfp

  MODULE PURE FUNCTION radian_int(deg) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: deg
    REAL(DFP) :: Ans
  END FUNCTION radian_int
END INTERFACE Radian

!----------------------------------------------------------------------------
!                                                                    Degrees
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Converts an angle from radians to degrees.
!
! Description:
!   Returns `rad` in degrees. It uses the same value of pi as `Radian`
!   (`3.1415926535`), so `Degrees(Radian(x))` gives back `x`.
!
! Arguments:
!   rad : REAL(DFP) - angle in radians
!
! Returns:
!   Ans : REAL(DFP) - angle in degrees
!
! Formula:
!   $$\mathrm{Ans} = \frac{\mathrm{rad}}{\pi}\,180$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MiscUtility, ONLY: Degrees
! USE MiscUtility, ONLY: Radian
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: piTol = 1.0E-6_DFP
! REAL(DFP) :: pi
!
! pi = ACOS(-1.0_DFP)
!
! CALL OK(ABS(Degrees(0.0_DFP)) .LT. tol, "Degrees(0)")
! CALL OK(ABS(Degrees(3.1415926535_DFP) - 180.0_DFP) .LT. tol, &
!         "Degrees(3.1415926535) = 180")
! CALL OK(ABS(Degrees(pi) - 180.0_DFP) .LT. piTol, "Degrees(pi) ~ 180")
! CALL OK(ABS(Degrees(-0.5_DFP * pi) + 90.0_DFP) .LT. piTol, &
!         "Degrees(-pi/2) ~ -90")
!
! ! Degrees is the inverse of Radian
! CALL OK(ABS(Degrees(Radian(37.5_DFP)) - 37.5_DFP) .LT. tol, &
!         "Degrees(Radian(x)) = x")
!
! END PROGRAM main
!```

INTERFACE Degrees
  MODULE PURE FUNCTION degrees_dfp(rad) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: rad
    REAL(DFP) :: Ans
  END FUNCTION degrees_dfp
END INTERFACE Degrees

!----------------------------------------------------------------------------
!                                                           Loc_NearestPoint
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns the column of an array of points nearest to a point.
!
! Description:
!   `Array` contains `N` points in xij format: column `i` is the point
!   `Array(:, i)`. The function returns the index `id` of the column
!   with the smallest Euclidean distance to `x`.
!
!   - If several columns have the same distance, the first is returned.
!   - `SIZE(Array, 1)` must equal `SIZE(x)`, otherwise an error is
!     printed and the program stops.
!   - At most three coordinates per point are supported.
!   - The distances are stored as `INTEGER(I4B)`, so they are truncated
!     before they are compared. Two points whose distances differ by
!     less than one may not be told apart; the first one is returned.
!
!   `SearchNearestCoord` is another name for the same function.
!
! Arguments:
!   Array : REAL(DFP), rank(2) - points in xij format,
!     `SIZE(Array, 1) = SIZE(x) <= 3`
!   x : REAL(DFP), rank(1) - the point
!
! Returns:
!   id : INTEGER(I4B) - index of the nearest column, `1 <= id <= N`
!
! Formula:
!   $$\mathrm{id} = \arg\min_{i} \left\| \mathrm{Array}(:, i) - x
!     \right\|_2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MiscUtility, ONLY: Loc_NearestPoint
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: xij(2, 4), x3(3, 3)
!
! ! corners of a square, in xij format (one point per column)
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [10.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 10.0_DFP]
! xij(:, 4) = [10.0_DFP, 10.0_DFP]
!
! CALL OK(Loc_NearestPoint(xij, [9.0_DFP, 8.0_DFP]) .EQ. 4_I4B, &
!         "nearest to (9,8) is column 4")
! CALL OK(Loc_NearestPoint(xij, [1.0_DFP, 2.0_DFP]) .EQ. 1_I4B, &
!         "nearest to (1,2) is column 1")
! CALL OK(Loc_NearestPoint(xij, [10.0_DFP, 0.0_DFP]) .EQ. 2_I4B, &
!         "a point of the array returns its own column")
!
! ! a tie returns the first column
! CALL OK(Loc_NearestPoint(xij, [5.0_DFP, 0.0_DFP]) .EQ. 1_I4B, &
!         "tie returns the first column")
!
! ! three dimensional points
! x3(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
! x3(:, 2) = [0.0_DFP, 0.0_DFP, 20.0_DFP]
! x3(:, 3) = [20.0_DFP, 0.0_DFP, 0.0_DFP]
! CALL OK(Loc_NearestPoint(x3, [1.0_DFP, 2.0_DFP, 18.0_DFP]) .EQ. 2_I4B, &
!         "3D: nearest is column 2")
!
! END PROGRAM main
!```

INTERFACE Loc_NearestPoint
  MODULE FUNCTION Loc_Nearest_Point(Array, x) RESULT(id)
    REAL(DFP), INTENT(IN) :: Array(:, :)
  !! Nodal coordinates in XiJ format
    REAL(DFP), INTENT(IN) :: x(:)
    INTEGER(I4B) :: id
  END FUNCTION Loc_Nearest_Point
END INTERFACE Loc_NearestPoint

!----------------------------------------------------------------------------
!                                                         SearchNearestCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Another name for Loc_NearestPoint.
!
! Description:
!   `SearchNearestCoord(Array, x)` calls the same function as
!   `Loc_NearestPoint(Array, x)` and returns the index of the column of
!   `Array` nearest to `x`. See `Loc_NearestPoint` for the details.
!
! Arguments:
!   Array : REAL(DFP), rank(2) - points in xij format,
!     `SIZE(Array, 1) = SIZE(x) <= 3`
!   x : REAL(DFP), rank(1) - the point
!
! Returns:
!   id : INTEGER(I4B) - index of the nearest column
!
! Formula:
!   $$\mathrm{id} = \arg\min_{i} \left\| \mathrm{Array}(:, i) - x
!     \right\|_2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MiscUtility, ONLY: SearchNearestCoord
! USE MiscUtility, ONLY: Loc_NearestPoint
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: xij(2, 20), x(2)
! INTEGER(I4B) :: ii, id
!
! ! 20 points close to the origin
! DO ii = 1, 20
!   xij(1, ii) = 0.05_DFP * REAL(ii, DFP)
!   xij(2, ii) = 0.03_DFP * REAL(ii, DFP)
! END DO
!
! ! put the point x in column 15
! x = [11.0_DFP, 100.0_DFP]
! xij(:, 15) = x
!
! id = SearchNearestCoord(Array=xij, x=x)
! CALL OK(id .EQ. 15_I4B, "x is found in column 15")
! CALL OK(id .EQ. Loc_NearestPoint(xij, x), &
!         "same result as Loc_NearestPoint")
!
! END PROGRAM main
!```

INTERFACE SearchNearestCoord
  MODULE PROCEDURE Loc_Nearest_Point
END INTERFACE SearchNearestCoord

!----------------------------------------------------------------------------
!                                                             ExecuteCommand
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Runs a system command and waits for it to finish.
!
! Description:
!   Runs `TRIM(CMD)` with the intrinsic `EXECUTE_COMMAND_LINE` and waits
!   for it to finish.
!
!   - If the command cannot be run (`CMDSTAT /= 0`), an error message is
!     printed and the program stops.
!   - The exit status of the command is not checked, so a command that
!     runs but fails (for example `false`) does not stop the program.
!   - `Str` is not used.
!
! Arguments:
!   CMD : CHARACTER(*) - the command to run
!   Str : CHARACTER(*) - not used
!
! Returns:
!   None. The command is run as a side effect.
!
! Formula:
!   None.
!
! Examples
!
!```fortran
! PROGRAM main
! USE MiscUtility, ONLY: ExecuteCommand
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! a command that succeeds
! CALL ExecuteCommand("true", "")
! CALL OK(.TRUE., "ExecuteCommand('true') returns")
!
! ! a command that writes to the terminal
! CALL ExecuteCommand("echo ExecuteCommand works", "")
! CALL OK(.TRUE., "ExecuteCommand('echo ...') returns")
!
! ! the exit status of the command is not checked
! CALL ExecuteCommand("false", "")
! CALL OK(.TRUE., "ExecuteCommand('false') also returns")
!
! END PROGRAM main
!```

INTERFACE ExecuteCommand
  MODULE SUBROUTINE exe_cmd(CMD, Str)
    CHARACTER(LEN=*), INTENT(IN) :: CMD, Str
  END SUBROUTINE exe_cmd
END INTERFACE ExecuteCommand

!----------------------------------------------------------------------------
!                                                                  GetUnitNo
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns a unit number that is not connected to a file.
!
! Description:
!   Searches the unit numbers 10 to 1000 and returns the first one that
!   exists and is not connected to a file. If no such unit is found, an
!   error message is printed and the program stops.
!
!   The unit is only found, not opened. Open it before calling
!   `GetUnitNo` again, or the same number is returned.
!
! Arguments:
!   None.
!
! Returns:
!   ans : INTEGER(I4B) - a free unit number, `10 <= ans <= 1000`
!
! Formula:
!   $$\mathrm{ans} = \min \{\, u \in [10, 1000] : u \text{ not open}
!     \,\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE MiscUtility, ONLY: GetUnitNo
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: u, v
! LOGICAL(LGT) :: isOpen
!
! u = GetUnitNo()
! CALL OK(u .GE. 10_I4B .AND. u .LE. 1000_I4B, "unit is in [10, 1000]")
!
! INQUIRE (UNIT=u, OPENED=isOpen)
! CALL OK(.NOT. isOpen, "unit is not connected")
!
! ! once u is open, the next call returns another unit
! OPEN (UNIT=u, STATUS="SCRATCH", ACTION="READWRITE")
! v = GetUnitNo()
! CALL OK(v .NE. u, "an open unit is not returned")
! INQUIRE (UNIT=v, OPENED=isOpen)
! CALL OK(.NOT. isOpen, "second unit is not connected")
! CLOSE (u)
!
! END PROGRAM main
!```

INTERFACE GetUnitNo
  MODULE FUNCTION getUnitNo_1() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION getUnitNo_1
END INTERFACE GetUnitNo

!----------------------------------------------------------------------------
!                                                                  Factorial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Returns the factorial of an integer.
!
! Description:
!   Returns `N!`, computed by recursion, with `0! = 1`.
!
!   - `N` must not be negative. A negative `N` never reaches the base
!     case `N = 0`, so the recursion does not end.
!   - The result is `INTEGER(I4B)`, so `N` must be at most 12; `13!`
!     overflows.
!
! Arguments:
!   N : INTEGER(I4B) - non-negative integer, `0 <= N <= 12`
!
! Returns:
!   Ans : INTEGER(I4B) - `N!`
!
! Formula:
!   $$\mathrm{Ans} = N! = \prod_{k=1}^{N} k, \qquad 0! = 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE MiscUtility, ONLY: Factorial
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: n
!
! CALL OK(Factorial(0_I4B) .EQ. 1_I4B, "0! = 1")
! CALL OK(Factorial(1_I4B) .EQ. 1_I4B, "1! = 1")
! CALL OK(Factorial(5_I4B) .EQ. 120_I4B, "5! = 120")
! CALL OK(Factorial(10_I4B) .EQ. 3628800_I4B, "10! = 3628800")
!
! ! 12! is the largest factorial that fits in INTEGER(I4B)
! CALL OK(Factorial(12_I4B) .EQ. 479001600_I4B, "12! = 479001600")
!
! ! n! = n (n-1)!
! n = 7_I4B
! CALL OK(Factorial(n) .EQ. n * Factorial(n - 1_I4B), "n! = n (n-1)!")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE RECURSIVE FUNCTION Factorial(N) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: N
    INTEGER(I4B) :: Ans
  END FUNCTION Factorial
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    Int2Str
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-04-03
! summary: Converts an integer to a string.
!
! Description:
!   Writes `I` with the format `(I15)` and left adjusts the digits. The
!   result has length 15 and is padded with trailing blanks, so use
!   `TRIM(Int2Str(I))` when the blanks are not wanted.
!
! Arguments:
!   I : INTEGER(I4B) - the integer
!
! Returns:
!   Int2Str : CHARACTER(LEN=15) - the digits of `I`, left adjusted
!
! Formula:
!   None.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE MiscUtility, ONLY: Int2Str
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(LEN=15) :: s
!
! CALL OK(TRIM(Int2Str(42_I4B)) .EQ. "42", "Int2Str(42)")
! CALL OK(TRIM(Int2Str(0_I4B)) .EQ. "0", "Int2Str(0)")
! CALL OK(TRIM(Int2Str(-7_I4B)) .EQ. "-7", "Int2Str(-7)")
! CALL OK(TRIM(Int2Str(HUGE(0_I4B))) .EQ. "2147483647", &
!         "Int2Str(HUGE)")
!
! ! the digits are left adjusted, the result has length 15
! s = Int2Str(42_I4B)
! CALL OK(s(1:2) .EQ. "42", "left adjusted")
! CALL OK(LEN(Int2Str(42_I4B)) .EQ. 15_I4B, "length is 15")
!
! ! typical use
! CALL OK("step-"//TRIM(Int2Str(3_I4B)) .EQ. "step-3", "concatenation")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Int2Str(I)
    INTEGER(I4B), INTENT(IN) :: I
    CHARACTER(LEN=15) :: Int2Str
  END FUNCTION Int2Str
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Real2Str
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Converts a real number to a string.
!
! Description:
!   Writes `I` with the format `(G17.7)` and left adjusts the result.
!   It is available for `REAL(SP)` and `REAL(DP)`.
!
!   - The value is written with 7 significant digits, e.g. `1.5` gives
!     `"1.500000"`.
!   - Very large or very small values are written in exponent form,
!     e.g. `1.0E10` gives `"0.1000000E+11"`.
!   - The result has length 20 and is padded with trailing blanks.
!
! Arguments:
!   I : REAL(SP) or REAL(DP) - the real number
!
! Returns:
!   ans : CHARACTER(LEN=20) - `I` written as text, left adjusted
!
! Formula:
!   None.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MiscUtility, ONLY: Real2Str
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: rtol = 1.0E-6_DFP
! CHARACTER(LEN=20) :: s
! REAL(DFP) :: r
!
! ! format is G17.7, the result is left adjusted
! s = Real2Str(1.5_DFP)
! CALL OK(TRIM(s) .EQ. "1.500000", "Real2Str(1.5)")
! CALL OK(LEN(Real2Str(1.5_DFP)) .EQ. 20_I4B, "length is 20")
! CALL OK(s(1:1) .NE. " ", "left adjusted")
!
! ! reading the string back gives the value to 7 significant digits
! s = Real2Str(1.0_DFP / 3.0_DFP)
! READ (s, *) r
! CALL OK(ABS(r - 1.0_DFP / 3.0_DFP) .LT. rtol, "1/3 read back")
!
! s = Real2Str(-2.25_DFP)
! READ (s, *) r
! CALL OK(ABS(r + 2.25_DFP) .LT. rtol, "-2.25 read back")
!
! ! large values use the exponent form
! s = Real2Str(1.0E10_DFP)
! READ (s, *) r
! CALL OK(ABS(r - 1.0E10_DFP) .LT. rtol * 1.0E10_DFP, "1e10 read back")
! CALL OK(INDEX(s, "E") .GT. 0, "1e10 has an exponent")
!
! END PROGRAM main
!```

INTERFACE Real2Str
  MODULE FUNCTION SP2Str(I)
    REAL(SP), INTENT(IN) :: I
    CHARACTER(LEN=20) :: SP2Str
  END FUNCTION SP2Str

  MODULE FUNCTION DP2Str(I)
    REAL(DP), INTENT(IN) :: I
    CHARACTER(LEN=20) :: DP2Str
  END FUNCTION DP2Str
END INTERFACE Real2Str

!----------------------------------------------------------------------------
!                                                                       ARTH
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns an arithmetic progression.
!
! Description:
!   Returns the `n` terms `first, first + increment, ...`. It is
!   available for `REAL(SP)`, `REAL(DP)` and `INTEGER(I4B)`. For
!   `n <= 0` the result is empty.
!
!   For `n > 16` the terms are computed by doubling blocks, so for
!   reals the last digits can differ slightly from
!   `first + (k-1) * increment`.
!
! Arguments:
!   first : REAL(SP), REAL(DP) or INTEGER(I4B) - first term
!   increment : same type as `first` - difference between two terms
!   n : INTEGER(I4B) - number of terms
!
! Returns:
!   ans : rank(1), same type as `first` - the progression, `SIZE = n`
!
! Formula:
!   $$\mathrm{ans}_k = \mathrm{first} + (k - 1)\,\mathrm{increment},
!     \qquad k = 1, \ldots, n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE MiscUtility, ONLY: ARTH
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: ii
! INTEGER(I4B) :: expected(40)
!
! CALL OK(ALL(ARTH(1_I4B, 2_I4B, 5_I4B) .EQ. [1, 3, 5, 7, 9]), &
!         "ARTH(1, 2, 5)")
! CALL OK(ALL(ARTH(10_I4B, -3_I4B, 4_I4B) .EQ. [10, 7, 4, 1]), &
!         "negative increment")
! CALL OK(ALL(ARTH(4_I4B, 0_I4B, 3_I4B) .EQ. [4, 4, 4]), "zero increment")
! CALL OK(SIZE(ARTH(1_I4B, 1_I4B, 0_I4B)) .EQ. 0_I4B, "n = 0 is empty")
!
! ! more than 16 terms
! expected = [(3_I4B + (ii - 1_I4B) * 5_I4B, ii=1, 40)]
! CALL OK(ALL(ARTH(3_I4B, 5_I4B, 40_I4B) .EQ. expected), "n = 40")
!
! END PROGRAM main
!```

INTERFACE ARTH
  MODULE PURE FUNCTION arth_r(first, increment, n)
    REAL(SP), INTENT(IN) :: first, increment
    INTEGER(I4B), INTENT(IN) :: n
    REAL(SP) :: arth_r(n)
  END FUNCTION arth_r

  MODULE PURE FUNCTION arth_d(first, increment, n)
    REAL(DP), INTENT(IN) :: first, increment
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DP) :: arth_d(n)
  END FUNCTION arth_d

  MODULE PURE FUNCTION arth_i(first, increment, n)
    INTEGER(I4B), INTENT(IN) :: first, increment, n
    INTEGER(I4B) :: arth_i(n)
  END FUNCTION arth_i
END INTERFACE ARTH

!----------------------------------------------------------------------------
!                                                                  OuterDiff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns the outer difference of two vectors.
!
! Description:
!   Returns the matrix of all differences `a(i) - b(j)`. It is available
!   for `REAL(SP)`, `REAL(DP)` and `INTEGER(I4B)` vectors; `a` and `b`
!   must have the same type.
!
! Arguments:
!   a : rank(1) - first vector
!   b : rank(1), same type as `a` - second vector
!
! Returns:
!   ans : rank(2), same type as `a` - `SHAPE(ans) = [SIZE(a), SIZE(b)]`
!
! Formula:
!   $$\mathrm{ans}_{ij} = a_i - b_j$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MiscUtility, ONLY: OuterDiff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B) :: ia(3), ib(2), ians(3, 2)
! REAL(DFP) :: ra(2), rb(3), rans(2, 3)
!
! ! integer vectors
! ia = [1, 2, 3]
! ib = [10, 20]
! ians = OuterDiff(ia, ib)
! CALL OK(ALL(SHAPE(ians) .EQ. [3, 2]), "shape is (SIZE(a), SIZE(b))")
! CALL OK(ALL(ians(:, 1) .EQ. [-9, -8, -7]), "integer column 1")
! CALL OK(ALL(ians(:, 2) .EQ. [-19, -18, -17]), "integer column 2")
!
! ! real vectors
! ra = [1.5_DFP, -0.5_DFP]
! rb = [0.5_DFP, 1.0_DFP, 2.0_DFP]
! rans = OuterDiff(ra, rb)
! CALL OK(ABS(rans(1, 1) - 1.0_DFP) .LT. tol, "real (1,1)")
! CALL OK(ABS(rans(2, 3) + 2.5_DFP) .LT. tol, "real (2,3)")
! CALL OK(ALL(ABS(rans(1, :) - (ra(1) - rb)) .LT. tol), "real row 1")
!
! END PROGRAM main
!```

INTERFACE OuterDiff
  MODULE PURE FUNCTION outerdiff_r(a, b)
    REAL(SP), DIMENSION(:), INTENT(IN) :: a, b
    REAL(SP), DIMENSION(SIZE(a), SIZE(b)) :: outerdiff_r
  END FUNCTION outerdiff_r

  MODULE PURE FUNCTION outerdiff_d(a, b)
    REAL(DP), DIMENSION(:), INTENT(IN) :: a, b
    REAL(DP), DIMENSION(SIZE(a), SIZE(b)) :: outerdiff_d
  END FUNCTION outerdiff_d

  MODULE PURE FUNCTION outerdiff_i(a, b)
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: a, b
    INTEGER(I4B), DIMENSION(SIZE(a), SIZE(b)) :: outerdiff_i
  END FUNCTION outerdiff_i
END INTERFACE OuterDiff

!----------------------------------------------------------------------------
!                                                                    IMAXLOC
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns the index of the largest entry of a vector.
!
! Description:
!   Returns `MAXLOC(arr)` as a scalar. It is available for `REAL(DFP)`
!   and `INTEGER(I4B)` vectors.
!
!   - If the largest value occurs more than once, the first index is
!     returned.
!   - For an empty vector the result is 0.
!
! Arguments:
!   arr : REAL(DFP) or INTEGER(I4B), rank(1) - the vector
!
! Returns:
!   ans : INTEGER(I4B) - index of the largest entry
!
! Formula:
!   $$\mathrm{ans} = \min \{\, i : \mathrm{arr}_i = \max_j
!     \mathrm{arr}_j \,\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MiscUtility, ONLY: IMAXLOC
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: empty(0)
!
! ! integer vector
! CALL OK(IMAXLOC([3_I4B, 9_I4B, 1_I4B]) .EQ. 2_I4B, "integer")
! CALL OK(IMAXLOC([3_I4B, 7_I4B, 7_I4B, 1_I4B]) .EQ. 2_I4B, &
!         "first of equal maxima")
! CALL OK(IMAXLOC([5_I4B]) .EQ. 1_I4B, "size 1")
! CALL OK(IMAXLOC(empty) .EQ. 0_I4B, "empty vector gives 0")
!
! ! real vector
! CALL OK(IMAXLOC([-1.0_DFP, -0.5_DFP, -2.0_DFP]) .EQ. 2_I4B, "real")
!
! END PROGRAM main
!```

INTERFACE IMAXLOC
  MODULE PURE FUNCTION imaxloc_r(arr)
    REAL(DFP), INTENT(IN) :: arr(:)
    INTEGER(I4B) :: imaxloc_r
  END FUNCTION imaxloc_r

  MODULE PURE FUNCTION imaxloc_i(iarr)
    INTEGER(I4B), INTENT(IN) :: iarr(:)
    INTEGER(I4B) :: imaxloc_i
  END FUNCTION imaxloc_i
END INTERFACE IMAXLOC

!----------------------------------------------------------------------------
!                                                                    IMINLOC
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: Returns the index of the smallest entry of a real vector.
!
! Description:
!   Returns `MINLOC(arr)` as a scalar. It is available for `REAL(DFP)`
!   vectors only.
!
!   - If the smallest value occurs more than once, the first index is
!     returned.
!   - For an empty vector the result is 0.
!
! Arguments:
!   arr : REAL(DFP), rank(1) - the vector
!
! Returns:
!   ans : INTEGER(I4B) - index of the smallest entry
!
! Formula:
!   $$\mathrm{ans} = \min \{\, i : \mathrm{arr}_i = \min_j
!     \mathrm{arr}_j \,\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MiscUtility, ONLY: IMINLOC
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CALL OK(IMINLOC([3.0_DFP, -1.0_DFP, 5.0_DFP]) .EQ. 2_I4B, "real")
! CALL OK(IMINLOC([3.0_DFP, -1.0_DFP, 5.0_DFP, -1.0_DFP]) .EQ. 2_I4B, &
!         "first of equal minima")
! CALL OK(IMINLOC([0.25_DFP]) .EQ. 1_I4B, "size 1")
!
! END PROGRAM main
!```

INTERFACE IMINLOC
  MODULE FUNCTION iminloc_r(arr)
    REAL(DFP), INTENT(IN) :: arr(:)
    INTEGER(I4B) :: iminloc_r
  END FUNCTION iminloc_r
END INTERFACE IMINLOC

!----------------------------------------------------------------------------
!                                                                        IMG
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-03-05
! summary: Returns the imaginary part of a complex number.
!
! Description:
!   `IMG` returns the imaginary part of `x`, the same as the intrinsic
!   `AIMAG(x)`. It is elemental and available for `COMPLEX(REAL32)`
!   and `COMPLEX(REAL64)`.
!
! Arguments:
!   x : COMPLEX(REAL32) or COMPLEX(REAL64) - complex number
!
! Returns:
!   ans : REAL, same kind as `x` - imaginary part of `x`
!
! Formula:
!   $$x = a + b\,i \quad\Rightarrow\quad \mathrm{ans} = b$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: DFPC
! USE MiscUtility, ONLY: IMG
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! COMPLEX(DFPC) :: z(3)
!
! CALL OK(ABS(IMG((1.0_DFP, 2.0_DFP)) - 2.0_DFP) .LT. tol, &
!         "IMG((1,2)) = 2")
! CALL OK(ABS(IMG((0.0_DFP, -4.0_DFP)) + 4.0_DFP) .LT. tol, &
!         "IMG((0,-4)) = -4")
! CALL OK(ABS(IMG((3.0_DFP, 0.0_DFP))) .LT. tol, "real number gives 0")
!
! ! IMG is elemental and equals the intrinsic AIMAG
! z = [(1.0_DFP, 1.0_DFP), (0.0_DFP, -4.0_DFP), (2.0_DFP, 0.5_DFP)]
! CALL OK(ALL(ABS(IMG(z) - AIMAG(z)) .LT. tol), "IMG(z) = AIMAG(z)")
!
! END PROGRAM main
!```

INTERFACE IMG
  MODULE ELEMENTAL FUNCTION IMG_1(x) RESULT(ans)
    COMPLEX(REAL32), INTENT(IN) :: x
    REAL(REAL32) :: ans
  END FUNCTION IMG_1

  MODULE ELEMENTAL FUNCTION IMG_2(x) RESULT(ans)
    COMPLEX(REAL64), INTENT(IN) :: x
    REAL(REAL64) :: ans
  END FUNCTION IMG_2
END INTERFACE IMG

!----------------------------------------------------------------------------
!                                                                  Safe_ACOS
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: ACOS that clamps its argument to [-1, 1].
!
! Description:
!   Returns `ACOS(c)` after clamping `c` to the interval `[-1, 1]`. This
!   avoids a NaN when round-off makes a cosine slightly larger than 1 or
!   smaller than -1. It is elemental.
!
! Arguments:
!   c : REAL(DFP) - the cosine
!
! Returns:
!   ans : REAL(DFP) - angle in `[0, pi]`
!
! Formula:
!   $$\mathrm{ans} = \arccos\left(\min(\max(c, -1), 1)\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MiscUtility, ONLY: Safe_ACOS
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: pi, c(3)
!
! pi = ACOS(-1.0_DFP)
!
! CALL OK(ABS(Safe_ACOS(0.5_DFP) - ACOS(0.5_DFP)) .LT. tol, &
!         "inside [-1,1] same as ACOS")
!
! ! round-off just outside [-1, 1] is clamped
! CALL OK(ABS(Safe_ACOS(1.0_DFP + 1.0E-12_DFP)) .LT. tol, &
!         "slightly above 1 gives 0")
! CALL OK(ABS(Safe_ACOS(-1.0_DFP - 1.0E-12_DFP) - pi) .LT. tol, &
!         "slightly below -1 gives pi")
! CALL OK(ABS(Safe_ACOS(5.0_DFP)) .LT. tol, "any c > 1 gives 0")
!
! ! Safe_ACOS is elemental
! c = [-2.0_DFP, 0.0_DFP, 2.0_DFP]
! CALL OK(ALL(ABS(Safe_ACOS(c) - [pi, 0.5_DFP * pi, 0.0_DFP]) .LT. tol), &
!         "elemental")
!
! END PROGRAM main
!```

INTERFACE
  MODULE ELEMENTAL FUNCTION safe_ACOS(c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: c
    REAL(DFP) :: ans
  END FUNCTION safe_ACOS
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Safe_ASIN
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-05
! summary: ASIN that clamps its argument to [-1, 1].
!
! Description:
!   Returns `ASIN(s)` after clamping `s` to the interval `[-1, 1]`. This
!   avoids a NaN when round-off makes a sine slightly larger than 1 or
!   smaller than -1. It is elemental.
!
! Arguments:
!   s : REAL(DFP) - the sine
!
! Returns:
!   ans : REAL(DFP) - angle in `[-pi/2, pi/2]`
!
! Formula:
!   $$\mathrm{ans} = \arcsin\left(\min(\max(s, -1), 1)\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MiscUtility, ONLY: Safe_ASIN
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: pi, s(3)
!
! pi = ACOS(-1.0_DFP)
!
! CALL OK(ABS(Safe_ASIN(0.5_DFP) - ASIN(0.5_DFP)) .LT. tol, &
!         "inside [-1,1] same as ASIN")
!
! ! round-off just outside [-1, 1] is clamped
! CALL OK(ABS(Safe_ASIN(1.0_DFP + 1.0E-12_DFP) - 0.5_DFP * pi) .LT. tol, &
!         "slightly above 1 gives pi/2")
! CALL OK(ABS(Safe_ASIN(-1.0_DFP - 1.0E-12_DFP) + 0.5_DFP * pi) .LT. tol, &
!         "slightly below -1 gives -pi/2")
!
! ! Safe_ASIN is elemental
! s = [-3.0_DFP, 0.0_DFP, 3.0_DFP]
! CALL OK(ALL(ABS(Safe_ASIN(s) - [-0.5_DFP * pi, 0.0_DFP, 0.5_DFP * pi]) &
!             .LT. tol), "elemental")
!
! END PROGRAM main
!```

INTERFACE
  MODULE ELEMENTAL FUNCTION safe_ASIN(s) RESULT(ans)
    REAL(DFP), INTENT(IN) :: s
    REAL(DFP) :: ans
  END FUNCTION safe_ASIN
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MiscUtility
