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
! date: 2026-02-26
! summary: Random numbers, random selection and seeded generators.
!
! Description:
!   This module contains two groups of random number routines.
!
!   - `RandomValue` uses the intrinsic `RANDOM_NUMBER`. It is not
!     `PURE`, and the result changes from call to call. Use the
!     intrinsic `RANDOM_SEED` to control the sequence. It returns a
!     uniform or an approximately normal real number, a real or an
!     integer between two bounds, or a randomly selected entry of a
!     vector or a matrix.
!   - `UniformRandomValue`, `UniformRandomValueScalar` and
!     `NormalRandomValue` are `PURE` and use the Park-Miller
!     "minimal standard" generator with an integer `seed`. The seed
!     is `INTENT(IN)` and is not updated, so the same seed always
!     gives the same result. The default seed is 1. The seed should
!     be a positive integer; `seed = 0` gives zeros.

MODULE RandomUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
IMPLICIT NONE

PRIVATE
PUBLIC :: RandomValue
PUBLIC :: UniformRandomValue
PUBLIC :: UniformRandomValueScalar
PUBLIC :: NormalRandomValue

!----------------------------------------------------------------------------
!                                                                 getRandom
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Returns a random real number from a distribution.
!
! Description:
!   Returns a random real number. It calls the intrinsic
!   `RANDOM_NUMBER`, so the result changes from call to call.
!
!   - If `distribution` is absent, the result is uniform in `[0, 1)`.
!   - If `distribution` is `"Binomial"` or `"binomial"`, the result is
!     the sum of 20 uniform numbers minus 10. It lies in `[-10, 10)`,
!     has mean 0 and variance `20/12`, and is close to a normal
!     distribution.
!   - Any other value of `distribution` gives the default, a uniform
!     number in `[0, 1)`. The name is case sensitive, so
!     `"BINOMIAL"` also gives the default.
!
! Arguments:
!   distribution : CHARACTER(*), OPTIONAL - name of the distribution,
!     `"Binomial"` or `"binomial"`
!
! Returns:
!   ans : REAL(DFP) - random number
!
! Formula:
!   $$\mathrm{ans} = u, \quad u \in [0, 1)$$
!   $$\mathrm{ans} = \sum_{i=1}^{20} u_i - 10
!     \quad \text{(Binomial)}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE RandomUtility, ONLY: RandomValue
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 1000
! INTEGER(I4B) :: i
! REAL(DFP) :: x(n)
! LOGICAL(LGT) :: isok
!
! ! default: uniform value in [0, 1)
! DO i = 1, n
!   x(i) = RandomValue()
! END DO
! isok = ALL(x .GE. 0.0_DFP) .AND. ALL(x .LT. 1.0_DFP)
! CALL OK(isok, "default is in [0, 1)")
!
! ! an unknown distribution falls back to the default
! DO i = 1, n
!   x(i) = RandomValue(distribution="Uniform")
! END DO
! isok = ALL(x .GE. 0.0_DFP) .AND. ALL(x .LT. 1.0_DFP)
! CALL OK(isok, "unknown distribution is in [0, 1)")
!
! ! Binomial: sum of 20 uniform values minus 10
! DO i = 1, n
!   x(i) = RandomValue(distribution="Binomial")
! END DO
! isok = ALL(x .GE. -10.0_DFP) .AND. ALL(x .LT. 10.0_DFP)
! CALL OK(isok, "Binomial is in [-10, 10)")
! CALL OK(ABS(SUM(x) / n) .LT. 0.3_DFP, "Binomial mean is close to 0")
!
! ! the lower case name is also accepted
! DO i = 1, n
!   x(i) = RandomValue(distribution="binomial")
! END DO
! CALL OK(ANY(x .LT. 0.0_DFP), "binomial gives negative values")
!
! END PROGRAM main
!```

INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue1(distribution) RESULT(ans)
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: distribution
    REAL(DFP) :: ans
  END FUNCTION obj_RandomValue1
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                             UniformRandom
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Returns a uniform random real number between from and to.
!
! Description:
!   Returns a uniform random real number between `from` and `to`. It
!   calls the intrinsic `RANDOM_NUMBER`, so the result changes from
!   call to call.
!
!   - The order of `from` and `to` does not matter.
!   - The result is in `[MIN(from, to), MAX(from, to))`. Because of
!     rounding, the upper bound can be reached.
!   - If `from = to`, the result is `from`.
!
! Arguments:
!   from : REAL(DFP) - one end of the interval
!   to : REAL(DFP) - other end of the interval
!
! Returns:
!   ans : REAL(DFP) - random number between `from` and `to`
!
! Formula:
!   $$\mathrm{ans} = \min(\mathrm{from}, \mathrm{to})
!     + |\mathrm{to} - \mathrm{from}|\, u, \quad u \in [0, 1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE RandomUtility, ONLY: RandomValue
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 1000
! INTEGER(I4B) :: i
! REAL(DFP) :: x(n)
! LOGICAL(LGT) :: isok
!
! ! values are between from and to
! DO i = 1, n
!   x(i) = RandomValue(from=2.0_DFP, to=5.0_DFP)
! END DO
! isok = ALL(x .GE. 2.0_DFP) .AND. ALL(x .LE. 5.0_DFP)
! CALL OK(isok, "value is in [2, 5]")
!
! ! the order of from and to does not matter
! DO i = 1, n
!   x(i) = RandomValue(from=5.0_DFP, to=2.0_DFP)
! END DO
! isok = ALL(x .GE. 2.0_DFP) .AND. ALL(x .LE. 5.0_DFP)
! CALL OK(isok, "from > to gives a value in [2, 5]")
!
! ! negative bounds
! DO i = 1, n
!   x(i) = RandomValue(from=-1.0_DFP, to=-0.5_DFP)
! END DO
! isok = ALL(x .GE. -1.0_DFP) .AND. ALL(x .LE. -0.5_DFP)
! CALL OK(isok, "value is in [-1, -0.5]")
!
! ! from equal to to
! CALL OK(RandomValue(3.0_DFP, 3.0_DFP) .EQ. 3.0_DFP, "from = to")
!
! END PROGRAM main
!```

INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue2(from, to) RESULT(ans)
    REAL(DFP), INTENT(IN) :: from, to
    REAL(DFP) :: ans
  END FUNCTION obj_RandomValue2
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Returns a random integer between from and to.
!
! Description:
!   Returns a random integer between `from` and `to`, both included.
!   It calls the intrinsic `RANDOM_NUMBER`, so the result changes from
!   call to call.
!
!   - A uniform real number in `[MIN(from, to), MAX(from, to))` is
!     rounded to the nearest integer with `NINT`.
!   - The values are not equally likely: the two end points are
!     selected half as often as the inner values.
!   - If `from = to`, the result is `from`.
!   - If `from > to`, the value `from - 1` is replaced by `from`, so
!     some values are never returned. For example,
!     `RandomValue(3, 1)` returns only 1 or 3.
!
! Arguments:
!   from : INTEGER(I4B) - one end of the interval
!   to : INTEGER(I4B) - other end of the interval
!
! Returns:
!   ans : INTEGER(I4B) - random integer between `from` and `to`
!
! Formula:
!   $$\mathrm{ans} = \mathrm{NINT}\left(\min(\mathrm{from}, \mathrm{to})
!     + |\mathrm{to} - \mathrm{from}|\, u\right), \quad u \in [0, 1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE RandomUtility, ONLY: RandomValue
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 1000
! INTEGER(I4B) :: i, k(n)
! LOGICAL(LGT) :: isok
!
! ! values are between from and to, both included
! DO i = 1, n
!   k(i) = RandomValue(from=1_I4B, to=3_I4B)
! END DO
! isok = ALL(k .GE. 1_I4B) .AND. ALL(k .LE. 3_I4B)
! CALL OK(isok, "value is in [1, 3]")
! isok = ANY(k .EQ. 1_I4B) .AND. ANY(k .EQ. 3_I4B)
! CALL OK(isok, "both end points are reached")
!
! ! the order of from and to does not matter
! DO i = 1, n
!   k(i) = RandomValue(from=3_I4B, to=1_I4B)
! END DO
! isok = ALL(k .GE. 1_I4B) .AND. ALL(k .LE. 3_I4B)
! CALL OK(isok, "from > to gives a value in [1, 3]")
!
! ! negative bounds
! DO i = 1, n
!   k(i) = RandomValue(from=-5_I4B, to=-2_I4B)
! END DO
! isok = ALL(k .GE. -5_I4B) .AND. ALL(k .LE. -2_I4B)
! CALL OK(isok, "value is in [-5, -2]")
!
! ! from equal to to
! CALL OK(RandomValue(4_I4B, 4_I4B) .EQ. 4_I4B, "from = to")
!
! END PROGRAM main
!```

INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue3(from, to) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: from, to
    INTEGER(I4B) :: ans
  END FUNCTION obj_RandomValue3
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Returns a randomly selected entry of an integer vector.
!
! Description:
!   Selects a random index `p` between 1 and `SIZE(val)` with the
!   integer `RandomValue(from, to)` and returns `val(p)`. It calls the
!   intrinsic `RANDOM_NUMBER`, so the result changes from call to call.
!
!   - The first and the last entries are selected half as often as
!     the other entries (see the integer `RandomValue`).
!   - `val` should not be empty. This is not checked.
!
! Arguments:
!   val : INTEGER(I4B), rank(1) - vector, `SIZE(val) >= 1`
!
! Returns:
!   ans : INTEGER(I4B) - an entry of `val`
!
! Formula:
!   $$\mathrm{ans} = \mathrm{val}_p, \quad
!     p = \mathrm{RandomValue}(1, \mathrm{SIZE}(\mathrm{val}))$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE RandomUtility, ONLY: RandomValue
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 1000
! INTEGER(I4B) :: i, k(n), val(4)
! LOGICAL(LGT) :: isok
!
! val = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
!
! ! the selected value is an entry of val
! DO i = 1, n
!   k(i) = RandomValue(val)
! END DO
! isok = .TRUE.
! DO i = 1, n
!   isok = isok .AND. ANY(val .EQ. k(i))
! END DO
! CALL OK(isok, "selected value is an entry of val")
!
! ! the first and the last entries can be selected
! isok = ANY(k .EQ. val(1)) .AND. ANY(k .EQ. val(4))
! CALL OK(isok, "first and last entries are selected")
!
! ! a vector with one entry
! CALL OK(RandomValue([7_I4B]) .EQ. 7_I4B, "vector of size 1")
!
! END PROGRAM main
!```

INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue4(val) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: val(:)
    INTEGER(I4B) :: ans
  END FUNCTION obj_RandomValue4
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                 RandomValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Returns a randomly selected entry of an integer matrix.
!
! Description:
!   Selects a random row `i` and a random column `j` with the integer
!   `RandomValue(from, to)` and returns `val(i, j)`. It calls the
!   intrinsic `RANDOM_NUMBER`, so the result changes from call to call.
!
!   - The first and the last rows and columns are selected half as
!     often as the others (see the integer `RandomValue`).
!   - `val` should not be empty. This is not checked.
!
! Arguments:
!   val : INTEGER(I4B), rank(2) - matrix, `SIZE(val) >= 1`
!
! Returns:
!   ans : INTEGER(I4B) - an entry of `val`
!
! Formula:
!   $$\mathrm{ans} = \mathrm{val}_{ij}, \quad
!     i = \mathrm{RandomValue}(1, \mathrm{SIZE}(\mathrm{val}, 1)), \quad
!     j = \mathrm{RandomValue}(1, \mathrm{SIZE}(\mathrm{val}, 2))$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE RandomUtility, ONLY: RandomValue
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 1000
! INTEGER(I4B) :: i, k(n), val(2, 3), one(1, 1)
! LOGICAL(LGT) :: isok
!
! val = RESHAPE([1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B], &
!               [2_I4B, 3_I4B])
!
! ! the selected value is an entry of val
! DO i = 1, n
!   k(i) = RandomValue(val)
! END DO
! isok = .TRUE.
! DO i = 1, n
!   isok = isok .AND. ANY(val .EQ. k(i))
! END DO
! CALL OK(isok, "selected value is an entry of val")
!
! ! the corner entries can be selected
! isok = ANY(k .EQ. val(1, 1)) .AND. ANY(k .EQ. val(2, 3))
! CALL OK(isok, "corner entries are selected")
!
! ! a matrix with one entry
! one = 9_I4B
! CALL OK(RandomValue(one) .EQ. 9_I4B, "matrix of size 1x1")
!
! END PROGRAM main
!```

INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue5(val) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: val(:, :)
    INTEGER(I4B) :: ans
  END FUNCTION obj_RandomValue5
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Returns a randomly selected entry of a real vector.
!
! Description:
!   Selects a random index `p` between 1 and `SIZE(val)` with the
!   integer `RandomValue(from, to)` and returns `val(p)`. It calls the
!   intrinsic `RANDOM_NUMBER`, so the result changes from call to call.
!
!   - The first and the last entries are selected half as often as
!     the other entries (see the integer `RandomValue`).
!   - `val` should not be empty. This is not checked.
!
! Arguments:
!   val : REAL(DFP), rank(1) - vector, `SIZE(val) >= 1`
!
! Returns:
!   ans : REAL(DFP) - an entry of `val`
!
! Formula:
!   $$\mathrm{ans} = \mathrm{val}_p, \quad
!     p = \mathrm{RandomValue}(1, \mathrm{SIZE}(\mathrm{val}))$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE RandomUtility, ONLY: RandomValue
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 1000
! INTEGER(I4B) :: i
! REAL(DFP) :: x(n), val(3)
! LOGICAL(LGT) :: isok
!
! val = [0.5_DFP, -1.5_DFP, 2.25_DFP]
!
! ! the selected value is an entry of val
! DO i = 1, n
!   x(i) = RandomValue(val)
! END DO
! isok = .TRUE.
! DO i = 1, n
!   isok = isok .AND. ANY(val .EQ. x(i))
! END DO
! CALL OK(isok, "selected value is an entry of val")
!
! ! the first and the last entries can be selected
! isok = ANY(x .EQ. val(1)) .AND. ANY(x .EQ. val(3))
! CALL OK(isok, "first and last entries are selected")
!
! ! a vector with one entry
! CALL OK(RandomValue([3.5_DFP]) .EQ. 3.5_DFP, "vector of size 1")
!
! END PROGRAM main
!```

INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue6(val) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val(:)
    REAL(DFP) :: ans
  END FUNCTION obj_RandomValue6
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                 RandomValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Returns a randomly selected entry of a real matrix.
!
! Description:
!   Selects a random row `i` and a random column `j` with the integer
!   `RandomValue(from, to)` and returns `val(i, j)`. It calls the
!   intrinsic `RANDOM_NUMBER`, so the result changes from call to call.
!
!   - The first and the last rows and columns are selected half as
!     often as the others (see the integer `RandomValue`).
!   - `val` should not be empty. This is not checked.
!
! Arguments:
!   val : REAL(DFP), rank(2) - matrix, `SIZE(val) >= 1`
!
! Returns:
!   ans : REAL(DFP) - an entry of `val`
!
! Formula:
!   $$\mathrm{ans} = \mathrm{val}_{ij}, \quad
!     i = \mathrm{RandomValue}(1, \mathrm{SIZE}(\mathrm{val}, 1)), \quad
!     j = \mathrm{RandomValue}(1, \mathrm{SIZE}(\mathrm{val}, 2))$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE RandomUtility, ONLY: RandomValue
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 1000
! INTEGER(I4B) :: i
! REAL(DFP) :: x(n), val(3, 2), one(1, 1)
! LOGICAL(LGT) :: isok
!
! val = RESHAPE([0.1_DFP, 0.2_DFP, 0.3_DFP, 0.4_DFP, 0.5_DFP, 0.6_DFP], &
!               [3_I4B, 2_I4B])
!
! ! the selected value is an entry of val
! DO i = 1, n
!   x(i) = RandomValue(val)
! END DO
! isok = .TRUE.
! DO i = 1, n
!   isok = isok .AND. ANY(val .EQ. x(i))
! END DO
! CALL OK(isok, "selected value is an entry of val")
!
! ! the corner entries can be selected
! isok = ANY(x .EQ. val(1, 1)) .AND. ANY(x .EQ. val(3, 2))
! CALL OK(isok, "corner entries are selected")
!
! ! a matrix with one entry
! one = -2.0_DFP
! CALL OK(RandomValue(one) .EQ. -2.0_DFP, "matrix of size 1x1")
!
! END PROGRAM main
!```

INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue7(val) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val(:, :)
    REAL(DFP) :: ans
  END FUNCTION obj_RandomValue7
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Returns a vector of seeded uniform random numbers in (0, 1).
!
! Description:
!   Returns `n` pseudorandom numbers in `(0, 1)` from the Park-Miller
!   "minimal standard" generator. It is based on `rvec_uniform_01`
!   of John Burkardt.
!
!   - The function is `PURE` and `seed` is not updated, so the same
!     `seed` always gives the same vector.
!   - The default value of `seed` is 1.
!   - `seed` should be a positive integer. If `seed = 0`, the result
!     is a vector of zeros.
!
! Arguments:
!   n : INTEGER(I4B) - number of values
!   seed : INTEGER(I4B), OPTIONAL - seed of the generator, default 1
!
! Returns:
!   ans : REAL(DFP), rank(1) - pseudorandom values, `SIZE(ans) = n`
!
! Formula:
!   $$s_0 = \mathrm{seed}, \quad
!     s_i = 16807\, s_{i-1} \bmod (2^{31} - 1), \quad
!     \mathrm{ans}_i = \frac{s_i}{2^{31} - 1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE RandomUtility, ONLY: UniformRandomValue
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 1000
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
! REAL(DFP) :: u(n), v(n), w(3)
! LOGICAL(LGT) :: isok
!
! u = UniformRandomValue(n, seed=7_I4B)
!
! ! size and range
! CALL OK(SIZE(UniformRandomValue(n, seed=7_I4B)) .EQ. n, "size of result")
! isok = ALL(u .GT. 0.0_DFP) .AND. ALL(u .LT. 1.0_DFP)
! CALL OK(isok, "values are in (0, 1)")
!
! ! the same seed gives the same values
! v = UniformRandomValue(n, seed=7_I4B)
! CALL OK(ALL(u .EQ. v), "same seed gives the same values")
!
! ! a different seed gives different values
! v = UniformRandomValue(n, seed=8_I4B)
! CALL OK(ANY(u .NE. v), "different seed gives different values")
!
! ! the default seed is 1
! w = UniformRandomValue(3_I4B)
! CALL OK(ALL(w .EQ. UniformRandomValue(3_I4B, seed=1_I4B)), &
!         "default seed is 1")
!
! ! first value for seed = 1 is 16807 / (2**31 - 1)
! CALL OK(ABS(w(1) - 7.826369259012501E-6_DFP) .LT. tol, &
!         "first value for seed 1")
!
! ! seed = 0 gives zeros
! w = UniformRandomValue(3_I4B, seed=0_I4B)
! CALL OK(ALL(w .EQ. 0.0_DFP), "seed 0 gives zeros")
!
! END PROGRAM main
!```

INTERFACE UniformRandomValue
  MODULE PURE FUNCTION UniformRandomValue1(n, seed) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! the number of entries in the vector
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    !! the seed value, which should not be 0. It is not updated.
    !! The default value of seed is 1
    REAL(DFP) :: ans(n)
    !! the vector of pseudorandom values
  END FUNCTION UniformRandomValue1
END INTERFACE UniformRandomValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Returns a vector of seeded uniform random numbers in (a, b).
!
! Description:
!   Returns `n` pseudorandom numbers between `a` and `b`. The unit
!   values of `UniformRandomValue(n, seed)` are scaled to `(a, b)`.
!
!   - The function is `PURE` and `seed` is not updated, so the same
!     `seed` always gives the same vector.
!   - The default value of `seed` is 1.
!   - If `a > b`, the values are in `(b, a)`.
!   - If `seed = 0`, the result is a vector of zeros, not of `a`.
!
! Arguments:
!   n : INTEGER(I4B) - number of values
!   a : REAL(DFP) - one end of the interval
!   b : REAL(DFP) - other end of the interval
!   seed : INTEGER(I4B), OPTIONAL - seed of the generator, default 1
!
! Returns:
!   ans : REAL(DFP), rank(1) - pseudorandom values, `SIZE(ans) = n`
!
! Formula:
!   $$\mathrm{ans}_i = a + (b - a)\, u_i$$
!   where $u_i$ are the values of `UniformRandomValue(n, seed)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE RandomUtility, ONLY: UniformRandomValue
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: n = 1000
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: a, b, x(n), y(n), u(n)
! LOGICAL(LGT) :: isok
!
! a = -2.0_DFP
! b = 3.0_DFP
!
! x = UniformRandomValue(n, a, b, seed=11_I4B)
!
! ! range
! isok = ALL(x .GT. a) .AND. ALL(x .LT. b)
! CALL OK(isok, "values are in (a, b)")
!
! ! the same seed gives the same values
! y = UniformRandomValue(n, a, b, seed=11_I4B)
! CALL OK(ALL(x .EQ. y), "same seed gives the same values")
!
! ! the result is a + (b - a) * u, where u is the unit version
! u = UniformRandomValue(n, seed=11_I4B)
! CALL OK(ALL(ABS(x - (a + (b - a) * u)) .LT. tol), "x = a + (b - a) u")
!
! ! a > b gives values in (b, a)
! x = UniformRandomValue(n, b, a, seed=11_I4B)
! isok = ALL(x .GT. a) .AND. ALL(x .LT. b)
! CALL OK(isok, "a > b gives values in (b, a)")
!
! END PROGRAM main
!```

INTERFACE UniformRandomValue
  MODULE PURE FUNCTION UniformRandomValue2(n, a, b, seed) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! the number of pseudorandom numbers to return
    REAL(DFP), INTENT(IN) :: a
    REAL(DFP), INTENT(IN) :: b
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    !! the default value of seed is 1
    REAL(DFP) :: ans(n)
  END FUNCTION UniformRandomValue2
END INTERFACE UniformRandomValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Returns one seeded uniform random number in (0, 1).
!
! Description:
!   Returns one pseudorandom number in `(0, 1)` from the Park-Miller
!   "minimal standard" generator. The result is the same as the first
!   entry of `UniformRandomValue(1, seed)`.
!
!   - The function is `PURE` and `seed` is not updated, so repeated
!     calls with the same `seed` return the same value.
!   - The default value of `seed` is 1.
!   - If `seed = 0`, the result is 0.
!
! Arguments:
!   seed : INTEGER(I4B), OPTIONAL - seed of the generator, default 1
!
! Returns:
!   ans : REAL(DFP) - pseudorandom value in `(0, 1)`
!
! Formula:
!   $$\mathrm{ans} = \frac{16807\, \mathrm{seed} \bmod (2^{31} - 1)}
!     {2^{31} - 1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE RandomUtility, ONLY: UniformRandomValue
! USE RandomUtility, ONLY: UniformRandomValueScalar
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
! REAL(DFP) :: u, v(1)
!
! ! default seed is 1, value is 16807 / (2**31 - 1)
! u = UniformRandomValueScalar()
! CALL OK(ABS(u - 7.826369259012501E-6_DFP) .LT. tol, "default seed")
!
! ! a given seed
! u = UniformRandomValueScalar(seed=12345_I4B)
! CALL OK(ABS(u - 0.09661652850250932_DFP) .LT. tol, "seed 12345")
! CALL OK(u .GT. 0.0_DFP .AND. u .LT. 1.0_DFP, "value is in (0, 1)")
!
! ! same as the first entry of UniformRandomValue
! v = UniformRandomValue(1_I4B, seed=12345_I4B)
! CALL OK(u .EQ. v(1), "same as UniformRandomValue(1, seed)")
!
! ! the seed is not updated, so the same seed gives the same value
! CALL OK(UniformRandomValueScalar(5_I4B) .EQ. &
!         UniformRandomValueScalar(5_I4B), "seed is not updated")
!
! ! seed = 0 gives zero
! CALL OK(UniformRandomValueScalar(0_I4B) .EQ. 0.0_DFP, "seed 0 gives 0")
!
! END PROGRAM main
!```

INTERFACE UniformRandomValueScalar
  MODULE PURE FUNCTION UniformRandomValueScalar1(seed) RESULT(ans)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    !! "seed" value, which should NOT be 0. It is not updated.
    REAL(DFP) :: ans
    !! a new pseudorandom variate, strictly between 0 and 1.
  END FUNCTION UniformRandomValueScalar1
END INTERFACE UniformRandomValueScalar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Returns a seeded random unit vector.
!
! Description:
!   Returns a vector of length 1 with a random direction. It takes `m`
!   values of `NormalRandomValue(m, seed)` and divides them by their
!   Euclidean norm, so the direction is uniformly distributed on the
!   unit sphere.
!
!   - This function is not `PUBLIC`, so it can only be used inside
!     `RandomUtility`.
!   - The same `seed` always gives the same vector.
!   - The limits of `NormalRandomValue` also apply here.
!
! Arguments:
!   m : INTEGER(I4B) - size of the vector
!   seed : INTEGER(I4B), OPTIONAL - seed of the generator, default 1
!
! Returns:
!   w : REAL(DFP), rank(1) - unit vector, `SIZE(w) = m`
!
! Formula:
!   $$w = \frac{z}{\|z\|_2}, \quad z = \mathrm{NormalRandomValue}(m,
!     \mathrm{seed})$$

INTERFACE UniformUnitRandomValue
  MODULE PURE FUNCTION UniformUnitRandomValue1(m, seed) RESULT(w)
    INTEGER(I4B), INTENT(IN) :: m
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    REAL(DFP) :: w(m)
  END FUNCTION UniformUnitRandomValue1
END INTERFACE UniformUnitRandomValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Returns seeded samples of the standard normal distribution.
!
! Description:
!   Returns `n` samples of the standard normal distribution (mean 0,
!   standard deviation 1). It uses the Box-Muller method on pairs of
!   uniform values from `UniformRandomValue(., seed)`. It is based on
!   `rvec_normal_01` of John Burkardt.
!
!   - The function is `PURE` and `seed` is not updated, so the same
!     `seed` always gives the same vector.
!   - The default value of `seed` is 1. `seed` should not be 0.
!   - Each pair of uniform values gives two normal values. If `n` is
!     odd, the second value of the last pair is discarded. Nothing is
!     kept between calls.
!   - If `n <= 0`, the result is an empty vector.
!   - For `n = 1`, both uniform values are the same number, so the
!     result is not a true normal sample.
!
! Arguments:
!   n : INTEGER(I4B) - number of samples
!   seed : INTEGER(I4B), OPTIONAL - seed of the generator, default 1
!
! Returns:
!   x : REAL(DFP), rank(1) - normal samples, `SIZE(x) = MAX(n, 0)`
!
! Formula:
!   $$x_{2p-1} = \sqrt{-2 \ln u_{2p-1}}\, \cos(2 \pi u_{2p}), \quad
!     x_{2p} = \sqrt{-2 \ln u_{2p-1}}\, \sin(2 \pi u_{2p})$$
!   where $u_i$ are the values of `UniformRandomValue(., seed)`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE RandomUtility, ONLY: NormalRandomValue
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! odd n: the second value of the last pair is discarded
! INTEGER(I4B), PARAMETER :: n = 10001
! REAL(DFP) :: x(n), y(n), mu, sd
!
! x = NormalRandomValue(n, seed=12345_I4B)
!
! ! sample mean and standard deviation
! mu = SUM(x) / n
! sd = SQRT(SUM((x - mu)**2) / (n - 1))
! CALL OK(ABS(mu) .LT. 0.05_DFP, "mean is close to 0")
! CALL OK(ABS(sd - 1.0_DFP) .LT. 0.05_DFP, "std is close to 1")
!
! ! the same seed gives the same values
! y = NormalRandomValue(n, seed=12345_I4B)
! CALL OK(ALL(x .EQ. y), "same seed gives the same values")
!
! ! a different seed gives different values
! y = NormalRandomValue(n, seed=7_I4B)
! CALL OK(ANY(x .NE. y), "different seed gives different values")
!
! ! n = 0 gives an empty vector
! CALL OK(SIZE(NormalRandomValue(0_I4B, seed=1_I4B)) .EQ. 0_I4B, "n = 0")
!
! END PROGRAM main
!```

INTERFACE NormalRandomValue
  MODULE PURE FUNCTION NormalRandomValue1(n, seed) RESULT(x)
    INTEGER(I4B), INTENT(IN) :: n
    !! number of random numbers.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    !! seed value
    REAL(DFP) :: x(n)
  END FUNCTION NormalRandomValue1
END INTERFACE NormalRandomValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE RandomUtility
