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

SUBMODULE(RandomUtility) Methods
USE InputUtility, ONLY: Input
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RandomValue1
REAL(DFP) :: val, y
INTEGER(I4B) :: i

IF (PRESENT(distribution)) THEN
  SELECT CASE (TRIM(distribution))
  CASE ("Binomial", "binomial")
    val = 0.0D0
    DO i = 1, 20
      CALL RANDOM_NUMBER(y)
      val = val + y
    END DO
    ans = val - 10.0_DFP
  CASE DEFAULT
    CALL RANDOM_NUMBER(ans)
  END SELECT
ELSE
  CALL RANDOM_NUMBER(ans)
END IF

END PROCEDURE obj_RandomValue1

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RandomValue2
REAL(DFP) :: a, diff, val(2)

val(1) = from
val(2) = to

diff = ABS(val(1) - val(2))
CALL RANDOM_NUMBER(a)
Ans = a * diff + MINVAL(val)
END PROCEDURE obj_RandomValue2

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RandomValue3
REAL(DFP) :: a
INTEGER(I4B) :: lo, hi

lo = MIN(from, to)
hi = MAX(from, to)

! each of the hi - lo + 1 integers gets an interval of the same
! length in [0, 1); the difference is computed in REAL(DFP) so the
! full INTEGER(I4B) range does not overflow
CALL RANDOM_NUMBER(a)
ans = lo + FLOOR(a * (REAL(hi, DFP) - REAL(lo, DFP) + 1.0_DFP))
IF (ans .GT. hi) ans = hi
END PROCEDURE obj_RandomValue3

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RandomValue4
INTEGER(I4B) :: posi, from, to
! an empty val has no entry to select
IF (SIZE(val) .EQ. 0) THEN
  ans = 0
  RETURN
END IF

from = 1
to = SIZE(val)
posi = RandomValue(from=from, to=to)
ans = val(posi)
END PROCEDURE obj_RandomValue4

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RandomValue5
INTEGER(I4B) :: i1, i2, from, to

! an empty val has no entry to select
IF (SIZE(val) .EQ. 0) THEN
  ans = 0
  RETURN
END IF

from = 1
to = SIZE(val, 1)
i1 = RandomValue(from=from, to=to)

to = SIZE(val, 2)
i2 = RandomValue(from=from, to=to)

ans = Val(i1, i2)
END PROCEDURE obj_RandomValue5

!----------------------------------------------------------------------------
!                                                                 RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RandomValue6
INTEGER(I4B) :: posi, from, to

! an empty val has no entry to select
IF (SIZE(val) .EQ. 0) THEN
  ans = 0.0_DFP
  RETURN
END IF

from = 1
to = SIZE(val)
posi = RandomValue(from=from, to=to)
ans = val(posi)
END PROCEDURE obj_RandomValue6

!----------------------------------------------------------------------------
!                                                                 RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RandomValue7
INTEGER(I4B) :: i1, i2, from, to

! an empty val has no entry to select
IF (SIZE(val) .EQ. 0) THEN
  ans = 0.0_DFP
  RETURN
END IF

from = 1
to = SIZE(val, 1)
i1 = RandomValue(from=from, to=to)

to = SIZE(val, 2)
i2 = RandomValue(from=from, to=to)

ans = val(i1, i2)
END PROCEDURE obj_RandomValue7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UniformRandomValue1
INTEGER(I4B) :: i, k, seed0

seed0 = ValidSeed(seed)

DO i = 1, n
  k = seed0 / 127773
  seed0 = 16807 * (seed0 - k * 127773) - k * 2836
  IF (seed0 < 0) THEN
    seed0 = seed0 + 2147483647
  END IF
  ans(i) = REAL(seed0, kind=8) * 4.656612875D-10
END DO
END PROCEDURE UniformRandomValue1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UniformRandomValue2
INTEGER(I4B) :: i, k, seed0

seed0 = ValidSeed(seed)

DO i = 1, n
  k = seed0 / 127773
  seed0 = 16807 * (seed0 - k * 127773) - k * 2836
  IF (seed0 < 0) THEN
    seed0 = seed0 + 2147483647
  END IF
  ans(i) = a + (b - a) * REAL(seed0, kind=8) * 4.656612875D-10
END DO
END PROCEDURE UniformRandomValue2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UniformUnitRandomValue1
REAL(DFP) :: norm

!  Get M values from a standard normal distribution.
w = NormalRandomValue(m, seed)

!  Compute the length of the vector.
norm = SQRT(SUM(w(1:m)**2))

!  Normalize the vector.
w(1:m) = w(1:m) / norm

END PROCEDURE UniformUnitRandomValue1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UniformRandomValueScalar1
INTEGER(i4b) :: seed0
INTEGER(i4b) :: k

seed0 = ValidSeed(seed)

k = seed0 / 127773

seed0 = 16807 * (seed0 - k * 127773) - k * 2836

IF (seed0 < 0) THEN
  seed0 = seed0 + 2147483647
END IF

!  Although SEED can be represented exactly as a 32 bit integer,
!  it generally cannot be represented exactly as a 32 bit real number!
ans = REAL(seed0, kind=8) * 4.656612875D-10
END PROCEDURE UniformRandomValueScalar1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE NormalRandomValue1
INTEGER(I4B) :: k, m
REAL(DFP) :: r(n + 1)
REAL(DFP), PARAMETER :: twopi = 2.0_DFP * 3.141592653589793_DFP

IF (n .LE. 0) RETURN

! Box-Muller: each pair of uniform values gives two normal values.
! For odd n one more pair is drawn, and its second value is not used.
m = (n + 1) / 2
r(1:2 * m) = UniformRandomValue(2 * m, seed)

DO k = 1, n / 2
  x(2 * k - 1) = SQRT(-2.0_DFP * LOG(r(2 * k - 1))) * COS(twopi * r(2 * k))
  x(2 * k) = SQRT(-2.0_DFP * LOG(r(2 * k - 1))) * SIN(twopi * r(2 * k))
END DO

IF (MOD(n, 2) .EQ. 1) THEN
  x(n) = SQRT(-2.0_DFP * LOG(r(2 * m - 1))) * COS(twopi * r(2 * m))
END IF
END PROCEDURE NormalRandomValue1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!                                                                 ValidSeed
!----------------------------------------------------------------------------

! Maps seed (default 1) into 1..2147483646, the valid range of the
! Park-Miller generator, so that seed = 0 or a negative seed cannot
! give zero (and LOG(0) in NormalRandomValue).

PURE FUNCTION ValidSeed(seed) RESULT(ans)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
  INTEGER(I4B) :: ans
  ans = MODULO(INPUT(option=seed, default=1_I4B), 2147483647_I4B)
  IF (ans .EQ. 0) ans = 1
END FUNCTION ValidSeed

END SUBMODULE Methods
