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

SUBMODULE(Random_Method) Methods
USE InputUtility, ONLY: Input
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
INTEGER(I4B) :: seedSize
LOGICAL(LGT) :: isok

CALL RANDOM_SEED(size=seedSize)

isok = ALLOCATED(obj%random_int_seed)

IF (.NOT. isok) THEN
  ALLOCATE (obj%random_int_seed(seedSize))
END IF

CALL RANDOM_SEED(get=obj%random_int_seed)
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                 getRandom
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
REAL(DFP) :: xr, a, diff, val(2)

val(1) = from
val(2) = to
diff = ABS(DBLE(from) - DBLE(to))

CALL RANDOM_NUMBER(a)
xr = a * diff + MINVAL(val)
ans = NINT(xr)
IF (ans == from - 1) THEN
  ans = from
END IF
IF (ans == to + 1) THEN
  ans = to
END IF
END PROCEDURE obj_RandomValue3

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RandomValue4
INTEGER(I4B) :: posi, from, to
from = 1
to = SIZE(val)
posi = RandomValue(obj=obj, from=from, to=to)
ans = val(posi)
END PROCEDURE obj_RandomValue4

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RandomValue5
INTEGER(I4B) :: i1, i2, from, to

from = 1
to = SIZE(val, 1)
i1 = RandomValue(obj=obj, from=from, to=to)

to = SIZE(val, 2)
i2 = RandomValue(obj=obj, from=from, to=to)

ans = Val(i1, i2)
END PROCEDURE obj_RandomValue5

!----------------------------------------------------------------------------
!                                                                 RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RandomValue6
INTEGER(I4B) :: posi, from, to

from = 1
to = SIZE(val)
posi = RandomValue(obj=obj, from=from, to=to)
ans = val(posi)
END PROCEDURE obj_RandomValue6

!----------------------------------------------------------------------------
!                                                                 RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RandomValue7
INTEGER(I4B) :: i1, i2, from, to

from = 1
to = SIZE(val, 1)
i1 = RandomValue(obj=obj, from=from, to=to)

to = SIZE(val, 2)
i2 = RandomValue(obj=obj, from=from, to=to)

ans = val(i1, i2)
END PROCEDURE obj_RandomValue7

!----------------------------------------------------------------------------
!                                                                 SaveRandom
!----------------------------------------------------------------------------

MODULE PROCEDURE SaveRandom
CALL RANDOM_SEED(put=obj%random_int_seed)
END PROCEDURE SaveRandom

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UniformRandomValue1
INTEGER(I4B) :: i, k, seed0

seed0 = INPUT(option=seed, default=1_I4B)

IF (seed0 == 0) THEN
  ans(1:n) = 0.0
  RETURN
END IF

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

seed0 = INPUT(option=seed, default=1_I4B)

IF (seed0 == 0) THEN
  ans(1:n) = 0.0
  RETURN
END IF

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

seed0 = INPUT(option=seed, default=1_I4B)

IF (seed0 == 0) THEN
  ans = 0.0
  RETURN
END IF

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
INTEGER(I4B) :: m
REAL(DFP) :: r(n + 1)
REAL(DFP), PARAMETER :: r8_pi = 3.141592653589793D+00
INTEGER(I4B) :: x_hi_index
INTEGER(I4B) :: x_lo_index
! integer(i4b), save :: made = 0
! real(dfp), save :: y = 0.0D+00
! integer(i4b), save :: saved = 0
INTEGER(I4B) :: saved
INTEGER(I4B) :: made
REAL(DFP) :: y

made = 0
y = 0.0_DFP
saved = 0

!  I'd like to allow the user to reset the internal data.
!  But this won't work properly if we have a saved value Y.
!  I'm making a crock option that allows the user to signal
!  explicitly that any internal memory should be flushed,
!  by passing in a negative value for N.

IF (n < 0) THEN
  ! n = made
  made = 0
  saved = 0
  y = 0.0D+00
  RETURN
ELSE IF (n == 0) THEN
  RETURN
END IF

!  Record the range of X we need to fill in.
x_lo_index = 1
x_hi_index = n

!  Use up the old value, if we have it.
IF (saved == 1) THEN
  x(1) = y
  saved = 0
  x_lo_index = 2
END IF

!  Maybe we don't need any more values.
IF (x_hi_index - x_lo_index + 1 == 0) THEN

!  If we need just one new value, do that here to avoid null arrays.
ELSE IF (x_hi_index - x_lo_index + 1 == 1) THEN

  r(1) = UniformRandomValueScalar(seed)

  IF (r(1) == 0.0D+00) THEN
    ! write (*, '(a)') ' '
    ! write (*, '(a)') 'rvec_NORMAL_01 - Fatal error!'
    ! write (*, '(a)') '  R8_UNIFORM_01 returned a value of 0.'
    ! stop 1
    RETURN
  END IF

  r(2) = UniformRandomValueScalar(seed)

  x(x_hi_index) = &
    SQRT(-2.0D+00 * LOG(r(1))) * COS(2.0D+00 * r8_pi * r(2))
  y = SQRT(-2.0D+00 * LOG(r(1))) * SIN(2.0D+00 * r8_pi * r(2))

  saved = 1

  made = made + 2

!  If we require an even number of values, that's easy.
ELSE IF (MOD(x_hi_index - x_lo_index + 1, 2) == 0) THEN

  m = (x_hi_index - x_lo_index + 1) / 2

  r = UniformRandomValue(2 * m, seed)

  x(x_lo_index:x_hi_index - 1:2) = &
    SQRT(-2.0D+00 * LOG(r(1:2 * m - 1:2))) &
    * COS(2.0D+00 * r8_pi * r(2:2 * m:2))

  x(x_lo_index + 1:x_hi_index:2) = &
    SQRT(-2.0D+00 * LOG(r(1:2 * m - 1:2))) &
    * SIN(2.0D+00 * r8_pi * r(2:2 * m:2))

  made = made + x_hi_index - x_lo_index + 1

!  If we require an odd number of values, we generate an even number,
!  and handle the last pair specially, storing one in X(N), and
!  saving the other for later.

ELSE

  x_hi_index = x_hi_index - 1

  m = (x_hi_index - x_lo_index + 1) / 2 + 1

  r = UniformRandomValue(2 * m, seed)

  x(x_lo_index:x_hi_index - 1:2) = &
    SQRT(-2.0D+00 * LOG(r(1:2 * m - 3:2))) &
    * COS(2.0D+00 * r8_pi * r(2:2 * m - 2:2))

  x(x_lo_index + 1:x_hi_index:2) = &
    SQRT(-2.0D+00 * LOG(r(1:2 * m - 3:2))) &
    * SIN(2.0D+00 * r8_pi * r(2:2 * m - 2:2))

  x(n) = SQRT(-2.0E+00 * LOG(r(2 * m - 1))) &
         * COS(2.0D+00 * r8_pi * r(2 * m))

  y = SQRT(-2.0D+00 * LOG(r(2 * m - 1))) &
      * SIN(2.0D+00 * r8_pi * r(2 * m))

  saved = 1

  made = made + x_hi_index - x_lo_index + 2

END IF
END PROCEDURE NormalRandomValue1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
