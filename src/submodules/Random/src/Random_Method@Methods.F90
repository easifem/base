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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE initRandom
INTEGER(I4B) :: SeedSize

CALL RANDOM_SEED(size=SeedSize)
IF (.NOT. ALLOCATED(obj%random_int_seed)) THEN
  ALLOCATE (obj%random_int_seed(SeedSize))
END IF
call RANDOM_SEED(get=obj%random_int_seed)
END PROCEDURE initRandom

!----------------------------------------------------------------------------
!                                                                 getRandom
!----------------------------------------------------------------------------

MODULE PROCEDURE getRandom
REAL(DFP) :: val, y
INTEGER(I4B) :: i

IF (PRESENT(distribution)) THEN
  SELECT CASE (TRIM(distribution))
  CASE ("Binomial", "binomial")
    val = 0.0d0
    DO i = 1, 20
      CALL RANDOM_NUMBER(y)
      val = val + y
    END DO
    Ans = val - 10.0_DFP
  CASE DEFAULT
    CALL RANDOM_NUMBER(Ans)
  END SELECT
ELSE
  CALL RANDOM_NUMBER(Ans)
END IF

END PROCEDURE getRandom

!----------------------------------------------------------------------------
!                                                                 SaveRandom
!----------------------------------------------------------------------------

MODULE PROCEDURE SaveRandom
CALL RANDOM_SEED(put=obj%Random_INT_SEED)
END PROCEDURE SaveRandom

!----------------------------------------------------------------------------
!                                                              UniformRandom
!----------------------------------------------------------------------------

MODULE PROCEDURE uniformRandom
REAL(DFP) :: a, diff, val(2)

val(1) = From
val(2) = To
diff = abs(From - To)
CALL RANDOM_NUMBER(a)
Ans = a * diff + minval(val)
END PROCEDURE uniformRandom

!----------------------------------------------------------------------------
!                                                           getRandomInteger
!----------------------------------------------------------------------------

MODULE PROCEDURE getRandomInteger
REAL(DFP) :: xr, a, diff, val(2)

val(1) = From
val(2) = To
diff = abs(dble(from) - dble(to))

call random_number(a)
xr = a * diff + minval(val)
Ans = nint(xr)
if (Ans == From - 1) then
  Ans = From
end if
if (Ans == To + 1) then
  Ans = To
end if
END PROCEDURE getRandomInteger

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE select_random_int_from_vec
INTEGER(I4B) :: posi
posi = getRandomInteger(obj, From=1, To=size(Val))
Ans = Val(posi)
END PROCEDURE select_random_int_from_vec

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE select_random_int_from_array
INTEGER(I4B) :: i1, i2
i1 = getRandomInteger(obj, From=1, To=SIZE(Val, 1))
i2 = getRandomInteger(obj, From=1, To=SIZE(Val, 2))
Ans = Val(i1, i2)
END PROCEDURE select_random_int_from_array

!----------------------------------------------------------------------------
!                                                                 RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE select_random_real_from_vec
INTEGER(I4B) :: posi
posi = getRandomInteger(obj, From=1, To=size(Val))
Ans = Val(posi)
END PROCEDURE select_random_real_from_vec

!----------------------------------------------------------------------------
!                                                                 RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE select_random_real_from_array
INTEGER(I4B) :: i1, i2
i1 = getRandomInteger(obj, From=1, To=SIZE(Val, 1))
i2 = getRandomInteger(obj, From=1, To=SIZE(Val, 2))
Ans = Val(i1, i2)
END PROCEDURE select_random_real_from_array

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

module procedure rvec_uniform_01
integer(i4b) i
integer(i4b) k
integer(i4b) seed0
!
seed0 = INPUT(option=seed, default=1_I4B)
!
if (seed0 == 0) then
  r = 0.0
  return
end if
!
do i = 1, n
  k = seed0 / 127773
  seed0 = 16807 * (seed0 - k * 127773) - k * 2836
  if (seed0 < 0) then
    seed0 = seed0 + 2147483647
  end if
  r(i) = real(seed0, kind=8) * 4.656612875D-10
end do
end procedure

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE rvec_uniform_ab
integer(i4b) i
integer(i4b) k
integer(i4b) seed0
!
seed0 = INPUT(option=seed, default=1_I4B)
!
if (seed0 == 0) then
  r = 0.0
  return
end if
!
do i = 1, n
  k = seed0 / 127773
  seed0 = 16807 * (seed0 - k * 127773) - k * 2836
  if (seed0 < 0) then
    seed0 = seed0 + 2147483647
  end if
  r(i) = a + (b - a) * real(seed0, kind=8) * 4.656612875D-10
end do
!
END PROCEDURE rvec_uniform_ab

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE rvec_uniform_unit
real(kind=8) norm
!
!  Get M values from a standard normal distribution.
!
w = rvec_normal_01(m, seed)
!
!  Compute the length of the vector.
!
norm = sqrt(sum(w(1:m)**2))
!
!  Normalize the vector.
!
w(1:m) = w(1:m) / norm

return
END PROCEDURE rvec_uniform_unit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

module procedure rvec_normal_01
integer(i4b) m
real(dfp) r(n + 1)
real(dfp), parameter :: r8_pi = 3.141592653589793D+00
integer(i4b) x_hi_index
integer(i4b) x_lo_index
! integer(i4b), save :: made = 0
! real(dfp), save :: y = 0.0D+00
! integer(i4b), save :: saved = 0
integer(i4b) :: saved
integer(i4b) :: made
real(dfp) :: y
!
made = 0
y = 0.0_DFP
saved = 0
!
!  I'd like to allow the user to reset the internal data.
!  But this won't work properly if we have a saved value Y.
!  I'm making a crock option that allows the user to signal
!  explicitly that any internal memory should be flushed,
!  by passing in a negative value for N.
!
if (n < 0) then
  ! n = made
  made = 0
  saved = 0
  y = 0.0D+00
  return
else if (n == 0) then
  return
end if
!
!  Record the range of X we need to fill in.
!
x_lo_index = 1
x_hi_index = n
!
!  Use up the old value, if we have it.
!
if (saved == 1) then
  x(1) = y
  saved = 0
  x_lo_index = 2
end if
!
!  Maybe we don't need any more values.
!
if (x_hi_index - x_lo_index + 1 == 0) then
!
!  If we need just one new value, do that here to avoid null arrays.
!
else if (x_hi_index - x_lo_index + 1 == 1) then

  r(1) = r8_uniform_01(seed)

  if (r(1) == 0.0D+00) then
    ! write (*, '(a)') ' '
    ! write (*, '(a)') 'rvec_NORMAL_01 - Fatal error!'
    ! write (*, '(a)') '  R8_UNIFORM_01 returned a value of 0.'
    ! stop 1
    return
  end if

  r(2) = r8_uniform_01(seed)

  x(x_hi_index) = &
    sqrt(-2.0D+00 * log(r(1))) * cos(2.0D+00 * r8_pi * r(2))
  y = sqrt(-2.0D+00 * log(r(1))) * sin(2.0D+00 * r8_pi * r(2))

  saved = 1

  made = made + 2
!
!  If we require an even number of values, that's easy.
!
else if (mod(x_hi_index - x_lo_index + 1, 2) == 0) then

  m = (x_hi_index - x_lo_index + 1) / 2

  r = rvec_uniform_01(2 * m, seed)

  x(x_lo_index:x_hi_index - 1:2) = &
    sqrt(-2.0D+00 * log(r(1:2 * m - 1:2))) &
    * cos(2.0D+00 * r8_pi * r(2:2 * m:2))

  x(x_lo_index + 1:x_hi_index:2) = &
    sqrt(-2.0D+00 * log(r(1:2 * m - 1:2))) &
    * sin(2.0D+00 * r8_pi * r(2:2 * m:2))

  made = made + x_hi_index - x_lo_index + 1
!
!  If we require an odd number of values, we generate an even number,
!  and handle the last pair specially, storing one in X(N), and
!  saving the other for later.
!
else

  x_hi_index = x_hi_index - 1

  m = (x_hi_index - x_lo_index + 1) / 2 + 1

  r = rvec_uniform_01(2 * m, seed)

  x(x_lo_index:x_hi_index - 1:2) = &
    sqrt(-2.0D+00 * log(r(1:2 * m - 3:2))) &
    * cos(2.0D+00 * r8_pi * r(2:2 * m - 2:2))

  x(x_lo_index + 1:x_hi_index:2) = &
    sqrt(-2.0D+00 * log(r(1:2 * m - 3:2))) &
    * sin(2.0D+00 * r8_pi * r(2:2 * m - 2:2))

  x(n) = sqrt(-2.0E+00 * log(r(2 * m - 1))) &
         * cos(2.0D+00 * r8_pi * r(2 * m))

  y = sqrt(-2.0D+00 * log(r(2 * m - 1))) &
      * sin(2.0D+00 * r8_pi * r(2 * m))

  saved = 1

  made = made + x_hi_index - x_lo_index + 2

end if
end procedure

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

module procedure r8_uniform_01
integer(i4b) seed0
integer(i4b) k
!
seed0 = INPUT(option=seed, default=1_I4B)
!
if (seed0 == 0) then
  ans = 0.0
  return
end if
!
k = seed0 / 127773

seed0 = 16807 * (seed0 - k * 127773) - k * 2836

if (seed0 < 0) then
  seed0 = seed0 + 2147483647
end if
!
!  Although SEED can be represented exactly as a 32 bit integer,
!  it generally cannot be represented exactly as a 32 bit real number!
!
ans = real(seed0, kind=8) * 4.656612875D-10
end procedure

END SUBMODULE Methods
