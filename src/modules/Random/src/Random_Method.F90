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

MODULE Random_Method
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: Random_
IMPLICIT NONE

PRIVATE
PUBLIC :: Initiate
PUBLIC :: RandomValue
PUBLIC :: SaveRandom
PUBLIC :: UniformRandomValue
PUBLIC :: UniformRandomValueScalar
PUBLIC :: NormalRandomValue

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Initiate a random object

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate(obj)
    CLASS(Random_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Initiate
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                                 getRandom
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: GetRandom value
!
!# RandomValue
!
! Get the random value. `distribution` can be
!
! - "Binomial"
!

INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue1(obj, distribution) RESULT(Ans)
    CLASS(Random_), INTENT(IN) :: obj
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: distribution
    REAL(DFP) :: Ans
  END FUNCTION obj_RandomValue1
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                             UniformRandom
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Get uniform random value
!
!# RandomValue
!
! Get the uniform random value between from and to.

INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue2(obj, from, to) RESULT(ans)
    CLASS(Random_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: from, to
    REAL(DFP) :: ans
  END FUNCTION obj_RandomValue2
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Get a random integer between from and to
!
!# RandomValue
!
! Get a random integer between from and to.
!
INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue3(obj, from, to) RESULT(ans)
    CLASS(Random_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: from, to
    INTEGER(I4B) :: ans
  END FUNCTION obj_RandomValue3
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: select random integer from a vector
!
!# RandomValue
!
! Select a random integer from a vector.
INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue4(obj, val) RESULT(ans)
    CLASS(Random_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: val(:)
    INTEGER(I4B) :: ans
  END FUNCTION obj_RandomValue4
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                 RandomValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: select a random integer from an array.
!
!# RandomValue
!
! Get a random integer value from an array.
!
INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue5(obj, val) RESULT(ans)
    CLASS(Random_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: val(:, :)
    INTEGER(I4B) :: ans
  END FUNCTION obj_RandomValue5
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Select a random real number from a vector
!
!# RandomValue
!
! Get a random real number from a vector.
!
INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue6(obj, val) RESULT(ans)
    CLASS(Random_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    REAL(DFP) :: ans
  END FUNCTION obj_RandomValue6
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                 RandomValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: Select a random real number from a array.
!
!# RandomValue
!
! Select a random real number from a array.
!
INTERFACE RandomValue
  MODULE FUNCTION obj_RandomValue7(obj, val) RESULT(ans)
    CLASS(Random_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    REAL(DFP) :: ans
  END FUNCTION obj_RandomValue7
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                 SaveRandom
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE SaveRandom(obj)
    CLASS(Random_), INTENT(INOUT) :: obj
  END SUBROUTINE SaveRandom
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: a unit pseudorandom real vector
!
!# Introduction
!
! This subroutine is taken from rvec_uniform_01 of John Burkardt

INTERFACE UniformRandomValue
  MODULE PURE FUNCTION UniformRandomValue1(n, seed) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! the number of entries in the vector
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    !! the seed value, which should not be 0. On output seed value is
    !! updated. The default value of seed is 1
    REAL(DFP) :: ans(n)
    !! the vector of pseudorandom values
  END FUNCTION UniformRandomValue1
END INTERFACE UniformRandomValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: returns a pseudorandom vector between a and b

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
! summary: returns a unit pseudorandom

INTERFACE UniformRandomValueScalar
  MODULE PURE FUNCTION UniformRandomValueScalar1(seed) RESULT(ans)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    !! "seed" value, which should, NOT be 0.
    !! On output, SEED has been updated.
    REAL(DFP) :: ans
    !! a new pseudorandom variate, strictly between 0 and 1.
  END FUNCTION UniformRandomValueScalar1
END INTERFACE UniformRandomValueScalar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-26
! summary: returns a uniformly random unit vector

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
! summary: Samples the unit normal probability distribution.
!
!# Introduction
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!    This routine can generate a vector of values on one call.  It
!    has the feature that it should provide the same results
!    in the same order no matter how we break up the task.
!
!    The Box-Muller method is used, which is efficient, but
!    generates an even number of values each time.  On any call
!    to this routine, an even number of new values are generated.
!    Depending on the situation, one value may be left over.
!    In that case, it is saved for the next call.

!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of values desired.  If N is
!    negative, then the code will flush its internal memory; in particular,
!    if there is a saved value to be used on the next call, it is
!    instead discarded.  This is useful if the user has reset the
!    random number seed, for instance.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X(N), a sample of the standard normal PDF.
!
!  Local parameters:
!
!    Local, integer MADE, records the number of values that have
!    been computed.  On input with negative N, this value overwrites
!    the return value of N, so the user can get an accounting of
!    how much work has been done.
!
!    Local, real ( kind = 8 ) R(N+1), is used to store some uniform
!    random values.  Its dimension is N+1, but really it is only needed
!    to be the smallest even number greater than or equal to N.
!
!    Local, integer SAVED, is 0 or 1 depending on whether there is a
!    single saved value left over from the previous call.
!
!    Local, integer X_LO_INDEX, X_HI_INDEX, records the range of entries of
!    X that we need to compute.  This starts off as 1:N, but is adjusted
!    if we have a saved value that can be immediately stored in X(1),
!    and so on.
!
!    Local, real ( kind = 8 ) Y, the value saved from the previous call, if
!    SAVED is 1.

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

END MODULE Random_Method
