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
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

PUBLIC :: Initiate
PUBLIC :: RandomValue
PUBLIC :: SaveRandom
PUBLIC :: uniformRandom
PUBLIC :: rvec_uniform_01
PUBLIC :: rvec_uniform_ab
PUBLIC :: rvec_uniform_unit
PUBLIC :: rvec_normal_01
PUBLIC :: r8_uniform_01

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE initRandom(obj)
    CLASS(Random_), INTENT(INOUT) :: obj
  END SUBROUTINE initRandom
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initRandom
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                                 getRandom
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getRandom(obj, distribution) RESULT(Ans)
    CLASS(Random_), INTENT(IN) :: obj
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: distribution
    REAL(DFP) :: Ans
  END FUNCTION getRandom
END INTERFACE

INTERFACE RandomValue
  MODULE PROCEDURE getRandom
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
!                                                             UniformRandom
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION uniformRandom(obj, From, To) RESULT(Ans)
    CLASS(Random_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: From, To
    REAL(DFP) :: Ans
  END FUNCTION uniformRandom
END INTERFACE

INTERFACE RandomValue
  MODULE PROCEDURE uniformRandom
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                             RandomInteger
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getRandomInteger(obj, From, To) RESULT(Ans)
    CLASS(Random_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: From, To
    INTEGER(I4B) :: Ans
  END FUNCTION getRandomInteger
END INTERFACE

INTERFACE RandomValue
  MODULE PROCEDURE getRandomInteger
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION select_random_int_from_vec(obj, Val) RESULT(Ans)
    CLASS(Random_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: Val(:)
    INTEGER(I4B) :: Ans
  END FUNCTION select_random_int_from_vec
END INTERFACE

INTERFACE
  MODULE FUNCTION select_random_int_from_array(obj, Val) RESULT(Ans)
    CLASS(Random_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: Val(:, :)
    INTEGER(I4B) :: Ans
  END FUNCTION select_random_int_from_array
END INTERFACE

INTERFACE
  MODULE FUNCTION select_random_real_from_vec(obj, Val) RESULT(Ans)
    CLASS(Random_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: Val(:)
    REAL(DFP) :: Ans
  END FUNCTION select_random_real_from_vec
END INTERFACE

INTERFACE
  MODULE FUNCTION select_random_real_from_array(obj, Val) RESULT(Ans)
    CLASS(Random_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
    REAL(DFP) :: Ans
  END FUNCTION select_random_real_from_array
END INTERFACE

INTERFACE RandomValue
  MODULE PROCEDURE select_random_int_from_vec, select_random_int_from_array,&
    & select_random_real_from_vec, select_random_real_from_array
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         28 Aug 2022
! summary: a unit pseudorandom real vector
!
!# Introduction
!
! This subroutine is taken from rvec_uniform_01 of John Burkardt
!
!    An rvec is a vector of real ( kind = 8 ) values.
!
!    For now, the input quantity SEED is an integer ( kind = 4 ) variable.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.

INTERFACE
  MODULE PURE FUNCTION rvec_uniform_01(n, seed) RESULT(r)
    INTEGER(I4B), INTENT(IN) :: n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    REAL(DFP) :: r(n)
  END FUNCTION rvec_uniform_01
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary: returns a scaled pseudorandom rvec

INTERFACE
  MODULE PURE FUNCTION rvec_uniform_ab(n, a, b, seed) RESULT(r)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: a
    REAL(DFP), INTENT(IN) :: b
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    REAL(DFP) :: r(n)
  END FUNCTION rvec_uniform_ab
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary:         returns a uniformly random unit vector

INTERFACE
  MODULE PURE FUNCTION rvec_uniform_unit(m, seed) RESULT(w)
    INTEGER(I4B), INTENT(IN) :: m
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    REAL(DFP) :: w(m)
  END FUNCTION rvec_uniform_unit
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary:Samples the unit normal probability distribution.
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

INTERFACE
  MODULE PURE FUNCTION rvec_normal_01(n, seed) RESULT(x)
    INTEGER(I4B), INTENT(IN) :: n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    REAL(DFP) :: x(n)
  END FUNCTION rvec_normal_01
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary:  returns a unit pseudorandom
!
!# Introduction
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer ( kind = 4 ) variable.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2^31 - 1 )
!      r8_uniform_01 = seed / ( 2^31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      R8_UNIFORM_01
!      SEED      SEED
!
!         12345   207482415  0.096616
!     207482415  1790989824  0.833995
!    1790989824  2035175616  0.947702
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0. On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.

INTERFACE
  MODULE PURE FUNCTION r8_uniform_01(seed) RESULT(ans)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    REAL(DFP) :: ans
  END FUNCTION r8_uniform_01
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Random_Method
