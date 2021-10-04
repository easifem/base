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

SUBMODULE(Random_Method) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE initRandom
  INTEGER( I4B ):: SeedSize

  CALL RANDOM_SEED (size = SeedSize )
  IF( .NOT. ALLOCATED( obj%random_int_seed ) ) THEN
    ALLOCATE( obj%random_int_seed(SeedSize) )
  ENDIF
  call RANDOM_SEED( get=obj%random_int_seed )
END PROCEDURE initRandom

!----------------------------------------------------------------------------
!                                                                 getRandom
!----------------------------------------------------------------------------

MODULE PROCEDURE getRandom
  REAL( DFP ) :: val, y
  INTEGER( I4B ) :: i

  IF( PRESENT( distribution ) ) THEN
    SELECT CASE( TRIM( distribution ) )
    CASE( "Binomial", "binomial" )
      val=0.0d0
      DO i = 1, 20
        CALL RANDOM_NUMBER( y )
        val = val + y
      ENDDO
      Ans = val - 10.0_DFP
    CASE DEFAULT
      CALL RANDOM_NUMBER( Ans )
    END SELECT
  ELSE
    CALL RANDOM_NUMBER( Ans )
  END IF

END PROCEDURE getRandom

!----------------------------------------------------------------------------
!                                                                 SaveRandom
!----------------------------------------------------------------------------

MODULE PROCEDURE SaveRandom
  CALL RANDOM_SEED( put=obj%Random_INT_SEED )
END PROCEDURE SaveRandom

!----------------------------------------------------------------------------
!                                                              UniformRandom
!----------------------------------------------------------------------------

MODULE PROCEDURE uniformRandom
  REAL( DFP ) :: a, diff, val(2)

  val(1)=From
  val(2)=To
  diff=abs(From-To)
  CALL RANDOM_NUMBER( a )
  Ans= a * diff + minval(val)
END PROCEDURE uniformRandom

!----------------------------------------------------------------------------
!                                                           getRandomInteger
!----------------------------------------------------------------------------

MODULE PROCEDURE getRandomInteger
  REAL( DFP ) :: xr, a, diff, val(2)

  val(1)=From
  val(2)=To
  diff=abs(dble(from)- dble(to) )

  call random_number(a)
  xr=a*diff+minval(val)
  Ans=nint(xr)
  if(Ans==From-1)then
      Ans=From
  endif
  if(Ans==To+1)then
      Ans=To
  endif
END PROCEDURE getRandomInteger

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE select_random_int_from_vec
  INTEGER( I4B ) :: posi
  posi = getRandomInteger( obj, From=1, To=size(Val) )
  Ans=Val(posi)
END PROCEDURE select_random_int_from_vec

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE select_random_int_from_array
  INTEGER( I4B ) :: i1, i2
  i1 = getRandomInteger( obj, From=1, To=SIZE(Val,1) )
  i2 = getRandomInteger( obj, From=1, To=SIZE(Val,2) )
  Ans = Val( i1, i2 )
END PROCEDURE select_random_int_from_array

!----------------------------------------------------------------------------
!                                                                 RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE select_random_real_from_vec
  INTEGER( I4B ) :: posi
  posi = getRandomInteger( obj, From=1, To=size(Val) )
  Ans=Val(posi)
END PROCEDURE select_random_real_from_vec

!----------------------------------------------------------------------------
!                                                                 RandomValue
!----------------------------------------------------------------------------

MODULE PROCEDURE select_random_real_from_array
  INTEGER( I4B ) :: i1, i2
  i1 = getRandomInteger( obj, From=1, To=SIZE(Val,1) )
  i2 = getRandomInteger( obj, From=1, To=SIZE(Val,2) )
  Ans = Val( i1, i2 )
END PROCEDURE select_random_real_from_array

END SUBMODULE Constructor