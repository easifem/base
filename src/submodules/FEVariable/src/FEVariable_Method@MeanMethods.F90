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

SUBMODULE(FEVariable_Method) MeanMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Mean1
  REAL( DFP ) :: val0
  REAL( DFP ), ALLOCATABLE :: val1( : ), val2( :, : )
  !!
  SELECT CASE (obj%rank)
  !!
  !! Scalar
  !!
  CASE (SCALAR)
    !!
    IF( obj%defineOn .EQ. NODAL ) THEN
      ans = NodalVariable( MEAN( obj, TypeFEVariableScalar ), &
        & TypeFEVariableScalar, &
        & TypeFEVariableConstant )
    ELSE
      ans = QuadratureVariable( MEAN( obj, TypeFEVariableScalar ), &
        & TypeFEVariableScalar, &
        & TypeFEVariableConstant )
    END IF
  !!
  !! Vector
  !!
  CASE (VECTOR)
    !!
    IF( obj%defineOn .EQ. NODAL ) THEN
      ans = NodalVariable( MEAN( obj, TypeFEVariableVector ), &
        & TypeFEVariableVector, &
        & TypeFEVariableConstant )
    ELSE
      ans = QuadratureVariable( MEAN( obj, TypeFEVariableVector ), &
        & TypeFEVariableVector, &
        & TypeFEVariableConstant )
    END IF
    !!
  CASE (MATRIX)
    !!
    IF( obj%defineOn .EQ. NODAL ) THEN
      ans = NodalVariable( MEAN( obj, TypeFEVariableMatrix ), &
        & TypeFEVariableMatrix, &
        & TypeFEVariableConstant )
    ELSE
      ans = QuadratureVariable( MEAN( obj, TypeFEVariableMatrix ), &
        & TypeFEVariableMatrix, &
        & TypeFEVariableConstant )
    END IF
    !!
  END SELECT
  !!
END PROCEDURE fevar_Mean1

!----------------------------------------------------------------------------
!                                                                  Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Mean2
  REAL( DFP ) :: val0
  !!
  ans = SUM( obj%val( : ) ) / SIZE( obj%val )
  !!
END PROCEDURE fevar_Mean2

!----------------------------------------------------------------------------
!                                                                  Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Mean3
  REAL( DFP ), ALLOCATABLE :: val2( :, : ), val3( :, :, : )
  INTEGER( I4B ) :: ii, jj
  !!
  CALL Reallocate( ans, obj%s(1) )
  !!
  SELECT CASE( obj%varType )
    !!
  CASE( Constant )
    !!
    ans = obj%val( : )
    !!
  CASE( Space, Time )
    !!
    val2 = RESHAPE( obj%val, obj%s(1:2) )
    !!
    DO ii = 1, obj%s(2)
      ans = ans + val2( :, ii )
    END DO
    !!
    ans = ans / obj%s(2)
    !!
  CASE( SpaceTime )
    !!
    val3 = RESHAPE( obj%val, obj%s(1:3) )
    DO jj = 1, obj%s(3)
      DO ii = 1, obj%s(2)
        ans = ans + val3( :, ii, jj )
      END DO
    END DO
    !!
    ans = ans / obj%s(2) / obj%s(3)
    !!
  END SELECT
  !!
  IF( ALLOCATED( val2 ) ) DEALLOCATE( val2 )
  IF( ALLOCATED( val3 ) ) DEALLOCATE( val3 )
  !!
END PROCEDURE fevar_Mean3

!----------------------------------------------------------------------------
!                                                                  Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Mean4
  REAL( DFP ), ALLOCATABLE :: val3( :, :, : ), val4( :, :, :, : )
  INTEGER( I4B ) :: ii, jj
  !!
  CALL Reallocate( ans, obj%s(1), obj%s(2) )
  !!
  SELECT CASE( obj%varType )
    !!
  CASE( Constant )
    !!
    ans = RESHAPE( obj%val, obj%s(1:2) )
    !!
  CASE( Space, Time )
    !!
    val3 = RESHAPE( obj%val, obj%s(1:3) )
    !!
    DO ii = 1, obj%s(3)
      ans = ans + val3( :, :, ii )
    END DO
    !!
    ans = ans / obj%s(3)
    !!
  CASE( SpaceTime )
    !!
    val4 = RESHAPE( obj%val, obj%s(1:4) )
    !!
    DO jj = 1, obj%s(4)
      DO ii = 1, obj%s(3)
        ans = ans + val4( :, :, ii, jj )
      END DO
    END DO
    !!
    ans = ans / obj%s(3) / obj%s(4)
    !!
  END SELECT
  !!
  IF( ALLOCATED( val3 ) ) DEALLOCATE( val3 )
  IF( ALLOCATED( val4 ) ) DEALLOCATE( val4 )
  !!
END PROCEDURE fevar_Mean4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MeanMethods
