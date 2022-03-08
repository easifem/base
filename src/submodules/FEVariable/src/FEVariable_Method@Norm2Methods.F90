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

SUBMODULE(FEVariable_Method) Norm2Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_norm2
!! Internal variable
REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), m2(:,:), r3(:, :, :), m3(:,:,:)
INTEGER(I4B) :: jj, kk
!!
!! main
!!
SELECT CASE (obj%vartype)
!!
!!
!!
!!
CASE (constant)
  !!
  IF( obj%defineon .EQ. nodal ) THEN
    ans = NodalVariable( &
      & NORM2(obj%val(:)), &
      & typeFEVariableScalar, &
      & typeFEVariableConstant)
  ELSE
    ans = QuadratureVariable( &
      & NORM2(obj%val(:)), &
      & typeFEVariableScalar, &
      & typeFEVariableConstant)
  END IF
!!
!!
!!
!!
CASE (space)
  !!
  r2 = GET(obj, TypeFEVariableVector, TypeFEVariableSpace)
  CALL Reallocate(r1, size(r2,2))
  DO jj = 1, size(r1)
    r1(jj) = NORM2(r2(:,jj))
  END DO
  !!
  IF( obj%defineon .EQ. nodal ) THEN
    ans = NodalVariable(&
      & r1, &
      & typeFEVariableScalar, &
      & typeFEVariableSpace)
  ELSE
    ans = QuadratureVariable(&
      & r1, &
      & typeFEVariableScalar, &
      & typeFEVariableSpace)
  END IF
!!
!!
!!
!!
CASE (time)
  !!
  r2 = GET(obj, TypeFEVariableVector, TypeFEVariableTime)
  CALL Reallocate(r1, size(r2,2))
  DO jj = 1, size(r1)
    r1(jj) = NORM2(r2(:,jj))
  END DO
  !!
  IF( obj%defineon .EQ. nodal ) THEN
    ans = NodalVariable(&
      & r1, &
      & typeFEVariableScalar, &
      & typeFEVariableTime)
  ELSE
    ans = QuadratureVariable(&
      & r1, &
      & typeFEVariableScalar, &
      & typeFEVariableTime)
  END IF
!!
!!
!!
!!
CASE (spacetime)
  !!
  r3 = GET(obj, TypeFEVariableVector, TypeFEVariableSpaceTime)
  CALL Reallocate( r2, size(r3,2), size(r3,3) )
  !!
  DO kk = 1, SIZE(r3, 3)
    DO jj = 1, SIZE(r3, 2)
      r2(jj, kk) = NORM2(r3(:, jj, kk))
    END DO
  END DO
  !!
  IF( obj%defineon .EQ. nodal ) THEN
    ans = NodalVariable(&
      & r2, &
      & typeFEVariableScalar, &
      & typeFEVariableSpaceTime)
  ELSE
    ans = QuadratureVariable(&
      & r2, &
      & typeFEVariableScalar, &
      & typeFEVariableSpaceTime)
  END IF
  !!
END SELECT
!!
!!
!!
!!
END PROCEDURE fevar_norm2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Norm2Methods
