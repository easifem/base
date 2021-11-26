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

SUBMODULE(FEMatrix_Method) MassMatrixMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_1
!! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:), rhobar(:)
INTEGER(I4B) :: nips, ips
LOGICAL(LGT) :: isNodal
!! main
CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
nips = SIZE(trial%N, 2)
CALL reallocate(rhobar, nips)
!! making rho at quadrature
IF (PRESENT(rho)) THEN
  CALL getInterpolation(obj=trial, val=rho, interpol=rhobar)
ELSE
  rhobar = 1.0_DFP
END IF
!! performing scalar computation
realval = trial%js * trial%ws * trial%thickness * rhobar
!! performing outerproduct
DO ips = 1, nips
  ans = ans + &
    & OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips)) &
    & * realval(ips)
END DO
IF (ALLOCATED(realval)) DEALLOCATE (realval)
IF (ALLOCATED(rhobar)) DEALLOCATE (rhobar)
END PROCEDURE MassMatrix_1

!----------------------------------------------------------------------------
!                                                                  MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_2
REAL(DFP), ALLOCATABLE :: Mat4(:, :, :, :)
REAL(DFP), ALLOCATABLE :: rhobar(:, :)
REAL(DFP), ALLOCATABLE ::realval(:)
INTEGER(I4B) :: a, b, ipt, ips
!! main
CALL Reallocate( Mat4, SIZE(test(1)%N, 1),  SIZE(trial(1)%N, 1), &
     & SIZE(test(1)%T), SIZE(trial(1)%T) )
CALL Reallocate( rhobar, SIZE(trial(1)%N, 2), SIZE(trial) )
!! get rho at the quadrature points
IF (PRESENT(rho)) THEN
  CALL getInterpolation(obj=trial, val=rho, interpol=rhobar)
ELSE
  rhobar = 1.0_DFP
END IF
!! $$\int_{\Omega } N^{I}T_{a}\rho N^{J}T_{b}d\Omega$$
IF (Term1 .EQ. 0 .AND. Term2 .EQ. 0) THEN
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%Js * trial(ipt)%Ws * trial(ipt)%Thickness &
     & * rhobar(:, ipt) * trial(ipt)%Wt * trial(ipt)%Jt
    DO ips = 1, SIZE(trial(1)%N, 2)
      DO b = 1, SIZE(trial(1)%T)
        DO a = 1, SIZE(test(1)%T)
          Mat4(:, :, a, b) = Mat4(:, :, a, b) &
            & + realval(ips) &
            & * test(ipt)%T(a) &
            & * trial(ipt)%T(b) &
            & * OUTERPROD(a=test(ipt)%N(:, ips), b=trial(ipt)%N(:, ips))
        END DO
      END DO
    END DO
  END DO
!!  $$\int \frac{\partial N^{I}T_{a}}{\partial t} \rho
!! \frac{\partial N^{J}T_{b}}{\partial t} d\Omega dt$$
ELSE IF (Term1 .EQ. 1 .AND. Term2 .EQ. 1) THEN
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%Js * trial(ipt)%Ws * trial(ipt)%Thickness &
     & * rhobar(:, ipt) * trial(ipt)%Wt * trial(ipt)%Jt
    DO ips = 1, SIZE(trial(1)%N, 2)
      DO b = 1, SIZE(trial(1)%T)
        DO a = 1, SIZE(test(1)%T)
          Mat4(:, :, a, b) = Mat4(:, :, a, b) &
            & + realval(ips) &
            & * OUTERPROD(a=test(ipt)%dNTdt(:, a, ips), &
                & b=trial(ipt)%dNTdt(:, b, ips))
        END DO
      END DO
    END DO
 END DO
!! $$\int \frac{\partial N^{I}T_{a}}{\partial t} \rho N^{J}T_{b}d\Omega dt$$
ELSE IF (Term1 .EQ. 0 .AND. Term2 .EQ. 1) THEN
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%Js * trial(ipt)%Ws * trial(ipt)%Thickness &
     & * rhobar(:, ipt) * trial(ipt)%Wt * trial(ipt)%Jt
    DO ips = 1, SIZE(trial(1)%N, 2)
      DO b = 1, SIZE(trial(1)%T)
        DO a = 1, SIZE(test(1)%T)
          Mat4(:, :, a, b) = Mat4(:, :, a, b) &
            & + realval(ips) &
            & * test(ipt)%T(a) &
            & * OUTERPROD(a=test(ipt)%N(:, ips), &
                & b=trial(ipt)%dNTdt(:, b, ips))
        END DO
      END DO
    END DO
  END DO
!! $$\int N^{I}T_{a}\rho \frac{\partial N^{J}T_{b}}{\partial t} d\Omega dt$$
ELSE IF (Term1 .EQ. 1 .AND. Term2 .EQ. 0) THEN
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%Js * trial(ipt)%Ws * trial(ipt)%Thickness &
     & * rhobar(:, ipt) * trial(ipt)%Wt * trial(ipt)%Jt
    DO ips = 1, SIZE(trial(1)%N, 2)
      DO b = 1, SIZE(trial(1)%T)
        DO a = 1, SIZE(test(1)%T)
          Mat4(:, :, a, b) = Mat4(:, :, a, b) &
            & + realval(ips) &
            & * trial(ipt)%T(b) &
            & * OUTERPROD(a=test(ipt)%dNTdt(:, a, ips), &
                & b=trial(ipt)%N(:, ips))
        END DO
      END DO
    END DO
  END DO
END IF
!! Convert mat4 to mat2
CALL Convert(From=Mat4, To=ans)
DEALLOCATE (Mat4, rhobar, realval)
END PROCEDURE MassMatrix_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MassMatrixMethods
