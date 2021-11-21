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

MODULE PROCEDURE Space_MassMatrix
! Define internal variable
REAL(DFP), ALLOCATABLE :: RealVal(:), RhoBar(:)
INTEGER(I4B) :: nns1, nns2, nips, ips
LOGICAL(LGT) :: isNodal
  !! main
nns1 = SIZE(Test%N, 1)
nns2 = SIZE(Trial%N, 1)
ALLOCATE (Ans(nns1, nns2))
Ans = 0.0_DFP
nips = SIZE(Trial%N, 2)
ALLOCATE (RhoBar(nips))
  !!
  !! making rho at quadrature
  !!
IF (PRESENT(Rho)) THEN
  CALL getInterpolation(obj=Trial, Val=Rho, Interpol=RhoBar)
ELSE
  RhoBar = 1.0_DFP
END IF
  !!
  !! performing scalar computation
  !!
RealVal = Trial%Js * Trial%Ws * Trial%Thickness * RhoBar
DEALLOCATE (RhoBar)
  !!
  !! performing outerproduct
  !!
DO ips = 1, nips
  Ans = Ans + &
    & OUTERPROD(a=Test%N(:, ips), b=Trial%N(:, ips)) &
    & * RealVal(ips)
END DO
IF (ALLOCATED(RealVal)) DEALLOCATE (RealVal)
  !!
  !! making n-diagonal copies
  !!
IF (PRESENT(nCopy)) THEN
  CALL MakeDiagonalCopies(Ans, nCopy)
END IF

END PROCEDURE Space_MassMatrix

!----------------------------------------------------------------------------
!                                                                  MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE st_massMatrix_a
REAL(DFP), ALLOCATABLE :: Mat4(:, :, :, :)
REAL(DFP), ALLOCATABLE :: RhoBar(:, :)
REAL(DFP), ALLOCATABLE ::RealVal(:)
INTEGER(I4B) :: a, b, ipt, ips
!! main
CALL Reallocate( Mat4, SIZE(Test(1)%N, 1),  SIZE(Trial(1)%N, 1), &
     & SIZE(Test(1)%T), SIZE(Trial(1)%T) )
CALL Reallocate( RhoBar, SIZE(Trial(1)%N, 2), SIZE(Trial) )
!!
!! get Rho at the quadrature points
!!
IF (PRESENT(Rho)) THEN
  CALL getInterpolation(obj=Trial, Val=Rho, Interpol=RhoBar)
ELSE
  RhoBar = 1.0_DFP
END IF
!!
!! $$\int_{\Omega } N^{I}T_{a}\rho N^{J}T_{b}d\Omega$$
!!
IF (Term1 .EQ. 0 .AND. Term2 .EQ. 0) THEN
  DO ipt = 1, SIZE(Trial)
    RealVal = Trial(ipt)%Js * Trial(ipt)%Ws * Trial(ipt)%Thickness &
     & * RhoBar(:, ipt) * Trial(ipt)%Wt * Trial(ipt)%Jt
    DO ips = 1, SIZE(Trial(1)%N, 2)
      DO b = 1, SIZE(Trial(1)%T)
        DO a = 1, SIZE(Test(1)%T)
          Mat4(:, :, a, b) = Mat4(:, :, a, b) &
            & + RealVal(ips) &
            & * Test(ipt)%T(a) &
            & * Trial(ipt)%T(b) &
            & * OUTERPROD(a=Test(ipt)%N(:, ips), b=Trial(ipt)%N(:, ips))
        END DO
      END DO
    END DO
  END DO
!!
!!  $$\int \frac{\partial N^{I}T_{a}}{\partial t} \rho
!! \frac{\partial N^{J}T_{b}}{\partial t} d\Omega dt$$
!!
ELSE IF (Term1 .EQ. 1 .AND. Term2 .EQ. 1) THEN
  DO ipt = 1, SIZE(Trial)
    RealVal = Trial(ipt)%Js * Trial(ipt)%Ws * Trial(ipt)%Thickness &
     & * RhoBar(:, ipt) * Trial(ipt)%Wt * Trial(ipt)%Jt
    DO ips = 1, SIZE(Trial(1)%N, 2)
      DO b = 1, SIZE(Trial(1)%T)
        DO a = 1, SIZE(Test(1)%T)
          Mat4(:, :, a, b) = Mat4(:, :, a, b) &
            & + RealVal(ips) &
            & * OUTERPROD(a=Test(ipt)%dNTdt(:, a, ips), &
                & b=Trial(ipt)%dNTdt(:, b, ips))
        END DO
      END DO
    END DO
 END DO
!!
!! $$\int \frac{\partial N^{I}T_{a}}{\partial t} \rho N^{J}T_{b}d\Omega dt$$
!!
ELSE IF (Term1 .EQ. 0 .AND. Term2 .EQ. 1) THEN
  DO ipt = 1, SIZE(Trial)
    RealVal = Trial(ipt)%Js * Trial(ipt)%Ws * Trial(ipt)%Thickness &
     & * RhoBar(:, ipt) * Trial(ipt)%Wt * Trial(ipt)%Jt
    DO ips = 1, SIZE(Trial(1)%N, 2)
      DO b = 1, SIZE(Trial(1)%T)
        DO a = 1, SIZE(Test(1)%T)
          Mat4(:, :, a, b) = Mat4(:, :, a, b) &
            & + RealVal(ips) &
            & * Test(ipt)%T(a) &
            & * OUTERPROD(a=Test(ipt)%N(:, ips), &
                & b=Trial(ipt)%dNTdt(:, b, ips))
        END DO
      END DO
    END DO
  END DO
!!
!! $$\int N^{I}T_{a}\rho \frac{\partial N^{J}T_{b}}{\partial t} d\Omega dt$$
!!
ELSE IF (Term1 .EQ. 1 .AND. Term2 .EQ. 0) THEN
  DO ipt = 1, SIZE(Trial)
    RealVal = Trial(ipt)%Js * Trial(ipt)%Ws * Trial(ipt)%Thickness &
     & * RhoBar(:, ipt) * Trial(ipt)%Wt * Trial(ipt)%Jt
    DO ips = 1, SIZE(Trial(1)%N, 2)
      DO b = 1, SIZE(Trial(1)%T)
        DO a = 1, SIZE(Test(1)%T)
          Mat4(:, :, a, b) = Mat4(:, :, a, b) &
            & + RealVal(ips) &
            & * Trial(ipt)%T(b) &
            & * OUTERPROD(a=Test(ipt)%dNTdt(:, a, ips), &
                & b=Trial(ipt)%N(:, ips))
        END DO
      END DO
    END DO
  END DO
END IF
!!
!! Convert mat4 to mat2
!!
CALL Convert(From=Mat4, To=Ans)
!!
!! Make diagonal copies
!!
IF (PRESENT(nCopy)) THEN
  CALL MakeDiagonalCopies(Ans, nCopy)
END IF
!!
DEALLOCATE (Mat4, RhoBar, RealVal)
END PROCEDURE st_massMatrix_a

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MassMatrixMethods
