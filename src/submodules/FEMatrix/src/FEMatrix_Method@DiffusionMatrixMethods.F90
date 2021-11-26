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

SUBMODULE(FEMatrix_Method) DiffusionMatrixMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_1
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: i
!! main
call reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
realval = trial%js * trial%ws * trial%thickness
DO i = 1, SIZE(trial%N, 2)
   ans = ans + realval(i) * matmul(test%dNdXt(:,:,i), &
        & transpose(trial%dNdXt(:,:,i)))
  ! DO j = 1, trial%refelem%NSD
  !   ans = ans + &
  !     & OUTERPROD(a=test%dNdXt(:, j, i), b= &
  !     & trial%dNdXt(:, j, i)) &
  !     & * realval(i)
  ! END DO
END DO
DEALLOCATE (realval)
END PROCEDURE DiffusionMatrix_1

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_2
REAL(DFP), ALLOCATABLE :: KBar(:, :, :), realval(:)
INTEGER(I4B) :: i
INTEGER(I4B), ALLOCATABLE :: S(:)

S = SHAPE(K)
ALLOCATE (KBar(S(1), S(2), SIZE(trial%N, 2)))
CALL getInterpolation(obj=trial, Interpol=KBar, Val=K)
realval = trial%Js * trial%Ws * trial%Thickness
ALLOCATE (ans(SIZE(test%N, 1), SIZE(trial%N, 1)))
ans = 0.0_DFP
DO i = 1, SIZE(trial%N, 2)
  ans = ans + realval(i) * MATMUL( &
    & MATMUL(test%dNdXt(:, :, i), KBar(:, :, i)), &
    & TRANSPOSE(trial%dNdXt(:, :, i)))
END DO
DEALLOCATE (KBar, realval, S)
END PROCEDURE DiffusionMatrix_2

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_3
REAL(DFP), ALLOCATABLE :: C1Bar(:, :), C2Bar(:, :), realval(:)
INTEGER(I4B) :: i

CALL getProjectionOfdNdXt(obj=test, cdNdXt=C1Bar, Val=C1)
CALL getProjectionOfdNdXt(obj=trial, cdNdXt=C2Bar, Val=C2)
realval = trial%Js * trial%Ws * trial%Thickness
ALLOCATE (ans(SIZE(test%N, 1), SIZE(trial%N, 1)))
ans = 0.0_DFP
DO i = 1, SIZE(trial%N, 2)
  ans = ans + realval(i) * OUTERPROD(C1Bar(:, i), C2Bar(:, i))
END DO
DEALLOCATE (realval, C1Bar, C2Bar)
END PROCEDURE DiffusionMatrix_3

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat2_STDiffusionMatrix_1
REAL(DFP), ALLOCATABLE :: Mat4(:, :, :, :), realval(:)
INTEGER(I4B) :: ips, ipt, a, b

ALLOCATE (Mat4(SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T)))
Mat4 = 0.0_DFP

DO ipt = 1, SIZE(trial)
  realval = trial(ipt)%Js * trial(ipt)%Ws * trial(ipt)%Thickness &
   & * trial(ipt)%Wt * trial(ipt)%Jt
  DO ips = 1, SIZE(test)
    DO b = 1, SIZE(trial(1)%T)
      DO a = 1, SIZE(test(1)%T)
        Mat4(:, :, a, b) = Mat4(:, :, a, b) + realval(ips) &
          & * MATMUL(test(ipt)%dNTdXt(:, a, :, ips), &
          & TRANSPOSE(trial(ipt)%dNTdXt(:, b, :, ips)))
      END DO
    END DO
  END DO
END DO
CALL Convert(From=Mat4, To=ans)
DEALLOCATE (Mat4, realval)
END PROCEDURE mat2_STDiffusionMatrix_1

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat2_STDiffusionMatrix_2
REAL(DFP), ALLOCATABLE :: Mat4(:, :, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: Dummy(:, :)
REAL(DFP), ALLOCATABLE :: KBar(:, :, :, :)
INTEGER(I4B) :: ips, ipt, a, b

ALLOCATE (Mat4(SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T)))
Mat4 = 0.0_DFP
CALL getInterpolation(obj=trial, Val=K, Interpol=KBar)
DO ipt = 1, SIZE(trial)
  realval = trial(ipt)%Js * trial(ipt)%Ws * trial(ipt)%Thickness &
   & * trial(ipt)%Wt * trial(ipt)%Jt
  DO ips = 1, SIZE(test)
    DO b = 1, SIZE(trial(1)%T)
      Dummy = realval(ips) * MATMUL(KBar(:, :, ips, ipt), &
        & TRANSPOSE(trial(ipt)%dNTdXt(:, b, :, ips)))
      DO a = 1, SIZE(test(1)%T)
        Mat4(:, :, a, b) = Mat4(:, :, a, b) +  &
          & MATMUL(test(ipt)%dNTdXt(:, a, :, ips), Dummy)
      END DO
    END DO
  END DO
END DO
CALL Convert(From=Mat4, To=ans)
DEALLOCATE (Mat4, realval, KBar, Dummy)
END PROCEDURE mat2_STDiffusionMatrix_2

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat2_STDiffusionMatrix_3
REAL(DFP), ALLOCATABLE :: Mat4(:, :, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: c1dNTdXt(:, :, :), c2dNTdXT(:, :, :)
INTEGER(I4B) :: ips, ipt, a, b

ALLOCATE ( &
  & Mat4(SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T)))
Mat4 = 0.0_DFP
DO ipt = 1, SIZE(trial)
  realval = trial(ipt)%Js * trial(ipt)%Ws * trial(ipt)%Thickness &
   & * trial(ipt)%Wt * trial(ipt)%Jt
  CALL getProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=c1dNTdXt, Val=C1)
  CALL getProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=c2dNTdXt, Val=C2)
  DO ips = 1, SIZE(test)
    DO b = 1, SIZE(trial(1)%T)
      DO a = 1, SIZE(test(1)%T)
        Mat4(:, :, a, b) = Mat4(:, :, a, b) +  &
          & realval(ips) * &
          & OUTERPROD(a=c1dNTdXt(:, a, ips), b=c2dNTdXt(:, b, ips))
      END DO
    END DO
  END DO
END DO
CALL Convert(From=Mat4, To=ans)
DEALLOCATE (Mat4, realval, c1dNTdXt, c2dNTdXT)
END PROCEDURE mat2_STDiffusionMatrix_3

END SUBMODULE DiffusionMatrixMethods
