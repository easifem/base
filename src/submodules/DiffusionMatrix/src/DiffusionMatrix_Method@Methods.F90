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

SUBMODULE(DiffusionMatrix_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_1
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ii
!! main
CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
realval = trial%js * trial%ws * trial%thickness
DO ii = 1, SIZE(trial%N, 2)
  ans = ans + realval(ii) * MATMUL(test%dNdXt(:, :, ii), &
    & TRANSPOSE(trial%dNdXt(:, :, ii)))
END DO
!!
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
!!
DEALLOCATE (realval)
END PROCEDURE DiffusionMatrix_1

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_2
  ! CALL DM_1(ans=ans, test=test, trial=trial, k=k, opt=opt)
  !!
  REAL(DFP), ALLOCATABLE :: realval(:), kbar(:)
  INTEGER(I4B) :: ii
  !! main
  CALL getInterpolation(obj=trial, Interpol=kbar, val=k)
  !!
  realval = trial%js * trial%ws * trial%thickness * kbar
  !!
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  !!
  DO ii = 1, SIZE(realval)
    !!
    ans = ans + realval(ii) * MATMUL(test%dNdXt(:, :, ii), &
      & TRANSPOSE(trial%dNdXt(:, :, ii)))
    !!
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (kbar, realval)
END PROCEDURE DiffusionMatrix_2

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_3
  ! CALL DM_2(ans=ans, test=test, trial=trial, k=k, opt=opt)
  !!
  REAL(DFP), ALLOCATABLE :: c1bar(:, :), c2bar(:, :), realval(:)
  INTEGER(I4B) :: ii
  !!
  !! main
  !!
  CALL getProjectionOfdNdXt(obj=test, cdNdXt=c1bar, val=k)
  !!
  CALL getProjectionOfdNdXt(obj=trial, cdNdXt=c2bar, val=k)
  !!
  realval = trial%js * trial%ws * trial%thickness
  !!
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  !!
  DO ii = 1, SIZE(realval)
    ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (c1bar, c2bar, realval)
END PROCEDURE DiffusionMatrix_3

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_4
  ! CALL DM_3(ans=ans, test=test, trial=trial, k=k, opt=opt)
  !! internal variable
  REAL(DFP), ALLOCATABLE :: kbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ii
  !! main
  CALL getInterpolation(obj=trial, Interpol=kbar, val=k)
  !!
  realval = trial%js * trial%ws * trial%thickness
  !!
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  !!
  DO ii = 1, SIZE(realval)
    ans = ans + realval(ii) * MATMUL(&
      & MATMUL(test%dNdXt(:, :, ii), kbar(:, :, ii)), &
      & TRANSPOSE(trial%dNdXt(:, :, ii)))
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (kbar, realval)
  !!
END PROCEDURE DiffusionMatrix_4
!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_5
  !! scalar
  !! scalar
  !! CALL DM_6(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  REAL(DFP), ALLOCATABLE :: realval(:), cbar(:)
  INTEGER(I4B) :: ii
  !! main
  CALL getInterpolation(obj=trial, Interpol=cbar, val=c1)
  CALL getInterpolation(obj=trial, Interpol=realval, val=c2)
  realval = realval * trial%js * trial%ws * trial%thickness * cbar
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  DO ii = 1, SIZE(realval)
    ans = ans + realval(ii) * MATMUL(test%dNdXt(:, :, ii), &
      & TRANSPOSE(trial%dNdXt(:, :, ii)))
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (cbar, realval)
END PROCEDURE DiffusionMatrix_5

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_6
  !! scalar
  !! vector
  !! CALL DM_7(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  !!
  REAL(DFP), ALLOCATABLE :: c1bar(:, :), c2bar(:, :), realval(:)
  INTEGER(I4B) :: ii
  !! main
  CALL getProjectionOfdNdXt(obj=test, cdNdXt=c1bar, val=c2)
  CALL getProjectionOfdNdXt(obj=trial, cdNdXt=c2bar, val=c2)
  CALL getInterpolation(obj=trial, interpol=realval, val=c1)
  realval = realval * trial%js * trial%ws * trial%thickness
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  DO ii = 1, SIZE(realval)
    ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (c1bar, c2bar, realval)
  !!
END PROCEDURE DiffusionMatrix_6

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_7
  !! scalar
  !! matrix
  !! CALL DM_5(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: kbar(:, :,:)
  INTEGER(I4B) :: ii
  !!
  !! main
  !!
  CALL getInterpolation(obj=trial, Interpol=realval, val=c1)
  !!
  CALL getInterpolation(obj=trial, Interpol=kbar, val=c2)
  !!
  realval =  realval * trial%js * trial%ws * trial%thickness
  !!
  DO ii = 1, SIZE(realval)
    ans = ans + realval(ii) * MATMUL(&
        & MATMUL(test%dNdXt(:, :, ii), kbar(:, :, ii)), &
        & TRANSPOSE(trial%dNdXt(:, :, ii)))
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE(realval, kbar)
  !!
END PROCEDURE DiffusionMatrix_7

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_8
  !! vector
  !! scalar
  ans = DiffusionMatrix(test=test, trial=trial, c1=c2, c2=c1, &
    & c1rank=TypeFEVariableScalar, c2rank=TypeFEVariableVector, &
    & opt=opt)
  !!
END PROCEDURE DiffusionMatrix_8

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_9
  !! vector
  !! vector
  !! CALL DM_4(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: c1bar(:, :), c2bar(:, :), realval(:)
  INTEGER(I4B) :: ii
  !!
  !!
  !!
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  !!
  CALL getProjectionOfdNdXt(obj=test, cdNdXt=c1bar, val=c1)
  !!
  CALL getProjectionOfdNdXt(obj=trial, cdNdXt=c2bar, val=c2)
  !!
  realval = trial%js * trial%ws * trial%thickness
  !!
  DO ii = 1, SIZE(realval)
    ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (c1bar, c2bar, realval)
  !!
END PROCEDURE DiffusionMatrix_9

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_10
  !! vector
  !! matrix
  !! CALL DM_10(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  !!
  !! internal variable
  REAL(DFP), ALLOCATABLE :: matbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: c1bar(:, :)
  REAL(DFP), ALLOCATABLE :: c2bar(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  TYPE(FEVariable_) :: k
  INTEGER(I4B) :: ii
  !! main
  CALL getInterpolation(obj=trial, interpol=c2bar, val=c1)
  CALL getInterpolation(obj=trial, interpol=matbar, val=c2)
  CALL Reallocate( c1bar, SIZE(matbar, 2),  SIZE(matbar, 3))
  !!
  DO ii = 1, SIZE(c2bar, 2)
    c1bar(:,ii) = MATMUL(c2bar(:,ii), matbar(:,:,ii))
  END DO
  !!
  k = QuadratureVariable(c1bar, typeFEVariableVector, typeFEVariableSpace )
  CALL getProjectionOfdNdXt(obj=test, cdNdXt=c1bar, val=k)
  CALL getProjectionOfdNdXt(obj=trial, cdNdXt=c2bar, val=k)
  realval = trial%js * trial%ws * trial%thickness
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  DO ii = 1, SIZE(realval)
    ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (c1bar, c2bar, realval, matbar)
  !!
END PROCEDURE DiffusionMatrix_10

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_11
  !! matrix
  !! scalar
  ! CALL DM_5(ans=ans, test=test, trial=trial, c1=c2, c2=c1, opt=opt)
  ans = DiffusionMatrix( test=test, trial=trial, c1=c2, c2=c1, &
    & c1rank=TypeFEVariableScalar, c2rank=TypeFEVariableMatrix, opt=opt )
  !!
END PROCEDURE DiffusionMatrix_11

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_12
  !! matrix
  !! vector
  !! CALL DM_9(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  !!
  !! internal variable
  REAL(DFP), ALLOCATABLE :: matbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: c1bar(:, :)
  REAL(DFP), ALLOCATABLE :: c2bar(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  TYPE(FEVariable_) :: k
  INTEGER(I4B) :: ii
  !! main
  CALL getInterpolation(obj=trial, interpol=matbar, val=c1)
  CALL getInterpolation(obj=trial, interpol=c2bar, val=c2)
  CALL Reallocate( c1bar, SIZE(matbar, 1),  SIZE(matbar, 3))
  !!
  DO ii = 1, SIZE(c2bar, 2)
    c1bar(:,ii) = MATMUL(matbar(:,:,ii), c2bar(:,ii))
  END DO
  !!
  k = QuadratureVariable(c1bar, typeFEVariableVector, typeFEVariableSpace )
  CALL getProjectionOfdNdXt(obj=test, cdNdXt=c1bar, val=k)
  CALL getProjectionOfdNdXt(obj=trial, cdNdXt=c2bar, val=k)
  realval = trial%js * trial%ws * trial%thickness
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  DO ii = 1, SIZE(realval)
    ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (c1bar, c2bar, realval, matbar)
  !!
END PROCEDURE DiffusionMatrix_12

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_13
  !! matrix
  !! matrix
  !! CALL DM_8(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  !!
  !! internal variable
  REAL(DFP), ALLOCATABLE :: k1bar(:, :, :), k2bar(:, :, :), realval(:)
  INTEGER(I4B) :: ii
  !! main
  CALL getInterpolation(obj=trial, Interpol=k1bar, val=c1)
  !!
  CALL getInterpolation(obj=trial, Interpol=k2bar, val=c2)
  !!
  realval = trial%js * trial%ws * trial%thickness
  !!
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  !!
  DO ii = 1, SIZE(realval)
    !!
    ans = ans + realval(ii) * MATMUL( &
        & MATMUL(test%dNdXt(:, :, ii),&
        & MATMUL(k1bar(:, :, ii), k2bar(:, :, ii))), &
        & TRANSPOSE(trial%dNdXt(:, :, ii)))
    !!
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (k1bar, k2bar, realval)
  !!
END PROCEDURE DiffusionMatrix_13

! !----------------------------------------------------------------------------
! !                                                            DiffusionMatrix
! !----------------------------------------------------------------------------

! MODULE PROCEDURE DiffusionMatrix_14
! REAL(DFP), ALLOCATABLE :: c1bar(:, :), c2bar(:, :), realval(:)
! INTEGER(I4B) :: ii
!   !! main
! CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
! CALL getProjectionOfdNdXt(obj=test, cdNdXt=c1bar, val=c1)
! CALL getProjectionOfdNdXt(obj=trial, cdNdXt=c2bar, val=c2)
! CALL getInterpolation(obj=trial, interpol=realval, val=c3)
! realval = trial%js * trial%ws * trial%thickness * realval
! DO ii = 1, SIZE(realval)
!   ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
! END DO
! DEALLOCATE (c1bar, c2bar, realval)
! END PROCEDURE DiffusionMatrix_14

END SUBMODULE Methods
