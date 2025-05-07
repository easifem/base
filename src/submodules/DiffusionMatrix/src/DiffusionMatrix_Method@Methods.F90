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
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ii

CALL Reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
realval = trial%js * trial%ws * trial%thickness
DO ii = 1, SIZE(trial%N, 2)
  ans = ans + realval(ii) * MATMUL(test%dNdXt(:, :, ii), &
                                   TRANSPOSE(trial%dNdXt(:, :, ii)))
END DO
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (realval)
END PROCEDURE DiffusionMatrix_1

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix1_
REAL(DFP), PARAMETER :: one = 1.0_DFP
REAL(DFP) :: realval
INTEGER(I4B) :: ii, jj, ips, dim

nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, trial%nips
  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)

  DO dim = 1, trial%nsd
    CALL OuterProd_(a=test%dNdXt(1:nrow, dim, ips), &
                    b=trial%dNdXt(1:ncol, dim, ips), &
                    nrow=ii, ncol=jj, ans=ans, scale=realval, anscoeff=one)
  END DO

END DO

IF (PRESENT(opt)) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF

END PROCEDURE DiffusionMatrix1_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_2
REAL(DFP), ALLOCATABLE :: realval(:), kbar(:)
INTEGER(I4B) :: ii
CALL GetInterpolation(obj=trial, Interpol=kbar, val=k)
realval = trial%js * trial%ws * trial%thickness * kbar
CALL Reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
DO ii = 1, SIZE(realval)
  ans = ans + realval(ii) * MATMUL(test%dNdXt(:, :, ii), &
                                   TRANSPOSE(trial%dNdXt(:, :, ii)))
END DO
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (kbar, realval)
END PROCEDURE DiffusionMatrix_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix2_
REAL(DFP) :: realval, kbar(trial%nips)
INTEGER(I4B) :: ii

CALL GetInterpolation_(obj=trial, Interpol=kbar, val=k, tsize=ii)
nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = 0.0

DO ii = 1, trial%nips
  realval = trial%js(ii) * trial%ws(ii) * trial%thickness(ii) * kbar(ii)
  ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + &
                        realval * MATMUL(test%dNdXt(:, :, ii), &
                                         TRANSPOSE(trial%dNdXt(:, :, ii)))
END DO

IF (PRESENT(opt)) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF

END PROCEDURE DiffusionMatrix2_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_3
REAL(DFP), ALLOCATABLE :: c1bar(:, :), c2bar(:, :), realval(:)
INTEGER(I4B) :: ii
CALL getProjectionOfdNdXt(obj=test, cdNdXt=c1bar, val=k)
CALL getProjectionOfdNdXt(obj=trial, cdNdXt=c2bar, val=k)
realval = trial%js * trial%ws * trial%thickness
CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
DO ii = 1, SIZE(realval)
  ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
END DO
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (c1bar, c2bar, realval)
END PROCEDURE DiffusionMatrix_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix3_
REAL(DFP) :: c1bar(test%nns, test%nips), c2bar(trial%nns, trial%nips)
REAL(DFP) :: realval
INTEGER(I4B) :: ii, jj, kk
REAL(DFP), PARAMETER :: one = 1.0_DFP

CALL getProjectionOfdNdXt_(obj=test, cdNdXt=c1bar, val=k, nrow=nrow, ncol=ii)
CALL getProjectionOfdNdXt_(obj=trial, cdNdXt=c2bar, val=k, nrow=ncol, ncol=ii)

ans(1:nrow, 1:ncol) = 0.0

DO ii = 1, trial%nips
  realval = trial%js(ii) * trial%ws(ii) * trial%thickness(ii)
  CALL OuterProd_(a=c1bar(1:nrow, ii), b=c2bar(1:ncol, ii), &
                  nrow=jj, ncol=kk, ans=ans, &
                  scale=realval, anscoeff=one)
END DO

IF (PRESENT(opt)) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF
END PROCEDURE DiffusionMatrix3_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_4
! CALL DM_3(ans=ans, test=test, trial=trial, k=k, opt=opt)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ii
CALL getInterpolation(obj=trial, Interpol=kbar, val=k)
realval = trial%js * trial%ws * trial%thickness
CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
DO ii = 1, SIZE(realval)
  ans = ans + realval(ii) * MATMUL(&
    & MATMUL(test%dNdXt(:, :, ii), kbar(:, :, ii)), &
    & TRANSPOSE(trial%dNdXt(:, :, ii)))
END DO
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (kbar, realval)
END PROCEDURE DiffusionMatrix_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix4_
REAL(DFP) :: kbar(test%nsd, test%nsd, trial%nips)
REAL(DFP) :: realval
REAL(DFP), PARAMETER :: one = 1.0_DFP
INTEGER(I4B) :: ii, jj, kk

CALL getInterpolation_(obj=trial, Interpol=kbar, val=k, &
                       dim1=ii, dim2=jj, dim3=kk)
nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = 0.0

DO ii = 1, trial%nips
  realval = trial%js(ii) * trial%ws(ii) * trial%thickness(ii)
  ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + &
              realval * MATMUL(MATMUL(test%dNdXt(:, :, ii), kbar(:, :, ii)), &
                                         TRANSPOSE(trial%dNdXt(:, :, ii)))
END DO

IF (PRESENT(opt)) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF

END PROCEDURE DiffusionMatrix4_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_5
REAL(DFP), ALLOCATABLE :: realval(:), cbar(:)
INTEGER(I4B) :: ii
CALL getInterpolation(obj=trial, Interpol=cbar, val=c1)
CALL getInterpolation(obj=trial, Interpol=realval, val=c2)
realval = realval * trial%js * trial%ws * trial%thickness * cbar
CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
DO ii = 1, SIZE(realval)
  ans = ans + realval(ii) * MATMUL(test%dNdXt(:, :, ii), &
    & TRANSPOSE(trial%dNdXt(:, :, ii)))
END DO
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (cbar, realval)
END PROCEDURE DiffusionMatrix_5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix5_
REAL(DFP) :: realval(trial%nips), cbar(trial%nips)
INTEGER(I4B) :: ii

CALL getInterpolation_(obj=trial, Interpol=cbar, val=c1, tsize=ii)
CALL getInterpolation_(obj=trial, Interpol=realval, val=c2, tsize=ii)
realval = realval * trial%js * trial%ws * trial%thickness * cbar

nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = 0.0

DO ii = 1, trial%nips
  ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + &
                        realval(ii) * MATMUL(test%dNdXt(:, :, ii), &
                                             TRANSPOSE(trial%dNdXt(:, :, ii)))
END DO

IF (PRESENT(opt)) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF

END PROCEDURE DiffusionMatrix5_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_6
REAL(DFP), ALLOCATABLE :: c1bar(:, :), c2bar(:, :), realval(:)
INTEGER(I4B) :: ii
CALL getProjectionOfdNdXt(obj=test, cdNdXt=c1bar, val=c2)
CALL getProjectionOfdNdXt(obj=trial, cdNdXt=c2bar, val=c2)
CALL getInterpolation(obj=trial, interpol=realval, val=c1)
realval = realval * trial%js * trial%ws * trial%thickness
CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
DO ii = 1, SIZE(realval)
  ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
END DO
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (c1bar, c2bar, realval)
END PROCEDURE DiffusionMatrix_6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix6_
REAL(DFP) :: c1bar(test%nns, test%nips), c2bar(trial%nns, trial%nips), &
             realval(trial%nips)
INTEGER(I4B) :: ii, jj, kk
REAL(DFP), PARAMETER :: one = 1.0_DFP

CALL getProjectionOfdNdXt_(obj=test, cdNdXt=c1bar, val=c2, &
                           nrow=nrow, ncol=ii)
CALL getProjectionOfdNdXt_(obj=trial, cdNdXt=c2bar, val=c2, &
                           nrow=ncol, ncol=ii)

CALL getInterpolation_(obj=trial, interpol=realval, val=c1, &
                       tsize=ii)

realval = realval * trial%js * trial%ws * trial%thickness

ans(1:nrow, 1:ncol) = 0.0

DO ii = 1, trial%nips
  CALL OuterProd_(a=c1bar(1:nrow, ii), b=c2bar(1:ncol, ii), &
                  nrow=jj, ncol=kk, ans=ans, &
                  scale=realval(ii), anscoeff=one)
END DO

IF (PRESENT(opt)) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF

END PROCEDURE DiffusionMatrix6_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_7
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :)
INTEGER(I4B) :: ii
CALL getInterpolation(obj=trial, Interpol=realval, val=c1)
CALL getInterpolation(obj=trial, Interpol=kbar, val=c2)
realval = realval * trial%js * trial%ws * trial%thickness
DO ii = 1, SIZE(realval)
  ans = ans + realval(ii) * MATMUL(&
      & MATMUL(test%dNdXt(:, :, ii), kbar(:, :, ii)), &
      & TRANSPOSE(trial%dNdXt(:, :, ii)))
END DO
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (realval, kbar)
END PROCEDURE DiffusionMatrix_7

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_8
ans = DiffusionMatrix( &
  & test=test, &
  & trial=trial, &
  & c1=c2, &
  & c2=c1, &
  & c1rank=TypeFEVariableScalar, &
  & c2rank=TypeFEVariableVector, &
  & opt=opt)
END PROCEDURE DiffusionMatrix_8

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_9
REAL(DFP), ALLOCATABLE :: c1bar(:, :), c2bar(:, :), realval(:)
INTEGER(I4B) :: ii
  !! main
CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
CALL getProjectionOfdNdXt(obj=test, cdNdXt=c1bar, val=c1)
CALL getProjectionOfdNdXt(obj=trial, cdNdXt=c2bar, val=c2)
realval = trial%js * trial%ws * trial%thickness
DO ii = 1, SIZE(realval)
  ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
END DO
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (c1bar, c2bar, realval)
END PROCEDURE DiffusionMatrix_9

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_10
REAL(DFP), ALLOCATABLE :: matbar(:, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: c2bar(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
TYPE(FEVariable_) :: k
INTEGER(I4B) :: ii
CALL getInterpolation(obj=trial, interpol=c2bar, val=c1)
CALL getInterpolation(obj=trial, interpol=matbar, val=c2)
CALL Reallocate(c1bar, SIZE(matbar, 2), SIZE(matbar, 3))
DO ii = 1, SIZE(c2bar, 2)
  c1bar(:, ii) = MATMUL(c2bar(:, ii), matbar(:, :, ii))
END DO
k = QuadratureVariable(c1bar, typeFEVariableVector, typeFEVariableSpace)
CALL getProjectionOfdNdXt(obj=test, cdNdXt=c1bar, val=k)
CALL getProjectionOfdNdXt(obj=trial, cdNdXt=c2bar, val=k)
realval = trial%js * trial%ws * trial%thickness
CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
DO ii = 1, SIZE(realval)
  ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
END DO
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (c1bar, c2bar, realval, matbar)
END PROCEDURE DiffusionMatrix_10

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_11
ans = DiffusionMatrix( &
  & test=test, &
  & trial=trial, &
  & c1=c2, c2=c1, &
  & c1rank=TypeFEVariableScalar, &
  & c2rank=TypeFEVariableMatrix, &
  & opt=opt)
END PROCEDURE DiffusionMatrix_11

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_12
REAL(DFP), ALLOCATABLE :: matbar(:, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: c2bar(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
TYPE(FEVariable_) :: k
INTEGER(I4B) :: ii
CALL getInterpolation(obj=trial, interpol=matbar, val=c1)
CALL getInterpolation(obj=trial, interpol=c2bar, val=c2)
CALL Reallocate(c1bar, SIZE(matbar, 1), SIZE(matbar, 3))
DO ii = 1, SIZE(c2bar, 2)
  c1bar(:, ii) = MATMUL(matbar(:, :, ii), c2bar(:, ii))
END DO
k = QuadratureVariable(c1bar, typeFEVariableVector, typeFEVariableSpace)
CALL getProjectionOfdNdXt(obj=test, cdNdXt=c1bar, val=k)
CALL getProjectionOfdNdXt(obj=trial, cdNdXt=c2bar, val=k)
realval = trial%js * trial%ws * trial%thickness
CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
DO ii = 1, SIZE(realval)
  ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
END DO
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (c1bar, c2bar, realval, matbar)
END PROCEDURE DiffusionMatrix_12

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_13
REAL(DFP), ALLOCATABLE :: k1bar(:, :, :), k2bar(:, :, :), realval(:)
INTEGER(I4B) :: ii
CALL getInterpolation(obj=trial, Interpol=k1bar, val=c1)
CALL getInterpolation(obj=trial, Interpol=k2bar, val=c2)
CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
realval = trial%js * trial%ws * trial%thickness
DO ii = 1, SIZE(realval)
  ans = ans + realval(ii) * MATMUL( &
      & MATMUL(test%dNdXt(:, :, ii),&
      & MATMUL(k1bar(:, :, ii), k2bar(:, :, ii))), &
      & TRANSPOSE(trial%dNdXt(:, :, ii)))
END DO
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (k1bar, k2bar, realval)
END PROCEDURE DiffusionMatrix_13

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_14
SELECT CASE (opt(1))
CASE (1)
  CALL DiffusionMatrix_14a(test, trial, ans)
CASE (2)
  CALL DiffusionMatrix_14b(test, trial, ans)
END SELECT
END PROCEDURE DiffusionMatrix_14

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE DiffusionMatrix_14a(test, trial, ans)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:), m4(:, :, :, :)
  INTEGER(I4B) :: ii, jj, nsd, ips
  realval = trial%js * trial%ws * trial%thickness
  nsd = test%nsd
  CALL Reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), nsd, nsd)
  DO ips = 1, SIZE(trial%N, 2)
    DO jj = 1, nsd
      DO ii = 1, nsd
        m4(:, :, ii, jj) = m4(:, :, ii, jj) &
          & + realval(ips) * OUTERPROD( &
          & test%dNdXt(:, ii, ips), &
          & trial%dNdXt(:, jj, ips))
      END DO
    END DO
  END DO
  CALL Convert(from=m4, to=ans)
  DEALLOCATE (realval, m4)
END SUBROUTINE DiffusionMatrix_14a

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE DiffusionMatrix_14b(test, trial, ans)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:), m4(:, :, :, :)
  INTEGER(I4B) :: ii, jj, nsd, ips
  realval = trial%js * trial%ws * trial%thickness
  nsd = test%nsd
  CALL Reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), nsd, nsd)
  DO ips = 1, SIZE(trial%N, 2)
    DO jj = 1, nsd
      DO ii = 1, nsd
        m4(:, :, ii, jj) = m4(:, :, ii, jj) &
          & + realval(ips) * OUTERPROD( &
          & test%dNdXt(:, jj, ips), &
          & trial%dNdXt(:, ii, ips))
      END DO
    END DO
  END DO
  CALL Convert(from=m4, to=ans)
  DEALLOCATE (realval, m4)
END SUBROUTINE DiffusionMatrix_14b

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_15
SELECT CASE (opt(1))
CASE (1)
  CALL DiffusionMatrix_15a(test, trial, k, ans)
CASE (2)
  CALL DiffusionMatrix_15b(test, trial, k, ans)
END SELECT
END PROCEDURE DiffusionMatrix_15

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE DiffusionMatrix_15a(test, trial, k, ans)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  CLASS(FEVariable_), INTENT(IN) :: k
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:), kbar(:), m4(:, :, :, :)
  INTEGER(I4B) :: ii, jj, nsd, ips
  nsd = test%nsd
  CALL Reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), nsd, nsd)
  CALL getInterpolation(obj=trial, Interpol=kbar, val=k)
  realval = trial%js * trial%ws * trial%thickness * kbar
  DO ips = 1, SIZE(trial%N, 2)
    DO jj = 1, nsd
      DO ii = 1, nsd
        m4(:, :, ii, jj) = m4(:, :, ii, jj) &
          & + realval(ips) * OUTERPROD( &
          & test%dNdXt(:, ii, ips), &
          & trial%dNdXt(:, jj, ips))
      END DO
    END DO
  END DO
  CALL Convert(from=m4, to=ans)
  DEALLOCATE (kbar, realval, m4)
END SUBROUTINE DiffusionMatrix_15a

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE DiffusionMatrix_15b(test, trial, k, ans)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  CLASS(FEVariable_), INTENT(IN) :: k
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  !! internal variables
  REAL(DFP), ALLOCATABLE :: realval(:), kbar(:), m4(:, :, :, :)
  INTEGER(I4B) :: ii, jj, nsd, ips
  nsd = test%nsd
  CALL Reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), nsd, nsd)
  CALL getInterpolation(obj=trial, Interpol=kbar, val=k)
  realval = trial%js * trial%ws * trial%thickness * kbar
  DO ips = 1, SIZE(trial%N, 2)
    DO jj = 1, nsd
      DO ii = 1, nsd
        m4(:, :, ii, jj) = m4(:, :, ii, jj) &
          & + realval(ips) * OUTERPROD( &
          & test%dNdXt(:, jj, ips), &
          & trial%dNdXt(:, ii, ips))
      END DO
    END DO
  END DO
  CALL Convert(from=m4, to=ans)
  DEALLOCATE (kbar, realval, m4)
END SUBROUTINE DiffusionMatrix_15b

END SUBMODULE Methods
