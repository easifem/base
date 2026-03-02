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
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableConstant
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableTime
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableVector
USE BaseType, ONLY: TypeFEVariableMatrix
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE ReallocateUtility, ONLY: Reallocate
USE RealMatrix_Method, ONLY: MakeDiagonalCopies
USE RealMatrix_Method, ONLY: MakeDiagonalCopies_
USE ProductUtility, ONLY: OUTERPROD
USE ProductUtility, ONLY: OUTERPROD_
USE ElemShapeData_Method, ONLY: GetInterpolation
USE ElemShapeData_Method, ONLY: GetInterpolation_
USE ElemShapeData_Method, ONLY: GetProjectionOfdNdXt
USE ElemShapeData_Method, ONLY: GetProjectionOfdNdXt_
USE FEVariable_Method, ONLY: QuadratureVariable
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: FEVariableGetInterpolation_ => GetInterpolation_
USE FEVariable_Method, ONLY: FEVariableSize => Size
USE ConvertUtility, ONLY: Convert
USE ConvertUtility, ONLY: Convert_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_1
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok

isok = PRESENT(opt)
nrow = test%nns
ncol = trial%nns
IF (isok) THEN
  nrow = opt * nrow
  ncol = opt * ncol
END IF

CALL Reallocate(ans, nrow, ncol)
CALL DiffusionMatrix_(test=test, trial=trial, ans=ans, nrow=nrow, &
                      ncol=ncol, opt=opt)
END PROCEDURE DiffusionMatrix_1

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix1_
REAL(DFP) :: realval
INTEGER(I4B) :: dim1, dim2, ips, ii
LOGICAL(LGT) :: isok

nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, trial%nips
  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)

  DO ii = 1, trial%nsd
    CALL OuterProd_(a=test%dNdXt(1:nrow, ii, ips), &
                    b=trial%dNdXt(1:ncol, ii, ips), &
                    nrow=dim1, ncol=dim2, ans=ans, scale=realval, &
                    anscoeff=math%one)
  END DO
END DO

isok = PRESENT(opt)
IF (isok) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF
END PROCEDURE DiffusionMatrix1_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_2
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok

nrow = test%nns
ncol = trial%nns
isok = PRESENT(opt)
IF (isok) THEN
  nrow = opt * nrow
  ncol = opt * ncol
END IF
CALL Reallocate(ans, nrow, ncol)

! MODULE PURE FUNCTION DiffusionMatrix_2(test, trial, k, krank, opt) &
CALL DiffusionMatrix_(test=test, trial=trial, k=k, krank=krank, &
                      ans=ans, nrow=nrow, ncol=ncol, opt=opt)
END PROCEDURE DiffusionMatrix_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix2_
REAL(DFP) :: realval, kbar, T(0)
INTEGER(I4B) :: ips, ii, dim1, dim2
LOGICAL(LGT) :: isok

nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, trial%nips

  CALL FEVariableGetInterpolation_( &
    obj=k, rank=krank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=kbar)

  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips) * kbar

  DO ii = 1, trial%nsd
    CALL OuterProd_(a=test%dNdXt(1:nrow, ii, ips), &
                    b=trial%dNdXt(1:ncol, ii, ips), &
                    nrow=dim1, ncol=dim2, ans=ans, scale=realval, &
                    anscoeff=math%one)
  END DO
END DO

isok = PRESENT(opt)
IF (isok) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF

END PROCEDURE DiffusionMatrix2_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_3
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok
REAL(DFP), ALLOCATABLE :: c1bar(:), c2bar(:)

nrow = test%nns
ncol = trial%nns
isok = PRESENT(opt)
IF (isok) THEN
  nrow = nrow * opt
  ncol = ncol * opt
END IF
CALL Reallocate(ans, nrow, ncol)

CALL Reallocate(c1bar, test%nns)
CALL Reallocate(c2bar, trial%nns)
CALL DiffusionMatrix_(test=test, trial=trial, k=k, krank=krank, &
                      c1bar=c1bar, c2bar=c2bar, ans=ans, &
                      nrow=nrow, ncol=ncol, opt=opt)

DEALLOCATE (c1bar, c2bar)
END PROCEDURE DiffusionMatrix_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix3_
REAL(DFP) :: T(0), realval, kbar(fevaropt%defaultVectorSize)
INTEGER(I4B) :: i1, i2, ips, kbar_size, cbar_size
LOGICAL(LGT) :: isok

nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, trial%nips

  CALL FEVariableGetInterpolation_( &
    obj=k, rank=krank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=kbar, tsize=kbar_size)

  CALL GetProjectionOfdNdXt_(obj=test, ips=ips, c=kbar(1:kbar_size), &
                             ans=c1bar, tsize=cbar_size)

  CALL GetProjectionOfdNdXt_(obj=trial, ips=ips, c=kbar(1:kbar_size), &
                             ans=c2bar, tsize=cbar_size)

  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)

  CALL OuterProd_(a=c1bar(1:cbar_size), b=c2bar(1:cbar_size), &
                  nrow=i1, ncol=i2, ans=ans, &
                  scale=realval, anscoeff=math%one)

END DO

isok = PRESENT(opt)
IF (isok) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF
END PROCEDURE DiffusionMatrix3_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_4
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok

nrow = test%nns
ncol = trial%nns
isok = PRESENT(opt)
IF (isok) THEN
  nrow = nrow * opt
  ncol = ncol * opt
END IF

CALL Reallocate(ans, nrow, ncol)
CALL DiffusionMatrix_(test=test, trial=trial, k=k, krank=krank, &
                      opt=opt, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE DiffusionMatrix_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix4_
REAL(DFP) :: kbar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize), &
             T(0), realval
INTEGER(I4B) :: ips, kbar_i, kbar_j, nsd1, nsd2
LOGICAL(LGT) :: isok

isok = PRESENT(opt)
nrow = test%nns
ncol = trial%nns
nsd1 = test%nsd
nsd2 = trial%nsd
ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, trial%nips
  CALL FEVariableGetInterpolation_( &
    obj=k, rank=krank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=kbar, nrow=kbar_i, ncol=kbar_j)

  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)

  CALL Matvec_AKBt( &
    amat=test%dNdXt(:, :, ips), &
    bmat=trial%dNdXt(:, :, ips), &
    kmat=kbar, scale=realval, addContribution=math%yes, &
    ans=ans, nns1=nrow, nns2=ncol, nsd1=nsd1, nsd2=nsd2)
END DO

IF (isok) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF
END PROCEDURE DiffusionMatrix4_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_5
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok

nrow = test%nns
ncol = trial%nns
isok = PRESENT(opt)
IF (isok) THEN
  nrow = nrow * opt
  ncol = ncol * opt
END IF

CALL Reallocate(ans, nrow, ncol)
CALL DiffusionMatrix_(test=test, trial=trial, c1=c1, c2=c2, &
                      c1rank=c1rank, c2rank=c2rank, opt=opt, &
                      ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE DiffusionMatrix_5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix5_
REAL(DFP) :: realval, c1bar, c2bar, T(0)
INTEGER(I4B) :: ips, ii, i1, i2
LOGICAL(LGT) :: isok

isok = PRESENT(opt)
nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, trial%nips

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=c1bar)

  CALL FEVariableGetInterpolation_( &
    obj=c2, rank=c2rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=c2bar)

  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips) &
    * c1bar * c2bar

  DO ii = 1, trial%nsd
    CALL OuterProd_(a=test%dNdXt(1:nrow, ii, ips), &
                    b=trial%dNdXt(1:ncol, ii, ips), &
                    nrow=i1, ncol=i2, ans=ans, scale=realval, &
                    anscoeff=math%one)
  END DO

END DO

IF (isok) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF

END PROCEDURE DiffusionMatrix5_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_6
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok
REAL(DFP), ALLOCATABLE :: c1bar(:), c2bar(:)

nrow = test%nns
ncol = trial%nns
isok = PRESENT(opt)
IF (isok) THEN
  nrow = nrow * opt
  ncol = ncol * opt
END IF

CALL Reallocate(ans, nrow, ncol)
CALL Reallocate(c1bar, test%nns)
CALL Reallocate(c2bar, trial%nns)

CALL DiffusionMatrix_(test=test, trial=trial, c1=c1, c2=c2, &
                      c1rank=c1rank, c2rank=c2rank, opt=opt, &
                      c1bar=c1bar, c2bar=c2bar, &
                      ans=ans, nrow=nrow, ncol=ncol)

DEALLOCATE (c1bar, c2bar)
END PROCEDURE DiffusionMatrix_6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix6_
REAL(DFP) :: realval, T(0), vecbar(fevaropt%defaultVectorSize), rhobar
INTEGER(I4B) :: ips, cbar_size, vecbar_size, i1, i2
LOGICAL(LGT) :: isok

isok = PRESENT(opt)
nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, trial%nips

  CALL FEVariableGetInterpolation_( &
    obj=c2, rank=c2rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=vecbar, tsize=vecbar_size)

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=rhobar)

  CALL GetProjectionOfdNdXt_(obj=test, ips=ips, c=vecbar(1:vecbar_size), &
                             ans=c1bar, tsize=cbar_size)

  CALL GetProjectionOfdNdXt_(obj=trial, ips=ips, c=vecbar(1:vecbar_size), &
                             ans=c2bar, tsize=cbar_size)

  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips) * rhobar

  CALL OuterProd_(a=c1bar(1:cbar_size), b=c2bar(1:cbar_size), &
                  nrow=i1, ncol=i2, ans=ans, scale=realval, &
                  anscoeff=math%one)

END DO

IF (isok) THEN
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

CALL GetInterpolation(obj=trial, ans=realval, val=c1)
CALL GetInterpolation(obj=trial, ans=kbar, val=c2)
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
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix7_
! CALL GetInterpolation(obj=trial, ans=realval, val=c1)
! CALL GetInterpolation(obj=trial, ans=kbar, val=c2)
! realval = realval * trial%js * trial%ws * trial%thickness
! DO ii = 1, SIZE(realval)
!   ans = ans + realval(ii) * MATMUL(&
!       & MATMUL(test%dNdXt(:, :, ii), kbar(:, :, ii)), &
!       & TRANSPOSE(trial%dNdXt(:, :, ii)))
! END DO
! IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
! DEALLOCATE (realval, kbar)

REAL(DFP) :: kbar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize), &
             T(0), realval, rhobar
INTEGER(I4B) :: ips, kbar_i, kbar_j, nsd1, nsd2
LOGICAL(LGT) :: isok

isok = PRESENT(opt)
nrow = test%nns
ncol = trial%nns
nsd1 = test%nsd
nsd2 = trial%nsd
ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, trial%nips
  CALL FEVariableGetInterpolation_( &
    obj=c2, rank=c2rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=kbar, nrow=kbar_i, ncol=kbar_j)

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=rhobar)

  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips) * rhobar

  CALL Matvec_AKBt( &
    amat=test%dNdXt(:, :, ips), &
    bmat=trial%dNdXt(:, :, ips), &
    kmat=kbar, scale=realval, addContribution=math%yes, &
    ans=ans, nns1=nrow, nns2=ncol, nsd1=nsd1, nsd2=nsd2)
END DO

IF (isok) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF
END PROCEDURE DiffusionMatrix7_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_8
ans = DiffusionMatrix( &
      test=test, trial=trial, c1=c2, c2=c1, c1rank=TypeFEVariableScalar, &
      c2rank=TypeFEVariableVector, opt=opt)
END PROCEDURE DiffusionMatrix_8

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix8_
CALL DiffusionMatrix_( &
  test=test, trial=trial, c1=c2, c2=c1, c1rank=TypeFEVariableScalar, &
  c2rank=TypeFEVariableVector, c1bar=c1bar, c2bar=c2bar, &
  opt=opt, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE DiffusionMatrix8_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_9
REAL(DFP), ALLOCATABLE :: c1bar(:), c2bar(:)
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok

nrow = test%nns
ncol = trial%nns
isok = PRESENT(opt)
IF (isok) THEN
  nrow = nrow * opt
  ncol = ncol * opt
END IF

CALL Reallocate(ans, nrow, ncol)
CALL Reallocate(c1bar, test%nns)
CALL Reallocate(c2bar, trial%nns)

CALL DiffusionMatrix_( &
  test=test, trial=trial, c1=c1, c2=c2, c1rank=c1rank, c2rank=c2rank, &
  c1bar=c1bar, c2bar=c2bar, ans=ans, nrow=nrow, ncol=ncol, opt=opt)

DEALLOCATE (c1bar, c2bar)
END PROCEDURE DiffusionMatrix_9

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix9_
REAL(DFP) :: T(0), realval, vec1bar(fevaropt%defaultVectorSize), &
             vec2bar(fevaropt%defaultVectorSize)
INTEGER(I4B) :: i1, i2, ips, vec1bar_size, cbar_size, vec2bar_size
LOGICAL(LGT) :: isok

nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, trial%nips

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=vec1bar, tsize=vec1bar_size)

  CALL FEVariableGetInterpolation_( &
    obj=c2, rank=c2rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=vec2bar, tsize=vec2bar_size)

  CALL GetProjectionOfdNdXt_(obj=test, ips=ips, c=vec1bar(1:vec1bar_size), &
                             ans=c1bar, tsize=cbar_size)

  CALL GetProjectionOfdNdXt_(obj=trial, ips=ips, c=vec2bar(1:vec2bar_size), &
                             ans=c2bar, tsize=cbar_size)

  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)

  CALL OuterProd_(a=c1bar(1:cbar_size), b=c2bar(1:cbar_size), &
                  nrow=i1, ncol=i2, ans=ans, scale=realval, &
                  anscoeff=math%one)

END DO

isok = PRESENT(opt)
IF (isok) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF
END PROCEDURE DiffusionMatrix9_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_10
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok
REAL(DFP), ALLOCATABLE :: c1bar(:), c2bar(:)

nrow = test%nns
ncol = trial%nns
isok = PRESENT(opt)
IF (isok) THEN
  nrow = nrow * opt
  ncol = ncol * opt
END IF

CALL Reallocate(ans, nrow, ncol)
CALL Reallocate(c1bar, test%nns)
CALL Reallocate(c2bar, trial%nns)

CALL DiffusionMatrix_( &
  test=test, trial=trial, c1=c1, c2=c2, c1rank=c1rank, &
  c2rank=c2rank, c1bar=c1bar, c2bar=c2bar, ans=ans, nrow=nrow, &
  ncol=ncol, opt=opt)

DEALLOCATE (c1bar, c2bar)
END PROCEDURE DiffusionMatrix_10

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix_
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix10_
REAL(DFP) :: T(0), realval, vecbar(fevaropt%defaultVectorSize), &
             matbar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize)
INTEGER(I4B) :: i1, i2, ips, cbar_size, vecbar_size, mat_ncol, mat_nrow
LOGICAL(LGT) :: isok

isok = PRESENT(opt)
nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, trial%nips

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=vecbar, tsize=vecbar_size)

  CALL FEVariableGetInterpolation_( &
    obj=c2, rank=c2rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=matbar, nrow=mat_nrow, ncol=mat_ncol)

  vecbar(1:mat_ncol) = MATMUL(vecbar(1:mat_nrow), &
                              matbar(1:mat_nrow, 1:mat_ncol))

  CALL GetProjectionOfdNdXt_(obj=test, ips=ips, c=vecbar(1:mat_ncol), &
                             ans=c1bar, tsize=cbar_size)

  CALL GetProjectionOfdNdXt_(obj=trial, ips=ips, c=vecbar(1:mat_ncol), &
                             ans=c2bar, tsize=cbar_size)

  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)

  CALL OuterProd_(a=c1bar(1:cbar_size), b=c2bar(1:cbar_size), &
                  nrow=i1, ncol=i2, ans=ans, scale=realval, &
                  anscoeff=math%one)

END DO

IF (isok) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF
END PROCEDURE DiffusionMatrix10_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_11
ans = DiffusionMatrix( &
      test=test, trial=trial, c1=c2, c2=c1, c1rank=TypeFEVariableScalar, &
      c2rank=TypeFEVariableMatrix, opt=opt)
END PROCEDURE DiffusionMatrix_11

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix11_
CALL DiffusionMatrix_( &
  test=test, trial=trial, c1=c2, c2=c1, c1rank=TypeFEVariableScalar, &
  c2rank=TypeFEVariableMatrix, opt=opt, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE DiffusionMatrix11_

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
CALL getInterpolation(obj=trial, ans=matbar, val=c1)
CALL getInterpolation(obj=trial, ans=c2bar, val=c2)
CALL Reallocate(c1bar, SIZE(matbar, 1), SIZE(matbar, 3))
DO ii = 1, SIZE(c2bar, 2)
  c1bar(:, ii) = MATMUL(matbar(:, :, ii), c2bar(:, ii))
END DO
k = QuadratureVariable(c1bar, typeFEVariableVector, typeFEVariableSpace)
CALL GetProjectionOfdNdXt(obj=test, ans=c1bar, c=k, &
                          crank=TypeFEVariableVector)
CALL GetProjectionOfdNdXt(obj=trial, ans=c2bar, c=k, &
                          crank=TypeFEVariableVector)
realval = trial%js * trial%ws * trial%thickness
CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
DO ii = 1, SIZE(realval)
  ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
END DO
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (c1bar, c2bar, realval, matbar)
END PROCEDURE DiffusionMatrix_12

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix12_
REAL(DFP) :: T(0), realval, vecbar(fevaropt%defaultVectorSize), &
             matbar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize)
INTEGER(I4B) :: i1, i2, ips, cbar_size, vecbar_size, mat_ncol, mat_nrow
LOGICAL(LGT) :: isok

isok = PRESENT(opt)
nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, trial%nips

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=matbar, nrow=mat_nrow, ncol=mat_ncol)

  CALL FEVariableGetInterpolation_( &
    obj=c2, rank=c2rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=vecbar, tsize=vecbar_size)

  vecbar(1:mat_nrow) = MATMUL(matbar(1:mat_nrow, 1:mat_ncol), &
                              vecbar(1:mat_nrow))

  CALL GetProjectionOfdNdXt_(obj=test, ips=ips, c=vecbar(1:mat_nrow), &
                             ans=c1bar, tsize=cbar_size)

  CALL GetProjectionOfdNdXt_(obj=trial, ips=ips, c=vecbar(1:mat_nrow), &
                             ans=c2bar, tsize=cbar_size)

  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)

  CALL OuterProd_(a=c1bar(1:cbar_size), b=c2bar(1:cbar_size), &
                  nrow=i1, ncol=i2, ans=ans, scale=realval, &
                  anscoeff=math%one)

END DO

IF (isok) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF
END PROCEDURE DiffusionMatrix12_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_13
REAL(DFP), ALLOCATABLE :: k1bar(:, :, :), k2bar(:, :, :), realval(:)
INTEGER(I4B) :: ii
CALL getInterpolation(obj=trial, ans=k1bar, val=c1)
CALL getInterpolation(obj=trial, ans=k2bar, val=c2)
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
!                                                          DiffusionMatrix_
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix13_
REAL(DFP) :: T(0), realval
REAL(DFP) :: mat1bar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize)
REAL(DFP) :: mat2bar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize)
REAL(DFP) :: kbar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize)
INTEGER(I4B) :: i1, i2, ips, cbar_size, vecbar_size, mat1_ncol, mat1_nrow, &
                mat2_nrow, mat2_ncol
LOGICAL(LGT) :: isok

isok = PRESENT(opt)
nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, trial%nips

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=mat1bar, nrow=mat1_nrow, &
    ncol=mat1_ncol)

  CALL FEVariableGetInterpolation_( &
    obj=c2, rank=c2rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=mat2bar, nrow=mat2_nrow, &
    ncol=mat2_ncol)

  kbar(1:mat1_nrow, 1:mat2_ncol) = MATMUL( &
                                   mat1bar(1:mat1_nrow, 1:mat1_ncol), &
                                   mat2bar(1:mat2_nrow, 1:mat2_ncol))

  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)

  CALL Matvec_AKBt( &
    amat=test%dNdXt(:, :, ips), bmat=trial%dNdXt(:, :, ips), &
    kmat=kbar, scale=realval, addContribution=math%yes, &
    ans=ans, nns1=nrow, nns2=ncol, nsd1=mat1_nrow, nsd2=mat2_ncol)

END DO

IF (isok) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF
END PROCEDURE DiffusionMatrix13_

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_14
INTEGER(I4B) :: nrow, ncol, nsd
REAL(DFP), ALLOCATABLE :: mat4(:, :, :, :)

nrow = test%nns
ncol = trial%nns
nsd = test%nsd
CALL Reallocate(ans, nrow * nsd, ncol * nsd)
CALL Reallocate(mat4, nrow, ncol, nsd, nsd)

CALL DiffusionMatrix_(test=test, trial=trial, mat4=mat4, opt=opt, &
                      ans=ans, nrow=nrow, ncol=ncol)

DEALLOCATE (mat4)
END PROCEDURE DiffusionMatrix_14

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix14_
! Internal variable
REAL(DFP) :: realval
INTEGER(I4B) :: ii, jj, nsd, ips, i1, i2, i3, i4
LOGICAL(LGT) :: isopt2

nsd = test%nsd
i1 = test%nns
i2 = trial%nns
isopt2 = opt(1) .EQ. math%two_i

DO ips = 1, test%nips
  realval = test%js(ips) * test%ws(ips) * test%thickness(ips)

  DO jj = 1, nsd
    DO ii = 1, nsd

      IF (isopt2) THEN
        CALL OuterProd_(a=test%dNdXt(1:i1, jj, ips), &
                        b=trial%dNdXt(1:i2, ii, ips), &
                        nrow=i3, ncol=i4, ans=ans, scale=realval, &
                        anscoeff=math%one)
      ELSE
        CALL OuterProd_(a=test%dNdXt(1:i1, ii, ips), &
                        b=trial%dNdXt(1:i2, jj, ips), &
                        nrow=i3, ncol=i4, ans=ans, scale=realval, &
                        anscoeff=math%one)
      END IF

    END DO
  END DO
END DO

i3 = nsd
i4 = nsd

CALL Convert_(from=mat4(1:i1, 1:i2, 1:i3, 1:i4), to=ans, nrow=nrow, &
              ncol=ncol)
END PROCEDURE DiffusionMatrix14_

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix_15
INTEGER(I4B) :: nrow, ncol, nsd
REAL(DFP), ALLOCATABLE :: mat4(:, :, :, :)

nrow = test%nns
ncol = trial%nns
nsd = test%nsd
CALL Reallocate(ans, nrow * nsd, ncol * nsd)
CALL Reallocate(mat4, nrow, ncol, nsd, nsd)

CALL DiffusionMatrix_(test=test, trial=trial, mat4=mat4, opt=opt, &
                      ans=ans, nrow=nrow, ncol=ncol, k=k, krank=krank)

DEALLOCATE (mat4)
END PROCEDURE DiffusionMatrix_15

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE DiffusionMatrix15_
! Internal variable
REAL(DFP) :: realval, T(0), kbar
INTEGER(I4B) :: ii, jj, nsd, ips, i1, i2, i3, i4
LOGICAL(LGT) :: isopt2

nsd = test%nsd
i1 = test%nns
i2 = trial%nns
isopt2 = opt(1) .EQ. math%two_i

DO ips = 1, test%nips

  CALL FEVariableGetInterpolation_( &
    obj=k, rank=krank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=kbar)

  realval = test%js(ips) * test%ws(ips) * test%thickness(ips) * kbar

  DO jj = 1, nsd
    DO ii = 1, nsd

      IF (isopt2) THEN

        CALL OuterProd_(a=test%dNdXt(1:i1, jj, ips), &
                        b=trial%dNdXt(1:i2, ii, ips), &
                        nrow=i3, ncol=i4, ans=ans, scale=realval, &
                        anscoeff=math%one)
      ELSE

        CALL OuterProd_(a=test%dNdXt(1:i1, ii, ips), &
                        b=trial%dNdXt(1:i2, jj, ips), &
                        nrow=i3, ncol=i4, ans=ans, scale=realval, &
                        anscoeff=math%one)

      END IF

    END DO
  END DO

END DO

i3 = nsd
i4 = nsd

CALL Convert_(from=mat4(1:i1, 1:i2, 1:i3, 1:i4), to=ans, nrow=nrow, &
              ncol=ncol)
END PROCEDURE DiffusionMatrix15_

!----------------------------------------------------------------------------
!                                                                Matvec_AKAt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-02
! summary: This is an internal routine for performing matmul
PURE SUBROUTINE Matvec_AKAt(amat, kmat, scale, addContribution, ans, &
                            nns1, nns2, nsd1, nsd2)
  REAL(DFP), INTENT(IN) :: amat(:, :)
  REAL(DFP), INTENT(IN) :: kmat(:, :)
  REAL(DFP), INTENT(IN) :: scale
  !! scale
  LOGICAL(LGT), INTENT(IN) :: addContribution
  !! if true we do not reset ans
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  !! the size of ans would be nns1, nns2
  INTEGER(I4B), INTENT(IN) :: nns1, nns2, nsd1, nsd2

  ! Internal variables
  INTEGER(I4B) :: ii, jj, aa, bb

  IF (.NOT. addContribution) ans(1:nns1, 1:nns2) = math%zero

  DO CONCURRENT(ii=1:nns1, jj=1:nns2, aa=1:nsd1, bb=1:nsd2)
    ans(ii, jj) = ans(ii, jj) + &
                  scale * amat(ii, aa) * amat(jj, bb) * kmat(aa, bb)
  END DO
END SUBROUTINE Matvec_AKAt

!----------------------------------------------------------------------------
!                                                                Matvec_AKBt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-02
! summary: This is an internal routine for performing matmul
PURE SUBROUTINE Matvec_AKBt(amat, bmat, kmat, scale, addContribution, ans, &
                            nns1, nns2, nsd1, nsd2)
  REAL(DFP), INTENT(IN) :: amat(:, :)
  !! upperbound of amat is nns1, nsd1
  REAL(DFP), INTENT(IN) :: bmat(:, :)
  !! upperbound of amat is nns2, nsd2
  REAL(DFP), INTENT(IN) :: kmat(:, :)
  !! upperbound of amat is nsd1, nsd2
  REAL(DFP), INTENT(IN) :: scale
  !! scale
  LOGICAL(LGT), INTENT(IN) :: addContribution
  !! if true we do not reset ans
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  !! the size of ans would be nns1, nns2
  INTEGER(I4B), INTENT(IN) :: nns1, nns2, nsd1, nsd2

  ! Internal variables
  INTEGER(I4B) :: ii, jj, aa, bb

  IF (.NOT. addContribution) ans(1:nns1, 1:nns2) = math%zero

  DO CONCURRENT(ii=1:nns1, jj=1:nns2, aa=1:nsd1, bb=1:nsd2)
    ans(ii, jj) = ans(ii, jj) + &
                  scale * amat(ii, aa) * bmat(jj, bb) * kmat(aa, bb)
  END DO
END SUBROUTINE Matvec_AKBt

END SUBMODULE Methods
