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

SUBMODULE(MassMatrix_Method) Methods
USE ReallocateUtility, ONLY: Reallocate
USE ElemshapeData_Method, ONLY: GetInterpolation
USE ElemshapeData_Method, ONLY: GetInterpolation_
USE ProductUtility, ONLY: OuterProd_
USE ProductUtility, ONLY: OuterProd
USE ProductUtility, ONLY: OTimesTilda
USE ConvertUtility, ONLY: Convert
USE ConvertUtility, ONLY: Convert_
USE RealMatrix_Method, ONLY: MakeDiagonalCopies
USE RealMatrix_Method, ONLY: MakeDiagonalCopies_
USE EyeUtility, ONLY: Eye
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: varopt => TypeFEVariableOpt
USE InputUtility, ONLY: Input
USE FEVariable_Method, ONLY: FEVariableSize => Size
USE FEVariable_Method, ONLY: FEVariableGetInterpolation_ => GetInterpolation_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_1
INTEGER(I4B) :: nrow, ncol, opt0

opt0 = Input(option=opt, default=math%one_i)
nrow = test%nns * opt0
ncol = trial%nns * opt0
CALL Reallocate(ans, nrow, ncol)
CALL MassMatrix_(test=test, trial=trial, ans=ans, nrow=nrow, ncol=ncol, &
                 opt=opt0)
END PROCEDURE MassMatrix_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix1_
REAL(DFP) :: realval
INTEGER(I4B) :: ii, jj, ips, opt0
LOGICAL(LGT) :: isok

nrow = test%nns
ncol = trial%nns
opt0 = Input(default=math%one_i, option=opt)
ans(1:nrow * opt0, 1:ncol * opt0) = 0.0

DO ips = 1, trial%nips
  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)

  CALL OuterProd_( &
    a=test%N(1:nrow, ips), b=trial%N(1:ncol, ips), nrow=ii, ncol=jj, &
    ans=ans, scale=realval, anscoeff=math%one)
END DO

isok = opt0 .GT. 1
IF (isok) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt0, nrow=nrow, ncol=ncol)
  nrow = opt0 * nrow
  ncol = opt0 * ncol
END IF
END PROCEDURE MassMatrix1_

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_2
INTEGER(I4B) :: nrow, ncol, opt0

opt0 = Input(option=opt, default=math%one_i)
nrow = test%nns * opt0
ncol = trial%nns * opt0
CALL Reallocate(ans, nrow, ncol)
CALL MassMatrix_(test=test, trial=trial, ans=ans, nrow=nrow, ncol=ncol, &
                 opt=opt0, rho=rho, rhorank=rhorank)
END PROCEDURE MassMatrix_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix2_
INTEGER(I4B) :: ips, i1, i2, opt0
REAL(DFP) :: realval, rhobar, T(0)
LOGICAL(LGT) :: isok

opt0 = Input(default=math%one_i, option=opt)
nrow = test%nns
ncol = trial%nns
ans(1:nrow * opt0, 1:ncol * opt0) = math%zero

DO ips = 1, test%nips

  CALL FEVariableGetInterpolation_( &
    obj=rho, rank=rhorank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=rhobar)

  realval = rhobar * trial%js(ips) * trial%ws(ips) * trial%thickness(ips)

  CALL OuterProd_( &
    a=test%N(1:nrow, ips), b=trial%N(1:ncol, ips), nrow=i1, ncol=i2, &
    ans=ans, scale=realval, anscoeff=math%one)
END DO

isok = opt0 .GT. 1
IF (isok) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt0, nrow=nrow, ncol=ncol)
  nrow = opt0 * nrow
  ncol = opt0 * ncol
END IF
END PROCEDURE MassMatrix2_

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_3
! SELECT CASE (opt)
! CASE (1)
!   CALL MM_3a(ans=ans, test=test, trial=trial, rho=rho)
! CASE (2)
!   CALL MM_3b(ans=ans, test=test, trial=trial, rho=rho)
! CASE (3)
!   CALL MM_3c(ans=ans, test=test, trial=trial, rho=rho)
! CASE (4)
!   CALL MM_3d(ans=ans, test=test, trial=trial, rho=rho)
! END SELECT
END PROCEDURE MassMatrix_3

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix3_
! SELECT CASE (opt)
! CASE (1)
!   CALL MM_3a(ans=ans, test=test, trial=trial, rho=rho)
! CASE (2)
!   CALL MM_3b(ans=ans, test=test, trial=trial, rho=rho)
! CASE (3)
!   CALL MM_3c(ans=ans, test=test, trial=trial, rho=rho)
! CASE (4)
!   CALL MM_3d(ans=ans, test=test, trial=trial, rho=rho)
! END SELECT
END PROCEDURE MassMatrix3_

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

! PURE SUBROUTINE MM_3a(test, trial, rho, rhorank, ans, nrow, ncol)
!   CLASS(ElemshapeData_), INTENT(IN) :: test
!   ! Shapedata for test function
!   CLASS(ElemshapeData_), INTENT(IN) :: trial
!   ! Shapedata for trial function
!   CLASS(FEVariable_), INTENT(IN) :: rho
!   ! vector variable
!   TYPE(FEVariableVector_), INTENT(IN) :: rhorank
!   REAL(DFP), INTENT(INOUT) :: ans(:, :)
!   INTEGER(I4B), INTENT(OUT) :: nrow, ncol
!
!   ! Define internal variable
!   REAL(DFP), ALLOCATABLE :: realval(:)
!   REAL(DFP), ALLOCATABLE :: m2(:, :)
!   REAL(DFP), ALLOCATABLE :: vbar(:, :)
!   REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
!   INTEGER(I4B) :: ii, ips
!
!   ! main
!   CALL GetInterpolation(obj=trial, ans=vbar, val=rho)
!   CALL Reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), SIZE(vbar, 1), 1)
!   realval = trial%js * trial%ws * trial%thickness
!
!   DO ips = 1, SIZE(realval)
!     m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
!     DO ii = 1, SIZE(vbar, 1)
!       m4(:, :, ii, 1) = m4(:, :, ii, 1) &
!         & + realval(ips) * vbar(ii, ips) * m2
!     END DO
!   END DO
!
!   CALL Convert(From=m4, To=ans)
!   DEALLOCATE (realval, m2, vbar, m4)
! END SUBROUTINE MM_3a

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

! PURE SUBROUTINE MM_3b(ans, test, trial, rho)
!   REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
!   CLASS(ElemshapeData_), INTENT(IN) :: test
!   ! Shapedata for test function
!   CLASS(ElemshapeData_), INTENT(IN) :: trial
!   ! Shapedata for trial function
!   CLASS(FEVariable_), INTENT(IN) :: rho
!   ! vector variable
!
!   ! Define internal variable
!   REAL(DFP), ALLOCATABLE :: realval(:)
!   REAL(DFP), ALLOCATABLE :: m2(:, :)
!   REAL(DFP), ALLOCATABLE :: vbar(:, :)
!   REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
!   INTEGER(I4B) :: ii, ips
!
!   ! main
!   CALL GetInterpolation(obj=trial, ans=vbar, val=rho)
!   CALL Reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), 1, SIZE(vbar, 1))
!   realval = trial%js * trial%ws * trial%thickness
!
!   DO ips = 1, SIZE(realval)
!     m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
!     DO ii = 1, SIZE(vbar, 1)
!       m4(:, :, 1, ii) = m4(:, :, 1, ii) &
!         & + realval(ips) * vbar(ii, ips) * m2
!     END DO
!   END DO
!
!   CALL Convert(From=m4, To=ans)
!   DEALLOCATE (realval, m2, vbar, m4)
! END SUBROUTINE MM_3b

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

! PURE SUBROUTINE MM_3c(ans, test, trial, rho)
!   REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
!   CLASS(ElemshapeData_), INTENT(IN) :: test
!   ! Shapedata for test function
!   CLASS(ElemshapeData_), INTENT(IN) :: trial
!   ! Shapedata for trial function
!   CLASS(FEVariable_), INTENT(IN) :: rho
!   ! vector variable
!   ! Define internal variable
!   REAL(DFP), ALLOCATABLE :: realval(:)
!   REAL(DFP), ALLOCATABLE :: m2(:, :)
!   REAL(DFP), ALLOCATABLE :: vbar(:, :)
!   REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
!   INTEGER(I4B) :: ips, ii
!
!   ! main
!   CALL GetInterpolation(obj=trial, ans=vbar, val=rho)
!   CALL Reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), &
!     & SIZE(vbar, 1), SIZE(vbar, 1))
!
!   realval = trial%js * trial%ws * trial%thickness
!
!   DO ips = 1, SIZE(vbar, 2)
!     m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
!     DO ii = 1, SIZE(vbar, 1)
!       m4(:, :, ii, ii) = m4(:, :, ii, ii) &
!         & + realval(ips) * vbar(ii, ips) * m2
!     END DO
!   END DO
!
!   CALL Convert(from=m4, to=ans)
!
!   DEALLOCATE (realval, m2, vbar, m4)
! END SUBROUTINE MM_3c

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

! PURE SUBROUTINE MM_3d(ans, test, trial, rho)
!   REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
!   CLASS(ElemshapeData_), INTENT(IN) :: test
!   ! Shapedata for test function
!   CLASS(ElemshapeData_), INTENT(IN) :: trial
!   ! Shapedata for trial function
!   CLASS(FEVariable_), INTENT(IN) :: rho
!   ! vector variable
!   ! Define internal variable
!   REAL(DFP), ALLOCATABLE :: realval(:)
!   REAL(DFP), ALLOCATABLE :: m2(:, :)
!   REAL(DFP), ALLOCATABLE :: vbar(:, :)
!   REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
!   INTEGER(I4B) :: ips, ii, jj
!
!   ! main
!   CALL GetInterpolation(obj=trial, ans=vbar, val=rho)
!   CALL Reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), &
!     & SIZE(vbar, 1), SIZE(vbar, 1))
!
!   realval = trial%js * trial%ws * trial%thickness
!
!   DO ips = 1, SIZE(vbar, 2)
!     m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
!     DO jj = 1, SIZE(vbar, 1)
!       DO ii = 1, SIZE(vbar, 1)
!         m4(:, :, ii, jj) = m4(:, :, ii, jj) &
!           & + realval(ips) * vbar(ii, ips) &
!           & * vbar(jj, ips) * m2
!       END DO
!     END DO
!   END DO
!
!   CALL Convert(from=m4, to=ans)
!
!   DEALLOCATE (realval, m2, vbar, m4)
! END SUBROUTINE MM_3d

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_4
INTEGER(I4B) :: rhobar_i, rhobar_j, nns1, nns2
REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)

rhobar_i = FEVariableSize(obj=rho, dim=1)
rhobar_j = FEVariableSize(obj=rho, dim=2)
nns1 = test%nns
nns2 = trial%nns

CALL Reallocate(m4, nns1, nns2, rhobar_i, rhobar_j)
CALL Reallocate(ans, nns1 * rhobar_i, nns2 * rhobar_j)

CALL MassMatrix_(test=test, trial=trial, rho=rho, rhorank=rhorank, &
                 ans=ans, nrow=nns1, ncol=nns2, m4=m4)
! nns1 and nns2 are dummary values here as we dont use them

DEALLOCATE (m4)
END PROCEDURE MassMatrix_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix4_
INTEGER(I4B) :: ips, rhobar_i, rhobar_j, nns1, nns2
INTEGER(I4B) :: i1, i2, i3, i4
REAL(DFP) :: realval, T(0), &
             rhobar(varopt%defaultMatrixSize, varopt%defaultMatrixSize)

! main

rhobar_i = FEVariableSize(obj=rho, dim=1)
rhobar_j = FEVariableSize(obj=rho, dim=2)
nns1 = test%nns
nns2 = trial%nns

! nrow = nns1 * rhobar_i
! ncol = nns2 * rhobar_j

m4(1:nns1, 1:nns2, 1:rhobar_i, 1:rhobar_j) = math%zero

DO ips = 1, test%nips

  CALL FEVariableGetInterpolation_( &
    obj=rho, rank=rhorank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=rhobar, nrow=i1, ncol=i2)

  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)

  CALL OuterProd_(a=test%N(1:nns1, ips), b=trial%N(1:nns2, ips), &
                  c=rhobar(1:rhobar_i, 1:rhobar_j), &
                  scale=realval, anscoeff=math%one, &
                  ans=m4, dim1=i1, dim2=i2, dim3=i3, dim4=i4)
END DO

CALL Convert_(from=m4(1:nns1, 1:nns2, 1:rhobar_i, 1:rhobar_j), &
              to=ans, nrow=nrow, ncol=ncol)
END PROCEDURE MassMatrix4_

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_5
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: m2(:, :), eyemat(:, :), nij(:, :)
REAL(DFP), ALLOCATABLE :: lambdaBar(:)
REAL(DFP), ALLOCATABLE :: muBar(:)
REAL(DFP), ALLOCATABLE :: rhoBar(:)
REAL(DFP), ALLOCATABLE :: acoeff(:)
REAL(DFP), ALLOCATABLE :: bcoeff(:)
REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
INTEGER(I4B) :: ii, jj, ips, nsd, nns

! main
CALL GetInterpolation(obj=trial, ans=lambdaBar, val=lambda)
CALL GetInterpolation(obj=trial, ans=muBar, val=mu)
CALL GetInterpolation(obj=trial, ans=rhoBar, val=rho)

ALLOCATE (acoeff(SIZE(lambdaBar, 1)), bcoeff(SIZE(lambdaBar, 1)))

bcoeff = SQRT(rhoBar * muBar)
acoeff = SQRT(rhoBar * (lambdaBar + 2.0_DFP * muBar)) - bcoeff

nsd = trial%nsd
eyemat = Eye(nsd, 1.0_DFP)
nns = SIZE(test%N, 1)
ALLOCATE (m4(nns, nns, nsd, nsd))

realval = trial%js * trial%ws * trial%thickness

DO ips = 1, SIZE(realval)
  m2 = OUTERPROD(a=test%normal(:, ips), b=trial%normal(:, ips))
  nij = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))

  DO jj = 1, nsd
    DO ii = 1, nsd

      m4(:, :, ii, jj) = m4(:, :, ii, jj) + realval(ips) *  &
        & (acoeff(ips) * m2(ii, jj) + bcoeff(ips) * eyemat(ii, jj)) * nij

    END DO
  END DO
END DO

CALL Convert(From=m4, To=ans)

DEALLOCATE (realval, m2, lambdaBar, muBar, rhoBar, acoeff, bcoeff, m4,  &
  & eyemat, nij)
END PROCEDURE MassMatrix_5

!----------------------------------------------------------------------------
!                                                                MassMatrix_
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix5_
! REAL(DFP), ALLOCATABLE :: realval(:)
! REAL(DFP), ALLOCATABLE :: m2(:, :), eyemat(:, :), nij(:, :)
! REAL(DFP), ALLOCATABLE :: lambdaBar(:)
! REAL(DFP), ALLOCATABLE :: muBar(:)
! REAL(DFP), ALLOCATABLE :: rhoBar(:)
! REAL(DFP), ALLOCATABLE :: acoeff(:)
! REAL(DFP), ALLOCATABLE :: bcoeff(:)
! REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
! INTEGER(I4B) :: ii, jj, ips, nsd, nns
! REAL(DFP) :: lambdaBar, muBar, rhoBar, acoeff, bcoeff
!
! ! main
! ALLOCATE (acoeff(SIZE(lambdaBar, 1)), bcoeff(SIZE(lambdaBar, 1)))
!
! bcoeff = SQRT(rhoBar * muBar)
! acoeff = SQRT(rhoBar * (lambdaBar + 2.0_DFP * muBar)) - bcoeff
!
! nsd = trial%nsd
! eyemat = Eye(nsd, 1.0_DFP)
! nns = SIZE(test%N, 1)
! ALLOCATE (m4(nns, nns, nsd, nsd))
!
! realval = trial%js * trial%ws * trial%thickness
!
! DO ips = 1, SIZE(realval)
!   m2 = OUTERPROD(a=test%normal(:, ips), b=trial%normal(:, ips))
!   nij = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
!
!   DO jj = 1, nsd
!     DO ii = 1, nsd
!
!       m4(:, :, ii, jj) = m4(:, :, ii, jj) + realval(ips) *  &
!         & (acoeff(ips) * m2(ii, jj) + bcoeff(ips) * eyemat(ii, jj)) * nij
!
!     END DO
!   END DO
! END DO
!
! CALL Convert(From=m4, To=ans)
!
! DEALLOCATE (realval, m2, lambdaBar, muBar, rhoBar, acoeff, bcoeff, m4,  &
!   & eyemat, nij)
END PROCEDURE MassMatrix5_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix6_
REAL(DFP) :: realval
INTEGER(I4B) :: ii, jj, ips

nrow = nns1
ncol = nns2
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, nips
  realval = js(ips) * ws(ips) * thickness(ips)

  CALL OuterProd_( &
    a=N(1:nrow, ips), b=M(1:ncol, ips), nrow=ii, ncol=jj, &
    ans=ans, scale=realval, anscoeff=math%one)
END DO
END PROCEDURE MassMatrix6_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix7_
LOGICAL(LGT) :: isok
INTEGER(I4B) :: a, b, c, d, mynns1, mynns2

IF (.NOT. skipVertices) THEN
  CALL MassMatrix_(N=N, M=M, js=js, ws=ws, thickness=thickness, &
               nips=nips, nns1=nns1, nns2=nns2, ans=ans, nrow=nrow, ncol=ncol)
  RETURN
END IF

isok = (nns1 .GT. tVertices) .AND. (nns2 .GT. tVertices)
IF (.NOT. isok) THEN
  nrow = 0
  ncol = 0
  RETURN
END IF

a = tVertices + 1
b = nns1
c = tVertices + 1
d = nns2
mynns1 = nns1 - tVertices
mynns2 = nns2 - tVertices

CALL MassMatrix_(N=N(a:b, :), M=M(c:d, :), js=js, ws=ws, thickness=thickness, &
           nips=nips, nns1=mynns1, nns2=mynns2, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE MassMatrix7_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix8_
INTEGER(I4B) :: spaceRow, spaceCol, timeRow, timeCol

CALL MassMatrix_( &
  N=spaceN, M=spaceM, js=js, ws=ws, thickness=spaceThickness, nips=nips, &
  nns1=nns1, nns2=nns2, ans=spaceMat, nrow=spaceRow, ncol=spaceCol)

CALL MassMatrix_( &
  N=timeN, M=timeM, js=jt, ws=wt, thickness=timeThickness, nips=nipt, &
  nns1=nnt1, nns2=nnt2, ans=timeMat, nrow=timeRow, ncol=timeCol)

CALL OTimesTilda(a=spaceMat(1:spaceRow, 1:spaceCol), &
                 b=timeMat(1:timeRow, 1:timeCol), &
                 ans=ans, nrow=nrow, ncol=ncol, &
                 anscoeff=math%zero, scale=math%one)

END PROCEDURE MassMatrix8_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix9_
LOGICAL(LGT) :: donothing
INTEGER(I4B) :: a, b, c, d, e, f, g, h, mynns1, mynns2, mynnt1, mynnt2

IF (.NOT. skipVertices) THEN
  CALL MassMatrix_( &
    spaceN=spaceN, spaceM=spaceM, timeN=timeN, timeM=timeM, js=js, ws=ws, &
    jt=jt, wt=wt, spaceThickness=spaceThickness, &
    timeThickness=timeThickness, nips=nips, nns1=nns1, nns2=nns2, &
    nipt=nipt, nnt1=nnt1, nnt2=nnt2, spaceMat=spaceMat, timeMat=timeMat, &
    ans=ans, nrow=nrow, ncol=ncol)
  RETURN
END IF

donothing = (nns1 .LE. tSpaceVertices) &
            .OR. (nns2 .LE. tSpaceVertices) &
            .OR. (nnt1 .LE. tTimeVertices) &
            .OR. (nnt2 .LE. tTimeVertices)

IF (donothing) THEN
  nrow = 0
  ncol = 0
  RETURN
END IF

a = tSpaceVertices + 1
b = nns1
c = tSpaceVertices + 1
d = nns2
e = tTimeVertices + 1
f = nnt1
g = tTimeVertices + 1
h = nnt2

mynns1 = nns1 - tSpaceVertices
mynns2 = nns2 - tSpaceVertices
mynnt1 = nnt1 - tTimeVertices
mynnt2 = nnt2 - tTimeVertices

CALL MassMatrix_( &
  spaceN=spaceN(a:b, :), spaceM=spaceM(c:d, :), &
  timeN=timeN(e:f, :), timeM=timeM(g:h, :), js=js, ws=ws, &
  jt=jt, wt=wt, spaceThickness=spaceThickness, &
  timeThickness=timeThickness, nips=nips, nns1=mynns1, nns2=mynns2, &
  nipt=nipt, nnt1=mynnt1, nnt2=mynnt2, spaceMat=spaceMat, timeMat=timeMat, &
  ans=ans, nrow=nrow, ncol=ncol)

END PROCEDURE MassMatrix9_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
