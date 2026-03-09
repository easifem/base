! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(STForceVectorNormal_Method) Methods
USE ReallocateUtility, ONLY: Reallocate
USE ProductUtility, ONLY: OuterProd_
USE ProductUtility, ONLY: OTimesTilda_
USE FEVariable_Method, ONLY: FEVariableSize => Size
USE FEVariable_Method, ONLY: FEVariableGetInterpolation_ => GetInterpolation_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE InputUtility, ONLY: Input

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVectorNormal_1
REAL(DFP) :: scale0, c_dot_n, realval_space, realval_time, realval
INTEGER(I4B) :: ips, ipt, nsd, i1, i2, nips, nipt
LOGICAL(LGT) :: isadd0

nrow = testSpace%nns
ncol = testTime%nns
nsd = testSpace%nsd
nips = testSpace%nips
nipt = testTime%nips

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, nips
  c_dot_n = DOT_PRODUCT(testSpace%normal(1:nsd, ips), c(1:nsd))

  realval_space = testSpace%js(ips) &
    * testSpace%ws(ips) &
    * testSpace%thickness(ips)

  DO ipt = 1, nipt
    realval_time = testTime%js(ipt) * testTime%ws(ipt)
    realval = realval_space * realval_time * c_dot_n * scale0

    CALL OuterProd_(a=testSpace%N(1:nrow, ips), &
                    b=testTime%N(1:ncol, ipt), &
                    anscoeff=math%one, &
                    scale=realval, ans=ans, nrow=i1, ncol=i2)
  END DO

END DO
END PROCEDURE STForceVectorNormal_1

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVectorNormal_2
REAL(DFP) :: scale0, c_dot_n, realval_space, realval_time, realval
INTEGER(I4B) :: ips, ipt, nsd, i1, i2, nips, nipt
LOGICAL(LGT) :: isadd0

nrow = test(1)%nns
ncol = test(1)%nnt
nsd = test(1)%nsd
nips = test(1)%nips
nipt = SIZE(test)

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ipt = 1, nipt
  realval_time = test(ipt)%jt * test(ipt)%wt

  DO ips = 1, nips

    c_dot_n = DOT_PRODUCT(test(ipt)%normal(1:nsd, ips), c(1:nsd))

    realval_space = test(ipt)%js(ips) * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = realval_space * realval_time * c_dot_n * scale0

    CALL OuterProd_(a=test(ipt)%N(1:nrow, ips), &
                    b=test(ipt)%T(1:ncol), &
                    anscoeff=math%one, &
                    scale=realval, ans=ans, nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE STForceVectorNormal_2

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVectorNormal_3
REAL(DFP) :: scale0, c_dot_n, realval_space, realval_time, realval, &
             cbar(fevaropt%defaultVectorSize)
INTEGER(I4B) :: ips, ipt, nsd, i1, i2, nips, nipt
LOGICAL(LGT) :: isadd0

nrow = testSpace%nns
ncol = testTime%nns
nsd = testSpace%nsd
nips = testSpace%nips
nipt = testTime%nips

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, nips

  realval_space = testSpace%js(ips) * testSpace%ws(ips) &
    * testSpace%thickness(ips)

  DO ipt = 1, nipt

    CALL FEVariableGetInterpolation_( &
      obj=c, rank=crank, N=testSpace%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=testTime%N(:, ipt), nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=cbar, tsize=i1)

    c_dot_n = DOT_PRODUCT(testSpace%normal(1:nsd, ips), cbar(1:nsd))

    realval_time = testTime%js(ipt) * testTime%ws(ipt)
    realval = realval_space * realval_time * c_dot_n * scale0

    CALL OuterProd_(a=testSpace%N(1:nrow, ips), &
                    b=testTime%N(1:ncol, ipt), &
                    anscoeff=math%one, &
                    scale=realval, ans=ans, nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE STForceVectorNormal_3

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVectorNormal_4
REAL(DFP) :: scale0, c_dot_n, realval_space, realval_time, realval, &
             cbar(fevaropt%defaultVectorSize)
INTEGER(I4B) :: ips, ipt, nsd, i1, i2, nips, nipt
LOGICAL(LGT) :: isadd0

nrow = test(1)%nns
ncol = test(1)%nnt
nsd = test(1)%nsd
nips = test(1)%nips
nipt = SIZE(test)

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ipt = 1, nipt
  realval_time = test(ipt)%jt * test(ipt)%wt

  DO ips = 1, nips

    CALL FEVariableGetInterpolation_( &
      obj=c, rank=crank, N=test(ipt)%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=test(ipt)%T, nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=cbar, tsize=i1)

    c_dot_n = DOT_PRODUCT(test(ipt)%normal(1:nsd, ips), cbar(1:nsd))

    realval_space = test(ipt)%js(ips) * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = realval_space * realval_time * c_dot_n * scale0

    CALL OuterProd_(a=test(ipt)%N(1:nrow, ips), &
                    b=test(ipt)%T(1:ncol), &
                    anscoeff=math%one, &
                    scale=realval, ans=ans, nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE STForceVectorNormal_4

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVectorNormal_5
REAL(DFP) :: scale0, c_dot_n, realval_space, realval_time, realval, &
             c1bar
INTEGER(I4B) :: ips, ipt, nsd, i1, i2, nips, nipt
LOGICAL(LGT) :: isadd0

nrow = testSpace%nns
ncol = testTime%nns
nsd = testSpace%nsd
nips = testSpace%nips
nipt = testTime%nips

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, nips
  c_dot_n = DOT_PRODUCT(testSpace%normal(1:nsd, ips), c2(1:nsd))

  realval_space = testSpace%js(ips) * testSpace%ws(ips) &
    * testSpace%thickness(ips)

  DO ipt = 1, nipt

    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=testSpace%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=testTime%N(:, ipt), nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=c1bar)

    realval_time = testTime%js(ipt) * testTime%ws(ipt)
    realval = realval_space * realval_time * c_dot_n * scale0 * c1bar

    CALL OuterProd_(a=testSpace%N(1:nrow, ips), &
                    b=testTime%N(1:ncol, ipt), &
                    anscoeff=math%one, &
                    scale=realval, ans=ans, nrow=i1, ncol=i2)
  END DO
END DO

END PROCEDURE STForceVectorNormal_5

!----------------------------------------------------------------------------
!                                                       STForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVectorNormal_6
REAL(DFP) :: scale0, c_dot_n, realval_space, realval_time, realval, c1bar
INTEGER(I4B) :: ips, ipt, nsd, i1, i2, nips, nipt
LOGICAL(LGT) :: isadd0

nrow = test(1)%nns
ncol = test(1)%nnt
nsd = test(1)%nsd
nips = test(1)%nips
nipt = SIZE(test)

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ipt = 1, nipt
  realval_time = test(ipt)%jt * test(ipt)%wt

  DO ips = 1, nips

    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=test(ipt)%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=test(ipt)%T, nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=c1bar)

    c_dot_n = DOT_PRODUCT(test(ipt)%normal(1:nsd, ips), c2(1:nsd))

    realval_space = test(ipt)%js(ips) * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = realval_space * realval_time * c_dot_n * scale0 * c1bar

    CALL OuterProd_(a=test(ipt)%N(1:nrow, ips), &
                    b=test(ipt)%T(1:ncol), &
                    anscoeff=math%one, &
                    scale=realval, ans=ans, nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE STForceVectorNormal_6

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVectorNormal_7
REAL(DFP) :: scale0, c_dot_n, realval_space, realval_time, realval, &
             c2bar(fevaropt%defaultVectorSize), c1bar
INTEGER(I4B) :: ips, ipt, nsd, i1, i2, nips, nipt
LOGICAL(LGT) :: isadd0

nrow = testSpace%nns
ncol = testTime%nns
nsd = testSpace%nsd
nips = testSpace%nips
nipt = testTime%nips

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ips = 1, nips

  realval_space = testSpace%js(ips) * testSpace%ws(ips) &
    * testSpace%thickness(ips)

  DO ipt = 1, nipt

    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=testSpace%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=testTime%N(:, ipt), nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=c1bar)

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=testSpace%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=testTime%N(:, ipt), nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=c2bar, tsize=i1)

    c_dot_n = DOT_PRODUCT(testSpace%normal(1:nsd, ips), c2bar(1:nsd))

    realval_time = testTime%js(ipt) * testTime%ws(ipt)
    realval = realval_space * realval_time * c_dot_n * scale0 * c1bar

    CALL OuterProd_(a=testSpace%N(1:nrow, ips), &
                    b=testTime%N(1:ncol, ipt), &
                    anscoeff=math%one, &
                    scale=realval, ans=ans, nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE STForceVectorNormal_7

!----------------------------------------------------------------------------
!                                                        STForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVectorNormal_8
REAL(DFP) :: scale0, c_dot_n, realval_space, realval_time, realval, &
             c2bar(fevaropt%defaultVectorSize), c1bar
INTEGER(I4B) :: ips, ipt, nsd, i1, i2, nips, nipt
LOGICAL(LGT) :: isadd0

nrow = test(1)%nns
ncol = test(1)%nnt
nsd = test(1)%nsd
nips = test(1)%nips
nipt = SIZE(test)

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ipt = 1, nipt
  realval_time = test(ipt)%jt * test(ipt)%wt

  DO ips = 1, nips

    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=test(ipt)%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=test(ipt)%T, nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=c1bar)

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=test(ipt)%T, nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=c2bar, tsize=i1)

    c_dot_n = DOT_PRODUCT(test(ipt)%normal(1:nsd, ips), c2bar(1:nsd))

    realval_space = test(ipt)%js(ips) * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = realval_space * realval_time * c_dot_n * scale0 * c1bar

    CALL OuterProd_(a=test(ipt)%N(1:nrow, ips), &
                    b=test(ipt)%T(1:ncol), &
                    anscoeff=math%one, &
                    scale=realval, ans=ans, nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE STForceVectorNormal_8

!----------------------------------------------------------------------------
!                                                       STForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVectorNormal_9
REAL(DFP) :: realval, realval_space, realval_time, scale0, &
             kbar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize), &
             vbar(fevaropt%defaultVectorSize), &
             c_dot_n
INTEGER(I4B) :: ips, ipt, kbar_nrow, kbar_ncol, nsd, nips, nipt, i1, i2, &
                kbar_nrow, kbar_ncol
LOGICAL(LGT) :: isadd0

nrow = testSpace%nns
ncol = testTime%nns
nsd = testSpace%nsd
nips = testSpace%nips
nipt = testTime%nips

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

vbar = math%zero
kbar = math%zero

DO ips = 1, nips

  realval_space = testSpace%js(ips) * testSpace%ws(ips) &
    * testSpace%thickness(ips)

  DO ipt = 1, nipt

    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=testSpace%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=testTime%N(:, ipt), nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=kbar, nrow=kbar_nrow, ncol=kbar_ncol)

    vbar(1:kbar_nrow) = MATMUL(kbar(1:kbar_nrow, 1:kbar_ncol), &
                               c2(1:kbar_ncol))

    c_dot_n = DOT_PRODUCT(testSpace%normal(1:kbar_nrow, ips), &
                          vbar(1:kbar_nrow))

    realval_time = testTime%js(ipt) * testTime%ws(ipt)

    realval = realval_space * realval_time * c_dot_n * scale0

    CALL OuterProd_(a=testSpace%N(1:nrow, ips), &
                    b=testTime%N(1:ncol, ipt), &
                    anscoeff=math%one, &
                    scale=realval, ans=ans, &
                    nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE STForceVectorNormal_9

!----------------------------------------------------------------------------
!                                                       STForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVectorNormal_10
REAL(DFP) :: realval, realval_space, realval_time, scale0, &
             kbar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize), &
             vbar(fevaropt%defaultVectorSize), &
             c_dot_n
INTEGER(I4B) :: ips, ipt, kbar_nrow, kbar_ncol, nsd, nips, nipt, i1, i2, &
                kbar_nrow, kbar_ncol
LOGICAL(LGT) :: isadd0

nrow = test(1)%nns
ncol = test(1)%nnt
nsd = test(1)%nsd
nips = test(1)%nips
nipt = SIZE(test)

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

vbar = math%zero
kbar = math%zero

DO ipt = 1, nipt
  realval_time = test(ipt)%jt * test(ipt)%wt

  DO ips = 1, nips
    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=test(ipt)%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=test(ipt)%T, nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=kbar, nrow=kbar_nrow, ncol=kbar_ncol)

    vbar(1:kbar_nrow) = MATMUL(kbar(1:kbar_nrow, 1:kbar_ncol), &
                               c2(1:kbar_ncol))

    c_dot_n = DOT_PRODUCT(test(ipt)%normal(1:kbar_nrow, ips), &
                          vbar(1:kbar_nrow))

    realval_space = test(ipt)%js(ips) * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = realval_space * realval_time * c_dot_n * scale0

    CALL OuterProd_(a=test(ipt)%N(1:nrow, ips), &
                    b=test(ipt)%T(1:ncol), &
                    anscoeff=math%one, &
                    scale=realval, ans=ans, &
                    nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE STForceVectorNormal_10

!----------------------------------------------------------------------------
!                                                       STForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVectorNormal_11
REAL(DFP) :: realval, realval_space, realval_time, scale0, &
             kbar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize), &
             vbar(fevaropt%defaultVectorSize), &
             c2bar(fevaropt%defaultVectorSize), c_dot_n
INTEGER(I4B) :: ips, ipt, kbar_nrow, kbar_ncol, nsd, nips, nipt, i1, i2, &
                kbar_nrow, kbar_ncol
LOGICAL(LGT) :: isadd0

nrow = testSpace%nns
ncol = testTime%nns
nsd = testSpace%nsd
nips = testSpace%nips
nipt = testTime%nips

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

vbar = math%zero
kbar = math%zero

DO ips = 1, nips

  realval_space = testSpace%js(ips) * testSpace%ws(ips) &
    * testSpace%thickness(ips)

  DO ipt = 1, nipt

    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=testSpace%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=testTime%N(:, ipt), nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=kbar, nrow=kbar_nrow, ncol=kbar_ncol)

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=testSpace%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=testTime%N(:, ipt), nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=c2bar, tsize=i1)

    vbar(1:kbar_nrow) = MATMUL(kbar(1:kbar_nrow, 1:kbar_ncol), &
                               c2bar(1:kbar_ncol))

    c_dot_n = DOT_PRODUCT(testSpace%normal(1:kbar_nrow, ips), &
                          vbar(1:kbar_nrow))

    realval_time = testTime%js(ipt) * testTime%ws(ipt)

    realval = realval_space * realval_time * c_dot_n * scale0

    CALL OuterProd_(a=testSpace%N(1:nrow, ips), &
                    b=testTime%N(1:ncol, ipt), &
                    anscoeff=math%one, &
                    scale=realval, ans=ans, &
                    nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE STForceVectorNormal_11

!----------------------------------------------------------------------------
!                                                       STForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVectorNormal_12
REAL(DFP) :: realval, realval_space, realval_time, scale0, &
             kbar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize), &
             vbar(fevaropt%defaultVectorSize), &
             c2bar(fevaropt%defaultVectorSize), c_dot_n
INTEGER(I4B) :: ips, ipt, kbar_nrow, kbar_ncol, nsd, nips, nipt, i1, i2, &
                kbar_nrow, kbar_ncol
LOGICAL(LGT) :: isadd0

nrow = test(1)%nns
ncol = test(1)%nnt
nsd = test(1)%nsd
nips = test(1)%nips
nipt = SIZE(test)

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

vbar = math%zero
kbar = math%zero

DO ipt = 1, nipt
  realval_time = test(ipt)%jt * test(ipt)%wt

  DO ips = 1, nips
    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=test(ipt)%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=test(ipt)%T, nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=kbar, nrow=kbar_nrow, ncol=kbar_ncol)

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=nrow, spaceIndx=ips, &
      timeIndx=ipt, T=test(ipt)%T, nnt=ncol, scale=math%one, &
      addContribution=math%no, ans=c2bar, tsize=i1)

    vbar(1:kbar_nrow) = MATMUL(kbar(1:kbar_nrow, 1:kbar_ncol), &
                               c2bar(1:kbar_ncol))

    c_dot_n = DOT_PRODUCT(test(ipt)%normal(1:kbar_nrow, ips), &
                          vbar(1:kbar_nrow))

    realval_space = test(ipt)%js(ips) * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = realval_space * realval_time * c_dot_n * scale0

    CALL OuterProd_(a=test(ipt)%N(1:nrow, ips), &
                    b=test(ipt)%T(1:ncol), &
                    anscoeff=math%one, &
                    scale=realval, ans=ans, &
                    nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE STForceVectorNormal_12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
