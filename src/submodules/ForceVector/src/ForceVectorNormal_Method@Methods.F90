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

SUBMODULE(ForceVectorNormal_Method) Methods
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

MODULE PROCEDURE ForceVectorNormal_1
! Define internal variable
REAL(DFP) :: realval
INTEGER(I4B) :: ips
LOGICAL(LGT) :: isadd0
REAL(DFP) :: scale0

tsize = test%nns

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) &
  ans(1:tsize) = math%zero

DO ips = 1, test%nips
  realval = DOT_PRODUCT(test%normal(1:test%nsd, ips), c(1:test%nsd))
  realval = realval * test%js(ips) * test%ws(ips) * test%thickness(ips) &
    * scale0
  ans(1:tsize) = ans(1:tsize) + realval * test%N(1:tsize, ips)
END DO
END PROCEDURE ForceVectorNormal_1

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVectorNormal_2
! Define internal variable
REAL(DFP) :: realval, cbar(fevaropt%defaultVectorSize), T(0)
INTEGER(I4B) :: ips, i1
LOGICAL(LGT) :: isadd0
REAL(DFP) :: scale0

tsize = test%nns

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) &
  ans(1:tsize) = math%zero

DO ips = 1, test%nips

  CALL FEVariableGetInterpolation_( &
    obj=c, rank=crank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=cbar, tsize=i1)

  realval = DOT_PRODUCT(test%normal(1:test%nsd, ips), cbar(1:test%nsd))
  realval = realval * test%js(ips) * test%ws(ips) * test%thickness(ips) &
    * scale0
  ans(1:tsize) = ans(1:tsize) + realval * test%N(1:tsize, ips)

END DO

END PROCEDURE ForceVectorNormal_2

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVectorNormal_3
REAL(DFP) :: realval, c1bar, T(0)
INTEGER(I4B) :: ips
LOGICAL(LGT) :: isadd0
REAL(DFP) :: scale0

tsize = test%nns

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) &
  ans(1:tsize) = math%zero

DO ips = 1, test%nips

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=c1bar)

  realval = DOT_PRODUCT(test%normal(1:test%nsd, ips), c2(1:test%nsd))
  realval = realval * test%js(ips) * test%ws(ips) * test%thickness(ips) * &
    c1bar * scale0

  ans(1:tsize) = ans(1:tsize) + realval * test%N(1:tsize, ips)

END DO
END PROCEDURE ForceVectorNormal_3

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVectorNormal_4
REAL(DFP) :: realval, c1bar, T(0), c2bar(fevaropt%defaultVectorSize)
INTEGER(I4B) :: ips, i1
LOGICAL(LGT) :: isadd0
REAL(DFP) :: scale0

tsize = test%nns

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) &
  ans(1:tsize) = math%zero

DO ips = 1, test%nips

  CALL FEVariableGetInterpolation_( &
    obj=c2, rank=c2rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=c2bar, tsize=i1)

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=c1bar)

  realval = DOT_PRODUCT(test%normal(1:test%nsd, ips), c2bar(1:test%nsd))
  realval = realval * test%js(ips) * test%ws(ips) * test%thickness(ips) * &
    c1bar * scale0

  ans(1:tsize) = ans(1:tsize) + realval * test%N(1:tsize, ips)

END DO
END PROCEDURE ForceVectorNormal_4

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVectorNormal_5
! Define internal variable
REAL(DFP) :: realval, scale0, T(0), &
             kbar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize), &
             vbar(fevaropt%defaultVectorSize)
INTEGER(I4B) :: ips, kbar_nrow, kbar_ncol
LOGICAL(LGT) :: isadd0

tsize = test%nns

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:tsize) = math%zero

vbar = math%zero
kbar = math%zero

DO ips = 1, test%nips

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=kbar, nrow=kbar_nrow, ncol=kbar_ncol)

  vbar(1:kbar_nrow) = MATMUL(kbar(1:kbar_nrow, 1:kbar_ncol), c2(1:kbar_ncol))
  realval = DOT_PRODUCT(test%normal(1:kbar_nrow, ips), vbar(1:kbar_nrow))
  realval = realval * test%js(ips) * test%ws(ips) * test%thickness(ips) &
    * scale0

  ans(1:tsize) = ans(1:tsize) + realval * test%N(1:tsize, ips)
END DO
END PROCEDURE ForceVectorNormal_5

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVectorNormal_6
! Define internal variable
REAL(DFP) :: realval, scale0, T(0), &
             kbar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize), &
             vbar(fevaropt%defaultVectorSize), &
             cbar(fevaropt%defaultVectorSize)
INTEGER(I4B) :: ips, kbar_nrow, kbar_ncol, cbar_size
LOGICAL(LGT) :: isadd0

tsize = test%nns

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:tsize) = math%zero

vbar = math%zero
kbar = math%zero

DO ips = 1, test%nips

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=kbar, nrow=kbar_nrow, ncol=kbar_ncol)

  CALL FEVariableGetInterpolation_( &
    obj=c2, rank=c2rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=cbar, tsize=cbar_size)

  vbar(1:kbar_nrow) = MATMUL( &
                      kbar(1:kbar_nrow, 1:kbar_ncol), &
                      cbar(1:kbar_ncol))

  realval = DOT_PRODUCT(test%normal(1:test%nsd, ips), vbar(1:test%nsd))
  realval = realval * test%js(ips) * test%ws(ips) * test%thickness(ips) &
    * scale0

  ans(1:tsize) = ans(1:tsize) + realval * test%N(1:tsize, ips)

END DO
END PROCEDURE ForceVectorNormal_6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
