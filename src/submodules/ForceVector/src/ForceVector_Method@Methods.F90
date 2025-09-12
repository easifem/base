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

SUBMODULE(ForceVector_Method) Methods
USE ReallocateUtility, ONLY: Reallocate
USE ElemshapeData_Method, ONLY: GetInterpolation, GetInterpolation_
USE ProductUtility, ONLY: OuterProd, OuterProd_
USE FEVariable_Method, ONLY: FEVariableSize => Size

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector1
INTEGER(I4B) :: tsize
CALL Reallocate(ans, test%nns)
CALL ForceVector_(test=test, ans=ans, tsize=tsize)
END PROCEDURE ForceVector1

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_1
! Define internal variable
REAL(DFP) :: realval
INTEGER(I4B) :: ips

! main
tsize = test%nns
ans(1:tsize) = 0.0_DFP

DO ips = 1, test%nips
  realval = test%js(ips) * test%ws(ips) * test%thickness(ips)
  ans(1:tsize) = ans(1:tsize) + realval * test%N(1:tsize, ips)
END DO

END PROCEDURE ForceVector_1

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector2
INTEGER(I4B) :: tsize
tsize = test%nns
CALL Reallocate(ans, tsize)
CALL ForceVector_(test=test, c=c, crank=crank, ans=ans, tsize=tsize)
END PROCEDURE ForceVector2

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_2
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

REAL(DFP) :: realval
INTEGER(I4B) :: ips

tsize = test%nns
ans(1:tsize) = 0.0_DFP

DO ips = 1, test%nips
  CALL GetInterpolation_(obj=test, ans=realval, val=c, scale=one, &
                         addContribution=no, timeIndx=1, spaceIndx=ips)

  realval = test%js(ips) * test%ws(ips) * test%thickness(ips) * realval

  ans(1:tsize) = ans(1:tsize) + realval * test%N(1:tsize, ips)
END DO

END PROCEDURE ForceVector_2

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector3
INTEGER(I4B) :: nrow, ncol

nrow = FEVariableSize(c, 1)
ncol = test%nns
CALL Reallocate(ans, nrow, ncol)
CALL ForceVector_(test=test, c=c, crank=crank, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE ForceVector3

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_3
! Define internal variable
REAL(DFP) :: realval, cbar3(3)
INTEGER(I4B) :: ips, tsize
REAL(DFP), ALLOCATABLE :: cbar(:)
LOGICAL(LGT) :: isok

nrow = FEVariableSize(c, 1)
ncol = test%nns
ans(1:nrow, 1:ncol) = 0.0_DFP

isok = nrow .GT. 3_I4B
IF (isok) THEN

  ALLOCATE (cbar(nrow))
  DO ips = 1, test%nips
    realval = test%js(ips) * test%ws(ips) * test%thickness(ips)
    CALL GetInterpolation_(obj=test, val=c, ans=cbar, tsize=tsize, &
                           scale=1.0_DFP, &
                           addContribution=.FALSE., &
                           timeIndx=1_I4B, spaceIndx=ips)

    CALL OuterProd_(a=cbar(1:tsize), b=test%N(1:test%nns, ips), &
                    anscoeff=1.0_DFP, scale=realval, &
                    ans=ans, nrow=nrow, ncol=ncol)
  END DO

  DEALLOCATE (cbar)

ELSE

  DO ips = 1, test%nips
    realval = test%js(ips) * test%ws(ips) * test%thickness(ips)
    CALL GetInterpolation_(obj=test, val=c, ans=cbar3, tsize=tsize, &
                           scale=1.0_DFP, &
                           addContribution=.FALSE., &
                           timeIndx=1_I4B, spaceIndx=ips)

    CALL OuterProd_(a=cbar3(1:tsize), b=test%N(1:test%nns, ips), &
                    anscoeff=1.0_DFP, scale=realval, &
                    ans=ans, nrow=nrow, ncol=ncol)
  END DO

END IF

END PROCEDURE ForceVector_3

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector4
INTEGER(I4B) :: dim1, dim2, dim3
dim1 = FEVariableSize(c, 1)
dim2 = FEVariableSize(c, 2)
dim3 = test%nns
CALL Reallocate(ans, dim1, dim2, dim3)
CALL ForceVector_(test=test, c=c, crank=crank, ans=ans, dim1=dim1, &
                  dim2=dim2, dim3=dim3)
END PROCEDURE ForceVector4

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_4
! Define internal variable
REAL(DFP), ALLOCATABLE :: cbar(:, :)
REAL(DFP) :: realval
INTEGER(I4B) :: ips, ic, jc

! main
ic = FEVariableSize(c, 1)
jc = FEVariableSize(c, 2)
dim3 = test%nns
ans(1:ic, 1:jc, 1:dim3) = 0.0_DFP

ALLOCATE (cbar(ic, jc))

DO ips = 1, test%nips
  realval = test%js(ips) * test%ws(ips) * test%thickness(ips)
  CALL GetInterpolation_(obj=test, val=c, ans=cbar, nrow=ic, &
                         ncol=jc, scale=1.0_DFP, &
                         addContribution=.FALSE., &
                         timeIndx=1_I4B, spaceIndx=ips)

  CALL OuterProd_(a=cbar(1:ic, 1:jc), b=test%N(1:test%nns, ips), &
                  anscoeff=1.0_DFP, scale=realval, &
                  ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
END DO

DEALLOCATE (cbar)
END PROCEDURE ForceVector_4

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector5
INTEGER(I4B) :: tsize
tsize = test%nns
CALL Reallocate(ans, tsize)
CALL ForceVector_(test=test, c1=c1, c2=c2, c1rank=c1rank, c2rank=c2rank, &
                  ans=ans, tsize=tsize)
END PROCEDURE ForceVector5

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_5
REAL(DFP) :: c1bar, c2bar, realval
INTEGER(I4B) :: ips

! main
tsize = test%nns
ans(1:tsize) = 0.0_DFP

DO ips = 1, test%nips
  CALL GetInterpolation_(obj=test, ans=c1bar, val=c1, &
                         scale=1.0_DFP, addContribution=.FALSE., &
                         timeIndx=1_I4B, spaceIndx=ips)

  CALL GetInterpolation_(obj=test, ans=c2bar, val=c2, &
                         scale=1.0_DFP, addContribution=.FALSE., &
                         timeIndx=1_I4B, spaceIndx=ips)

  realval = test%js(ips) * test%ws(ips) * test%thickness(ips) * c1bar * c2bar

  ans(1:tsize) = ans(1:tsize) + realval * test%N(1:tsize, ips)
END DO

END PROCEDURE ForceVector_5

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector6
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: c1bar(:)
REAL(DFP), ALLOCATABLE :: c2bar(:, :)
INTEGER(I4B) :: ips

! main
CALL GetInterpolation(obj=test, ans=c1bar, val=c1)
CALL GetInterpolation(obj=test, ans=c2bar, val=c2)
realval = test%js * test%ws * test%thickness * c1bar
CALL Reallocate(ans, SIZE(c2bar, 1), SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * OUTERPROD(c2bar(:, ips), test%N(:, ips))
END DO

DEALLOCATE (realval, c1bar, c2bar)
END PROCEDURE ForceVector6

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_6
! Define internal variable
REAL(DFP), ALLOCATABLE :: c2bar(:)
REAL(DFP) :: c1bar, realval, c2bar3(3)
INTEGER(I4B) :: ips, tsize
LOGICAL(LGT) :: isok

nrow = FEVariableSize(c2, 1)
ncol = test%nns
ans(1:nrow, 1:ncol) = 0.0_DFP

isok = nrow .GT. 3_I4B

IF (isok) THEN
  ALLOCATE (c2bar(nrow))
  DO ips = 1, test%nips
    CALL GetInterpolation_(obj=test, val=c2, ans=c2bar, tsize=tsize, &
                           scale=1.0_DFP, &
                           addContribution=.FALSE., &
                           timeIndx=1_I4B, spaceIndx=ips)

    CALL GetInterpolation_(obj=test, val=c1, ans=c1bar, &
                           scale=1.0_DFP, &
                           addContribution=.FALSE., &
                           timeIndx=1_I4B, spaceIndx=ips)

    realval = test%js(ips) * test%ws(ips) * test%thickness(ips) * c1bar

    CALL OuterProd_(a=c2bar(1:tsize), b=test%N(1:test%nns, ips), &
                    anscoeff=1.0_DFP, scale=realval, &
                    ans=ans, nrow=nrow, ncol=ncol)
  END DO

  DEALLOCATE (c2bar)

ELSE

  DO ips = 1, test%nips
    CALL GetInterpolation_(obj=test, val=c2, ans=c2bar3, tsize=tsize, &
                           scale=1.0_DFP, &
                           addContribution=.FALSE., &
                           timeIndx=1_I4B, spaceIndx=ips)

    CALL GetInterpolation_(obj=test, val=c1, ans=c1bar, &
                           scale=1.0_DFP, &
                           addContribution=.FALSE., &
                           timeIndx=1_I4B, spaceIndx=ips)

    realval = test%js(ips) * test%ws(ips) * test%thickness(ips) * c1bar

    CALL OuterProd_(a=c2bar3(1:tsize), b=test%N(1:test%nns, ips), &
                    anscoeff=1.0_DFP, scale=realval, &
                    ans=ans, nrow=nrow, ncol=ncol)
  END DO

END IF

END PROCEDURE ForceVector_6

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector7
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = FEVariableSize(c2, 1)
dim2 = FEVariableSize(c2, 2)
dim3 = test%nns
CALL Reallocate(ans, dim1, dim2, dim3)
CALL ForceVector_(test=test, c1=c1, c1rank=c1rank, c2=c2, &
                  c2rank=c2rank, ans=ans, dim1=dim1, dim2=dim2, &
                  dim3=dim3)
END PROCEDURE ForceVector7

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_7
! Define internal variable
REAL(DFP), ALLOCATABLE :: c2bar(:, :)
REAL(DFP) :: realval, c1bar
INTEGER(I4B) :: ips, ic, jc

! main
ic = FEVariableSize(c2, 1)
jc = FEVariableSize(c2, 2)
dim3 = test%nns
ans(1:ic, 1:jc, 1:dim3) = 0.0_DFP

ALLOCATE (c2bar(ic, jc))

DO ips = 1, test%nips
  CALL GetInterpolation_(obj=test, val=c2, ans=c2bar, nrow=ic, &
                         ncol=jc, scale=1.0_DFP, &
                         addContribution=.FALSE., &
                         timeIndx=1_I4B, spaceIndx=ips)

  CALL GetInterpolation_(obj=test, val=c1, ans=c1bar, scale=1.0_DFP, &
                         addContribution=.FALSE., &
                         timeIndx=1_I4B, spaceIndx=ips)

  realval = test%js(ips) * test%ws(ips) * test%thickness(ips) * c1bar

  CALL OuterProd_(a=c2bar(1:ic, 1:jc), b=test%N(1:test%nns, ips), &
                  anscoeff=1.0_DFP, scale=realval, &
                  ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
END DO

DEALLOCATE (c2bar)
END PROCEDURE ForceVector_7

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector8
INTEGER(I4B) :: tsize
tsize = test%nns
CALL Reallocate(ans, tsize)
CALL ForceVector_(test=test, c=c, ans=ans, tsize=tsize)
END PROCEDURE ForceVector8

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_8
! Define internal variable
INTEGER(I4B) :: ips
REAL(DFP) :: realval

tsize = test%nns
ans(1:tsize) = 0.0_DFP

DO ips = 1, test%nips
  realval = test%js(ips) * test%ws(ips) * test%thickness(ips) * c(ips)
  ans(1:tsize) = ans(1:tsize) + realval * test%N(1:tsize, ips)
END DO
END PROCEDURE ForceVector_8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
