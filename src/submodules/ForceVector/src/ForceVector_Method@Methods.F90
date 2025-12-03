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
USE ProductUtility, ONLY: OuterProd_
USE ProductUtility, ONLY: OTimesTilda_
USE FEVariable_Method, ONLY: FEVariableSize => Size
USE FEVariable_Method, ONLY: FEVariableGetInterpolation_ => GetInterpolation_
USE BaseType, ONLY: math => TypeMathOpt

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
REAL(DFP) :: realval, T(0), cbar
INTEGER(I4B) :: ips

tsize = test%nns

ans(1:tsize) = math%zero

DO ips = 1, test%nips
  CALL FEVariableGetInterpolation_( &
    obj=c, rank=crank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=cbar)

  realval = test%js(ips) * test%ws(ips) * test%thickness(ips) * cbar

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
REAL(DFP) :: realval, cbar(3), T(0)
INTEGER(I4B) :: ips, i1, i2

nrow = FEVariableSize(c, 1)
ncol = test%nns
ans(1:nrow, 1:ncol) = 0.0_DFP

DO ips = 1, test%nips
  realval = test%js(ips) * test%ws(ips) * test%thickness(ips)

  CALL FEVariableGetInterpolation_( &
    obj=c, rank=crank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=cbar, tsize=i1)

  CALL OuterProd_(a=cbar(1:nrow), b=test%N(1:ncol, ips), &
                  anscoeff=math%one, scale=realval, &
                  ans=ans, nrow=i1, ncol=i2)
END DO
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
REAL(DFP) :: cbar(3, 3), realval, T(0)
INTEGER(I4B) :: ips, i1, i2, i3

dim1 = FEVariableSize(c, 1)
dim2 = FEVariableSize(c, 2)
dim3 = test%nns

ans(1:dim1, 1:dim2, 1:dim3) = 0.0_DFP

DO ips = 1, test%nips
  realval = test%js(ips) * test%ws(ips) * test%thickness(ips)

  CALL FEVariableGetInterpolation_( &
    obj=c, rank=crank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=cbar, nrow=i1, ncol=i2)

  CALL OuterProd_(a=cbar(1:dim1, 1:dim2), b=test%N(1:dim3, ips), &
                  anscoeff=math%one, scale=realval, &
                  ans=ans, dim1=i1, dim2=i2, dim3=i3)
END DO
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
REAL(DFP) :: c1bar, c2bar, realval, T(0)
INTEGER(I4B) :: ips

tsize = test%nns
ans(1:tsize) = 0.0_DFP

DO ips = 1, test%nips

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=c1bar)

  CALL FEVariableGetInterpolation_( &
    obj=c2, rank=c2rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=c2bar)

  realval = test%js(ips) * test%ws(ips) * test%thickness(ips) &
    * c1bar * c2bar

  ans(1:tsize) = ans(1:tsize) + realval * test%N(1:tsize, ips)
END DO
END PROCEDURE ForceVector_5

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector6
INTEGER(I4B) :: nrow, ncol
nrow = FEVariableSize(c2, 1)
ncol = test%nns
CALL Reallocate(ans, nrow, ncol)
CALL ForceVector_(test=test, c1=c1, c1rank=c1rank, c2=c2, c2rank=c2rank, &
                  ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE ForceVector6

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_6
! Define internal variable
REAL(DFP) :: realval, c1bar, c2bar(3), T(0)
INTEGER(I4B) :: ips, i1, i2

nrow = FEVariableSize(c2, 1)
ncol = test%nns
ans(1:nrow, 1:ncol) = 0.0_DFP

DO ips = 1, test%nips

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=c1bar)

  CALL FEVariableGetInterpolation_( &
    obj=c2, rank=c2rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=c2bar, tsize=i1)

  realval = c1bar * test%js(ips) * test%ws(ips) * test%thickness(ips)

  CALL OuterProd_(a=c2bar(1:nrow), b=test%N(1:ncol, ips), &
                  anscoeff=math%one, scale=realval, &
                  ans=ans, nrow=i1, ncol=i2)
END DO
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
REAL(DFP) :: c2bar(3, 3), realval, c1bar, T(0)
INTEGER(I4B) :: ips, i1, i2, i3

! main
dim1 = FEVariableSize(c2, 1)
dim2 = FEVariableSize(c2, 2)
dim3 = test%nns
ans(1:dim1, 1:dim2, 1:dim3) = math%zero

DO ips = 1, test%nips

  CALL FEVariableGetInterpolation_( &
    obj=c1, rank=c1rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=c1bar)

  CALL FEVariableGetInterpolation_( &
    obj=c2, rank=c2rank, N=test%N, nns=test%nns, spaceIndx=ips, &
    timeIndx=math%one_i, T=T, nnt=math%zero_i, scale=math%one, &
    addContribution=math%no, ans=c2bar, nrow=i1, ncol=i2)

  realval = c1bar * test%js(ips) * test%ws(ips) * test%thickness(ips)

  CALL OuterProd_(a=c2bar(1:dim1, 1:dim2), b=test%N(1:dim3, ips), &
                  anscoeff=math%one, scale=realval, &
                  ans=ans, dim1=i1, dim2=i2, dim3=i3)
END DO
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
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_9
! Define internal variable
INTEGER(I4B) :: ips
REAL(DFP) :: realval

tsize = nns
ans(1:tsize) = 0.0_DFP

DO ips = 1, nips
  realval = js(ips) * ws(ips) * thickness(ips) * c(ips)
  ans(1:tsize) = ans(1:tsize) + realval * N(1:tsize, ips)
END DO
END PROCEDURE ForceVector_9

!----------------------------------------------------------------------------
!                                                                ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_10
LOGICAL(LGT) :: donothing
INTEGER(I4B) :: a, b, mynns

IF (.NOT. skipVertices) THEN
  CALL ForceVector_( &
    N=N, js=js, ws=ws, thickness=thickness, nns=nns, nips=nips, c=c, &
    ans=ans, tsize=tsize)
  RETURN
END IF

donothing = nns .LE. tVertices
IF (donothing) THEN
  tsize = 0
  RETURN
END IF

a = tVertices + 1
b = nns
mynns = nns - tVertices

CALL ForceVector_( &
  N=N(a:b, :), js=js, ws=ws, thickness=thickness, nns=mynns, nips=nips, c=c, &
  ans=ans, tsize=tsize)
END PROCEDURE ForceVector_10

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_11
! Define internal variable
INTEGER(I4B) :: ips, ipt
REAL(DFP) :: realval

tsize = nns * nnt
ans(1:tsize) = 0.0_DFP

DO ipt = 1, nipt
  DO ips = 1, nips
    realval = js(ips) * ws(ips) * spaceThickness(ips) * c(ips, ipt) * &
      wt(ipt) * jt(ipt) * timeThickness(ipt)
    CALL OTimesTilda_(a=timeN(1:nnt, ipt), b=spaceN(1:nns, ips), &
                      anscoeff=math%one, scale=realval, ans=ans, tsize=tsize)
  END DO
END DO
END PROCEDURE ForceVector_11

!----------------------------------------------------------------------------
!                                                                ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_12
LOGICAL(LGT) :: donothing
INTEGER(I4B) :: a, b, d, e, mynns, mynnt

IF (.NOT. skipVertices) THEN
  CALL ForceVector_( &
    spaceN=spaceN, timeN=timeN, js=js, ws=ws, jt=jt, wt=wt, &
    spaceThickness=spaceThickness, timeThickness=timeThickness, nns=nns, &
    nnt=nnt, nips=nips, nipt=nipt, c=c, ans=ans, tsize=tsize)
  RETURN
END IF

donothing = (nns .LE. tSpaceVertices) .OR. (nnt .LE. tTimeVertices)
IF (donothing) THEN
  tsize = 0
  RETURN
END IF

a = tSpaceVertices + 1
b = nns
mynns = nns - tSpaceVertices

d = tTimeVertices + 1
e = nnt
mynnt = nnt - tTimeVertices

CALL ForceVector_( &
  spaceN=spaceN(a:b, :), timeN=timeN(d:e, :), js=js, ws=ws, jt=jt, wt=wt, &
  spaceThickness=spaceThickness, timeThickness=timeThickness, nns=mynns, &
  nnt=mynnt, nips=nips, nipt=nipt, c=c, ans=ans, tsize=tsize)
END PROCEDURE ForceVector_12

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_13
! Define internal variable
INTEGER(I4B) :: ips
REAL(DFP) :: realval

tsize = nns
ans(1:tsize) = 0.0_DFP

DO ips = 1, nips
  realval = js(ips) * ws(ips) * thickness(ips) 
  ans(1:tsize) = ans(1:tsize) + realval * N(1:tsize, ips)
END DO
END PROCEDURE ForceVector_13

!----------------------------------------------------------------------------
!                                                                ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_14
LOGICAL(LGT) :: donothing
INTEGER(I4B) :: a, b, mynns

IF (.NOT. skipVertices) THEN
  CALL ForceVector_( &
    N=N, js=js, ws=ws, thickness=thickness, nns=nns, nips=nips, &
    ans=ans, tsize=tsize)
  RETURN
END IF

donothing = nns .LE. tVertices
IF (donothing) THEN
  tsize = 0
  RETURN
END IF

a = tVertices + 1
b = nns
mynns = nns - tVertices

CALL ForceVector_( &
  N=N(a:b, :), js=js, ws=ws, thickness=thickness, nns=mynns, nips=nips, &
  ans=ans, tsize=tsize)
END PROCEDURE ForceVector_14

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_15
! Define internal variable
INTEGER(I4B) :: ips, ipt
REAL(DFP) :: realval

tsize = nns * nnt
ans(1:tsize) = 0.0_DFP

DO ipt = 1, nipt
  DO ips = 1, nips
    realval = js(ips) * ws(ips) * spaceThickness(ips) * &
      wt(ipt) * jt(ipt) * timeThickness(ipt)
    CALL OTimesTilda_(a=timeN(1:nnt, ipt), b=spaceN(1:nns, ips), &
                      anscoeff=math%one, scale=realval, ans=ans, tsize=tsize)
  END DO
END DO
END PROCEDURE ForceVector_15

!----------------------------------------------------------------------------
!                                                                ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_16
LOGICAL(LGT) :: donothing
INTEGER(I4B) :: a, b, d, e, mynns, mynnt

IF (.NOT. skipVertices) THEN
  CALL ForceVector_( &
    spaceN=spaceN, timeN=timeN, js=js, ws=ws, jt=jt, wt=wt, &
    spaceThickness=spaceThickness, timeThickness=timeThickness, nns=nns, &
    nnt=nnt, nips=nips, nipt=nipt, ans=ans, tsize=tsize)
  RETURN
END IF

donothing = (nns .LE. tSpaceVertices) .OR. (nnt .LE. tTimeVertices)
IF (donothing) THEN
  tsize = 0
  RETURN
END IF

a = tSpaceVertices + 1
b = nns
mynns = nns - tSpaceVertices

d = tTimeVertices + 1
e = nnt
mynnt = nnt - tTimeVertices

CALL ForceVector_( &
  spaceN=spaceN(a:b, :), timeN=timeN(d:e, :), js=js, ws=ws, jt=jt, wt=wt, &
  spaceThickness=spaceThickness, timeThickness=timeThickness, nns=mynns, &
  nnt=mynnt, nips=nips, nipt=nipt, ans=ans, tsize=tsize)
END PROCEDURE ForceVector_16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
