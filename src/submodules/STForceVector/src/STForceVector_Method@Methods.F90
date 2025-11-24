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

SUBMODULE(STForceVector_Method) Methods
USE FEVariable_Method, ONLY: FEVariableGetInterpolation_ => GetInterpolation_
USE FEVariable_Method, ONLY: FEVariableSize => Size
USE ReallocateUtility, ONLY: Reallocate
USE ProductUtility, ONLY: OuterProd_
USE BaseType, ONLY: TypeDerivativeTerm
USE BaseType, ONLY: TypeFEVariableSpace, TypeFEVariableVector
USE BaseType, ONLY: TypeFEVariableMatrix
USE ElemshapeData_Method, ONLY: GetProjectionOfdNdXt
USE ElemshapeData_Method, ONLY: GetProjectionOfdNTdXt_

IMPLICIT NONE
CONTAINS

! #include "./include/STFV_1.F90"
! #include "./include/STFV_2.F90"
! #include "./include/STFV_3.F90"
! #include "./include/STFV_4.F90"
! #include "./include/STFV_5.F90"
! #include "./include/STFV_6.F90"
! #include "./include/STFV_7.F90"
!
! #include "./include/STFV_8.F90"
! #include "./include/STFV_9.F90"
! #include "./include/STFV_10.F90"
! #include "./include/STFV_11.F90"
! #include "./include/STFV_12.F90"
! #include "./include/STFV_13.F90"
! #include "./include/STFV_14.F90"
!
! #include "./include/STFV_15.F90"
! #include "./include/STFV_16.F90"
! #include "./include/STFV_17.F90"
! #include "./include/STFV_18.F90"
! #include "./include/STFV_19.F90"
! #include "./include/STFV_20.F90"
! #include "./include/STFV_21.F90"

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector1
INTEGER(I4B) :: nrow, ncol

nrow = test(1)%nns
ncol = test(1)%nnt
CALL Reallocate(ans, nrow, ncol)
CALL STForceVector_(ans=ans, test=test, nrow=nrow, ncol=ncol)
END PROCEDURE obj_STForceVector1

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_1
REAL(DFP) :: realval
INTEGER(I4B) :: ips, ipt, nipt, i1, i2

nipt = SIZE(test)

nrow = test(1)%nns
ncol = test(1)%nnt

ans(1:nrow, 1:ncol) = 0.0_DFP

DO ipt = 1, nipt
  DO ips = 1, test(ipt)%nips
    realval = test(ipt)%js(ips) * test(ipt)%ws(ips) * test(ipt)%jt * &
      test(ipt)%thickness(ips) * test(ipt)%wt

    CALL OuterProd_( &
      a=test(ipt)%N(1:nrow, ips), b=test(ipt)%T(1:ncol), anscoeff=1.0_DFP, &
      scale=realval, ans=ans, nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE obj_STForceVector_1

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector2
INTEGER(I4B) :: nrow, ncol

nrow = test(1)%nns
ncol = test(1)%nnt

CALL Reallocate(ans, nrow, ncol)
CALL STForceVector_(ans=ans, test=test, nrow=nrow, ncol=ncol, c=c, &
                    crank=crank)
END PROCEDURE obj_STForceVector2

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_2
REAL(DFP) :: realval
INTEGER(I4B) :: nipt, ipt, ips, i1, i2

nipt = SIZE(test)
nrow = test(1)%nns
ncol = test(1)%nnt

ans(1:nrow, 1:ncol) = 0.0_DFP

DO ipt = 1, nipt
  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.TRUE., ans=realval)

    realval = realval * test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips) * test(ipt)%jt * test(ipt)%wt

    CALL OuterProd_( &
      a=test(ipt)%N(1:nrow, ips), b=test(ipt)%T(1:ncol), anscoeff=1.0_DFP, &
      scale=realval, ans=ans, nrow=i1, ncol=i2)

  END DO
END DO
END PROCEDURE obj_STForceVector_2

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector3
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = FEVariableSize(obj=c, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt
CALL Reallocate(ans, dim1, dim2, dim3)
CALL STForceVector_(test=test, c=c, crank=crank, ans=ans, dim1=dim1, &
                    dim2=dim2, dim3=dim3)
END PROCEDURE obj_STForceVector3

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_3
INTEGER(I4B) :: ips, ipt, nipt, spaceCompo, i1, i2, i3
REAL(DFP) :: cbar(3), realval

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt

ans(1:dim1, 1:dim2, 1:dim3) = 0.0_DFP

DO ipt = 1, nipt

  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, spaceIndx=ips, &
      timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, scale=1.0_DFP, &
      addContribution=.TRUE., ans=cbar, tsize=spaceCompo)

    realval = test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips) * test(ipt)%jt * test(ipt)%wt

    CALL OuterProd_(a=cbar(1:dim1), b=test(ipt)%N(1:dim2, ips), &
                    c=test(ipt)%T(1:dim3), anscoeff=1.0_DFP, scale=realval, &
                    ans=ans, dim1=i1, dim2=i2, dim3=i3)

  END DO
END DO
END PROCEDURE obj_STForceVector_3

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector4
INTEGER(I4B) :: dim1, dim2, dim3, dim4

dim1 = FEVariableSize(obj=c, dim=1)
dim2 = FEVariableSize(obj=c, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt

CALL Reallocate(ans, dim1, dim2, dim3, dim4)

CALL STForceVector_(test=test, c=c, crank=crank, ans=ans, dim1=dim1, &
                    dim2=dim2, dim3=dim3, dim4=dim4)
END PROCEDURE obj_STForceVector4

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_4
INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3, i4
REAL(DFP) :: cbar(3, 3), realval

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c, dim=1)
dim2 = FEVariableSize(obj=c, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt

ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = 0.0_DFP

DO ipt = 1, nipt

  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, spaceIndx=ips, &
      timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, scale=1.0_DFP, &
      addContribution=.TRUE., ans=cbar, nrow=i1, ncol=i2)

    realval = test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips) * test(ipt)%jt * test(ipt)%wt

    CALL OuterProd_(a=cbar(1:dim1, 1:dim2), b=test(ipt)%N(1:dim3, ips), &
                    c=test(ipt)%T(1:dim4), anscoeff=1.0_DFP, scale=realval, &
                    ans=ans, dim1=i1, dim2=i2, dim3=i3, dim4=i4)

  END DO
END DO
END PROCEDURE obj_STForceVector_4

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector5
INTEGER(I4B) :: nrow, ncol

nrow = test(1)%nns
ncol = test(1)%nnt
CALL Reallocate(ans, nrow, ncol)
CALL STForceVector_(test=test, ans=ans, c1=c1, c1rank=c1rank, c2=c2, &
                    c2rank=c2rank, nrow=nrow, ncol=ncol)
END PROCEDURE obj_STForceVector5

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_5
REAL(DFP) :: realval, c1bar, c2bar
INTEGER(I4B) :: nipt, ipt, ips, i1, i2

nipt = SIZE(test)
nrow = test(1)%nns
ncol = test(1)%nnt

ans(1:nrow, 1:ncol) = 0.0_DFP

DO ipt = 1, nipt
  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.TRUE., ans=c1bar)

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.TRUE., ans=c2bar)

    realval = c1bar * c2bar * test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips) * test(ipt)%jt * test(ipt)%wt

    CALL OuterProd_( &
      a=test(ipt)%N(1:nrow, ips), b=test(ipt)%T(1:ncol), anscoeff=1.0_DFP, &
      scale=realval, ans=ans, nrow=i1, ncol=i2)

  END DO
END DO
END PROCEDURE obj_STForceVector_5

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector6
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt
CALL Reallocate(ans, dim1, dim2, dim3)
CALL STForceVector_(test=test, c1=c1, c1rank=c1rank, c2=c2, &
                    c2rank=c2rank, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE obj_STForceVector6

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_6
REAL(DFP) :: realval, c1bar, c2bar(3)
INTEGER(I4B) :: nipt, ipt, ips, i1, i2, i3

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt

ans(1:dim1, 1:dim2, 1:dim3) = 0.0_DFP

DO ipt = 1, nipt
  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.TRUE., ans=c1bar)

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.TRUE., ans=c2bar, tsize=i1)

    realval = c1bar * test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips) * test(ipt)%jt * test(ipt)%wt

    CALL OuterProd_( &
      a=c2bar(1:dim1), b=test(ipt)%N(1:dim2, ips), &
      c=test(ipt)%T(1:dim3), anscoeff=1.0_DFP, &
      scale=realval, ans=ans, dim1=i1, dim2=i2, dim3=i3)

  END DO
END DO
END PROCEDURE obj_STForceVector_6

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector7
INTEGER(I4B) :: dim1, dim2, dim3, dim4

dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = FEVariableSize(obj=c2, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt

CALL Reallocate(ans, dim1, dim2, dim3, dim4)
CALL STForceVector_(test=test, c1=c1, c1rank=c1rank, c2=c2, &
                    c2rank=c2rank, ans=ans, dim1=dim1, dim2=dim2, &
                    dim3=dim3, dim4=dim4)
END PROCEDURE obj_STForceVector7

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_7
INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3, i4
REAL(DFP) :: realval, c1bar, c2bar(3, 3)

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = FEVariableSize(obj=c2, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt

ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = 0.0_DFP

DO ipt = 1, nipt

  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.TRUE., ans=c1bar)

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.TRUE., ans=c2bar, nrow=i1, ncol=i2)

    realval = c1bar * test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips) * test(ipt)%jt * test(ipt)%wt

    CALL OuterProd_( &
      a=c2bar(1:dim1, 1:dim2), b=test(ipt)%N(1:dim3, ips), &
      c=test(ipt)%T(1:dim4), anscoeff=1.0_DFP, &
      scale=realval, ans=ans, dim1=i1, dim2=i2, dim3=i3, dim4=i4)

  END DO
END DO
END PROCEDURE obj_STForceVector_7

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector15
REAL(DFP), ALLOCATABLE :: temp(:, :)
INTEGER(I4B) :: nrow, ncol

nrow = test(1)%nns
ncol = test(1)%nnt
CALL Reallocate(temp, nrow, ncol)
CALL Reallocate(ans, nrow, ncol)
CALL STForceVector_(test=test, projection=projection, c=c, crank=crank, &
                    ans=ans, nrow=nrow, ncol=ncol, temp=temp)

DEALLOCATE (temp)
END PROCEDURE obj_STForceVector15

!----------------------------------------------------------------------------
!                                                            STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_15
REAL(DFP) :: realval
INTEGER(I4B) :: ips, ipt, nipt, i1, i2

nipt = SIZE(test)
nrow = test(1)%nns
ncol = test(1)%nnt

ans(1:nrow, 1:ncol) = 0.0_DFP

DO ipt = 1, nipt
  DO ips = 1, test(ipt)%nips

    realval = test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips) * test(ipt)%jt * test(ipt)%wt

    CALL GetProjectionOfdNTdXt_( &
      obj=test, ans=temp, c=c, crank=crank, nrow=i1, ncol=i2, ips=ips, &
      ipt=ipt)

    ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + realval * temp(1:i1, 1:i2)
  END DO

END DO
END PROCEDURE obj_STForceVector_15

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector16
INTEGER(I4B) :: nrow, ncol
REAL(DFP), ALLOCATABLE :: temp(:, :)

nrow = test(1)%nns
ncol = test(1)%nnt
CALL Reallocate(temp, nrow, ncol)
CALL Reallocate(ans, nrow, ncol)
CALL STForceVector_( &
  test=test, projection=projection, c1=c1, c1rank=c1rank, c2=c2, &
  c2rank=c2rank, ans=ans, nrow=nrow, ncol=ncol, temp=temp)
END PROCEDURE obj_STForceVector16

!----------------------------------------------------------------------------
!                                                            STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_16
INTEGER(I4B) :: nipt, ipt, ips, i1, i2
REAL(DFP) :: realval

nipt = SIZE(test)
nrow = test(1)%nns
ncol = test(1)%nnt

DO ipt = 1, nipt
  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.FALSE., ans=realval)

    realval = realval * test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips) * test(ipt)%wt * test(ipt)%jt

    CALL GetProjectionOfdNTdXt_( &
      obj=test, c=c1, crank=c1rank, ips=ips, ipt=ipt, &
      ans=temp, nrow=i1, ncol=i2)

    ans(1:i1, 1:i2) = ans(1:i1, 1:i2) + realval * temp(1:i1, 1:i2)
  END DO
END DO
END PROCEDURE obj_STForceVector_16

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector17
INTEGER(I4B) :: dim1, dim2, dim3
REAL(DFP), ALLOCATABLE :: temp(:, :)

dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt
CALL Reallocate(temp, dim2, dim3)
CALL Reallocate(ans, dim1, dim2, dim3)
CALL STForceVector_( &
  test=test, projection=projection, c1=c1, c1rank=c1rank, c2=c2, &
  c2rank=c2rank, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, temp=temp)
DEALLOCATE (temp)
END PROCEDURE obj_STForceVector17

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_17
INTEGER(I4B) :: nipt, ipt, ips, i1, i2, i3
REAL(DFP) :: realval, c2bar(3)

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt

DO ipt = 1, nipt
  DO ips = 1, test(ipt)%nips

    realval = test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips) * test(ipt)%wt * test(ipt)%jt

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.FALSE., ans=c2bar, tsize=i1)

    CALL GetProjectionOfdNTdXt_( &
      obj=test, c=c1, crank=c1rank, ips=ips, ipt=ipt, ans=temp, nrow=i1, &
      ncol=i2)

    CALL OuterProd_( &
      a=c2bar(1:dim1), b=temp(1:dim2, 1:dim3), &
      ans=ans, dim1=i1, dim2=i2, dim3=i3, anscoeff=1.0_DFP, scale=realval)

  END DO
END DO
END PROCEDURE obj_STForceVector_17

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector18
INTEGER(I4B) :: dim1, dim2, dim3, dim4
REAL(DFP), ALLOCATABLE :: temp(:, :)

dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = FEVariableSize(obj=c2, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt

CALL Reallocate(temp, dim3, dim4)
CALL Reallocate(ans, dim1, dim2, dim3, dim4)
CALL STForceVector_( &
  test=test, projection=projection, c1=c1, c1rank=c1rank, c2=c2, &
  c2rank=c2rank, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4, &
  temp=temp)

DEALLOCATE (temp)
END PROCEDURE obj_STForceVector18

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_18
INTEGER(I4B) :: nipt, ipt, ips, i1, i2, i3, i4
REAL(DFP) :: realval, c2bar(3, 3)

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = FEVariableSize(obj=c2, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt

DO ipt = 1, nipt
  DO ips = 1, test(ipt)%nips

    realval = test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips) * test(ipt)%wt * test(ipt)%jt

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.FALSE., ans=c2bar, nrow=i1, ncol=i2)

    CALL GetProjectionOfdNTdXt_( &
      obj=test, c=c1, crank=c1rank, ips=ips, ipt=ipt, ans=temp, nrow=i1, &
      ncol=i2)

    CALL OuterProd_( &
      a=c2bar(1:dim1, 1:dim2), b=temp(1:dim3, 1:dim4), &
      ans=ans, dim1=i1, dim2=i2, dim3=i3, dim4=i4, &
      anscoeff=1.0_DFP, scale=realval)

  END DO
END DO
END PROCEDURE obj_STForceVector_18

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector19
INTEGER(I4B) :: nrow, ncol
REAL(DFP), ALLOCATABLE :: temp(:, :)

nrow = test(1)%nns
ncol = test(1)%nnt
CALL Reallocate(temp, nrow, ncol)
CALL Reallocate(ans, nrow, ncol)

CALL STForceVector_( &
  test=test, projection=projection, c1=c1, c1rank=c1rank, c2=c2, &
  c2rank=c2rank, c3=c3, c3rank=c3rank, ans=ans, nrow=nrow, ncol=ncol, &
  temp=temp)

DEALLOCATE (temp)
END PROCEDURE obj_STForceVector19

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_19
INTEGER(I4B) :: nipt, ipt, ips, i1, i2
REAL(DFP) :: realval, c2bar, c3bar

nipt = SIZE(test)
nrow = test(1)%nns
ncol = test(1)%nnt

DO ipt = 1, nipt
  DO ips = 1, test(ipt)%nips
    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.FALSE., ans=c2bar)

    CALL FEVariableGetInterpolation_( &
      obj=c3, rank=c3rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.FALSE., ans=c3bar)

    realval = c2bar * c3bar * test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips) * test(ipt)%wt * test(ipt)%jt

    CALL GetProjectionOfdNTdXt_( &
      obj=test, c=c1, crank=c1rank, ips=ips, ipt=ipt, &
      ans=temp, nrow=i1, ncol=i2)

    ans(1:i1, 1:i2) = ans(1:i1, 1:i2) + realval * temp(1:i1, 1:i2)
  END DO
END DO
END PROCEDURE obj_STForceVector_19

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector20
INTEGER(I4B) :: dim1, dim2, dim3
REAL(DFP), ALLOCATABLE :: temp(:, :)

dim1 = FEVariableSize(obj=c3, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt

CALL Reallocate(temp, dim2, dim3)
CALL Reallocate(ans, dim1, dim2, dim3)

CALL STForceVector_( &
  test=test, projection=projection, c1=c1, c1rank=c1rank, c2=c2, &
  c2rank=c2rank, c3=c3, c3rank=c3rank, ans=ans, dim1=dim1, dim2=dim2, &
  dim3=dim3, temp=temp)

DEALLOCATE (temp)
END PROCEDURE obj_STForceVector20

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_20
INTEGER(I4B) :: nipt, ipt, ips, i1, i2, i3
REAL(DFP) :: realval, c2bar, c3bar(3)

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c3, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt

DO ipt = 1, nipt
  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.FALSE., ans=c2bar)

    CALL FEVariableGetInterpolation_( &
      obj=c3, rank=c3rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.FALSE., ans=c3bar, tsize=i1)

    CALL GetProjectionOfdNTdXt_( &
      obj=test, c=c1, crank=c1rank, ips=ips, ipt=ipt, ans=temp, nrow=i1, &
      ncol=i2)

    realval = c2bar * test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips) * test(ipt)%wt * test(ipt)%jt

    CALL OuterProd_( &
      a=c3bar(1:dim1), b=temp(1:dim2, 1:dim3), &
      ans=ans, dim1=i1, dim2=i2, dim3=i3, anscoeff=1.0_DFP, scale=realval)

  END DO
END DO
END PROCEDURE obj_STForceVector_20

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector21
INTEGER(I4B) :: dim1, dim2, dim3, dim4
REAL(DFP), ALLOCATABLE :: temp(:, :)

dim1 = FEVariableSize(obj=c3, dim=1)
dim2 = FEVariableSize(obj=c3, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt

CALL Reallocate(temp, dim3, dim4)
CALL Reallocate(ans, dim1, dim2, dim3, dim4)
CALL STForceVector_( &
  test=test, projection=projection, c1=c1, c1rank=c1rank, c2=c2, &
  c2rank=c2rank, c3=c3, c3rank=c3rank, ans=ans, dim1=dim1, dim2=dim2, &
  dim3=dim3, dim4=dim4, temp=temp)

DEALLOCATE (temp)
END PROCEDURE obj_STForceVector21

!----------------------------------------------------------------------------
!                                                          STForceVector21_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_21
INTEGER(I4B) :: nipt, ipt, ips, i1, i2, i3, i4
REAL(DFP) :: realval, c3bar(3, 3), c2bar

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c3, dim=1)
dim2 = FEVariableSize(obj=c3, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt

DO ipt = 1, nipt
  DO ips = 1, test(ipt)%nips

    CALL GetProjectionOfdNTdXt_( &
      obj=test, c=c1, crank=c1rank, ips=ips, ipt=ipt, ans=temp, nrow=i1, &
      ncol=i2)

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.FALSE., ans=c2bar)

    CALL FEVariableGetInterpolation_( &
      obj=c3, rank=c3rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nns, &
      scale=1.0_DFP, addContribution=.FALSE., ans=c3bar, nrow=i1, ncol=i2)

    realval = c2bar * test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips) * test(ipt)%wt * test(ipt)%jt

    CALL OuterProd_( &
      a=c3bar(1:dim1, 1:dim2), b=temp(1:dim3, 1:dim4), &
      ans=ans, dim1=i1, dim2=i2, dim3=i3, dim4=i4, &
      anscoeff=1.0_DFP, scale=realval)

  END DO
END DO
END PROCEDURE obj_STForceVector_21

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector8
! SELECT CASE (term1)
! CASE (TypeDerivativeTerm%NONE)
!   CALL STFV_1(ans=ans, test=test, term1=term1)
!
! CASE (TypeDerivativeTerm%t)
!   CALL STFV_8(ans=ans, test=test, term1=term1)
!
! CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
!   CALL STFV_15(ans=ans, test=test, term1=term1)
!
! CASE (TypeDerivativeTerm%xAll)
!
! END SELECT
END PROCEDURE obj_STForceVector8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector9
! SELECT CASE (term1)
! CASE (TypeDerivativeTerm%NONE)
!   CALL STFV_2(ans=ans, test=test, term1=term1, c=c, crank=crank)
!
! CASE (TypeDerivativeTerm%t)
!   CALL STFV_9(ans=ans, test=test, term1=term1, c=c, crank=crank)
!
! CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
!   CALL STFV_16(ans=ans, test=test, term1=term1, c=c, crank=crank)
!
! CASE (TypeDerivativeTerm%xAll)
! END SELECT
END PROCEDURE obj_STForceVector9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector10
! SELECT CASE (term1)
! CASE (TypeDerivativeTerm%NONE)
!   CALL STFV_3(ans=ans, test=test, term1=term1, c=c, crank=crank)
!
! CASE (TypeDerivativeTerm%t)
!   CALL STFV_10(ans=ans, test=test, term1=term1, c=c, crank=crank)
!
! CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
!   CALL STFV_17(ans=ans, test=test, term1=term1, c=c, crank=crank)
!
! CASE (TypeDerivativeTerm%xAll)
!
! END SELECT
END PROCEDURE obj_STForceVector10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector11
! SELECT CASE (term1)
! CASE (TypeDerivativeTerm%NONE)
!   CALL STFV_4(ans=ans, test=test, term1=term1, c=c, crank=crank)
!
! CASE (TypeDerivativeTerm%t)
!   CALL STFV_11(ans=ans, test=test, term1=term1, c=c, crank=crank)
!
! CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
!   CALL STFV_18(ans=ans, test=test, term1=term1, c=c, crank=crank)
!
! CASE (TypeDerivativeTerm%xAll)
!
! END SELECT
END PROCEDURE obj_STForceVector11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector12
! SELECT CASE (term1)
! CASE (TypeDerivativeTerm%NONE)
!   CALL STFV_5(ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
!               c2=c2, c2rank=c2rank)
! CASE (TypeDerivativeTerm%t)
!   CALL STFV_12(ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
!                c2=c2, c2rank=c2rank)
!
! CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
!   CALL STFV_19(ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
!                c2=c2, c2rank=c2rank)
!
! CASE (TypeDerivativeTerm%xAll)
!
! END SELECT
END PROCEDURE obj_STForceVector12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector13
! SELECT CASE (term1)
!
! CASE (TypeDerivativeTerm%NONE)
!   CALL STFV_6(ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
!               c2=c2, c2rank=c2rank)
!
! CASE (TypeDerivativeTerm%t)
!   CALL STFV_13(ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
!                c2=c2, c2rank=c2rank)
!
! CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
!   CALL STFV_20(ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
!                c2=c2, c2rank=c2rank)
!
! CASE (TypeDerivativeTerm%xAll)
! END SELECT
END PROCEDURE obj_STForceVector13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector14
! SELECT CASE (term1)
!
! CASE (TypeDerivativeTerm%NONE)
!   CALL STFV_7(ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
!               c2=c2, c2rank=c2rank)
!
! CASE (TypeDerivativeTerm%t)
!   CALL STFV_14(ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
!                c2=c2, c2rank=c2rank)
!
! CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
!   CALL STFV_21(ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
!                c2=c2, c2rank=c2rank)
!
! CASE (TypeDerivativeTerm%xAll)
! END SELECT
END PROCEDURE obj_STForceVector14

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
