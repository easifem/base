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
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeDerivativeTerm
USE BaseType, ONLY: TypeFEVariableMatrix
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableVector
USE Display_Method, ONLY: Display
USE ElemshapeData_Method, ONLY: GetProjectionOfdNTdXt_
USE FEVariable_Method, ONLY: FEVariableGetInterpolation_ => GetInterpolation_
USE FEVariable_Method, ONLY: FEVariableSize => Size
USE InputUtility, ONLY: Input
USE ProductUtility, ONLY: OuterProd_
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE
CONTAINS

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
REAL(DFP) :: realval, realval_space, realval_time
INTEGER(I4B) :: ips, ipt, nipt, i1, i2
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

nipt = SIZE(test)

nrow = test(1)%nns
ncol = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * test(ipt)%wt * test(ipt)%jt

  DO ips = 1, test(ipt)%nips
    realval_space = test(ipt)%js(ips) &
      * test(ipt)%ws(ips) * test(ipt)%thickness(ips)

    realval = realval_space * realval_time

    CALL OuterProd_( &
      a=test(ipt)%N(1:nrow, ips), b=test(ipt)%T(1:ncol), &
      anscoeff=math%one, scale=realval, ans=ans, nrow=i1, ncol=i2)
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
REAL(DFP) :: realval, cbar, realval_space, realval_time
INTEGER(I4B) :: nipt, ipt, ips, i1, i2
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

nipt = SIZE(test)
nrow = test(1)%nns
ncol = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * test(ipt)%jt * test(ipt)%wt

  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=cbar)

    realval_space = test(ipt)%js(ips) &
      * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = cbar * realval_space * realval_time

    CALL OuterProd_( &
      a=test(ipt)%N(1:nrow, ips), b=test(ipt)%T(1:ncol), &
      anscoeff=math%one, scale=realval, ans=ans, nrow=i1, ncol=i2)
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
REAL(DFP) :: cbar(fevaropt%defaultVectorSize), realval, realval_space, &
             realval_time
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:dim1, 1:dim2, 1:dim3) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * test(ipt)%jt * test(ipt)%wt

  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, spaceIndx=ips, &
      timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, scale=math%one, &
      addContribution=math%no, ans=cbar, tsize=spaceCompo)

    realval_space = test(ipt)%js(ips) &
      * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = realval_space * realval_time

    CALL OuterProd_(a=cbar(1:dim1), b=test(ipt)%N(1:dim2, ips), &
                    c=test(ipt)%T(1:dim3), anscoeff=math%one, &
                    scale=realval, ans=ans, dim1=i1, dim2=i2, dim3=i3)
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
REAL(DFP) :: cbar(3, 3), realval, realval_space, realval_time
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c, dim=1)
dim2 = FEVariableSize(obj=c, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) &
  ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * test(ipt)%jt * test(ipt)%wt

  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, spaceIndx=ips, &
      timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, scale=math%one, &
      addContribution=math%no, ans=cbar, nrow=i1, ncol=i2)

    realval_space = test(ipt)%js(ips) &
      * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = realval_space * realval_time

    CALL OuterProd_(a=cbar(1:dim1, 1:dim2), b=test(ipt)%N(1:dim3, ips), &
                    c=test(ipt)%T(1:dim4), anscoeff=math%one, &
                    scale=realval, ans=ans, dim1=i1, dim2=i2, dim3=i3, &
                    dim4=i4)
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
REAL(DFP) :: realval, c1bar, c2bar, realval_space, realval_time
INTEGER(I4B) :: nipt, ipt, ips, i1, i2
LOGICAL(LGT) :: isadd0
REAL(DFP) :: scale0

nipt = SIZE(test)
nrow = test(1)%nns
ncol = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * test(ipt)%jt * test(ipt)%wt

  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c1bar)

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c2bar)

    realval_space = test(ipt)%js(ips) * test(ipt)%ws(ips) * &
      test(ipt)%thickness(ips)

    realval = c1bar * c2bar * realval_space * realval_time

    CALL OuterProd_( &
      a=test(ipt)%N(1:nrow, ips), b=test(ipt)%T(1:ncol), &
      anscoeff=math%one, scale=realval, ans=ans, nrow=i1, ncol=i2)

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
REAL(DFP) :: realval, c1bar, c2bar(3), realval_space, realval_time
INTEGER(I4B) :: nipt, ipt, ips, i1, i2, i3
LOGICAL(LGT) :: isadd0
REAL(DFP) :: scale0

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) &
  ans(1:dim1, 1:dim2, 1:dim3) = math%zero

DO ipt = 1, nipt
  realval_time = scale * test(ipt)%jt * test(ipt)%wt

  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c1bar)

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c2bar, tsize=i1)

    realval_space = test(ipt)%js(ips) &
      * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = c1bar * realval_space * realval_time

    CALL OuterProd_( &
      a=c2bar(1:dim1), b=test(ipt)%N(1:dim2, ips), &
      c=test(ipt)%T(1:dim3), anscoeff=math%one, &
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
REAL(DFP) :: realval, realval_space, realval_time, c1bar, &
             c2bar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize)
LOGICAL(LGT) :: isadd0
REAL(DFP) :: scale0

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = FEVariableSize(obj=c2, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) &
  ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * test(ipt)%jt * test(ipt)%wt

  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c1bar)

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c2bar, nrow=i1, ncol=i2)

    realval_space = test(ipt)%js(ips) &
      * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = c1bar * realval_space * realval_time

    CALL OuterProd_( &
      a=c2bar(1:dim1, 1:dim2), b=test(ipt)%N(1:dim3, ips), &
      c=test(ipt)%T(1:dim4), anscoeff=math%one, &
      scale=realval, ans=ans, dim1=i1, dim2=i2, dim3=i3, dim4=i4)

  END DO
END DO
END PROCEDURE obj_STForceVector_7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector8
INTEGER(I4B) :: nrow, ncol
nrow = test(1)%nns
ncol = test(1)%nnt
CALL Reallocate(ans, nrow, ncol)
CALL STForceVector_(test=test, term1=term1, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE obj_STForceVector8

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_8
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)

SELECT CASE (term1)
CASE (TypeDerivativeTerm%NONE)
  CALL STFV_8a(test=test, ans=ans, nrow=nrow, ncol=ncol, &
               scale=scale0, addContribution=isadd0)

CASE (TypeDerivativeTerm%t)
  CALL STFV_8b(test=test, ans=ans, nrow=nrow, ncol=ncol, &
               scale=scale0, addContribution=isadd0)

CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
  CALL STFV_8c(test=test, ans=ans, term1=term1, nrow=nrow, ncol=ncol, &
               scale=scale0, addContribution=isadd0)

! CASE (TypeDerivativeTerm%xAll)
CASE DEFAULT
END SELECT
END PROCEDURE obj_STForceVector_8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! term1 is NONE
PURE SUBROUTINE STFV_8a(test, ans, nrow, ncol, scale, addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Internal variables
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2
  REAL(DFP) :: realval, realval_space, realval_time

  nipt = SIZE(test)
  nrow = test(1)%nns
  ncol = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:nrow, 1:ncol) = math%zero

  DO ipt = 1, nipt
    realval_time = test(ipt)%jt * test(ipt)%wt * scale

    DO ips = 1, test(ipt)%nips
      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = realval_space * realval_time

      CALL OuterProd_(a=test(ipt)%N(1:nrow, ips), &
                      b=test(ipt)%T(1:ncol), &
                      anscoeff=math%one, scale=realval, &
                      ans=ans, nrow=i1, ncol=i2)
    END DO
  END DO
END SUBROUTINE STFV_8a

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

! term1 is t
PURE SUBROUTINE STFV_8b(test, ans, nrow, ncol, scale, addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  REAL(DFP) :: realval, realval_space, realval_time
  INTEGER(I4B) :: ips, ipt, nipt

  !! main
  nipt = SIZE(test)
  nrow = test(1)%nns
  ncol = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:nrow, 1:ncol) = math%zero

  DO ipt = 1, nipt
    realval_time = test(ipt)%jt * test(ipt)%wt * scale

    DO ips = 1, test(ipt)%nips

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = realval_space * realval_time

      ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + &
                            realval * test(ipt)%dNTdt(1:nrow, 1:ncol, ips)
    END DO
  END DO
END SUBROUTINE STFV_8b

!----------------------------------------------------------------------------
!                                                                     STFV_15
!----------------------------------------------------------------------------

PURE SUBROUTINE STFV_8c(test, ans, term1, nrow, ncol, scale, &
                        addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! DEL_x, DEL_y, DEL_z
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  REAL(DFP) :: realval, realval_space, realval_time
  INTEGER(I4B) :: ips, ipt, nipt

  nipt = SIZE(test)
  nrow = test(1)%nns
  ncol = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:nrow, 1:ncol) = math%zero

  DO ipt = 1, nipt
    realval_time = test(ipt)%jt * test(ipt)%wt * scale

    DO ips = 1, test(ipt)%nips

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = realval_space * realval_time

      ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) &
                            + realval * &
                            test(ipt)%dNTdXt(1:nrow, 1:ncol, term1, ips)
    END DO
  END DO
END SUBROUTINE STFV_8c

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector9
INTEGER(I4B) :: nrow, ncol
nrow = test(1)%nns
ncol = test(1)%nnt
CALL Reallocate(ans, nrow, ncol)
CALL STForceVector_(test=test, term1=term1, c=c, crank=crank, ans=ans, &
                    nrow=nrow, ncol=ncol)
END PROCEDURE obj_STForceVector9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_9
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)

SELECT CASE (term1)
CASE (TypeDerivativeTerm%NONE)
  CALL STFV_9a(test=test, c=c, crank=crank, ans=ans, nrow=nrow, &
               ncol=ncol, scale=scale0, addContribution=isadd0)

CASE (TypeDerivativeTerm%t)
  CALL STFV_9b(test=test, c=c, crank=crank, ans=ans, nrow=nrow, &
               ncol=ncol, scale=scale0, addContribution=isadd0)

CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
  CALL STFV_9c(test=test, term1=term1, c=c, crank=crank, ans=ans, &
               nrow=nrow, ncol=ncol, scale=scale0, &
               addContribution=isadd0)

! CASE (TypeDerivativeTerm%xAll)
CASE DEFAULT
END SELECT
END PROCEDURE obj_STForceVector_9

!----------------------------------------------------------------------------
!                                                              STForceVector
!----------------------------------------------------------------------------

PURE SUBROUTINE STFV_9a(test, c, crank, ans, nrow, ncol, scale, &
                        addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariableScalar_), INTENT(IN) :: crank
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2
  REAL(DFP) :: realval, cbar, realval_space, realval_time

  nipt = SIZE(test)
  nrow = test(1)%nns
  ncol = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:nrow, 1:ncol) = math%zero

  DO ipt = 1, nipt
    realval_time = test(ipt)%jt * test(ipt)%wt * scale

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=cbar)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = realval_space * realval_time * cbar

      CALL OuterProd_( &
        a=test(ipt)%N(1:nrow, ips), b=test(ipt)%T(1:ncol), &
        anscoeff=math%one, scale=realval, ans=ans, nrow=i1, ncol=i2)

    END DO
  END DO
END SUBROUTINE STFV_9a

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! term is t
PURE SUBROUTINE STFV_9b(test, c, crank, ans, nrow, ncol, scale, &
                        addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariableScalar_), INTENT(IN) :: crank
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  REAL(DFP) :: realval, cbar, realval_space, realval_time
  INTEGER(I4B) :: ips, ipt, nipt

  nipt = SIZE(test)
  nrow = test(1)%nns
  ncol = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:nrow, 1:ncol) = math%zero

  DO ipt = 1, nipt

    realval_time = test(ipt)%jt * test(ipt)%wt * scale

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=cbar)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = realval_space * realval_time * cbar

      ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + &
                            realval * test(ipt)%dNTdt(1:nrow, 1:ncol, ips)
    END DO
  END DO
END SUBROUTINE STFV_9b

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

! term is x, y, z
PURE SUBROUTINE STFV_9c(test, term1, c, crank, ans, nrow, ncol, scale, &
                        addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  INTEGER(I4B), INTENT(IN) :: term1
  ! DEL_x, DEL_y, DEL_z
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariableScalar_), INTENT(IN) :: crank
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  REAL(DFP) :: realval, cbar, realval_space, realval_time
  INTEGER(I4B) :: ips, ipt, nipt

  nipt = SIZE(test)
  nrow = test(1)%nns
  ncol = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:nrow, 1:ncol) = math%zero

  DO ipt = 1, nipt
    realval_time = test(ipt)%jt * test(ipt)%wt * scale

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=cbar)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = cbar * realval_space * realval_time

      ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + realval * &
                            test(ipt)%dNTdXt(1:nrow, 1:ncol, term1, ips)

    END DO
  END DO
END SUBROUTINE STFV_9c

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector10
INTEGER(I4B) :: dim1, dim2, dim3
dim1 = FEVariableSize(obj=c, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt
CALL Reallocate(ans, dim1, dim2, dim3)
CALL STForceVector_(test=test, term1=term1, c=c, crank=crank, &
                    ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE obj_STForceVector10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_10
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)

SELECT CASE (term1)
CASE (TypeDerivativeTerm%NONE)
  CALL STFV_10a(test=test, c=c, crank=crank, ans=ans, dim1=dim1, &
                dim2=dim2, dim3=dim3, scale=scale0, &
                addContribution=isadd0)

CASE (TypeDerivativeTerm%t)
  CALL STFV_10b(test=test, c=c, crank=crank, ans=ans, dim1=dim1, &
                dim2=dim2, dim3=dim3, scale=scale0, &
                addContribution=isadd0)

CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
  CALL STFV_10c(test=test, c=c, crank=crank, ans=ans, dim1=dim1, &
                dim2=dim2, dim3=dim3, term1=term1, scale=scale0, &
                addContribution=isadd0)

! CASE (TypeDerivativeTerm%xAll)

CASE DEFAULT
END SELECT
END PROCEDURE obj_STForceVector_10

!----------------------------------------------------------------------------
!                                                              STForceVector
!----------------------------------------------------------------------------

PURE SUBROUTINE STFV_10a(test, c, crank, ans, dim1, dim2, dim3, &
                         scale, addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariableVector_), INTENT(IN) :: crank
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  REAL(DFP) :: realval, realval_space, realval_time, cbar(3)
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3

  nipt = SIZE(test)
  dim1 = FEVariableSize(obj=c, dim=1)
  dim2 = test(1)%nns
  dim3 = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:dim1, 1:dim2, 1:dim3) = math%zero

  DO ipt = 1, nipt
    realval_time = test(ipt)%jt * test(ipt)%wt * scale

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=cbar, tsize=i1)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = realval_space * realval_time

      CALL OuterProd_( &
        a=cbar(1:dim1), b=test(ipt)%N(1:dim2, ips), &
        c=test(ipt)%T(1:dim3), &
        ans=ans, dim1=i1, dim2=i2, dim3=i3, &
        anscoeff=math%one, scale=realval)
    END DO
  END DO
END SUBROUTINE STFV_10a

!----------------------------------------------------------------------------
!                                                              STForceVector
!----------------------------------------------------------------------------

! term1 is t
PURE SUBROUTINE STFV_10b(test, c, crank, ans, dim1, dim2, dim3, scale, &
                         addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariableVector_), INTENT(IN) :: crank
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  REAL(DFP) :: realval, realval_space, realval_time, cbar(3)
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3

  nipt = SIZE(test)
  dim1 = FEVariableSize(obj=c, dim=1)
  dim2 = test(1)%nns
  dim3 = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:dim1, 1:dim2, 1:dim3) = math%zero

  DO ipt = 1, nipt
    realval_time = test(ipt)%jt * test(ipt)%wt * scale

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=cbar, tsize=i1)

      realval_space = test(ipt)%js(ips) * test(ipt)%ws(ips) * &
        test(ipt)%thickness(ips)

      realval = realval_space * realval_time

      CALL OuterProd_( &
        a=cbar(1:dim1), b=test(ipt)%dNTdt(1:dim2, 1:dim3, ips), &
        ans=ans, dim1=i1, dim2=i2, dim3=i3, anscoeff=math%one, &
        scale=realval)

    END DO
  END DO
END SUBROUTINE STFV_10b

!----------------------------------------------------------------------------
!                                                              STForceVector
!----------------------------------------------------------------------------

! term1 is x, y, z
PURE SUBROUTINE STFV_10c(test, term1, c, crank, ans, dim1, dim2, dim3, &
                         scale, addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  INTEGER(I4B), INTENT(IN) :: term1
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariableVector_), INTENT(IN) :: crank
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  REAL(DFP) :: realval, realval_space, realval_time, cbar(3)
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3

  nipt = SIZE(test)
  dim1 = FEVariableSize(obj=c, dim=1)
  dim2 = test(1)%nns
  dim3 = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:dim1, 1:dim2, 1:dim3) = math%zero

  DO ipt = 1, nipt
    realval_time = scale * test(ipt)%jt * test(ipt)%wt

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=cbar, tsize=i1)

      realval_space = test(ipt)%js(ips) * test(ipt)%ws(ips) * &
        test(ipt)%thickness(ips)

      realval = realval_space * realval_time

      CALL OuterProd_( &
        a=cbar(1:dim1), b=test(ipt)%dNTdXt(1:dim2, 1:dim3, term1, ips), &
        ans=ans, dim1=i1, dim2=i2, dim3=i3, anscoeff=math%one, scale=realval)

    END DO
  END DO
END SUBROUTINE STFV_10c

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector11
INTEGER(I4B) :: dim1, dim2, dim3, dim4

dim1 = FEVariableSize(obj=c, dim=1)
dim2 = FEVariableSize(obj=c, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt
CALL Reallocate(ans, dim1, dim2, dim3, dim4)
CALL STForceVector_( &
  test=test, term1=term1, c=c, crank=crank, ans=ans, dim1=dim1, dim2=dim2, &
  dim3=dim3, dim4=dim4)
END PROCEDURE obj_STForceVector11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_11
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)

SELECT CASE (term1)
CASE (TypeDerivativeTerm%NONE)
  CALL STFV_11a(test=test, c=c, crank=crank, ans=ans, dim1=dim1, dim2=dim2, &
                dim3=dim3, dim4=dim4, scale=scale0, &
                addContribution=isadd0)

CASE (TypeDerivativeTerm%t)
  CALL STFV_11b(test=test, c=c, crank=crank, ans=ans, dim1=dim1, dim2=dim2, &
                dim3=dim3, dim4=dim4, scale=scale0, &
                addContribution=isadd0)

CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
  CALL STFV_11c(test=test, term1=term1, c=c, crank=crank, ans=ans, &
                dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4, scale=scale0, &
                addContribution=isadd0)

CASE DEFAULT
END SELECT
END PROCEDURE obj_STForceVector_11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! term1 is NONE
PURE SUBROUTINE STFV_11a(test, c, crank, ans, dim1, dim2, dim3, dim4, &
                         scale, addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariableMatrix_), INTENT(IN) :: crank
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  REAL(DFP) :: realval, cbar(3, 3), realval_space, realval_time
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3, i4

  dim1 = FEVariableSize(obj=c, dim=1)
  dim2 = FEVariableSize(obj=c, dim=2)
  dim3 = test(1)%nns
  dim4 = test(1)%nnt
  nipt = SIZE(test)

  IF (.NOT. addContribution) &
    ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero

  DO ipt = 1, nipt
    realval_time = test(ipt)%jt * test(ipt)%wt * scale

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=cbar, nrow=i1, ncol=i2)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = realval_space * realval_time

      CALL OuterProd_( &
        a=cbar(1:dim1, 1:dim2), b=test(ipt)%N(1:dim3, ips), &
        c=test(ipt)%T(1:dim4), ans=ans, dim1=i1, dim2=i2, dim3=i3, dim4=i4, &
        anscoeff=math%one, scale=realval)

    END DO
  END DO
END SUBROUTINE STFV_11a

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! term1 is t
PURE SUBROUTINE STFV_11b(test, c, crank, ans, dim1, dim2, dim3, dim4, &
                         scale, addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariableMatrix_), INTENT(IN) :: crank
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  REAL(DFP) :: realval, realval_space, realval_time, cbar(3, 3)
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3, i4

  dim1 = FEVariableSize(obj=c, dim=1)
  dim2 = FEVariableSize(obj=c, dim=2)
  dim3 = test(1)%nns
  dim4 = test(1)%nnt
  nipt = SIZE(test)

  IF (.NOT. addContribution) &
    ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero

  DO ipt = 1, nipt
    realval_time = scale * test(ipt)%jt * test(ipt)%wt

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=cbar, nrow=i1, ncol=i2)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = realval_space * realval_time

      CALL OuterProd_( &
        a=cbar(1:dim1, 1:dim2), b=test(ipt)%dNTdt(1:dim3, 1:dim4, ips), &
        ans=ans, dim1=i1, dim2=i2, dim3=i3, dim4=i4, &
        anscoeff=math%one, scale=realval)

    END DO
  END DO
END SUBROUTINE STFV_11b

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! term1 is t
PURE SUBROUTINE STFV_11c(test, term1, c, crank, ans, dim1, dim2, dim3, &
                         dim4, scale, addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  INTEGER(I4B), INTENT(IN) :: term1
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariableMatrix_), INTENT(IN) :: crank
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  REAL(DFP) :: realval, realval_space, realval_time, cbar(3, 3)
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3, i4

  dim1 = FEVariableSize(obj=c, dim=1)
  dim2 = FEVariableSize(obj=c, dim=2)
  dim3 = test(1)%nns
  dim4 = test(1)%nnt
  nipt = SIZE(test)

  IF (.NOT. addContribution) &
    ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero

  DO ipt = 1, nipt
    realval_time = scale * test(ipt)%jt * test(ipt)%wt

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c, rank=crank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=cbar, nrow=i1, ncol=i2)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = realval_space * realval_time

      CALL OuterProd_( &
        a=cbar(1:dim1, 1:dim2), &
        b=test(ipt)%dNTdXt(1:dim3, 1:dim4, term1, ips), &
        ans=ans, dim1=i1, dim2=i2, dim3=i3, dim4=i4, &
        anscoeff=math%one, scale=realval)

    END DO
  END DO
END SUBROUTINE STFV_11c

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector12
INTEGER(I4B) :: nrow, ncol
nrow = test(1)%nns
ncol = test(1)%nnt
CALL Reallocate(ans, nrow, ncol)
CALL STForceVector_(test=test, term1=term1, c1=c1, c1rank=c1rank, &
                    c2=c2, c2rank=c2rank, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE obj_STForceVector12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_12
LOGICAL(LGT) :: isadd0
REAL(DFP) :: scale0

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)

SELECT CASE (term1)
CASE (TypeDerivativeTerm%NONE)
  CALL STFV_12a(test=test, c1=c1, c1rank=c1rank, c2=c2, c2rank=c2rank, &
                ans=ans, nrow=nrow, ncol=ncol, scale=scale0, &
                addContribution=isadd0)

CASE (TypeDerivativeTerm%t)
  CALL STFV_12b(test=test, c1=c1, c1rank=c1rank, c2=c2, c2rank=c2rank, &
                ans=ans, nrow=nrow, ncol=ncol, scale=scale0, &
                addContribution=isadd0)

CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
  CALL STFV_12c(test=test, term1=term1, c1=c1, c1rank=c1rank, c2=c2, &
                c2rank=c2rank, ans=ans, nrow=nrow, ncol=ncol, &
                scale=scale0, addContribution=isadd0)

! CASE (TypeDerivativeTerm%xAll)

CASE DEFAULT
END SELECT
END PROCEDURE obj_STForceVector_12

!----------------------------------------------------------------------------
!                                                              STForceVector
!----------------------------------------------------------------------------

! term1 is none
PURE SUBROUTINE STFV_12a(test, c1, c1rank, c2, c2rank, ans, nrow, ncol, &
                         scale, addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  TYPE(FEVariable_), INTENT(IN) :: c1
  TYPE(FEVariable_), INTENT(IN) :: c2
  TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
  TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  REAL(DFP) :: realval, realval_space, realval_time, c1bar, c2bar
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2

  ! main
  nipt = SIZE(test)
  nrow = test(1)%nns
  ncol = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:nrow, 1:ncol) = math%zero

  DO ipt = 1, nipt
    realval_time = scale * test(ipt)%jt * test(ipt)%wt

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c1bar)

      CALL FEVariableGetInterpolation_( &
        obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c2bar)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = c1bar * c2bar * realval_space * realval_time

      CALL OuterProd_( &
        a=test(ipt)%N(1:nrow, ips), b=test(ipt)%T(1:ncol), &
        anscoeff=math%one, scale=realval, ans=ans, nrow=i1, ncol=i2)

    END DO
  END DO
END SUBROUTINE STFV_12a

!----------------------------------------------------------------------------
!                                                              STForceVector
!----------------------------------------------------------------------------

! term1 is t
PURE SUBROUTINE STFV_12b(test, c1, c1rank, c2, c2rank, ans, nrow, ncol, &
                         scale, addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  TYPE(FEVariable_), INTENT(IN) :: c1
  TYPE(FEVariable_), INTENT(IN) :: c2
  TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
  TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  REAL(DFP) :: realval, realval_space, realval_time, c1bar, c2bar
  INTEGER(I4B) :: ips, ipt, nipt

  ! main
  nipt = SIZE(test)
  nrow = test(1)%nns
  ncol = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:nrow, 1:ncol) = math%zero

  DO ipt = 1, nipt
    realval_time = scale * test(ipt)%jt * test(ipt)%wt

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c1bar)

      CALL FEVariableGetInterpolation_( &
        obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c2bar)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = c1bar * c2bar * realval_space * realval_time

      ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + &
                            realval * test(ipt)%dNTdt(1:nrow, 1:ncol, ips)

    END DO
  END DO
END SUBROUTINE STFV_12b

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

! term1 is x, y, z
PURE SUBROUTINE STFV_12c( &
  test, term1, c1, c1rank, c2, c2rank, ans, nrow, ncol, scale, &
  addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  INTEGER(I4B), INTENT(IN) :: term1
  TYPE(FEVariable_), INTENT(IN) :: c1
  TYPE(FEVariable_), INTENT(IN) :: c2
  TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
  TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Define internal variable
  REAL(DFP) :: realval, realval_space, realval_time, c1bar, c2bar
  INTEGER(I4B) :: ips, ipt, nipt

  ! main
  nipt = SIZE(test)
  nrow = test(1)%nns
  ncol = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:nrow, 1:ncol) = math%zero

  DO ipt = 1, nipt
    realval_time = scale * test(ipt)%jt * test(ipt)%wt

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c1bar)

      CALL FEVariableGetInterpolation_( &
        obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c2bar)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = c1bar * c2bar * realval_space * realval_time

      ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + &
                            realval * &
                            test(ipt)%dNTdXt(1:nrow, 1:ncol, term1, ips)
    END DO
  END DO
END SUBROUTINE STFV_12c

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector13
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt
CALL Reallocate(ans, dim1, dim2, dim3)
CALL STForceVector_( &
  test=test, term1=term1, c1=c1, c1rank=c1rank, c2=c2, c2rank=c2rank, &
  ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE obj_STForceVector13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_13
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)

SELECT CASE (term1)

CASE (TypeDerivativeTerm%NONE)
  CALL STFV_13a(test=test, c1=c1, c1rank=c1rank, c2=c2, c2rank=c2rank, &
                ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, scale=scale0, &
                addContribution=isadd0)

CASE (TypeDerivativeTerm%t)
  CALL STFV_13b(test=test, c1=c1, c1rank=c1rank, c2=c2, c2rank=c2rank, &
                ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, scale=scale0, &
                addContribution=isadd0)

CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
  CALL STFV_13c(test=test, term1=term1, c1=c1, c1rank=c1rank, c2=c2, &
                c2rank=c2rank, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, &
                scale=scale0, addContribution=isadd0)

! CASE (TypeDerivativeTerm%xAll)
CASE DEFAULT
END SELECT
END PROCEDURE obj_STForceVector_13

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

PURE SUBROUTINE STFV_13a(test, c1, c1rank, c2, c2rank, ans, dim1, dim2, &
                         dim3, scale, addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  TYPE(FEVariable_), INTENT(IN) :: c1
  TYPE(FEVariable_), INTENT(IN) :: c2
  TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
  TYPE(FEVariableVector_), INTENT(IN) :: c2rank
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Internal variables
  REAL(DFP) :: realval, realval_space, realval_time, c2bar(3), c1bar
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3

  nipt = SIZE(test)
  dim1 = FEVariableSize(obj=c2, dim=1)
  dim2 = test(1)%nns
  dim3 = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:dim1, 1:dim2, 1:dim3) = math%zero

  DO ipt = 1, nipt
    realval_time = scale * test(ipt)%jt * test(ipt)%wt

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c1bar)

      CALL FEVariableGetInterpolation_( &
        obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c2bar, tsize=i1)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = c1bar * realval_space * realval_time

      CALL OuterProd_( &
        a=c2bar(1:dim1), b=test(ipt)%N(1:dim2, ips), c=test(ipt)%T(1:dim3), &
        anscoeff=math%one, scale=realval, ans=ans, dim1=i1, dim2=i2, dim3=i3)

    END DO
  END DO
END SUBROUTINE STFV_13a

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

! term1 is t
PURE SUBROUTINE STFV_13b(test, c1, c1rank, c2, c2rank, ans, dim1, dim2, &
                         dim3, scale, addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  TYPE(FEVariable_), INTENT(IN) :: c1
  TYPE(FEVariable_), INTENT(IN) :: c2
  TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
  TYPE(FEVariableVector_), INTENT(IN) :: c2rank
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Internal variables
  REAL(DFP) :: realval, realval_space, realval_time, c2bar(3), c1bar
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3

  nipt = SIZE(test)
  dim1 = FEVariableSize(obj=c2, dim=1)
  dim2 = test(1)%nns
  dim3 = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:dim1, 1:dim2, 1:dim3) = math%zero

  DO ipt = 1, nipt
    realval_time = scale * test(ipt)%jt * test(ipt)%wt

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c1bar)

      CALL FEVariableGetInterpolation_( &
        obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c2bar, tsize=i1)

      realval_space = test(ipt)%js(ips) * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = c1bar * realval_space * realval_time

      CALL OuterProd_( &
        a=c2bar(1:dim1), b=test(ipt)%dNTdt(1:dim2, 1:dim3, ips), &
        anscoeff=math%one, scale=realval, ans=ans, dim1=i1, dim2=i2, &
        dim3=i3)

    END DO
  END DO
END SUBROUTINE STFV_13b

!----------------------------------------------------------------------------
!                                                           STForceVector_
!----------------------------------------------------------------------------

! term1 is x, y, z
PURE SUBROUTINE STFV_13c(test, term1, c1, c1rank, c2, c2rank, ans, dim1, &
                         dim2, dim3, scale, addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  INTEGER(I4B), INTENT(IN) :: term1
  TYPE(FEVariable_), INTENT(IN) :: c1
  TYPE(FEVariable_), INTENT(IN) :: c2
  TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
  TYPE(FEVariableVector_), INTENT(IN) :: c2rank
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT), INTENT(IN) :: addContribution

  ! Internal variables
  REAL(DFP) :: realval, realval_space, realval_time, c2bar(3), c1bar
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3

  nipt = SIZE(test)
  dim1 = FEVariableSize(obj=c2, dim=1)
  dim2 = test(1)%nns
  dim3 = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:dim1, 1:dim2, 1:dim3) = math%zero

  DO ipt = 1, nipt
    realval_time = scale * test(ipt)%jt * test(ipt)%wt

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c1bar)

      CALL FEVariableGetInterpolation_( &
        obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c2bar, tsize=i1)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = realval_space * realval_time * c1bar

      CALL OuterProd_( &
        a=c2bar(1:dim1), b=test(ipt)%dNTdXt(1:dim2, 1:dim3, term1, ips), &
        anscoeff=math%one, scale=realval, ans=ans, dim1=i1, dim2=i2, &
        dim3=i3)

    END DO
  END DO
END SUBROUTINE STFV_13c

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector14
INTEGER(I4B) :: dim1, dim2, dim3, dim4
dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = FEVariableSize(obj=c2, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt

CALL Reallocate(ans, dim1, dim2, dim3, dim4)
CALL STForceVector_( &
  test=test, term1=term1, c1=c1, c1rank=c1rank, c2=c2, c2rank=c2rank, &
  ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
END PROCEDURE obj_STForceVector14

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_14
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)

SELECT CASE (term1)

CASE (TypeDerivativeTerm%NONE)
  CALL STFV_14a( &
    test=test, c1=c1, c1rank=c1rank, c2=c2, c2rank=c2rank, &
    ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4, &
    scale=scale0, addContribution=isadd0)

CASE (TypeDerivativeTerm%t)
  CALL STFV_14b( &
    test=test, c1=c1, c1rank=c1rank, c2=c2, c2rank=c2rank, &
    ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4, &
    scale=scale0, addContribution=isadd0)

CASE (TypeDerivativeTerm%x, TypeDerivativeTerm%y, TypeDerivativeTerm%z)
  CALL STFV_14c( &
    test=test, term1=term1, c1=c1, c1rank=c1rank, c2=c2, &
    c2rank=c2rank, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4, &
    scale=scale0, addContribution=isadd0)

! CASE (TypeDerivativeTerm%xAll)
CASE DEFAULT
END SELECT
END PROCEDURE obj_STForceVector_14

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

! term1 is none
PURE SUBROUTINE STFV_14a( &
  test, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3, dim4, scale, &
  addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  TYPE(FEVariable_), INTENT(IN) :: c1
  TYPE(FEVariable_), INTENT(IN) :: c2
  TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
  TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution

  !! Internal variables
  REAL(DFP) :: realval, realval_space, realval_time, c1bar, c2bar(3, 3)
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3, i4

  nipt = SIZE(test)
  dim1 = FEVariableSize(obj=c2, dim=1)
  dim2 = FEVariableSize(obj=c2, dim=2)
  dim3 = test(1)%nns
  dim4 = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero

  DO ipt = 1, nipt
    realval_time = scale * test(ipt)%jt * test(ipt)%wt

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c1bar)

      CALL FEVariableGetInterpolation_( &
        obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c2bar, nrow=i1, ncol=i2)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = c1bar * realval_space * realval_time

      CALL OuterProd_( &
        a=c2bar(1:dim1, 1:dim2), b=test(ipt)%N(1:dim3, ips), &
        c=test(ipt)%T(1:dim4), anscoeff=math%one, scale=realval, &
        ans=ans, dim1=i1, dim2=i2, dim3=i3, dim4=i4)

    END DO
  END DO
END SUBROUTINE STFV_14a

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

! term1 is t
PURE SUBROUTINE STFV_14b( &
  test, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3, dim4, scale, &
  addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  TYPE(FEVariable_), INTENT(IN) :: c1
  TYPE(FEVariable_), INTENT(IN) :: c2
  TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
  TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution

  !! Internal variables
  REAL(DFP) :: realval, realval_time, realval_space, c1bar, c2bar(3, 3)
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3, i4

  nipt = SIZE(test)
  dim1 = FEVariableSize(obj=c2, dim=1)
  dim2 = FEVariableSize(obj=c2, dim=2)
  dim3 = test(1)%nns
  dim4 = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero

  DO ipt = 1, nipt
    realval_time = scale * test(ipt)%jt * test(ipt)%wt

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c1bar)

      CALL FEVariableGetInterpolation_( &
        obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c2bar, nrow=i1, ncol=i2)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = c1bar * realval_space * realval_time

      CALL OuterProd_( &
        a=c2bar(1:dim1, 1:dim2), b=test(ipt)%dNTdt(1:dim3, 1:dim4, ips), &
        anscoeff=math%one, scale=realval, ans=ans, dim1=i1, dim2=i2, &
        dim3=i3, dim4=i4)

    END DO
  END DO
END SUBROUTINE STFV_14b

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

! term1 is x, y, z
PURE SUBROUTINE STFV_14c( &
  test, term1, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3, dim4, &
  scale, addContribution)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  INTEGER(I4B), INTENT(IN) :: term1
  TYPE(FEVariable_), INTENT(IN) :: c1
  TYPE(FEVariable_), INTENT(IN) :: c2
  TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
  TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution

  !! Internal variables
  REAL(DFP) :: realval, realval_space, realval_time, c1bar, c2bar(3, 3)
  INTEGER(I4B) :: ips, ipt, nipt, i1, i2, i3, i4

  nipt = SIZE(test)
  dim1 = FEVariableSize(obj=c2, dim=1)
  dim2 = FEVariableSize(obj=c2, dim=2)
  dim3 = test(1)%nns
  dim4 = test(1)%nnt

  IF (.NOT. addContribution) &
    ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero

  DO ipt = 1, nipt
    realval_time = scale * test(ipt)%jt * test(ipt)%wt

    DO ips = 1, test(ipt)%nips

      CALL FEVariableGetInterpolation_( &
        obj=c1, rank=c1rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c1bar)

      CALL FEVariableGetInterpolation_( &
        obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
        spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
        scale=math%one, addContribution=math%no, ans=c2bar, nrow=i1, ncol=i2)

      realval_space = test(ipt)%js(ips) &
        * test(ipt)%ws(ips) &
        * test(ipt)%thickness(ips)

      realval = c1bar * realval_space * realval_time

      CALL OuterProd_( &
        a=c2bar(1:dim1, 1:dim2), &
        b=test(ipt)%dNTdXt(1:dim3, 1:dim4, term1, ips), anscoeff=math%one, &
        scale=realval, ans=ans, dim1=i1, dim2=i2, dim3=i3, dim4=i4)

    END DO
  END DO
END SUBROUTINE STFV_14c

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
REAL(DFP) :: realval, realval_space, realval_time
INTEGER(I4B) :: ips, ipt, nipt, i1, i2
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

nipt = SIZE(test)
nrow = test(1)%nns
ncol = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * test(ipt)%jt * test(ipt)%wt

  DO ips = 1, test(ipt)%nips
    realval_space = test(ipt)%js(ips) &
      * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = realval_space * realval_time

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
REAL(DFP) :: realval, realval_time, realval_space, c2bar
LOGICAL(LGT) :: isadd0
REAL(DFP) :: scale0

nipt = SIZE(test)
nrow = test(1)%nns
ncol = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * test(ipt)%wt * test(ipt)%jt

  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c2bar)

    realval_space = test(ipt)%js(ips) &
      * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = c2bar * realval_space * realval_time

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
REAL(DFP) :: realval, c2bar(fevaropt%defaultVectorSize), &
             realval_space, realval_time
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) &
  ans(1:dim1, 1:dim2, 1:dim3) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * test(ipt)%wt * test(ipt)%jt

  DO ips = 1, test(ipt)%nips

    realval_space = test(ipt)%js(ips) &
      * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = realval_space * realval_time

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c2bar, tsize=i1)

    CALL GetProjectionOfdNTdXt_( &
      obj=test, c=c1, crank=c1rank, ips=ips, ipt=ipt, ans=temp, nrow=i1, &
      ncol=i2)

    CALL OuterProd_( &
      a=c2bar(1:dim1), b=temp(1:dim2, 1:dim3), &
      ans=ans, dim1=i1, dim2=i2, dim3=i3, anscoeff=math%one, scale=realval)

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
REAL(DFP) :: realval, realval_space, realval_time, &
             c2bar(fevaropt%defaultMatrixSize, fevaropt%defaultMatrixSize)
LOGICAL(LGT) :: isadd0
REAL(DFP) :: scale0

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c2, dim=1)
dim2 = FEVariableSize(obj=c2, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) &
  ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * test(ipt)%wt * test(ipt)%jt

  DO ips = 1, test(ipt)%nips
    realval_space = test(ipt)%js(ips) &
      * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = realval_space * realval_time

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c2bar, nrow=i1, ncol=i2)

    CALL GetProjectionOfdNTdXt_( &
      obj=test, c=c1, crank=c1rank, ips=ips, ipt=ipt, ans=temp, nrow=i1, &
      ncol=i2)

    CALL OuterProd_( &
      a=c2bar(1:dim1, 1:dim2), b=temp(1:dim3, 1:dim4), &
      ans=ans, dim1=i1, dim2=i2, dim3=i3, dim4=i4, &
      anscoeff=math%one, scale=realval)

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
REAL(DFP) :: realval, c2bar, c3bar, realval_time, realval_space
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

nipt = SIZE(test)
nrow = test(1)%nns
ncol = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * test(ipt)%wt * test(ipt)%jt

  DO ips = 1, test(ipt)%nips
    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c2bar)

    CALL FEVariableGetInterpolation_( &
      obj=c3, rank=c3rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c3bar)

    realval_space = test(ipt)%js(ips) &
      * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = c2bar * c3bar * realval_space * realval_time

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
REAL(DFP) :: realval, c2bar, c3bar(fevaropt%defaultVectorSize), &
             realval_space, realval_time
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c3, dim=1)
dim2 = test(1)%nns
dim3 = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) &
  ans(1:dim1, 1:dim2, 1:dim3) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * test(ipt)%wt * test(ipt)%jt

  DO ips = 1, test(ipt)%nips

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c2bar)

    CALL FEVariableGetInterpolation_( &
      obj=c3, rank=c3rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c3bar, tsize=i1)

    CALL GetProjectionOfdNTdXt_( &
      obj=test, c=c1, crank=c1rank, ips=ips, ipt=ipt, ans=temp, nrow=i1, &
      ncol=i2)

    realval_space = test(ipt)%js(ips) &
      * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = c2bar * realval_space * realval_time

    CALL OuterProd_( &
      a=c3bar(1:dim1), b=temp(1:dim2, 1:dim3), &
      ans=ans, dim1=i1, dim2=i2, dim3=i3, anscoeff=math%one, scale=realval)

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
REAL(DFP) :: realval, c3bar(3, 3), c2bar, realval_space, realval_time
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

nipt = SIZE(test)
dim1 = FEVariableSize(obj=c3, dim=1)
dim2 = FEVariableSize(obj=c3, dim=2)
dim3 = test(1)%nns
dim4 = test(1)%nnt

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) &
  ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * test(ipt)%wt * test(ipt)%jt

  DO ips = 1, test(ipt)%nips

    CALL GetProjectionOfdNTdXt_( &
      obj=test, c=c1, crank=c1rank, ips=ips, ipt=ipt, ans=temp, nrow=i1, &
      ncol=i2)

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c2bar)

    CALL FEVariableGetInterpolation_( &
      obj=c3, rank=c3rank, N=test(ipt)%N, nns=test(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=test(ipt)%T, nnt=test(ipt)%nnt, &
      scale=math%one, addContribution=math%no, ans=c3bar, nrow=i1, ncol=i2)

    realval_space = test(ipt)%js(ips) &
      * test(ipt)%ws(ips) &
      * test(ipt)%thickness(ips)

    realval = c2bar * realval_space * realval_time

    CALL OuterProd_( &
      a=c3bar(1:dim1, 1:dim2), b=temp(1:dim3, 1:dim4), &
      ans=ans, dim1=i1, dim2=i2, dim3=i3, dim4=i4, &
      anscoeff=math%one, scale=realval)

  END DO
END DO
END PROCEDURE obj_STForceVector_21

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_22
REAL(DFP) :: realval, realval_space, realval_time
INTEGER(I4B) :: ips, ipt, nipt, nips, i1, i2
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

nrow = testSpace%nns
ncol = testTime%nns
nips = testSpace%nips
nipt = testTime%nips

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ipt = 1, nipt
  realval_time = scale * testTime%ws(ipt) * testTime%js(ipt)

  DO ips = 1, nips

    realval_space = testSpace%js(ips) &
      * testSpace%ws(ips) &
      * testSpace%thickness(ips)

    realval = realval_space * realval_time

    CALL OuterProd_( &
      a=testSpace%N(1:nrow, ips), b=testTime%N(1:ncol, ipt), &
      anscoeff=math%one, scale=realval, ans=ans, nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE obj_STForceVector_22

!----------------------------------------------------------------------------
!                                                              STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_23
REAL(DFP) :: realval, cbar, realval_space, realval_time
INTEGER(I4B) :: ips, ipt, nipt, nips, i1, i2
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

nrow = testSpace%nns
ncol = testTime%nns
nips = testSpace%nips
nipt = testTime%nips

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * testTime%ws(ipt) * testTime%js(ipt)

  DO ips = 1, nips

    CALL FEVariableGetInterpolation_( &
      obj=c, rank=crank, N=testSpace%N, nns=testSpace%nns, spaceIndx=ips, &
      timeIndx=ipt, T=testTime%N(:, ipt), nnt=testTime%nns, scale=math%one, &
      addContribution=math%no, ans=cbar)

    realval_space = testSpace%js(ips) &
      * testSpace%ws(ips) &
      * testSpace%thickness(ips)

    realval = cbar * realval_space * realval_time

    CALL OuterProd_( &
      a=testSpace%N(1:nrow, ips), b=testTime%N(1:ncol, ipt), &
      anscoeff=math%one, scale=realval, ans=ans, nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE obj_STForceVector_23

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_24
INTEGER(I4B) :: ips, ipt, nipt, nips, spaceCompo, i1, i2, i3
REAL(DFP) :: cbar(fevaropt%defaultVectorSize), realval, realval_space, &
             realval_time
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

dim1 = FEVariableSize(obj=c, dim=1)
dim2 = testSpace%nns
dim3 = testTime%nns
nips = testSpace%nips
nipt = testTime%nips

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) &
  ans(1:dim1, 1:dim2, 1:dim3) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * testTime%js(ipt) * testTime%ws(ipt)

  DO ips = 1, nips
    CALL FEVariableGetInterpolation_( &
      obj=c, rank=crank, N=testSpace%N, nns=testSpace%nns, spaceIndx=ips, &
      timeIndx=ipt, T=testTime%N(:, ipt), nnt=testTime%nns, scale=math%one, &
      addContribution=math%no, ans=cbar, tsize=spaceCompo)

    realval_space = testSpace%js(ips) &
      * testSpace%ws(ips) &
      * testSpace%thickness(ips)

    realval = realval_space * realval_time

    CALL OuterProd_(a=cbar(1:dim1), b=testSpace%N(1:dim2, ips), &
                    c=testtime%N(1:dim3, ipt), &
                    anscoeff=math%one, scale=realval, &
                    ans=ans, dim1=i1, dim2=i2, dim3=i3)
  END DO
END DO
END PROCEDURE obj_STForceVector_24

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STForceVector_25
REAL(DFP) :: realval, c1bar, c2bar, realval_space, realval_time
INTEGER(I4B) :: ips, ipt, nipt, nips, i1, i2
REAL(DFP) :: scale0
LOGICAL(LGT) :: isadd0

nrow = testSpace%nns
ncol = testTime%nns
nips = testSpace%nips
nipt = testTime%nips

isadd0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)
IF (.NOT. isadd0) ans(1:nrow, 1:ncol) = math%zero

DO ipt = 1, nipt
  realval_time = scale0 * testTime%ws(ipt) * testTime%js(ipt)

  DO ips = 1, nips

    CALL FEVariableGetInterpolation_( &
      obj=c1, rank=c1rank, N=testSpace%N, nns=testSpace%nns, spaceIndx=ips, &
      timeIndx=ipt, T=testTime%N(:, ipt), nnt=testTime%nns, scale=math%one, &
      addContribution=math%no, ans=c1bar)

    CALL FEVariableGetInterpolation_( &
      obj=c2, rank=c2rank, N=testSpace%N, nns=testSpace%nns, spaceIndx=ips, &
      timeIndx=ipt, T=testTime%N(:, ipt), nnt=testTime%nns, scale=math%one, &
      addContribution=math%no, ans=c2bar)

    realval_space = testSpace%js(ips) &
      * testSpace%ws(ips) &
      * testSpace%thickness(ips)

    realval = c1bar * c2bar * realval_space * realval_time

    CALL OuterProd_( &
      a=testSpace%N(1:nrow, ips), b=testTime%N(1:ncol, ipt), &
      anscoeff=math%one, scale=realval, ans=ans, nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE obj_STForceVector_25

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
