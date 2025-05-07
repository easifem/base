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

SUBMODULE(ConvectiveMatrix_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

#include "./CM_1.inc"
#include "./CM_2.inc"
#include "./CM_3.inc"
#include "./CM_4.inc"
#include "./CM_5.inc"
#include "./CM_6.inc"
#include "./CM_7.inc"
#include "./CM_8.inc"
#include "./CM_9.inc"
#include "./CM_10.inc"

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvectiveMatrix_1
IF (term1 .EQ. DEL_NONE) THEN
  IF (term2 .EQ. DEL_X_ALL) THEN
    !! del_none
    !! del_x_all
    CALL CM_9(ans=ans, test=test, trial=trial, &
      & term1=term2, term2=term2, opt=opt)
  ELSE
    !! del_none
    !! del_x, del_y, del_z
    CALL CM_7(ans=ans, test=test, trial=trial, &
      & term1=term2, term2=term2, opt=opt)
    !!
  END IF
ELSE
  !! term2 .eq. del_none
  IF (term1 .EQ. del_x_all) THEN
    !! del_x_all
    !! del_none
    CALL CM_10(ans=ans, test=test, trial=trial, &
      & term1=term2, term2=term2, opt=opt)
  ELSE
    !! del_x, del_y, del_z
    !! del_none
    CALL CM_8(ans=ans, test=test, trial=trial, &
      & term1=term2, term2=term2, opt=opt)
  END IF
END IF

END PROCEDURE ConvectiveMatrix_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvectiveMatrix1_

IF (term1 .EQ. DEL_NONE) THEN
  IF (term2 .EQ. DEL_X_ALL) THEN
    CALL CM9_(ans=ans, test=test, trial=trial, &
      & term1=term2, term2=term2, nrow=nrow, ncol=ncol, opt=opt)
  ELSE
    CALL CM7_(ans=ans, test=test, trial=trial, &
      & term1=term2, term2=term2, opt=opt, nrow=nrow, ncol=ncol)
  END IF
ELSE
  IF (term1 .EQ. del_x_all) THEN
    CALL CM10_(ans=ans, test=test, trial=trial, &
      & term1=term2, term2=term2, opt=opt, nrow=nrow, ncol=ncol)
  ELSE
    CALL CM8_(ans=ans, test=test, trial=trial, &
      & term1=term2, term2=term2, opt=opt, nrow=nrow, ncol=ncol)
  END IF
END IF

END PROCEDURE ConvectiveMatrix1_

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvectiveMatrix_2

IF (term1 .EQ. del_none) THEN
  IF (term2 .EQ. del_x_all) THEN
    CALL CM_5(ans=ans, test=test, trial=trial, c=c, &
      & term1=term1, term2=term2, opt=opt)
  ELSE
    CALL CM_3(ans=ans, test=test, trial=trial, c=c, &
      & term1=term2, term2=term2, opt=opt)
  END IF
ELSE
  IF (term1 .EQ. del_x_all) THEN
    CALL CM_6(ans=ans, test=test, trial=trial, c=c, &
      & term1=term1, term2=term2, opt=opt)
  ELSE
    CALL CM_4(ans=ans, test=test, trial=trial, c=c, &
      & term1=term2, term2=term2, opt=opt)
  END IF
END IF
  !!
END PROCEDURE ConvectiveMatrix_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvectiveMatrix2_

IF (term1 .EQ. del_none) THEN
  IF (term2 .EQ. del_x_all) THEN
    CALL CM5_(ans=ans, test=test, trial=trial, c=c, &
      & term1=term1, term2=term2, opt=opt, nrow=nrow, ncol=ncol)
  ELSE
    CALL CM3_(ans=ans, test=test, trial=trial, c=c, &
      & term1=term2, term2=term2, opt=opt, nrow=nrow, ncol=ncol)
  END IF
ELSE
  IF (term1 .EQ. del_x_all) THEN
    CALL CM6_(ans=ans, test=test, trial=trial, c=c, &
      & term1=term1, term2=term2, opt=opt, nrow=nrow, ncol=ncol)
  ELSE
    CALL CM4_(ans=ans, test=test, trial=trial, c=c, &
      & term1=term2, term2=term2, opt=opt, nrow=nrow, ncol=ncol)
  END IF
END IF

END PROCEDURE ConvectiveMatrix2_

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvectiveMatrix_3
  !!
IF (term1 .EQ. del_none) THEN
  CALL CM_1(ans=ans, test=test, trial=trial, c=c, &
    & term1=term1, term2=term2, opt=opt)
ELSE
  CALL CM_2(ans=ans, test=test, trial=trial, c=c, &
    & term1=term1, term2=term2, opt=opt)
END IF
  !!
END PROCEDURE ConvectiveMatrix_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvectiveMatrix3_
IF (term1 .EQ. del_none) THEN
  CALL CM1_(ans=ans, test=test, trial=trial, c=c, &
    & term1=term1, term2=term2, opt=opt, nrow=nrow, ncol=ncol)
ELSE
  CALL CM2_(ans=ans, test=test, trial=trial, c=c, &
    & term1=term1, term2=term2, opt=opt, nrow=nrow, ncol=ncol)
END IF
END PROCEDURE ConvectiveMatrix3_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE CM1_(ans, test, trial, c, term1, term2, opt, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  TYPE(FEVariable_), INTENT(IN) :: c
  INTEGER(I4B), INTENT(IN) :: term1
  INTEGER(I4B), INTENT(IN) :: term2
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  INTEGER(I4B) :: ips, ii, jj
  REAL(DFP) :: p(trial%nns, trial%nips)
  REAL(DFP), PARAMETER :: one = 1.0_DFP
  REAL(DFP) :: realVal

  nrow = test%nns
  ncol = trial%nns
  ans(1:nrow, 1:ncol) = 0.0_DFP

  CALL GetProjectionOfdNdXt_(obj=trial, cdNdXt=p, val=c, nrow=ii, ncol=jj)
  !!
  DO ips = 1, trial%nips
    realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)
    CALL OuterProd_(a=test%N(1:nrow, ips), &
                    b=p(1:ncol, ips), &
                    nrow=ii, ncol=jj, ans=ans, &
                    scale=realval, anscoeff=one)
  END DO

  IF (PRESENT(opt)) THEN
    CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
    nrow = opt * nrow
    ncol = opt * ncol
  END IF

END SUBROUTINE CM1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE CM2_(ans, test, trial, c, term1, term2, opt, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  TYPE(FEVariable_), INTENT(IN) :: c
  INTEGER(I4B), INTENT(IN) :: term1
  INTEGER(I4B), INTENT(IN) :: term2
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  INTEGER(I4B) :: ips, ii, jj
  REAL(DFP) :: p(test%nns, test%nips)
  REAL(DFP) :: realval
  REAL(DFP), PARAMETER :: one = 1.0_DFP

  nrow = test%nns
  ncol = trial%nns
  ans(1:nrow, 1:ncol) = 0.0_DFP

  CALL GetProjectionOfdNdXt_(obj=test, cdNdXt=p, val=c, nrow=ii, ncol=jj)

  DO ips = 1, trial%nips
    realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)
    CALL OuterProd_(a=p(1:nrow, ips), &
                    b=trial%N(1:ncol, ips), &
                    nrow=ii, ncol=jj, ans=ans, &
                    scale=realval, anscoeff=one)
  END DO

  IF (PRESENT(opt)) THEN
    CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
    nrow = opt * nrow
    ncol = opt * ncol
  END IF
END SUBROUTINE CM2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE CM3_(ans, test, trial, term1, term2, c, opt, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  INTEGER(I4B), INTENT(IN) :: term1
  INTEGER(I4B), INTENT(IN) :: term2
  TYPE(FEVariable_), INTENT(IN) :: c
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  INTEGER(I4B) :: ips, ii, jj
  REAL(DFP) :: realval(trial%nips)
  REAL(DFP), PARAMETER :: one = 1.0_DFP

  nrow = test%nns
  ncol = trial%nns
  ans(1:nrow, 1:ncol) = 0.0_DFP

  CALL GetInterpolation_(obj=trial, val=c, interpol=realval, tsize=ii)
  realval(1:ii) = trial%js * trial%ws * trial%thickness * realval(1:ii)

  DO ips = 1, trial%nips
    CALL OuterProd_(a=test%N(1:nrow, ips), &
                    b=trial%dNdXt(1:ncol, term2, ips), &
                    nrow=ii, ncol=jj, ans=ans, &
                    scale=realval(ips), anscoeff=one)
  END DO

  IF (PRESENT(opt)) THEN
    CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
    nrow = opt * nrow
    ncol = opt * ncol
  END IF
END SUBROUTINE CM3_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE CM4_(ans, test, trial, term1, term2, c, opt, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  INTEGER(I4B), INTENT(IN) :: term1
  INTEGER(I4B), INTENT(IN) :: term2
  TYPE(FEVariable_), INTENT(IN) :: c
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  INTEGER(I4B) :: ips, ii, jj
  REAL(DFP) :: realval(trial%nips)
  REAL(DFP), PARAMETER :: one = 1.0_DFP

  nrow = SIZE(test%N, 1)
  ncol = SIZE(trial%N, 1)
  ans(1:nrow, 1:ncol) = 0.0_DFP

  CALL GetInterpolation_(obj=trial, val=c, interpol=realval, tsize=ii)
  realval(1:ii) = trial%js * trial%ws * trial%thickness * realval(1:ii)

  DO ips = 1, trial%nips
    CALL OuterProd_(a=test%dNdXt(1:nrow, term1, ips), &
                    b=trial%N(1:ncol, ips), &
                    nrow=ii, ncol=jj, ans=ans, &
                    scale=realval(ips), anscoeff=one)
  END DO

  IF (PRESENT(opt)) THEN
    CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
    nrow = opt * nrow
    ncol = opt * ncol
  END IF
END SUBROUTINE CM4_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE CM5_(ans, test, trial, term1, term2, c, opt, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  INTEGER(I4B), INTENT(IN) :: term1
  INTEGER(I4B), INTENT(IN) :: term2
  TYPE(FEVariable_), INTENT(IN) :: c
  INTEGER(I4B), INTENT(IN) :: opt
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  INTEGER(I4B) :: ips, ii, jj, kk, nsd
  REAL(DFP) :: realval(trial%nips)
  REAL(DFP) :: m4_1(test%nns, trial%nns, trial%nsd, 1)
  REAL(DFP) :: m4_2(test%nns, trial%nns, 1, trial%nsd)
  REAL(DFP), PARAMETER :: one = 1.0_DFP

  CALL GetInterpolation_(obj=trial, val=c, interpol=realval, tsize=ii)
  realval(1:trial%nips) = trial%js * trial%ws * trial%thickness * realval(1:trial%nips)

  nrow = test%nns
  ncol = trial%nns
  nsd = trial%nsd

  IF (opt .EQ. 1) THEN
    m4_1 = 0.0_DFP
    DO ips = 1, trial%nips
      CALL OuterProd_(a=test%N(1:nrow, ips), &
                      b=trial%dNdXt(1:ncol, 1:nsd, ips), &
                      dim1=ii, dim2=jj, dim3=kk, &
                      ans=m4_1(1:nrow, 1:ncol, 1:nsd, 1), &
                      scale=realval(ips), anscoeff=one)
    END DO
    CALL Convert_(from=m4_1, to=ans, nrow=nrow, ncol=ncol)
  ELSE
    m4_2 = 0.0_DFP
    DO ips = 1, trial%nips
      CALL OuterProd_(a=test%N(1:nrow, ips), &
                      b=trial%dNdXt(1:ncol, 1:nsd, ips), &
                      dim1=ii, dim2=jj, dim3=kk, &
                      ans=m4_1(1:nrow, 1:ncol, 1, 1:nsd), &
                      scale=realval(ips), anscoeff=one)
    END DO
    CALL Convert_(from=m4_2, to=ans, nrow=nrow, ncol=ncol)
  END IF

END SUBROUTINE CM5_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE CM6_(ans, test, trial, term1, term2, c, opt, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  INTEGER(I4B), INTENT(IN) :: term1
  INTEGER(I4B), INTENT(IN) :: term2
  TYPE(FEVariable_), INTENT(IN) :: c
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  INTEGER(I4B) :: ips, ii, jj, kk
  REAL(DFP) :: realval(trial%nips)
  REAL(DFP) :: m4_1(test%nns, trial%nns, trial%nsd, 1)
  REAL(DFP) :: m4_2(test%nns, trial%nns, 1, trial%nsd)
  REAL(DFP), PARAMETER :: one = 1.0_DFP

  nrow = test%nns
  ncol = trial%nns

  CALL GetInterpolation_(obj=trial, val=c, interpol=realval, tsize=ii)
  realval(1:ii) = trial%js * trial%ws * trial%thickness * realval(1:ii)

  IF (opt .EQ. 1) THEN
    m4_1 = 0.0_DFP
    DO ips = 1, trial%nips
      DO ii = 1, trial%nsd
        CALL OuterProd_(a=trial%dNdXt(1:nrow, ii, ips), &
                        b=test%N(1:ncol, ips), &
                        nrow=jj, ncol=kk, ans=m4_1(1:nrow, 1:ncol, ii, 1), &
                        scale=realval(ips), anscoeff=one)
      END DO
    END DO
    CALL Convert_(from=m4_1, to=ans, nrow=nrow, ncol=ncol)
  ELSE
    m4_2 = 0.0_DFP
    DO ips = 1, trial%nips
      DO ii = 1, trial%nsd
        CALL OuterProd_(a=trial%dNdXt(1:nrow, ii, ips), &
                        b=test%N(1:ncol, ips), &
                        nrow=jj, ncol=kk, ans=m4_2(1:nrow, 1:ncol, 1, ii), &
                        scale=realval(ips), anscoeff=one)
      END DO
    END DO
    CALL Convert_(from=m4_2, to=ans, nrow=nrow, ncol=ncol)
  END IF

END SUBROUTINE CM6_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE CM7_(ans, test, trial, term1, term2, opt, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  INTEGER(I4B), INTENT(IN) :: term1
  INTEGER(I4B), INTENT(IN) :: term2
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  INTEGER(I4B) :: ips, ii, jj
  REAL(DFP) :: realval
  REAL(DFP), PARAMETER :: one = 1.0_DFP

  nrow = test%nns
  ncol = trial%nns
  ans(1:nrow, 1:ncol) = 0.0_DFP

  DO ips = 1, trial%nips
    realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)
    CALL OuterProd_(a=test%N(1:nrow, ips), &
                    b=trial%dNdXt(1:ncol, term2, ips), &
                    nrow=ii, ncol=jj, ans=ans, &
                    scale=realval, anscoeff=one)
  END DO

  IF (PRESENT(opt)) THEN
    CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
    nrow = opt * nrow
    ncol = opt * ncol
  END IF

END SUBROUTINE CM7_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE CM8_(ans, test, trial, term1, term2, opt, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  INTEGER(I4B), INTENT(IN) :: term1
  INTEGER(I4B), INTENT(IN) :: term2
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  INTEGER(I4B) :: ips, ii, jj
  REAL(DFP) :: realval
  REAL(DFP), PARAMETER :: one = 1.0_DFP

  nrow = test%nns
  ncol = trial%nns
  ans(1:nrow, 1:ncol) = 0.0_DFP

  DO ips = 1, trial%nips
    realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)
    CALL OuterProd_(a=test%dNdXt(1:nrow, term1, ips), &
                    b=trial%N(1:ncol, ips), &
                    nrow=ii, ncol=jj, ans=ans, &
                    scale=realval, anscoeff=one)
  END DO

  IF (PRESENT(opt)) THEN
    CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
    nrow = opt * nrow
    ncol = opt * ncol
  END IF

END SUBROUTINE CM8_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE CM9_(ans, test, trial, term1, term2, opt, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  INTEGER(I4B), INTENT(IN) :: term1
  INTEGER(I4B), INTENT(IN) :: term2
  INTEGER(I4B), INTENT(IN) :: opt
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  INTEGER(I4B) :: ips, ii, jj, kk
  REAL(DFP), PARAMETER :: one = 1.0_DFP
  REAL(DFP) :: realval
  REAL(DFP) :: m4_1(test%nns, trial%nns, trial%nsd, 1)
  REAL(DFP) :: m4_2(test%nns, trial%nns, 1, trial%nsd)

  nrow = test%nns
  ncol = trial%nns
  IF (opt .EQ. 1) THEN
    m4_1 = 0.0_DFP
    DO ips = 1, trial%nips
      realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)
      DO ii = 1, trial%nsd
        CALL OuterProd_(a=test%N(1:nrow, ips), &
                        b=trial%dNdXt(1:ncol, ii, ips), &
                        nrow=jj, ncol=kk, ans=m4_1(1:nrow, 1:ncol, ii, 1), &
                        scale=realval, anscoeff=one)
      END DO
    END DO
    CALL Convert_(from=m4_1, to=ans, nrow=nrow, ncol=ncol)
  ELSE
    m4_2 = 0.0_DFP
    DO ips = 1, trial%nips
      realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)
      DO ii = 1, trial%nsd
        CALL OuterProd_(a=test%N(1:nrow, ips), &
                        b=trial%dNdXt(1:ncol, ii, ips), &
                        nrow=jj, ncol=kk, ans=m4_2(1:nrow, 1:ncol, 1, ii), &
                        scale=realval, anscoeff=one)
      END DO
    END DO
    CALL Convert_(from=m4_2, to=ans, nrow=nrow, ncol=ncol)
  END IF

END SUBROUTINE CM9_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE CM10_(ans, test, trial, term1, term2, opt, nrow, ncol)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  INTEGER(I4B), INTENT(IN) :: term1
  INTEGER(I4B), INTENT(IN) :: term2
  INTEGER(I4B), INTENT(IN) :: opt
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  INTEGER(I4B) :: ips, ii, jj, kk
  REAL(DFP), PARAMETER :: one = 1.0_DFP
  REAL(DFP) :: realval
  REAL(DFP) :: m4_1(test%nns, trial%nns, trial%nsd, 1)
  REAL(DFP) :: m4_2(test%nns, trial%nns, 1, trial%nsd)

  nrow = test%nns
  ncol = trial%nns
  IF (opt .EQ. 1) THEN
    m4_1 = 0.0_DFP
    DO ips = 1, trial%nips
      realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)
      DO ii = 1, trial%nsd
        CALL OuterProd_(a=test%dNdXt(1:nrow, ii, ips), &
                        b=trial%N(1:ncol, ips), &
                        nrow=jj, ncol=kk, ans=m4_1(1:nrow, 1:ncol, ii, 1), &
                        scale=realval, anscoeff=one)
      END DO
    END DO
    CALL Convert_(from=m4_1, to=ans, nrow=nrow, ncol=ncol)
  ELSE
    m4_2 = 0.0_DFP
    DO ips = 1, trial%nips
      realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)
      DO ii = 1, trial%nsd
        CALL OuterProd_(a=test%dNdXt(1:nrow, ii, ips), &
                        b=trial%N(1:ncol, ips), &
                        nrow=jj, ncol=kk, ans=m4_2(1:nrow, 1:ncol, 1, ii), &
                        scale=realval, anscoeff=one)
      END DO
    END DO
    CALL Convert_(from=m4_2, to=ans, nrow=nrow, ncol=ncol)
  END IF

END SUBROUTINE CM10_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
