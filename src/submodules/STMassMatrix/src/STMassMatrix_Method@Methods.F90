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

SUBMODULE(STMassMatrix_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STMM_1(ans, test, trial, term1, term2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt

  !! Internal variable
  REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ips, ipt

  CALL Reallocate(IaJb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt
    DO ips = 1, SIZE(realval)
      IaJb = IaJb + realval(ips) * OUTERPROD(  &
      & OUTERPROD(test(ipt)%N(:,ips), test(ipt)%T),  &
      & OUTERPROD(trial(ipt)%N(:,ips), trial(ipt)%T))
    END DO
  END DO
  CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
  IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  DEALLOCATE (IaJb, realval)

END SUBROUTINE STMM_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STMM_2(ans, test, trial, term1, term2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_t
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt

  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ips, ipt

  CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))

  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt
    DO ips = 1, SIZE(realval)
      iajb = iajb + realval(ips) * OUTERPROD( &
      & test(ipt)%dNTdt(:,:,ips),  &
      & OUTERPROD(trial(ipt)%N(:,ips), trial(ipt)%T))
    END DO
  END DO
  CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
  IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  DEALLOCATE (iajb, realval)

END SUBROUTINE STMM_2


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STMM_3(ans, test, trial, term1, term2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_t
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ips, ipt

  CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))

  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt
    DO ips = 1, SIZE(realval)
      iajb = iajb + realval(ips) * OUTERPROD( &
      & OUTERPROD(test(ipt)%N(:,ips), test(ipt)%T),  &
      & trial(ipt)%dNTdt(:,:,ips))
    END DO
  END DO

  CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)

  IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)

  DEALLOCATE (iajb, realval)

END SUBROUTINE STMM_3


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _NT1_ test(ipt)%dNTdt(:,:,ips)
#define _NT2_ trial(ipt)%dNTdt(:,:,ips)

PURE SUBROUTINE STMM_4(ans, test, trial, term1, term2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_t
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips
  !!
  !! main
  !!
  CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt
    DO ips = 1, SIZE(realval)
      iajb = iajb + realval(ips) * OUTERPROD(_NT1_, _NT2_)
    END DO
  END DO
  !!
  CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  !!
  DEALLOCATE (iajb, realval)
  !!
END SUBROUTINE STMM_4
#undef _NT1_
#undef _NT2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _NT1_ OUTERPROD(test(ipt)%N(:,ips), test(ipt)%T)
#define _NT2_ OUTERPROD(trial(ipt)%N(:,ips), trial(ipt)%T)

PURE SUBROUTINE STMM_5(ans, test, trial, term1, term2, rho, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  CLASS(FEVariable_), INTENT(IN) :: rho
  !! scalar
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: rhobar(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips
  !!
  !! main
  !!
  CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  CALL getInterpolation(obj=trial, interpol=rhobar, val=rho)
  !!
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt * rhobar(:, ipt)
    DO ips = 1, SIZE(realval)
      iajb = iajb + realval(ips) * OUTERPROD(_NT1_, _NT2_)
    END DO
  END DO
  !!
  CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  !!
  DEALLOCATE (iajb, rhobar, realval)
  !!
END SUBROUTINE STMM_5
#undef _NT1_
#undef _NT2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _NT1_ test(ipt)%dNTdt(:,:,ips)
#define _NT2_ OUTERPROD(trial(ipt)%N(:,ips), trial(ipt)%T)

PURE SUBROUTINE STMM_6(ans, test, trial, term1, term2, rho, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  CLASS(FEVariable_), INTENT(IN) :: rho
  !! scalar
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: rhobar(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips
  !!
  !! main
  !!
  CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  CALL getInterpolation(obj=trial, interpol=rhobar, val=rho)
  !!
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt * rhobar(:, ipt)
    DO ips = 1, SIZE(realval)
      iajb = iajb + realval(ips) * OUTERPROD(_NT1_, _NT2_)
    END DO
  END DO
  !!
  CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  !!
  DEALLOCATE (iajb, realval)
  !!
END SUBROUTINE STMM_6
#undef _NT1_
#undef _NT2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _NT1_ OUTERPROD(test(ipt)%N(:,ips), test(ipt)%T)
#define _NT2_ trial(ipt)%dNTdt(:,:,ips)

PURE SUBROUTINE STMM_7(ans, test, trial, term1, term2, rho, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  CLASS(FEVariable_), INTENT(IN) :: rho
  !! scalar
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: rhobar(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips
  !!
  !! main
  !!
  CALL Reallocate( &
    & iajb, &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), &
    & SIZE(trial(1)%T))
  !!
  CALL getInterpolation(obj=trial, interpol=rhobar, val=rho)
  !!
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt * rhobar(:, ipt)
    DO ips = 1, SIZE(realval)
      iajb = iajb + realval(ips) * OUTERPROD(_NT1_, _NT2_)
    END DO
  END DO
  !!
  CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  !!
  DEALLOCATE (iajb, rhobar, realval)
  !!
END SUBROUTINE STMM_7
#undef _NT1_
#undef _NT2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _NT1_ test(ipt)%dNTdt(:,:,ips)
#define _NT2_ trial(ipt)%dNTdt(:,:,ips)

PURE SUBROUTINE STMM_8(ans, test, trial, term1, term2, rho, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  CLASS(FEVariable_), INTENT(IN) :: rho
  !! scalar
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: rhobar(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips
  !!
  !! main
  !!
  CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  CALL getInterpolation(obj=trial, interpol=rhobar, val=rho)
  !!
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt * rhobar(:, ipt)
    DO ips = 1, SIZE(realval)
      iajb = iajb + realval(ips) * OUTERPROD(_NT1_, _NT2_)
    END DO
  END DO
  !!
  CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  !!
  DEALLOCATE (iajb, rhobar, realval)
  !!
END SUBROUTINE STMM_8
#undef _NT1_
#undef _NT2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), [1.0_DFP])
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ 1

PURE SUBROUTINE STMM_9a(ans, test, trial, term1, term2, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
!! del_none
  INTEGER(I4B), INTENT(IN) :: term2
!! del_none
  CLASS(FEVariable_), INTENT(IN) :: rho
!! vector
!!
!! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
!!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & _DIM1_, _DIM2_, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
!!
  DO ipt = 1, SIZE(trial)
  !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt
  !!
    DO ips = 1, SIZE(realval)
    !!
      m6 = m6 + realval(ips) * outerprod( &
        & outerprod(test(ipt)%N(:, ips), &
        & trial(ipt)%N(:, ips)), &
        & _KIJ_, &
        & test(ipt)%T, &
        & trial(ipt)%T)
    END DO
  END DO
!!
  CALL Convert(from=m6, to=ans)
  DEALLOCATE (m6, vbar, realval)
!!
END SUBROUTINE STMM_9a

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod([1.0_DFP], vbar(:,ips, ipt))
#define _DIM1_ 1
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_9b(ans, test, trial, term1, term2, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
!! del_none
  INTEGER(I4B), INTENT(IN) :: term2
!! del_none
  CLASS(FEVariable_), INTENT(IN) :: rho
!! vector
!!
!! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
!!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & _DIM1_, _DIM2_, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
!!
  DO ipt = 1, SIZE(trial)
  !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt
  !!
    DO ips = 1, SIZE(realval)
    !!
      m6 = m6 + realval(ips) * outerprod( &
        & outerprod(test(ipt)%N(:, ips), &
        & trial(ipt)%N(:, ips)), &
        & _KIJ_, &
        & test(ipt)%T, &
        & trial(ipt)%T)
    END DO
  END DO
!!
  CALL Convert(from=m6, to=ans)
  DEALLOCATE (m6, vbar, realval)
!!
END SUBROUTINE STMM_9b

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ diag(vbar(:,ips, ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_9c(ans, test, trial, term1, term2, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
!! del_none
  INTEGER(I4B), INTENT(IN) :: term2
!! del_none
  CLASS(FEVariable_), INTENT(IN) :: rho
!! vector
!!
!! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
!!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & _DIM1_, _DIM2_, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
!!
  DO ipt = 1, SIZE(trial)
  !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt
  !!
    DO ips = 1, SIZE(realval)
    !!
      m6 = m6 + realval(ips) * outerprod( &
        & outerprod(test(ipt)%N(:, ips), &
        & trial(ipt)%N(:, ips)), &
        & _KIJ_, &
        & test(ipt)%T, &
        & trial(ipt)%T)
    END DO
  END DO
!!
  CALL Convert(from=m6, to=ans)
  DEALLOCATE (m6, vbar, realval)
!!
END SUBROUTINE STMM_9c

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), vbar(:,ips,ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_9d(ans, test, trial, term1, term2, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
!! del_none
  INTEGER(I4B), INTENT(IN) :: term2
!! del_none
  CLASS(FEVariable_), INTENT(IN) :: rho
!! vector
!!
!! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
!!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & _DIM1_, _DIM2_, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
!!
  DO ipt = 1, SIZE(trial)
  !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt
  !!
    DO ips = 1, SIZE(realval)
    !!
      m6 = m6 + realval(ips) * outerprod( &
        & outerprod(test(ipt)%N(:, ips), &
        & trial(ipt)%N(:, ips)), &
        & _KIJ_, &
        & test(ipt)%T, &
        & trial(ipt)%T)
    END DO
  END DO
!!
  CALL Convert(from=m6, to=ans)
  DEALLOCATE (m6, vbar, realval)
!!
END SUBROUTINE STMM_9d

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), [1.0_DFP])
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ 1

PURE SUBROUTINE STMM_10a(ans, test, trial, term1, term2, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
!! del_t
  INTEGER(I4B), INTENT(IN) :: term2
!! del_none
  CLASS(FEVariable_), INTENT(IN) :: rho
!! vector
!!
!! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: IJija(:, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: Jij(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
!!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & _DIM1_, _DIM2_, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
!!
  CALL Reallocate(IJija, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & SIZE(m6, 3), SIZE(m6, 4), &
    & SIZE(test(1)%T))
!!
  DO ipt = 1, SIZE(trial)
  !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
          & trial(ipt)%wt * trial(ipt)%jt
  !!
    DO ips = 1, SIZE(realval)
    !!
      Jij = OUTERPROD(trial(ipt)%N(:, ips), _KIJ_)
    !!
      DO a = 1, SIZE(m6, 5)
        IJija(:, :, :, :, a) = outerprod(test(ipt)%dNTdt(:, a, ips), Jij)
      END DO
    !!
      m6 = m6 + realval(ips) * outerprod(IJija, trial(ipt)%T)
    !!
    END DO
  END DO
!!
  CALL Convert(from=m6, to=ans)
!!
  DEALLOCATE (m6, IJija, vbar, Jij, realval)
!!
!END SUBROUTINE STMM_10a
END SUBROUTINE STMM_10a

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod([1.0_DFP], vbar(:,ips, ipt))
#define _DIM1_ 1
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_10b(ans, test, trial, term1, term2, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
!! del_t
  INTEGER(I4B), INTENT(IN) :: term2
!! del_none
  CLASS(FEVariable_), INTENT(IN) :: rho
!! vector
!!
!! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: IJija(:, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: Jij(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
!!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & _DIM1_, _DIM2_, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
!!
  CALL Reallocate(IJija, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & SIZE(m6, 3), SIZE(m6, 4), &
    & SIZE(test(1)%T))
!!
  DO ipt = 1, SIZE(trial)
  !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
          & trial(ipt)%wt * trial(ipt)%jt
  !!
    DO ips = 1, SIZE(realval)
    !!
      Jij = OUTERPROD(trial(ipt)%N(:, ips), _KIJ_)
    !!
      DO a = 1, SIZE(m6, 5)
        IJija(:, :, :, :, a) = outerprod(test(ipt)%dNTdt(:, a, ips), Jij)
      END DO
    !!
      m6 = m6 + realval(ips) * outerprod(IJija, trial(ipt)%T)
    !!
    END DO
  END DO
!!
  CALL Convert(from=m6, to=ans)
!!
  DEALLOCATE (m6, IJija, vbar, Jij, realval)
!!
!END SUBROUTINE STMM_10a
END SUBROUTINE STMM_10b

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ diag(vbar(:,ips, ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_10c(ans, test, trial, term1, term2, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
!! del_t
  INTEGER(I4B), INTENT(IN) :: term2
!! del_none
  CLASS(FEVariable_), INTENT(IN) :: rho
!! vector
!!
!! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: IJija(:, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: Jij(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
!!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & _DIM1_, _DIM2_, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
!!
  CALL Reallocate(IJija, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & SIZE(m6, 3), SIZE(m6, 4), &
    & SIZE(test(1)%T))
!!
  DO ipt = 1, SIZE(trial)
  !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
          & trial(ipt)%wt * trial(ipt)%jt
  !!
    DO ips = 1, SIZE(realval)
    !!
      Jij = OUTERPROD(trial(ipt)%N(:, ips), _KIJ_)
    !!
      DO a = 1, SIZE(m6, 5)
        IJija(:, :, :, :, a) = outerprod(test(ipt)%dNTdt(:, a, ips), Jij)
      END DO
    !!
      m6 = m6 + realval(ips) * outerprod(IJija, trial(ipt)%T)
    !!
    END DO
  END DO
!!
  CALL Convert(from=m6, to=ans)
!!
  DEALLOCATE (m6, IJija, vbar, Jij, realval)
!!
!END SUBROUTINE STMM_10a
END SUBROUTINE STMM_10c

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), vbar(:,ips,ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_10d(ans, test, trial, term1, term2, rho)
!PURE SUBROUTINE STMM_10a(ans, test, trial, term1, term2, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
!! del_t
  INTEGER(I4B), INTENT(IN) :: term2
!! del_none
  CLASS(FEVariable_), INTENT(IN) :: rho
!! vector
!!
!! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: IJija(:, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: Jij(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
!!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & _DIM1_, _DIM2_, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
!!
  CALL Reallocate(IJija, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & SIZE(m6, 3), SIZE(m6, 4), &
    & SIZE(test(1)%T))
!!
  DO ipt = 1, SIZE(trial)
  !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
          & trial(ipt)%wt * trial(ipt)%jt
  !!
    DO ips = 1, SIZE(realval)
    !!
      Jij = OUTERPROD(trial(ipt)%N(:, ips), _KIJ_)
    !!
      DO a = 1, SIZE(m6, 5)
        IJija(:, :, :, :, a) = outerprod(test(ipt)%dNTdt(:, a, ips), Jij)
      END DO
    !!
      m6 = m6 + realval(ips) * outerprod(IJija, trial(ipt)%T)
    !!
    END DO
  END DO
!!
  CALL Convert(from=m6, to=ans)
!!
  DEALLOCATE (m6, IJija, vbar, Jij, realval)
!!
!END SUBROUTINE STMM_10a
END SUBROUTINE STMM_10d

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), [1.0_DFP])
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ 1

PURE SUBROUTINE STMM_11a(ans, test, trial, term1, term2, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_t
  CLASS(FEVariable_), INTENT(IN) :: rho
  !! vector
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: ij(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
  !!
  !! main
  !!
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & _DIM1_, _DIM2_, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt
    !!
    DO ips = 1, SIZE(realval)
      !!
      ij = _KIJ_
      !!
      DO b = 1, SIZE(trial(1)%T)
        m6(:, :, :, :, :, b) = m6(:, :, :, :, :, b) &
          & + realval(ips) &
          & * outerprod(  &
            & test(ipt)%N(:, ips), &
            & trial(ipt)%dNTdt(:, b, ips), &
            & ij, test(ipt)%T)
      END DO
    END DO
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, ij, vbar, realval)
  !!
END SUBROUTINE STMM_11a

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod([1.0_DFP], vbar(:,ips, ipt))
#define _DIM1_ 1
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_11b(ans, test, trial, term1, term2, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_t
  CLASS(FEVariable_), INTENT(IN) :: rho
  !! vector
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: ij(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
  !!
  !! main
  !!
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & _DIM1_, _DIM2_, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt
    !!
    DO ips = 1, SIZE(realval)
      !!
      ij = _KIJ_
      !!
      DO b = 1, SIZE(trial(1)%T)
        m6(:, :, :, :, :, b) = m6(:, :, :, :, :, b) &
          & + realval(ips) &
          & * outerprod(  &
            & test(ipt)%N(:, ips), &
            & trial(ipt)%dNTdt(:, b, ips), &
            & ij, test(ipt)%T)
      END DO
    END DO
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, ij, vbar, realval)
END SUBROUTINE STMM_11b

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ diag(vbar(:,ips, ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_11c(ans, test, trial, term1, term2, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_t
  CLASS(FEVariable_), INTENT(IN) :: rho
  !! vector
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: ij(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
  !!
  !! main
  !!
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & _DIM1_, _DIM2_, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt
    !!
    DO ips = 1, SIZE(realval)
      !!
      ij = _KIJ_
      !!
      DO b = 1, SIZE(trial(1)%T)
        m6(:, :, :, :, :, b) = m6(:, :, :, :, :, b) &
          & + realval(ips) &
          & * outerprod(  &
            & test(ipt)%N(:, ips), &
            & trial(ipt)%dNTdt(:, b, ips), &
            & ij, test(ipt)%T)
      END DO
    END DO
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, ij, vbar, realval)
  !!
!END SUBROUTINE STMM_11a
END SUBROUTINE STMM_11c

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), vbar(:,ips,ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_11d(ans, test, trial, term1, term2, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_t
  CLASS(FEVariable_), INTENT(IN) :: rho
  !! vector
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: ij(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
  !!
  !! main
  !!
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & _DIM1_, _DIM2_, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt
    !!
    DO ips = 1, SIZE(realval)
      !!
      ij = _KIJ_
      !!
      DO b = 1, SIZE(trial(1)%T)
        m6(:, :, :, :, :, b) = m6(:, :, :, :, :, b) &
          & + realval(ips) &
          & * outerprod(  &
            & test(ipt)%N(:, ips), &
            & trial(ipt)%dNTdt(:, b, ips), &
            & ij, test(ipt)%T)
      END DO
    END DO
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, ij, vbar, realval)
  !!
END SUBROUTINE STMM_11d

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), [1.0_DFP])
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ 1

PURE SUBROUTINE STMM_12a(ans, test, trial, term1, term2, rho)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_t
CLASS(FEVariable_), INTENT(IN) :: rho
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: ij(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=vbar, val=rho)

!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
        & trial(ipt)%wt * trial(ipt)%jt
  !!
  DO ips = 1, SIZE(realval)
    !!
    ij = _KIJ_
    !!
    DO b = 1, SIZE(m6, 6)
      DO a = 1, SIZE(m6, 5)
        m6(:, :, :, :, a, b) = m6(:, :, :, :, a, b) &
          & + realval(ips) &
          & * outerprod( &
          & test(ipt)%dNTdt(:,a, ips), &
          & trial(ipt)%dNTdt(:,b, ips), &
          & ij )
      END DO
    END DO
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
DEALLOCATE (m6, ij, vbar, realval)
END SUBROUTINE STMM_12a

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod([1.0_DFP], vbar(:,ips, ipt))
#define _DIM1_ 1
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_12b(ans, test, trial, term1, term2, rho)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_t
CLASS(FEVariable_), INTENT(IN) :: rho
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: ij(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=vbar, val=rho)

!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
        & trial(ipt)%wt * trial(ipt)%jt
  !!
  DO ips = 1, SIZE(realval)
    !!
    ij = _KIJ_
    !!
    DO b = 1, SIZE(m6, 6)
      DO a = 1, SIZE(m6, 5)
        m6(:, :, :, :, a, b) = m6(:, :, :, :, a, b) &
          & + realval(ips) &
          & * outerprod( &
          & test(ipt)%dNTdt(:,a, ips), &
          & trial(ipt)%dNTdt(:,b, ips), &
          & ij )
      END DO
    END DO
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
DEALLOCATE (m6, ij, vbar, realval)
END SUBROUTINE STMM_12b

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ diag(vbar(:,ips, ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_12c(ans, test, trial, term1, term2, rho)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_t
CLASS(FEVariable_), INTENT(IN) :: rho
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: ij(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=vbar, val=rho)

!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
        & trial(ipt)%wt * trial(ipt)%jt
  !!
  DO ips = 1, SIZE(realval)
    !!
    ij = _KIJ_
    !!
    DO b = 1, SIZE(m6, 6)
      DO a = 1, SIZE(m6, 5)
        m6(:, :, :, :, a, b) = m6(:, :, :, :, a, b) &
          & + realval(ips) &
          & * outerprod( &
          & test(ipt)%dNTdt(:,a, ips), &
          & trial(ipt)%dNTdt(:,b, ips), &
          & ij )
      END DO
    END DO
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
DEALLOCATE (m6, ij, vbar, realval)
END SUBROUTINE STMM_12c

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), vbar(:,ips,ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_12d(ans, test, trial, term1, term2, rho)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_t
CLASS(FEVariable_), INTENT(IN) :: rho
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: ij(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=vbar, val=rho)

!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
        & trial(ipt)%wt * trial(ipt)%jt
  !!
  DO ips = 1, SIZE(realval)
    !!
    ij = _KIJ_
    !!
    DO b = 1, SIZE(m6, 6)
      DO a = 1, SIZE(m6, 5)
        m6(:, :, :, :, a, b) = m6(:, :, :, :, a, b) &
          & + realval(ips) &
          & * outerprod( &
          & test(ipt)%dNTdt(:,a, ips), &
          & trial(ipt)%dNTdt(:,b, ips), &
          & ij )
      END DO
    END DO
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
DEALLOCATE (m6, ij, vbar, realval)
END SUBROUTINE STMM_12d

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STMM_13(ans, test, trial, term1, term2, rho)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: rho
!! matrix
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=kbar, val=rho)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(kbar,1), SIZE(kbar,2), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt
  !!
  DO ips = 1, SIZE(realval)
    !!
    m6 = m6 + realval( ips ) * outerprod( &
      & outerprod(test(ipt)%N(:, ips), &
      & trial(ipt)%N(:, ips)), &
      & kbar(:,:,ips, ipt) , &
      & test(ipt)%T, &
      & trial(ipt)%T)
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
DEALLOCATE (m6, kbar, realval)
!!
END SUBROUTINE STMM_13



!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
PURE SUBROUTINE STMM_14(ans, test, trial, term1, term2, rho)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: rho
!! matrix
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: IJija(:, :, :, :, :)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :, :)
REAL(DFP), ALLOCATABLE :: Jij(:, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=kbar, val=rho)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(kbar,1), SIZE(kbar,2), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
CALL Reallocate(IJija, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(m6,3), SIZE(m6,4), &
  & SIZE(test(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
        & trial(ipt)%wt * trial(ipt)%jt
  !!
  DO ips = 1, SIZE(realval)
    !!
    Jij = OUTERPROD( trial(ipt)%N(:, ips), kbar(:,:,ips,ipt) )
    !!
    DO a = 1, SIZE(m6, 5)
      IJija(:, :, :, :, a) = outerprod(test(ipt)%dNTdt(:, a, ips), Jij)
    END DO
    !!
    m6 = m6 + realval(ips) * outerprod(IJija, trial(ipt)%T)
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
!!
DEALLOCATE (m6, IJija, kbar, Jij, realval)
  !!
END SUBROUTINE STMM_14



!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STMM_15(ans, test, trial, term1, term2, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_t
  CLASS(FEVariable_), INTENT(IN) :: rho
  !! vector
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: kbar(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
  !!
  !! main
  !!
  CALL getInterpolation(obj=trial, interpol=kbar, val=rho)
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & SIZE(kbar,1), SIZE(kbar,2), &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt
    !!
    DO ips = 1, SIZE(realval)
      !!
      DO b = 1, SIZE(m6,6)
        m6(:, :, :, :, :, b) = m6(:, :, :, :, :, b) &
          & + realval(ips) &
          & * outerprod( &
          & test(ipt)%N(:,ips), &
          & trial(ipt)%dNTdt(:,b,ips), &
          & kbar(:,:,ips, ipt), test(ipt)%T)
      END DO
    END DO
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, kbar, realval)
  !!
END SUBROUTINE STMM_15

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STMM_16(ans, test, trial, term1, term2, rho)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_t
CLASS(FEVariable_), INTENT(IN) :: rho
!! matrix
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=kbar, val=rho)

!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(kbar,1), size(kbar,2), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt
  !!
  DO ips = 1, SIZE(realval)
    !!
    DO b = 1, SIZE(m6, 6)
      DO a = 1, SIZE(m6, 5)
        m6(:, :, :, :, a, b) = m6(:, :, :, :, a, b) &
          & + realval(ips) &
          & * outerprod( &
          & test(ipt)%dNTdt(:,a, ips), &
          & trial(ipt)%dNTdt(:,b, ips), &
          & kbar(:,:,ips, ipt) )
      END DO
    END DO
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
DEALLOCATE (m6, kbar, realval)
  !!
END SUBROUTINE STMM_16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _NT1_ OUTERPROD(test(ipt)%N(:,ips), test(ipt)%T)
#define _NT2_ OUTERPROD(trial(ipt)%N(:,ips), trial(ipt)%T)
PURE SUBROUTINE STMM_17(ans, test, trial, term1, term2, c1, c2, opt)

REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! scalar
INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
REAL(DFP), ALLOCATABLE :: m2(:, :)
REAL(DFP), ALLOCATABLE :: m2b(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips
!!
!! main
!!
CALL Reallocate(IaJb, &
  & SIZE(test(1)%N, 1), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(trial(1)%T))
!!
CALL GetInterpolation(obj=trial, interpol=m2, val=c1)
CALL GetInterpolation(obj=trial, interpol=m2b, val=c2)
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * &
    & trial(ipt)%thickness * trial(ipt)%wt * &
    & trial(ipt)%jt * m2(:, ipt) * m2b(:,ipt)
  !!
  DO ips = 1, SIZE(realval)
    IaJb = IaJb + realval(ips) * OUTERPROD( _NT1_, _NT2_ )
  END DO
  !!
END DO
!!
CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
!!
IF(PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
!!
DEALLOCATE (IaJb, m2, m2b, realval)
END SUBROUTINE STMM_17
#undef _NT1_
#undef _NT2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _NT1_ test(ipt)%dNTdt(:,:,ips)
#define _NT2_ OUTERPROD(trial(ipt)%N(:,ips), trial(ipt)%T)
PURE SUBROUTINE STMM_18(ans, test, trial, term1, term2, c1, c2, opt)

REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! scalar
INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
REAL(DFP), ALLOCATABLE :: m2(:, :)
REAL(DFP), ALLOCATABLE :: m2b(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips
!!
!! main
!!
CALL Reallocate(IaJb, &
  & SIZE(test(1)%N, 1), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(trial(1)%T))
!!
CALL GetInterpolation(obj=trial, interpol=m2, val=c1)
CALL GetInterpolation(obj=trial, interpol=m2b, val=c2)
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * &
    & trial(ipt)%thickness * trial(ipt)%wt * &
    & trial(ipt)%jt * m2(:, ipt) * m2b(:,ipt)
  !!
  DO ips = 1, SIZE(realval)
    IaJb = IaJb + realval(ips) * OUTERPROD( _NT1_, _NT2_ )
  END DO
  !!
END DO
!!
CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
!!
IF(PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
!!
DEALLOCATE (IaJb, m2, m2b, realval)
END SUBROUTINE STMM_18
#undef _NT1_
#undef _NT2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _NT1_ OUTERPROD(test(ipt)%N(:,ips), test(ipt)%T)
#define _NT2_ trial(ipt)%dNTdt(:,:,ips)
PURE SUBROUTINE STMM_19(ans, test, trial, term1, term2, c1, c2, opt)

REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! scalar
INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
REAL(DFP), ALLOCATABLE :: m2(:, :)
REAL(DFP), ALLOCATABLE :: m2b(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips
!!
!! main
!!
CALL Reallocate(IaJb, &
  & SIZE(test(1)%N, 1), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(trial(1)%T))
!!
CALL GetInterpolation(obj=trial, interpol=m2, val=c1)
CALL GetInterpolation(obj=trial, interpol=m2b, val=c2)
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * &
    & trial(ipt)%thickness * trial(ipt)%wt * &
    & trial(ipt)%jt * m2(:, ipt) * m2b(:,ipt)
  !!
  DO ips = 1, SIZE(realval)
    IaJb = IaJb + realval(ips) * OUTERPROD( _NT1_, _NT2_ )
  END DO
  !!
END DO
!!
CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
!!
IF(PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
!!
DEALLOCATE (IaJb, m2, m2b, realval)
END SUBROUTINE STMM_19
#undef _NT1_
#undef _NT2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _NT1_ test(ipt)%dNTdt(:,:,ips)
#define _NT2_ trial(ipt)%dNTdt(:,:,ips)
PURE SUBROUTINE STMM_20(ans, test, trial, term1, term2, c1, c2, opt)

REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! scalar
INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
REAL(DFP), ALLOCATABLE :: m2(:, :)
REAL(DFP), ALLOCATABLE :: m2b(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips
!!
!! main
!!
CALL Reallocate(IaJb, &
  & SIZE(test(1)%N, 1), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(trial(1)%T))
!!
CALL GetInterpolation(obj=trial, interpol=m2, val=c1)
CALL GetInterpolation(obj=trial, interpol=m2b, val=c2)
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * &
    & trial(ipt)%thickness * trial(ipt)%wt * &
    & trial(ipt)%jt * m2(:, ipt) * m2b(:,ipt)
  !!
  DO ips = 1, SIZE(realval)
    IaJb = IaJb + realval(ips) * OUTERPROD( _NT1_, _NT2_ )
  END DO
  !!
END DO
!!
CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
!!
IF(PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
!!
DEALLOCATE (IaJb, m2, m2b, realval)
END SUBROUTINE STMM_20
#undef _NT1_
#undef _NT2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), [1.0_DFP])
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ 1

PURE SUBROUTINE STMM_21a(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    m6 = m6 + realval( ips ) * outerprod( &
      & outerprod(test(ipt)%N(:, ips), &
      & trial(ipt)%N(:, ips)), &
      & _KIJ_, &
      & test(ipt)%T, &
      & trial(ipt)%T)
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
DEALLOCATE (m6, vbar, c1bar, realval)
END SUBROUTINE STMM_21a

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod([1.0_DFP], vbar(:,ips, ipt))
#define _DIM1_ 1
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_21b(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    m6 = m6 + realval( ips ) * outerprod( &
      & outerprod(test(ipt)%N(:, ips), &
      & trial(ipt)%N(:, ips)), &
      & _KIJ_, &
      & test(ipt)%T, &
      & trial(ipt)%T)
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
DEALLOCATE (m6, vbar, c1bar, realval)
END SUBROUTINE STMM_21b

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ diag(vbar(:,ips, ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_21c(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    m6 = m6 + realval( ips ) * outerprod( &
      & outerprod(test(ipt)%N(:, ips), &
      & trial(ipt)%N(:, ips)), &
      & _KIJ_, &
      & test(ipt)%T, &
      & trial(ipt)%T)
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
DEALLOCATE (m6, vbar, c1bar, realval)
END SUBROUTINE STMM_21c

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), vbar(:,ips,ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_21d(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    m6 = m6 + realval( ips ) * outerprod( &
      & outerprod(test(ipt)%N(:, ips), &
      & trial(ipt)%N(:, ips)), &
      & _KIJ_, &
      & test(ipt)%T, &
      & trial(ipt)%T)
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
DEALLOCATE (m6, vbar, c1bar, realval)
END SUBROUTINE STMM_21d

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), [1.0_DFP])
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ 1

PURE SUBROUTINE STMM_22a(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: IJija(:, :, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: Jij(:, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
CALL Reallocate(IJija, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(m6,3), SIZE(m6,4), &
  & SIZE(test(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    Jij = OUTERPROD( trial(ipt)%N(:, ips), _KIJ_)
    !!
    DO a = 1, SIZE(m6, 5)
      IJija(:, :, :, :, a) = outerprod(test(ipt)%dNTdt(:, a, ips), Jij)
    END DO
    !!
    m6 = m6 + realval(ips) * outerprod(IJija, trial(ipt)%T)
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
!!
DEALLOCATE (m6, IJija, vbar, Jij, realval)
END SUBROUTINE STMM_22a

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod([1.0_DFP], vbar(:,ips, ipt))
#define _DIM1_ 1
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_22b(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: IJija(:, :, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: Jij(:, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
CALL Reallocate(IJija, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(m6,3), SIZE(m6,4), &
  & SIZE(test(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    Jij = OUTERPROD( trial(ipt)%N(:, ips), _KIJ_)
    !!
    DO a = 1, SIZE(m6, 5)
      IJija(:, :, :, :, a) = outerprod(test(ipt)%dNTdt(:, a, ips), Jij)
    END DO
    !!
    m6 = m6 + realval(ips) * outerprod(IJija, trial(ipt)%T)
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
!!
DEALLOCATE (m6, IJija, vbar, Jij, realval)
END SUBROUTINE STMM_22b

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ diag(vbar(:,ips, ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_22c(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: IJija(:, :, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: Jij(:, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
CALL Reallocate(IJija, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(m6,3), SIZE(m6,4), &
  & SIZE(test(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    Jij = OUTERPROD( trial(ipt)%N(:, ips), _KIJ_)
    !!
    DO a = 1, SIZE(m6, 5)
      IJija(:, :, :, :, a) = outerprod(test(ipt)%dNTdt(:, a, ips), Jij)
    END DO
    !!
    m6 = m6 + realval(ips) * outerprod(IJija, trial(ipt)%T)
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
!!
DEALLOCATE (m6, IJija, vbar, Jij, realval)
END SUBROUTINE STMM_22c

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), vbar(:,ips,ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_22d(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: IJija(:, :, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: Jij(:, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
CALL Reallocate(IJija, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(m6,3), SIZE(m6,4), &
  & SIZE(test(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    Jij = OUTERPROD( trial(ipt)%N(:, ips), _KIJ_)
    !!
    DO a = 1, SIZE(m6, 5)
      IJija(:, :, :, :, a) = outerprod(test(ipt)%dNTdt(:, a, ips), Jij)
    END DO
    !!
    m6 = m6 + realval(ips) * outerprod(IJija, trial(ipt)%T)
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
!!
DEALLOCATE (m6, IJija, vbar, Jij, realval)
END SUBROUTINE STMM_22d

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), [1.0_DFP])
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ 1

PURE SUBROUTINE STMM_23a(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_t
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: ij(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    ij = _KIJ_
    !!
    DO b = 1, SIZE(trial(1)%T)
      m6(:, :, :, :, :, b) = m6(:, :, :, :, :, b) &
        & + realval(ips) &
        & * outerprod(  &
          & test(ipt)%N(:, ips), &
          & trial(ipt)%dNTdt(:, b, ips), &
          & ij, test(ipt)%T)
    END DO
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
!!
DEALLOCATE (m6, ij, c1bar, vbar, realval)
END SUBROUTINE STMM_23a

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod([1.0_DFP], vbar(:,ips, ipt))
#define _DIM1_ 1
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_23b(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_t
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: ij(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    ij = _KIJ_
    !!
    DO b = 1, SIZE(trial(1)%T)
      m6(:, :, :, :, :, b) = m6(:, :, :, :, :, b) &
        & + realval(ips) &
        & * outerprod(  &
          & test(ipt)%N(:, ips), &
          & trial(ipt)%dNTdt(:, b, ips), &
          & ij, test(ipt)%T)
    END DO
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
!!
DEALLOCATE (m6, ij, c1bar, vbar, realval)
END SUBROUTINE STMM_23b

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ diag(vbar(:,ips, ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_23c(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_t
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: ij(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    ij = _KIJ_
    !!
    DO b = 1, SIZE(trial(1)%T)
      m6(:, :, :, :, :, b) = m6(:, :, :, :, :, b) &
        & + realval(ips) &
        & * outerprod(  &
          & test(ipt)%N(:, ips), &
          & trial(ipt)%dNTdt(:, b, ips), &
          & ij, test(ipt)%T)
    END DO
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
!!
DEALLOCATE (m6, ij, c1bar, vbar, realval)
END SUBROUTINE STMM_23c

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), vbar(:,ips,ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_23d(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_t
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: ij(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    ij = _KIJ_
    !!
    DO b = 1, SIZE(trial(1)%T)
      m6(:, :, :, :, :, b) = m6(:, :, :, :, :, b) &
        & + realval(ips) &
        & * outerprod(  &
          & test(ipt)%N(:, ips), &
          & trial(ipt)%dNTdt(:, b, ips), &
          & ij, test(ipt)%T)
    END DO
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
!!
DEALLOCATE (m6, ij, c1bar, vbar, realval)
END SUBROUTINE STMM_23d

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), [1.0_DFP])
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ 1

PURE SUBROUTINE STMM_24a(ans, test, trial, term1, term2, c1, c2)
#include "./STMM_24.inc"
END SUBROUTINE STMM_24a

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod([1.0_DFP], vbar(:,ips, ipt))
#define _DIM1_ 1
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_24b(ans, test, trial, term1, term2, c1, c2)
#include "./STMM_24.inc"
END SUBROUTINE STMM_24b

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ diag(vbar(:,ips, ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_24c(ans, test, trial, term1, term2, c1, c2)
#include "./STMM_24.inc"
END SUBROUTINE STMM_24c

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), vbar(:,ips,ipt))
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ SIZE(vbar, 1)

PURE SUBROUTINE STMM_24d(ans, test, trial, term1, term2, c1, c2)
#include "./STMM_24.inc"
END SUBROUTINE STMM_24d

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STMM_25(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! matrix
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=kbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(kbar,1), SIZE(kbar,2), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar( :, ipt )
  !!
  DO ips = 1, SIZE(realval)
    !!
    m6 = m6 + realval( ips ) * outerprod( &
      & outerprod(test(ipt)%N(:, ips), &
      & trial(ipt)%N(:, ips)), &
      & kbar(:,:,ips, ipt) , &
      & test(ipt)%T, &
      & trial(ipt)%T)
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
DEALLOCATE (m6, kbar, c1bar, realval)
!!
END SUBROUTINE STMM_25

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STMM_26(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! matrix
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: IJija(:, :, :, :, :)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :, :)
REAL(DFP), ALLOCATABLE :: Jij(:, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=kbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(kbar,1), SIZE(kbar,2), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
CALL Reallocate(IJija, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(m6,3), SIZE(m6,4), &
  & SIZE(test(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
        & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    Jij = OUTERPROD( trial(ipt)%N(:, ips), kbar(:,:,ips,ipt) )
    !!
    DO a = 1, SIZE(m6, 5)
      IJija(:, :, :, :, a) = outerprod(test(ipt)%dNTdt(:, a, ips), Jij)
    END DO
    !!
    m6 = m6 + realval(ips) * outerprod(IJija, trial(ipt)%T)
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
!!
DEALLOCATE (m6, IJija, kbar, Jij, realval)
  !!
END SUBROUTINE STMM_26

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STMM_27(ans, test, trial, term1, term2, c1, c2)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_t
  CLASS(FEVariable_), INTENT(IN) :: c1
  !! scalar
  CLASS(FEVariable_), INTENT(IN) :: c2
  !! vector
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: c1bar(:, :)
  REAL(DFP), ALLOCATABLE :: kbar(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips, a, b
  !!
  !! main
  !!
  CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
  CALL getInterpolation(obj=trial, interpol=kbar, val=c2)
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & SIZE(kbar,1), SIZE(kbar,2), &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt * c1bar(:,ipt)
    !!
    DO ips = 1, SIZE(realval)
      !!
      DO b = 1, SIZE(m6,6)
        m6(:, :, :, :, :, b) = m6(:, :, :, :, :, b) &
          & + realval(ips) &
          & * outerprod( &
          & test(ipt)%N(:,ips), &
          & trial(ipt)%dNTdt(:,b,ips), &
          & kbar(:,:,ips, ipt), test(ipt)%T)
      END DO
    END DO
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (m6, kbar, c1bar, realval)
  !!
END SUBROUTINE STMM_27


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STMM_28(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_t
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! matrix
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=kbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(kbar,1), size(kbar,2), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:,ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    DO b = 1, SIZE(m6, 6)
      DO a = 1, SIZE(m6, 5)
        m6(:, :, :, :, a, b) = m6(:, :, :, :, a, b) &
          & + realval(ips) &
          & * outerprod( &
          & test(ipt)%dNTdt(:,a, ips), &
          & trial(ipt)%dNTdt(:,b, ips), &
          & kbar(:,:,ips, ipt) )
      END DO
    END DO
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
DEALLOCATE (m6, kbar, c1bar, realval)
  !!
END SUBROUTINE STMM_28

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE MakeDiagonalCopiesIJab(ans, ncopy)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  INTEGER(I4B), INTENT(IN) :: ncopy
  !!
  REAL(DFP), ALLOCATABLE :: m2(:, :), m4(:, :, :, :)
  INTEGER(I4B) :: a, b
  !!
  m4 = ans
  !!
  CALL Reallocate(ans, &
    & ncopy * SIZE(m4, 1), &
    & ncopy * SIZE(m4, 2), &
    & SIZE(m4, 3), &
    & SIZE(m4, 4))
  !!
  DO b = 1, SIZE(m4, 4)
    DO a = 1, SIZE(m4, 3)
      CALL MakeDiagonalCopies(from=m4(:, :, a, b), to=m2, ncopy=ncopy)
      ans(:, :, a, b) = m2
    END DO
  END DO
  !!
  DEALLOCATE (m2, m4)
END SUBROUTINE MakeDiagonalCopiesIJab

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_1
SELECT CASE (term1)
  !!
  !!
  !!
  !!
CASE (del_t)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
      !! del_t
      !! del_t
    CALL STMM_4(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, opt=opt)
      !!
  CASE (del_none)
      !! del_t
      !! del_none
    CALL STMM_2(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, opt=opt)
      !!
  END SELECT
  !!
  !!
  !!
  !!
CASE (del_none)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
      !! del_none
      !! del_t
    CALL STMM_3(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, opt=opt)
      !!
  CASE (del_none)
      !! del_none
      !! del_none
    CALL STMM_1(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, opt=opt)
      !!
  END SELECT
END SELECT
  !!
END PROCEDURE mat4_STMassMatrix_1

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_2
  !!
SELECT CASE (term1)
  !!
  !!
  !!
  !!
CASE (del_t)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
      !!
      !! del_t
      !! del_t
      !!
    CALL STMM_8(ans=ans, test=test, trial=trial, rho=rho, &
      & term1=term1, term2=term2, opt=opt)
    !!
  CASE (del_none)
      !!
      !! del_t
      !! del_none
      !!
    CALL STMM_6(ans=ans, test=test, trial=trial, rho=rho, &
      & term1=term1, term2=term2, opt=opt)
    !!
  END SELECT
  !!
  !!
  !!
  !!
CASE (del_none)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
      !!
      !! del_none
      !! del_t
      !!
    CALL STMM_7(ans=ans, test=test, trial=trial, rho=rho, &
      & term1=term1, term2=term2, opt=opt)
    !!
  CASE (del_none)
      !!
      !! del_none
      !! del_none
      !!
    CALL STMM_5(ans=ans, test=test, trial=trial, rho=rho, &
      & term1=term1, term2=term2, opt=opt)
    !!
  END SELECT
END SELECT
  !!
END PROCEDURE mat4_STMassMatrix_2

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_3
  !!
  !! main
  !!
SELECT CASE (term1)
  !!
  !!
  !!
  !!
CASE (del_t)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
      !!
      !! del_t
      !! del_t
      !!
    SELECT CASE (opt)
    CASE (1)
      CALL STMM_12a(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    CASE (2)
      CALL STMM_12b(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    CASE (3)
      CALL STMM_12c(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    CASE (4)
      CALL STMM_12d(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    END SELECT
    !!
  CASE (del_none)
      !!
      !! del_t
      !! del_none
      !!
    SELECT CASE (opt)
    CASE (1)
      CALL STMM_10a(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    CASE (2)
      CALL STMM_10b(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    CASE (3)
      CALL STMM_10c(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    CASE (4)
      CALL STMM_10d(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    END SELECT
    !!
  END SELECT
  !!
  !!
  !!
  !!
CASE (del_none)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
      !!
      !! del_none
      !! del_t
      !!
    SELECT CASE (opt)
    CASE (1)
      CALL STMM_11a(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    CASE (2)
      CALL STMM_11b(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    CASE (3)
      CALL STMM_11c(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    CASE (4)
      CALL STMM_11d(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    END SELECT
    !!
  CASE (del_none)
      !!
      !! del_none
      !! del_none
      !!
    SELECT CASE (opt)
    CASE (1)
      CALL STMM_9a(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    CASE (2)
      CALL STMM_9b(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    CASE (3)
      CALL STMM_9c(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    CASE (4)
      CALL STMM_9d(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    END SELECT
    !!
  END SELECT
END SELECT
  !!
END PROCEDURE mat4_STMassMatrix_3

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_4
  !!
  !! main
  !!
SELECT CASE (term1)
  !!
  !!
  !!
  !!
CASE (del_t)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
      !!
      !! del_t
      !! del_t
      !!
    CALL STMM_16(ans=ans, test=test, trial=trial, rho=rho, &
      & term1=term1, term2=term2)
    !!
  CASE (del_none)
      !!
      !! del_t,
      !! del_none
      !!
    CALL STMM_14(ans=ans, test=test, trial=trial, rho=rho, &
      & term1=term1, term2=term2)
    !!
  END SELECT
  !!
  !!
  !!
  !!
CASE (del_none)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
      !!
      !! del_none,
      !! del_t
      !!
    CALL STMM_15(ans=ans, test=test, trial=trial, rho=rho, &
      & term1=term1, term2=term2)
    !!
  CASE (del_none)
      !!
      !! del_none,
      !! del_none,
      !!
    CALL STMM_13(ans=ans, test=test, trial=trial, rho=rho, &
      & term1=term1, term2=term2)
    !!
  END SELECT
END SELECT
END PROCEDURE mat4_STMassMatrix_4

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_5
SELECT CASE (term1)
  !!
  !!
  !!
  !!
CASE (del_t)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
    !!
    !! scalar
    !! scalar
    !! del_t
    !! del_t
    !!
    CALL STMM_20(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
      & term1=term1, term2=term2, opt=opt)
    !!
  CASE (del_none)
    !!
    !! scalar
    !! scalar
    !! del_t
    !! del_none
    !!
    CALL STMM_18(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
      & term1=term1, term2=term2, opt=opt)
    !!
  END SELECT
    !!
CASE (del_none)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
    !!
    !! scalar
    !! scalar
    !! del_none
    !! del_t
    !!
    CALL STMM_19(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
      & term1=term1, term2=term2, opt=opt)
    !!
  CASE (del_none)
    !!
    !! scalar
    !! scalar
    !! del_none
    !! del_none
    !!
    CALL STMM_17(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
      & term1=term1, term2=term2, opt=opt)
    !!
  END SELECT
END SELECT
  !!
END PROCEDURE mat4_STMassMatrix_5

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_6
  !!
SELECT CASE (term1)
  !!
  !!
  !!
  !!
CASE (del_t)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
      !!
      !! scalar
      !! vector
      !! del_t
      !! del_t
      !!
    SELECT CASE (opt)
    CASE (1)
      CALL STMM_24a(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    CASE (2)
      CALL STMM_24b(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    CASE (3)
      CALL STMM_24c(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    CASE (4)
      CALL STMM_24d(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    END SELECT
    !!
  CASE (del_none)
      !!
      !! scalar
      !! vector
      !! del_t
      !! del_none
      !!
    SELECT CASE (opt)
    CASE (1)
      CALL STMM_22a(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    CASE (2)
      CALL STMM_22b(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    CASE (3)
      CALL STMM_22c(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    CASE (4)
      CALL STMM_22d(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    END SELECT
    !!
  END SELECT
  !!
  !!
  !!
  !!
CASE (del_none)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
      !!
      !! scalar
      !! vector
      !! del_none
      !! del_t
      !!
    SELECT CASE (opt)
    CASE (1)
      CALL STMM_23a(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    CASE (2)
      CALL STMM_23b(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    CASE (3)
      CALL STMM_23c(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    CASE (4)
      CALL STMM_23d(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    END SELECT
    !!
  CASE (del_none)
      !!
      !! scalar
      !! vector
      !! del_none
      !! del_none
      !!
    SELECT CASE (opt)
    CASE (1)
      CALL STMM_21a(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    CASE (2)
      CALL STMM_21b(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    CASE (3)
      CALL STMM_21c(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    CASE (4)
      CALL STMM_21d(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    END SELECT
    !!
  END SELECT
END SELECT
  !!
END PROCEDURE mat4_STMassMatrix_6

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_7
SELECT CASE (term1)
  !!
  !!
  !!
  !!
CASE (del_t)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
      !!
      !! scalar
      !! matrix
      !! del_t
      !! del_t
      !!
    CALL STMM_28(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
      & term1=term1, term2=term2)
    !!
  CASE (del_none)
      !!
      !! scalar
      !! matrix
      !! del_t
      !! del_none
      !!
    CALL STMM_26(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
      & term1=term1, term2=term2)
    !!
  END SELECT
    !!
CASE (del_none)
    !!
  SELECT CASE (term2)
    !!
  CASE (del_t)
      !!
      !! scalar
      !! matrix
      !! del_none
      !! del_t
      !!
    CALL STMM_27(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
      & term1=term1, term2=term2)
    !!
  CASE (del_none)
      !!
      !! scalar
      !! matrix
      !! del_none
      !! del_none
      !!
    CALL STMM_25(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
      & term1=term1, term2=term2)
    !!
  END SELECT
END SELECT
END PROCEDURE mat4_STMassMatrix_7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
