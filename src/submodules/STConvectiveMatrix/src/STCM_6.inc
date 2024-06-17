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

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_6a(ans, test, trial, term1, term2, c, projecton, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! dx, dy, dz
  INTEGER(I4B), INTENT(IN) :: term2
  !! dx, dy, dz
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector variable
  CHARACTER(LEN=*), INTENT(IN) :: projecton
  !! trial
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !!
  !! Define internal variables
  INTEGER(I4B) :: ipt, ips
  REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  !!
  !! main
  !!
  CALL Reallocate(IaJb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt
    !!
    CALL GetProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=p, val=c)
    !!
    DO ips = 1, SIZE(realval)
      IaJb = IaJb + realval(ips) &
        & * outerprod( &
        & test(ipt)%dNTdXt(:, :, term1, ips), &
        & p(:, :, ips))
    END DO
    !!
  END DO
  !!
  CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
  IF( PRESENT(opt) ) CALL MakeDiagonalCopiesIJab(ans, opt)
  !! cleanup
  DEALLOCATE (IaJb, realval, p)
  !!
END SUBROUTINE STCM_6a

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_6b(ans, test, trial, term1, term2, c, projecton, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! dx, dy, dz
  INTEGER(I4B), INTENT(IN) :: term2
  !! dx, dy, dz
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector variable
  CHARACTER(LEN=*), INTENT(IN) :: projecton
  !! test
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !!
  !! Define internal variables
  INTEGER(I4B) :: ipt, ips
  REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  !!
  !! main
  !!
  CALL Reallocate(IaJb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
          & trial(ipt)%wt * trial(ipt)%jt
    !!
    CALL GetProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=p, val=c)
    !!
    DO ips = 1, SIZE(realval)
      IaJb = IaJb + realval(ips) &
        & * outerprod( &
        & p(:, :, ips), &
        & trial(ipt)%dNTdXt(:, :, term2, ips))
    END DO
    !!
  END DO
  !!
  CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
  IF( PRESENT(opt) ) CALL MakeDiagonalCopiesIJab(ans, opt)
  !! cleanup
  DEALLOCATE (IaJb, realval, p)
  !!
END SUBROUTINE STCM_6b
