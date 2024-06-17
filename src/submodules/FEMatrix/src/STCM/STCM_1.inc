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

PURE SUBROUTINE STCM_1a(ans, test, trial, term1, term2, c, projecton)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  TYPE(FEVariable_), INTENT(IN) :: c
  !! Convective velocity, it can be
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_dx
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
  ! Define internal variables
  INTEGER(I4B) :: ips, ipt
  REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
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
        & test(ipt)%N(:, ips), &
        & test(ipt)%T, &
        & p(:, :, ips))
    END DO
    !!
  END DO
  !!
  CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
  !!
  DEALLOCATE (IaJb, p, realval)
END SUBROUTINE STCM_1a

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_1b(ans, test, trial, term1, term2, c, projecton)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  TYPE(FEVariable_), INTENT(IN) :: c
  !! Convective velocity, it can be
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_dx
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
  ! Define internal variables
  INTEGER(I4B) :: ips, ipt
  REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
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
        & trial(ipt)%N(:, ips), &
        & trial(ipt)%T)
    END DO
    !!
  END DO
  !!
  CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
  !!
  DEALLOCATE (IaJb, p, realval)
END SUBROUTINE STCM_1b
