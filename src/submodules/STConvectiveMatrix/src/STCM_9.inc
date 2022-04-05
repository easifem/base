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

PURE SUBROUTINE STCM_9a(ans, test, trial, term1, term2, rho, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  TYPE(FEVariable_), INTENT(IN) :: rho
  !! scalar variable
  TYPE(FEVariable_), INTENT(IN) :: c
  !! convective velocity
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_dx
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !! ncopy
  ! Define internal variables
  INTEGER(I4B) :: ips, ipt
  REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: rhobar(:,:)
  !! main
  CALL Reallocate(IaJb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
  CALL GetInterpolation(obj=trial, val=rho, interpol=rhobar)
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt * rhobar(:,ipt)
    !!
    CALL GetProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=p, val=c)
    !!
    DO ips = 1, SIZE(realval)
      IaJb = IaJb + realval(ips) &
        & * outerprod( &
        & test(ipt)%N(:, ips), test(ipt)%T, &
        & p(:, :, ips))
    END DO
    !!
  END DO
  !!
  CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
  if(present(opt)) call MakeDiagonalCopiesIJab(ans, opt)
  !!
  DEALLOCATE (IaJb, p, realval, rhobar)
  !!
END SUBROUTINE STCM_9a

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_9b(ans, test, trial, term1, term2, rho, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  TYPE(FEVariable_), INTENT(IN) :: rho
  !! scalar variable
  TYPE(FEVariable_), INTENT(IN) :: c
  !! convective velocity
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_dx
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  ! Define internal variables
  INTEGER(I4B) :: ips, ipt
  REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: rhobar(:,:)
  !! main
  CALL Reallocate(IaJb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
  CALL GetInterpolation(obj=trial, val=rho, interpol=rhobar)
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt * rhobar(:,ipt)
    !!
    CALL GetProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=p, val=c)
    !!
    DO ips = 1, SIZE(realval)
      !!
      IaJb = IaJb + realval(ips) &
        & * outerprod( &
        & p(:, :, ips), &
        & trial(ipt)%N(:, ips), trial(ipt)%T)
      !!
    END DO
    !!
  END DO
  !!
  CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
  if(present(opt)) call MakeDiagonalCopiesIJab(ans, opt)
  !!
  DEALLOCATE (IaJb, p, realval, rhobar)
  !!
END SUBROUTINE STCM_9b
