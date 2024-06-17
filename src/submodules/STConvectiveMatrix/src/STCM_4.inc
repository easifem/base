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

PURE SUBROUTINE STCM_4a(ans, test, trial, term1, term2, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_t
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x, del_y, del_z
  TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
  !! Scalar variable
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !! Define internal variables
  INTEGER(I4B) :: ipt, ips
  REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: cbar(:, :)
  !!
  !! main
  !!
  !! make c bar at ips and ipt
  !!
  IF (PRESENT(c)) THEN
    CALL GetInterpolation(obj=trial, val=c, interpol=cbar)
  ELSE
    CALL Reallocate(cbar, SIZE(trial(1)%N, 2), SIZE(trial))
    cbar = 1.0_DFP
  END IF
  !!
  CALL Reallocate(IaJb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt * cbar(:, ipt)
    !!
    DO ips = 1, SIZE(realval)
      IaJb = IaJb + realval(ips) &
        & * outerprod( &
        & test(ipt)%dNTdt(:,:,ips), &
        & trial(ipt)%dNTdXt(:, :, term2, ips))
    END DO
    !!
  END DO
  !!
  CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
  IF( PRESENT(opt) ) CALL MakeDiagonalCopiesIJab(ans, opt)
  !! cleanup
  DEALLOCATE (IaJb, realval, cbar)
  !!
END SUBROUTINE STCM_4a

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_4b(ans, test, trial, term1, term2, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_x, del_y, del_z
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_t
  TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
  !! Scalar variable
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !! Define internal variables
  INTEGER(I4B) :: ipt, ips
  REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: cbar(:, :)
  !!
  !! main
  !!
  !! make c bar at ips and ipt
  !!
  IF (PRESENT(c)) THEN
    CALL GetInterpolation(obj=trial, val=c, interpol=cbar)
  ELSE
    CALL Reallocate(cbar, SIZE(trial(1)%N, 2), SIZE(trial))
    cbar = 1.0_DFP
  END IF
  !!
  CALL Reallocate(IaJb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
      & trial(ipt)%wt * trial(ipt)%jt * cbar(:, ipt)
    !!
    DO ips = 1, SIZE(realval)
      IaJb = IaJb + realval(ips) &
        & * outerprod( &
        & test(ipt)%dNTdXt(:, :, term1, ips), &
        & trial(ipt)%dNTdt(:,:,ips) &
        & )
    END DO
    !!
  END DO
  !!
  CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
  IF( PRESENT(opt) ) CALL MakeDiagonalCopiesIJab(ans, opt)
  !! cleanup
  DEALLOCATE (IaJb, realval, cbar)
  !!
END SUBROUTINE STCM_4b
