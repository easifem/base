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
! $$
! $$
!
! $$
! $$
!
!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE STCM_8b(ans, test, trial, term1, term2, rho, c, projectOn)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! not used
  INTEGER(I4B), INTENT(IN) :: term2
  !! not used
  TYPE(FEVariable_), INTENT(IN) :: rho
  !! scalar fe variable
  TYPE(FEVariable_), INTENT(IN) :: c
  !! vector fe variable
  CHARACTER(LEN=*), INTENT(IN) :: projectOn
  !! trial, test
  !!
  ! Define internal variables
  INTEGER(I4B) :: ips, ipt
  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: p(:, :, :)
  REAL(DFP), ALLOCATABLE :: rhobar(:,:)
  REAL(DFP), ALLOCATABLE :: realval(:)
  !!
  !! main
  !!
  CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
       & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  CALL GetInterpolation(obj=trial, val=rho, interpol=rhobar)
  !!
  IF ( TRIM(projectOn) .EQ. 'trial' ) THEN
    DO ipt = 1, SIZE(trial)
      realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
           & trial(ipt)%wt * trial(ipt)%jt * rhobar(:,ipt)
      CALL GetProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=p, val=c)
      DO ips = 1, SIZE(realval)
        iajb = iajb + realval(ips) &
             & * outerprod( test(ipt)%dNTdt(:,:,ips), p(:, :, ips))
      END DO
    END DO
  !!
  ELSE IF (TRIM(projectOn) .EQ. 'test') THEN
    DO ipt = 1, SIZE(trial)
      realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
           & trial(ipt)%wt * trial(ipt)%jt * rhobar(:,ipt)
      CALL GetProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=p, val=c)
      DO ips = 1, SIZE(realval)
        iajb = iajb + realval(ips) &
             & * outerprod( p(:, :, ips), trial(ipt)%dNTdt(:, :,ips))
      END DO
    END DO
  END IF
  !!
  CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
  !!
  IF (ALLOCATED(iajb)) DEALLOCATE (iajb)
  IF (ALLOCATED(p)) DEALLOCATE (p)
  IF (ALLOCATED(realval)) DEALLOCATE (realval)
  IF (ALLOCATED(rhobar)) DEALLOCATE (rhobar)
END SUBROUTINE STCM_8b
