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

! Define internal variables
INTEGER(I4B) :: ips, ipt
REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
REAL(DFP), ALLOCATABLE :: p(:, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
!! main
CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
     & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
!!
IF (term1 .EQ. 0 .AND. term2 .EQ. 1) THEN
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt
    CALL GetProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=p, val=c)
    DO ips = 1, SIZE(realval)
      iajb = iajb + realval(ips) &
           & * outerprod( &
           & outerprod(test(ipt)%N(:, ips), test(ipt)%T), &
           & p(:, :, ips))
    END DO
  END DO
!!
ELSE IF (term1 .EQ. 1 .AND. term2 .EQ. 0) THEN
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt
    CALL GetProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=p, val=c)
    DO ips = 1, SIZE(realval)
      iajb = iajb + realval(ips) &
           & * outerprod( &
           & p(:, :, ips), &
           & outerprod(trial(ipt)%N(:, ips), trial(ipt)%T))
    END DO
  END DO
END IF
!!
CALL swap(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
!!
IF (ALLOCATED(iajb)) DEALLOCATE (iajb)
IF (ALLOCATED(p)) DEALLOCATE (p)
IF (ALLOCATED(realval)) DEALLOCATE (realval)
