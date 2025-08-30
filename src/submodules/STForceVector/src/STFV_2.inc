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

!----------------------------------------------------------------------------
!                                                              STForceVector
!----------------------------------------------------------------------------

PURE SUBROUTINE STFV_2(ans, test, term1, c, crank)
  !! intent of dummy variable
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! DEL_NONE
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariableScalar_), INTENT(IN) :: crank
  !!
  !! Define internal variable
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: cbar(:, :)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL getInterpolation(obj=test, ans=cbar, val=c)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T))
  !!
  DO ipt = 1, SIZE(test)
    !!
    realval = test(ipt)%js*test(ipt)%ws*test(ipt)%thickness*cbar(:,ipt) * test(ipt)%jt
    !!
    DO ips = 1, SIZE(realval)
      ans = ans + realval(ips) * OUTERPROD( &
        & a=test(ipt)%N(:, ips), &
        & b=test(ipt)%T)
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval, cbar)
  !!
END SUBROUTINE STFV_2
