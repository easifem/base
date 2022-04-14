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
!                                                               STForceVector
!----------------------------------------------------------------------------

PURE SUBROUTINE STFV_15( ans, test, term1 )
  !! intent of dummy variable
  CLASS(STElemshapeData_), INTENT(IN) :: test( : )
  INTEGER( I4B ), INTENT( IN ) :: term1
  !! DEL_x, DEL_y, DEL_z
  REAL(DFP), ALLOCATABLE, INTENT( INOUT ) :: ans(:, :)
  !!
  !! Define internal variable
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T))
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js * test(ipt)%ws * test(ipt)%thickness
    !!
    DO ips = 1, SIZE(realval)
      ans = ans + realval(ips) * test(ipt)%dNTdXt(:, :, term1, ips)
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval)
  !!
END SUBROUTINE STFV_15
