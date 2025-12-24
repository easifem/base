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
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE CM_10(ans, test, trial, term1, term2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  ! test functions
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  ! trial functions
  INTEGER(I4B), INTENT(IN) :: term1
  ! del_x_all
  INTEGER(I4B), INTENT(IN) :: term2
  ! del_none
  INTEGER(I4B), INTENT(IN) :: opt
  !
  ! Define internal variables
  !
  INTEGER(I4B) :: ips, ii
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
  !
  ! main
  !
  realval = trial%js * trial%ws * trial%thickness
  !
  IF (opt .EQ. 1) THEN
    CALL Reallocate(m4, &
      & SIZE(test%N, 1), &
      & SIZE(trial%N, 1), &
      & trial%nsd, 1)
    !
    DO ips = 1, SIZE(realval)
      DO ii = 1, SIZE(m4, 3)
        m4(:, :, ii, 1) = m4(:, :, ii, 1) + outerprod( &
          & a=test%dNdXt(:, ii, ips), &
          & b=trial%N(:, ips)) * realval(ips)
      END DO
    END DO
  ELSE
    CALL Reallocate(m4, &
      & SIZE(test%N, 1), &
      & SIZE(trial%N, 1), &
      & 1, trial%nsd)
    !
    DO ips = 1, SIZE(realval)
      DO ii = 1, SIZE(m4, 4)
        m4(:, :, 1, ii) = m4(:, :, 1, ii) + outerprod( &
          & a=test%dNdXt(:, ii, ips), &
          & b=trial%N(:, ips)) * realval(ips)
      END DO
    END DO
  END IF
  !
  CALL Convert(from=m4, to=ans)
  !
  DEALLOCATE (realval, m4)
  !
END SUBROUTINE CM_10
