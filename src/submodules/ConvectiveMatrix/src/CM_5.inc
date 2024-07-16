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

PURE SUBROUTINE CM_5(ans, test, trial, term1, term2, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  !! test functions
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  !! trial functions
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x_all
  TYPE(FEVariable_), INTENT(IN) :: c
  !! A scalar finite element variable
  INTEGER( I4B ), INTENT( IN ) :: opt
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
  !!
  !! main
  !!
  CALL GetInterpolation(obj=trial, val=c, interpol=realval)
  !!
  realval = trial%js * trial%ws * trial%thickness * realval
  !!
  if( opt .eq. 1 ) then
    CALL Reallocate(m4, &
      & SIZE(test%N, 1), &
      & SIZE(trial%N, 1), &
      & trial%nsd, 1)
    !!
    !! test: rowConcat
    !!
    DO ips = 1, SIZE(realval)
      m4(:,:,:,1) = m4(:,:,:,1) + outerprod(a=test%N(:, ips), &
        & b=trial%dNdXt(:, :, ips)) * realval(ips)
    END DO
  else
    CALL Reallocate(m4, &
      & SIZE(test%N, 1), &
      & SIZE(trial%N, 1), &
      & 1, trial%nsd)
    !!
    !! test: rowConcat
    !!
    DO ips = 1, SIZE(realval)
      m4(:,:,1, :) = m4(:,:,1, :) + outerprod(a=test%N(:, ips), &
        & b=trial%dNdXt(:, :, ips)) * realval(ips)
    END DO
  end if
  !!
  CALL Convert(from=m4, to=ans)
  !! cleanup
  DEALLOCATE (realval, m4)
END SUBROUTINE CM_5
