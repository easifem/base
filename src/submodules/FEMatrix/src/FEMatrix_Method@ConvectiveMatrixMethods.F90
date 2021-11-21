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

submodule(FEMatrix_Method) ConvectiveMatrixMethods
implicit none
contains

!----------------------------------------------------------------------------
!                                                     getConvectiveMatrix_1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-21
! update: 2021-11-21
! summary: returns the convective matrix

subroutine getConvectiveMatrix_1(test, trial, C, Term1, Term2)
  !! Define internal variables
  integer(I4B) :: nips, ips
  real(DFP), allocatable :: cdNdXt(:), realVal(:)
  !! main
  call Reallocate(ans, size(test%N, 1), size(trial%N, 1))
  nips = size(trial%N, 2)
  realval = trial%js * trial%ws * trial%thickness
  !! make cdNdXt based upon the case.
  if (term1 .eq. 1 .and. term2 .eq. 0) then
    call test%getProjectionOfdNdXt(cdNdXt=cdNdXt, val=c)
    do ips = 1, size(trial%N, 2)
      ans = ans + outerprod(a=cdNdXt(:, ips), &
           & b=trial%N(:, ips)) * realval(ips)
    end do
  else if (term1 .eq. 0 .and. term2 .eq. 1) then
    call trial%getProjectionOfdNdXt(cdNdXt=cdNdXt, val=c)
    do ips = 1, size(trial%N, 2)
      ans = ans + outerprod(a=test%N(:, ips), &
           & b=cdNdXt(:, ips)) * realval(ips)
    end do
  end if
 !! cleanup
  deallocate (cdNdXt)

end subroutine getConvectiveMatrix_1

end submodule ConvectiveMatrixMethods
