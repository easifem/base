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


module test_BLAS1V
use easifemBase
implicit none
contains
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( RealVector_ ) :: obj
  real( dfp ) :: ans
  obj = RealVector(arange(1,1000,1))
  ans = asum(obj)
  call display( ans-sum(obj%val), "test1: 0 if correct : " )
end

subroutine test2
  integer( i4b ), parameter :: n = 10, m=5
  integer( i4b ) :: i
  type( RealVector_ ) :: obj( m )

  real( dfp ) :: ans_l(m), ans

  do i = 1, m
    obj( i ) = RealVector(arange(1,n,1))
  end do
  ans = 0.0

  !$omp parallel default(shared) private( i ) reduction(+:ans)
  CALL OMP_INITIATE
  !$omp do
  do i = 1, m
    ans = ans + asum(obj(i)) !! no parallel
  enddo
  !$omp enddo

  CALL OMP_FINALIZE
  !$omp end parallel

  call display( ans - (m*sum(obj(1)%val)), "test2: 0 if correct : " )
end

end module test_BLAS1V

!----------------------------------------------------------------------------
!                                                                 main
!----------------------------------------------------------------------------

program main
use test_BLAS1V
implicit none
call test1
call test2
end program main