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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
  integer( i4b ), parameter :: n = 100, m=5
  integer( i4b ) :: i
  type( RealVector_ ) :: obj( m )
  real( dfp ) :: ans_l(m), ans
  do i = 1, m
    obj( i ) = RealVector(arange(1,n,1))
  end do
  ans = ASUM(obj)
  call display( ans - (m*sum(obj(1)%val)), "test3: 0 if correct : " )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
  integer( i4b ), parameter :: n = 100
  integer( i4b ) :: i
  real( dfp ) :: a = 1.0_DFP
  type( RealVector_ ) :: x, y, z
  call random_number( x, n )
  call random_number( y, n )
  z%val = y%val + a * x%val
  call axpy( x = x, y = y, A = a  )
  call display( asum(y%val - z%val), "test4: 0 if correct : " )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
  integer( i4b ), parameter :: n = 100, m = 4
  integer( i4b ) :: i, tsize(m)
  real( dfp ) :: a( m ), ans
  type( RealVector_ ), allocatable :: x( : ), y( : ), z( : )
  tsize = n; a = 1.0
  call random_number( x, tsize )
  call random_number( y, tsize )
  call initiate( z, tsize )
  do i = 1, m
    z(i)%val = y(i)%val + a( i ) * x(i)%val
  end do
  call axpy( x = x, y = y, A = a )
  ans = 0.0
  do i = 1, m
    ans = ans + asum( y(i)%val - z(i)%val )
  end do
  call display( ans, "test5: 0 if correct : " )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test6
  integer( i4b ), parameter :: n = 10000
  type( RealVector_ ) :: x, y
  real( dfp ), allocatable :: z( : )
  call random_number( x, n )
  call copy( x = x, y = y )
  call display( asum( x%val - y%val ), "test6: 0 if correct : " )
  call copy( y=z, x=x )
  call display( asum( z - x%val ), "test6: 0 if correct : " )
  call copy( y=x, x=z )
  call display( asum( z - x%val ), "test6: 0 if correct : " )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test7
  integer( i4b ), parameter :: n = 10000, m = 5
  type( RealVector_ ), allocatable :: x( : ), y( : )
  type( RealVector_ ) :: z
  integer( i4b ) :: tsize( m ), i
  real( dfp ) :: ans
  tsize = n
  call random_number(x, tsize)
  call copy( x = x, y = y )
  ans = 0.0
  do i = 1, size( x )
    ans = ans + asum( x(i)%val - y(i)%val )
  end do
  call display( ans, "test7: 0 if correct : " )
  call copy( y=z, x=x )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module test_BLAS1V

!----------------------------------------------------------------------------
!                                                                 main
!----------------------------------------------------------------------------

program main
use test_BLAS1V
implicit none
call test1
call test2
call test3
call test4
call test5
call test6
call test7
end program main