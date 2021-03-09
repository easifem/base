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

module test_OpenMP
use easifemBase
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
write( *, * ) OMP%RANK
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
integer( i4b ) :: ans(4)
ans = OMP_partition(50, OMP%NUM_THREADS)
WRITE( *, * ) "my rank :: ", omp%rank, " ans :: ", ans
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
integer( i4b ), parameter :: n=100000
real( dfp ) :: a( n ), res, res_local
integer( i4b ) :: i, ans(4)

call display( "I am master thread:: I am in test3" )

a = arange(1,n,1)
res_local = 0.0
res= 0.0

!$omp parallel default(shared) private( i, ans, res_local ) reduction(+:res) NUM_THREADS(4)
CALL OMP_INITIATE

ans = OMP_partition(n, omp%NUM_THREADS)
res_local = asum( a(ans(1):ans(2):ans(3)) )
res = res + res_local

CALL OMP_FINALIZE
!$omp end parallel

call display( "I am master thread:: I am in test3" )
call display( ASUM(a),  "correct ans is : " )
call display( res, "rest : ")

end


end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_OpenMP
implicit none


!$OMP PARALLEL NUM_THREADS(4)
call OMP_Initiate( )
call test1
call test2
call OMP_Finalize()
!$OMP END PARALLEL

call test3

end program main