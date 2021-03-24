! This program is a part of EASIFEM library,
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D,
!
! This program is free software: you can redistribute it and/or modify,
! it under the terms of the GNU General Public License as published by,
! the Free Software Foundation, either version 3 of the License, or,
! (at your option) any later version.,
!
! This program is distributed in the hope that it will be useful,,
! but WITHOUT ANY WARRANTY; without even the implied warranty of,
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the,
! GNU General Public License for more details.,
!
! You should have received a copy of the GNU General Public License,
! along with this program.  If not, see <https: //www.gnu.org/licenses/>.
!

module test_VoigtRank2Tensor
use easifemBase
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( VoigtRank2Tensor_ ) :: obj
  real( dfp ) :: vec( 6 )
  call random_number( vec )
  call display( vec, "vec:", orient="row" )
  obj = VoigtRank2Tensor(vec, VoigtType=StressTypeVoigt)
  call display( obj, "stress type voigt")
  obj = VoigtRank2Tensor(vec, VoigtType=StrainTypeVoigt)
  call display( obj, "stress type voigt")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
  type( VoigtRank2Tensor_ ) :: obj
  real( dfp ) :: T( 3, 3 ), W( 3, 3 )
  call random_number( T )
  T = 0.5*(T + transpose(T))
  call display( T, "T = " )
  obj = VoigtRank2Tensor(T, VoigtType=StressTypeVoigt)
  call display( obj, "stress type voigt")
  W = obj
  call display( W, "W=obj (stress type): ")
  obj = VoigtRank2Tensor(T, VoigtType=StrainTypeVoigt)
  call display( obj, "strain type voigt")
  W = obj
  call display( W, "W=obj (strain type): ")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
  type( VoigtRank2Tensor_ ) :: obj
  real( dfp ) :: V( 6 ), W( 6 )
  call random_number( V )
  call initiate(obj, V, StressTypeVoigt)
  W = obj
  call display( asum( W - V ), "Pass if 0 : " )
end
end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_VoigtRank2Tensor
implicit none
call test1
call test2
call test3
end program main
