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

#define _obj_ RealVector_

module test_RealVector
use easifemBase
implicit none
contains
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( _obj_ ) :: obj
  call equalline()
  call allocateData(obj, 10)
  call display( obj, "test1=")
  call dashline()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
type(_obj_), allocatable :: obj( : )
call display("test2")
call equalline()
call initiate(obj, [10,10,5,5])
call display( obj, "initiate Obj(:)=")
call dashline()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
type(_obj_) :: obj
call display("test3:: initiate_obj_ab")
call equalline()
call initiate(obj, 2, 10)
obj%val(2) = 1.0_DFP
call display( obj, "initiate Obj(a:b)=")
call dashline()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
type(_obj_) :: obj
call display("test4:: initiate_obj_ab")
call equalline()
call random_number(obj=obj, tsize=5)
call display( obj, "obj =")
call dashline()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
type(_obj_), allocatable :: obj( : )
call display("test5:: random_number_obj_vec")
call equalline()
call random_number(obj=obj, tsize=[4,5,6])
call display( obj, "obj =")
call dashline()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test6
type(_obj_) :: obj1, obj2, obj3
call display("test6:: RealVector()")
call equalline()
obj1 = RealVector( tsize = 6 )
call display( obj1, "obj1 =")
obj2 = RealVector( [1,2,3,4,5] )
call display( obj2, "obj2 = ")
obj3 = RealVector( [1._dfp, 2.0_dfp, 3.0_dfp] )
call display( obj3, "obj3 = ")
call dashline()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test7
type(_obj_), pointer :: obj1, obj2, obj3
call display("test7:: RealVector_Pointer()")
call equalline()
obj1 => RealVector_Pointer( tsize = 6 )
call display( obj1, "obj1 =")
obj2 => RealVector_Pointer( [1,2,3,4,5] )
call display( obj2, "obj2 = ")
obj3 => RealVector_Pointer( [1._dfp, 2.0_dfp, 3.0_dfp] )
call display( obj3, "obj3 = ")
call dashline()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test8
type(_obj_) :: obj
integer( i4b ), allocatable :: nptrs( : )
obj = RealVector([1,2,3,4,5,6,7,8,9,10])
nptrs = ArrayValues(obj, 1_I4B )
call display( nptrs, "nptrs =")
nptrs = ArrayValues(obj, [2,3,4], 1_I4B)
call display( nptrs, "nptrs =")
nptrs = ArrayValues(obj, 1,10, 2, 1_I4B)
call display( nptrs, "nptrs =")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test9
type(_obj_) :: obj
real( dfp ), allocatable :: nptrs( : )
obj = RealVector([1,2,3,4,5,6,7,8,9,10])
nptrs = ArrayValues(obj, 1.0_DFP )
call display( nptrs, "nptrs =")
nptrs = ArrayValues(obj, [2,3,4], 1.0_DFP)
call display( nptrs, "nptrs =")
nptrs = ArrayValues(obj, 1,10, 2, 1.0_DFP)
call display( nptrs, "nptrs =")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test10
type(_obj_) :: obj, nptrs
obj = RealVector([1,2,3,4,5,6,7,8,9,10])
nptrs = obj
call display( nptrs, "nptrs =")
nptrs = ArrayValues(obj, [2,3,4], TypeRealVector)
call display( nptrs, "nptrs =")
nptrs = ArrayValues(obj, 1,10, 2, TypeRealVector)
call display( nptrs, "nptrs =")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test11
type(_obj_) :: obj( 3 )
integer( I4B ), allocatable :: nptrs( : )
obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
obj(3) = RealVector([21,22,23,24,25])
nptrs = ArrayValues(obj, 1_I4B )
call display( nptrs, "nptrs =")
nptrs = ArrayValues(obj, [2,3,4], 1_I4B)
call display( nptrs, "nptrs =")
nptrs = ArrayValues(obj, 1,10, 2, 1_I4B)
call display( nptrs, "nptrs =")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test12
type(_obj_) :: obj( 3 )
real( dfp ), allocatable :: nptrs( : )
obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
obj(3) = RealVector([21,22,23,24,25])
nptrs = ArrayValues(obj, 1.0_DFP )
call display( nptrs, "nptrs =")
nptrs = ArrayValues(obj, [2,3,4], 1.0_DFP )
call display( nptrs, "nptrs =")
nptrs = ArrayValues(obj, 1,10, 2, 1.0_DFP )
call display( nptrs, "nptrs =")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test13
type(_obj_) :: obj( 3 ), nptrs
obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
obj(3) = RealVector([21,22,23,24,25])
nptrs = ArrayValues(obj, TypeRealVector )
call display( nptrs, "nptrs =")
nptrs = ArrayValues(obj, [2,3,4], TypeRealVector )
call display( nptrs, "nptrs =")
nptrs = ArrayValues(obj, 1,10, 2, TypeRealVector )
call display( nptrs, "nptrs =")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test14
type(_obj_) :: obj
real(dfp), pointer :: ptr( : ) => null()
obj = RealVector([1,2,3,4,5,6,7,8,9,10])
ptr => ArrayValuesPointer(obj, 1.0_DFP)
call display( ptr, "ptr =" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test15
type(_obj_) :: obj
obj = RealVector([1,2,3,4,5,6,7,8,9,10])
call display( LOC(Obj=Obj, Value=6.0_DFP ), "LOC =" )
call display( LOC(Obj=Obj, Value=[6.0_DFP, 5.0_DFP] ), "LOC =" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test16
type(_obj_) :: obj
obj = RealVector([1,2,3,4,5,6,7,8,9,10])
call display( isPresent(Obj=Obj, Value=6.0_DFP ), "LOC =" )
! call display( isPresent(Obj=Obj, Value=[6.0_DFP, 5.0_DFP] ), "LOC =" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test17
type(_obj_) :: obj, obj1
obj = RealVector([1,2,3,4])
call display(obj, "obj=")
call append(obj, 5.0_DFP)
call display(obj, "obj=")
call append(obj, [6.0_DFP, 7.0_DFP])
call display(obj, "obj=")
call append(obj1, obj)
call display(obj1, "obj=")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test18
type(_obj_) :: obj1, obj2
call RANDOM_NUMBER( obj1, 100 )
call RANDOM_NUMBER( obj2, 100 )
CALL Display( DOT(obj1, obj2), "dot 1=" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test19
type(_obj_) :: obj1(2), obj2(2)
call RANDOM_NUMBER( obj1(1), 100 )
call RANDOM_NUMBER( obj1(2), 100 )
obj2 = obj1
CALL Display( DOT(obj1, obj2), "dot =" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test20
type(_obj_) :: obj1, obj2(2)
call RANDOM_NUMBER( obj2(1), 100 )
call RANDOM_NUMBER( obj2(2), 100 )
call RANDOM_NUMBER( obj1, 100 )
CALL Display( DOT(obj1, obj2), "dot =" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test21
type(_obj_) :: obj1
real( dfp ) :: val( 100 )
call RANDOM_NUMBER( obj1, 100 )
call RANDOM_NUMBER( val )
CALL Display( DOT(obj1, val), "dot =" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test22
type(_obj_) :: obj1
real( dfp ) :: val( 100 )
call RANDOM_NUMBER( obj1, 100 )
call RANDOM_NUMBER( val )
CALL Display( DOT(obj1, val), "dot =" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test23
type(_obj_) :: obj, obj1( 2 )
obj = RealVector( [1,2,3] )
call display( NORM2( obj ), "norm2 = " )
call display( NORM2SQR( obj ), "norm2sqr = " )
obj1(1) = RealVector( [1,2,3] )
obj1(2) = RealVector( [1,2,3] )
call display( NORM2( obj1 ), "norm2 = " )
call display( NORM2SQR( obj1 ), "norm2sqr = " )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


end module test_RealVector

!----------------------------------------------------------------------------
!                                                                 main
!----------------------------------------------------------------------------

program main
use test_RealVector
implicit none
call test1
call test2
call test3
call test4
call test5
call test6
call test7
call test8
call test9
call test10
call test11
call test12
call test13
call test14
call test15
call test16
call test17
call test18
call test19
call test20
call test21
call test22
call test23

end program main