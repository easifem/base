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

module test_Vector3D
use easifemBase
implicit none
contains

!----------------------------------------------------------------------------
!                                                                 test1
!----------------------------------------------------------------------------

subroutine test1
type( Vector3D_ ) :: obj
call initiate( obj, [1.0_DFP, 2.0_DFP, 3.0_DFP])
call display(obj, "test1=")
end

!----------------------------------------------------------------------------
!                                                                 test2
!----------------------------------------------------------------------------

subroutine test2
type( Vector3D_ ) :: obj, obj2
call initiate( obj, [1.0_DFP, 2.0_DFP, 3.0_DFP])
call initiate( obj2, obj )
call display(obj2, "test2=")
end

!----------------------------------------------------------------------------
!                                                                 test3
!----------------------------------------------------------------------------

subroutine test3
type( Vector3D_ ) :: obj
  obj = [1.0_DFP,2.0_DFP]
  call display( obj, "test3=")
end

!----------------------------------------------------------------------------
!                                                                 test4
!----------------------------------------------------------------------------

subroutine test4
type( Vector3D_ ) :: obj, obj2
call initiate( obj, [1.0_DFP, 2.0_DFP, 3.0_DFP])
obj2 = obj
call display(obj2, "test4=")
end

!----------------------------------------------------------------------------
!                                                                 test5
!----------------------------------------------------------------------------

subroutine test5
type( Vector3D_ ) :: obj
obj = Vector3D([1.0_DFP])
call display( obj, "test5=")
end

!----------------------------------------------------------------------------
!                                                                 test6
!----------------------------------------------------------------------------

subroutine test6
type( Vector3D_ ), pointer :: obj
obj => Vector3D_Pointer([1.0_DFP])
call display(obj, "test6=")
end

!----------------------------------------------------------------------------
!                                                                 test7
!----------------------------------------------------------------------------

subroutine test7
  type( Vector3D_ ) :: obj1, obj2
  obj1 = [1.0_dfp, 2.0_dfp, 3.0_dfp]
  obj2 = [1.0_dfp, 0.0_dfp, 3.0_dfp]
  CALL Equalline()
  CALL Display( "test7" )
  CALL Display( obj1, "obj1 = " )
  CALL Display( obj2, "obj2 = " )
  CALL Display( DOT_PRODUCT( obj1, obj2 ), "dot_product = " )
  CALL Display( obj1 .DOT. obj2, "dot_product = " )
  CALL DotLine()
end

!----------------------------------------------------------------------------
!                                                                 test8
!----------------------------------------------------------------------------

subroutine test8
  type( Vector3D_ ) :: obj
  real( dfp ) :: val(3)
  obj = [1.0_dfp, 2.0_dfp, 3.0_dfp]
  val = [1.0_dfp, 0.0_dfp, 3.0_dfp]
  CALL Equalline()
  CALL Display( "test8" )
  CALL Display( obj, "obj1 = " )
  CALL Display( val, "val = " )
  CALL Display( DOT_PRODUCT( obj=obj, val=val ), "dot_product = " )
  CALL Display( obj.DOT. val, "dot_product = " )
  CALL DotLine()
end

!----------------------------------------------------------------------------
!                                                                 test9
!----------------------------------------------------------------------------

subroutine test9
  type( Vector3D_ ) :: obj1, obj2, obj3
  obj1 = [1.0_dfp, 2.0_dfp, 3.0_dfp]
  obj2 = [1.0_dfp, 0.0_dfp, 3.0_dfp]
  CALL Equalline()
  CALL Display( "test9" )
  CALL Display( obj1, "obj1 = " )
  CALL Display( obj2, "obj2 = " )
  CALL Display( VECTOR_PRODUCT( obj1, obj2 ), "vector_product = " )
  CALL Display( obj1 .X. obj2, "vector_product = " )
  CALL DotLine()
end

!----------------------------------------------------------------------------
!                                                                 test10
!----------------------------------------------------------------------------

subroutine test10
  type( Vector3D_ ) :: obj
  real( dfp ) :: val(3)
  obj = [1.0_dfp, 2.0_dfp, 3.0_dfp]
  val = [1.0_dfp, 0.0_dfp, 3.0_dfp]
  CALL Equalline()
  CALL Display( "test10" )
  CALL Display( obj, "obj1 = " )
  CALL Display( val, "val = " )
  CALL Display( Vector_PRODUCT( obj=obj, val=val ), "vector_product = " )
  CALL Display( obj .X. val, "vector_product = " )
  CALL DotLine()
end

!----------------------------------------------------------------------------
!                                                                 test11
!----------------------------------------------------------------------------

subroutine test11
  type( Vector3D_ ) :: obj1, obj2, obj3
  obj1 = [1.0_dfp, 0.0_dfp, 0.0_dfp]
  obj2 = [0.0_dfp, 1.0_dfp, 0.0_dfp]
  obj3 = [1.0_dfp, 2.0_dfp, 1.0_dfp]
  CALL Equalline()
  CALL Display( "test11" )
  CALL Display( Vector_PRODUCT( obj1, obj2, obj3 ), "vector_product = " )
  CALL Display( obj1 .X. (obj2 .X. obj3), "vector_product = " )
  CALL DotLine()
end

!----------------------------------------------------------------------------
!                                                                 test12
!----------------------------------------------------------------------------

subroutine test12
  type( Vector3D_ ) :: obj1, obj2, obj3
  obj1 = [1.0_dfp, 0.0_dfp, 0.0_dfp]
  obj2 = [0.0_dfp, 1.0_dfp, 0.0_dfp]
  obj3 = [1.0_dfp, 2.0_dfp, 1.0_dfp]
  CALL Equalline()
  CALL Display( "test12" )
  CALL Display( DOT_PRODUCT( obj1, obj2, obj3 ), "dot_product = " )
  CALL DotLine()
end

!----------------------------------------------------------------------------
!                                                                 test13
!----------------------------------------------------------------------------

subroutine test13
  type( Vector3D_ ) :: obj
  obj = [1.0_dfp, 2.0_dfp, 3.0_dfp]
  CALL Equalline()
  CALL Display( "test13" )
  CALL Display( NORM2( obj ), "NORM2 = " )
  CALL Display( .NORM. obj, ".Norm. obj = ")
  CALL DotLine()
end

!----------------------------------------------------------------------------
!                                                                 test14
!----------------------------------------------------------------------------

subroutine test14
  type( Vector3D_ ) :: obj1, obj2
  obj1 = [0.0_dfp, 2.0_dfp, 3.0_dfp]
  obj2 = [1.0_dfp, 0.0_dfp]
  CALL Equalline()
  CALL Display( "test14" )
  CALL Display( ANGLE( obj1, obj2), "Angle = " )
  CALL Display( obj1 .ANGLE. obj2, ".Angle. = ")
  CALL Display( DEGREES( obj1 .ANGLE. obj2 ), "In degrees :: ")
  CALL DotLine()
end

!----------------------------------------------------------------------------
!                                                                 test15
!----------------------------------------------------------------------------

subroutine test15
  type( Vector3D_ ) :: obj1, obj2
  obj1 = [4.0_dfp, 2.0_dfp, 3.0_dfp]
  obj2 = [2.0_dfp, 0.0_dfp]
  CALL Equalline()
  CALL Display( "test15" )
  CALL Display( ProjectionVector( obj1, obj2), "PROJECTIONVector = " )
  CALL Display( obj1 .PROJECTIONVector. obj2, ".PROJECTIONVector. = ")
  CALL Display( PROJECTION(obj1, obj2), "PROJECTION = " )
  CALL Display( obj1 .PROJECTION. obj2, ".PROJECTION. = ")
  CALL DotLine()
end

!----------------------------------------------------------------------------
!                                                                 test16
!----------------------------------------------------------------------------

subroutine test16
  type( Vector3D_ ) :: obj
  obj = [1.0_dfp, 1.0_dfp, 0.0_dfp]
  CALL Equalline()
  CALL Display( "test16" )
  CALL Display(UnitVector( obj ), "UnitVector = " )
  CALL Display( .Hat. obj, ".Hat. = ")
  CALL Display(HAT(obj), "Hat = " )
  CALL DotLine()
end
end module test_Vector3D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_Vector3D
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

end program main