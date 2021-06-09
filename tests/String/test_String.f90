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

module test_m
use easifemBase
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test0
  type( string ) :: obj
  type( string ), allocatable :: splits( : )
  integer :: i, n
  obj = "/hello/word/text.txt"
  call obj%split(splits, "/")
  n = size(splits)
  call display( n, "test: 4=")
  do i = 1, n
    call display( splits(i), "test: i=" )
  end do
end subroutine test0

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
  type( string ) :: obj
  obj = "hello    "
  call display( len(obj%chars()), "test: 9 = " )
  call display( len(obj%chars()), "test: 9 = " )
end subroutine test3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
  type( string ) :: obj
  INTEGER( I4B ), allocatable :: indices( : )
  obj = "./hello/world/abc.ext"
  call display( obj%nmatchstr("/"), "test: 3 = " )
  call obj%strfind("/", indices)
  call display( indices, "test: [2,8,14] = ")

end subroutine test2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( string ) :: obj, path, name, ext
  obj = "./hello/world/abc.ext"
  path = obj%basedir()
  call display( path,'path')
  ext = obj%extension()
  call display( ext,'ext')
  name = obj%basename(extension=ext%chars())
  call display( name,'name')
end subroutine test1

end module test_m

program main
use test_m
implicit none
call test0
end program main