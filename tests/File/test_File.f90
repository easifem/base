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

module test_File
use easifemBase
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
type( file_ ) :: obj
call initiate( &
  & obj=obj, &
  & path=String("./"), &
  & filename=String("Hello1"), &
  & extension = String("txt"), &
  & status=String("REPLACE"), &
  & action=String("WRITE") )
call display( val=[1,2,3,4,5], msg="Message: ", unitNo=obj%unitNo )
call close(obj)
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
type( file_ ) :: obj
call open(obj, string("./"), string("Hello1"), string("txt"), "w")
call display( val=[1,2,3,4,5], msg="Message: ", unitNo=obj%unitNo )
call close(obj)
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
type( file_ ) :: obj
call open(obj, string("./"), string("Hello1"), string("txt"), "w+")
call display( val=[1,2,3,4,5], msg="Message: ", unitNo=obj%unitNo )
call delete(obj)
call close(obj)
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module test_File

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_File
implicit none
call test1
call test2
call test3
end program main

! !> authors: Dr. Vikas Sharma
! !
! ! This program test the file_ data type
! program main
! use easifem
! implicit none

! type( file_ ) :: obj


! call initiate( obj=obj, path="./", filename="Hello2", &
!   & extension = ".txt", status="REPLACE", action="WRITE", &
!   & isBinary=.true. )
! call closeFile(obj)

! call openFileToWrite( obj=obj, path="./", filename="Hello3", &
!   & extension = ".txt" )
! call closeFile(obj)

! call openBinaryFileToWrite( obj=obj, path="./", filename="Hello4", &
!   & extension = ".txt" )
! call closeFile(obj)

! end program main

! !> authors: Dr. Vikas Sharma
! !
! ! This program test the file_ data type
! program main
! use easifem
! implicit none

! ! block
! ! type( file_ ) :: obj

! ! call initiate( obj=obj, path="./", filename="Hello1", &
! !   & extension = ".txt", status="REPLACE", action="WRITE", &
! !   & Separator="," )
! ! call write( obj=obj, val = "This is data" )
! ! call write( obj=obj, val= 123_I4B )
! ! call write( obj=obj, val= [1.0, 2.0, 3.0], row=.true. )
! ! call write( obj=obj, val= eye3, transpose=.false. )
! ! call closeFile(obj)
! ! end block

! ! block
! ! type( file_ ) :: obj

! ! call initiate( obj=obj, path="./", filename="Hello1", &
! !   & extension = ".txt", status="REPLACE", action="WRITE", &
! !   & Separator=",", isbinary=.true. )
! ! call write( obj=obj, val= [1.0, 2.0, 3.0], row=.true. )
! ! call write( obj=obj, val= eye3, transpose=.false. )
! ! call closeFile(obj)
! ! end block

! block
! type( file_ ) :: obj
! INTEGER( I4B ) :: tlines

! call OpenFileToWrite( obj=obj, path="./", filename="Hello1", &
!   & extension = ".txt" )
! call write( obj=obj, val= [1.0, 2.0, 3.0], row=.true. )
! call write( obj=obj, val= eye3, transpose=.false. )
! tlines = TotalLines( obj )
! call display( tlines, "tlines :: ")
! end block

! end program main