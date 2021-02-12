!> authors: Dr. Vikas Sharma
!
! This program test the file_ data type
program main
use easifem
implicit none

! block
! type( file_ ) :: obj

! call initiate( obj=obj, path="./", filename="Hello1", &
!   & extension = ".txt", status="REPLACE", action="WRITE", &
!   & Separator="," )
! call write( obj=obj, val = "This is data" )
! call write( obj=obj, val= 123_I4B )
! call write( obj=obj, val= [1.0, 2.0, 3.0], row=.true. )
! call write( obj=obj, val= eye3, transpose=.false. )
! call closeFile(obj)
! end block

! block
! type( file_ ) :: obj

! call initiate( obj=obj, path="./", filename="Hello1", &
!   & extension = ".txt", status="REPLACE", action="WRITE", &
!   & Separator=",", isbinary=.true. )
! call write( obj=obj, val= [1.0, 2.0, 3.0], row=.true. )
! call write( obj=obj, val= eye3, transpose=.false. )
! call closeFile(obj)
! end block

block
type( file_ ) :: obj
INTEGER( I4B ) :: tlines

call OpenFileToWrite( obj=obj, path="./", filename="Hello1", &
  & extension = ".txt" )
call write( obj=obj, val= [1.0, 2.0, 3.0], row=.true. )
call write( obj=obj, val= eye3, transpose=.false. )
tlines = TotalLines( Obj )
call display( tlines, "tlines :: ")
end block

end program main