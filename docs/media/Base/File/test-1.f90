!> authors: Dr. Vikas Sharma
!
! This program test the file_ data type
program main
use easifem
implicit none

type( file_ ) :: obj

call initiate( obj=obj, path="./", filename="Hello1", &
  & extension = ".txt", status="REPLACE", action="WRITE" )
call closeFile(obj)

call initiate( obj=obj, path="./", filename="Hello2", &
  & extension = ".txt", status="REPLACE", action="WRITE", &
  & isBinary=.true. )
call closeFile(obj)

call openFileToWrite( obj=obj, path="./", filename="Hello3", &
  & extension = ".txt" )
call closeFile(obj)

call openBinaryFileToWrite( obj=obj, path="./", filename="Hello4", &
  & extension = ".txt" )
call closeFile(obj)

end program main