program main
implicit none

integer :: in, out, iostat, len
character( len = 1000 ) temp

open( newunit = in, file = '../src/blas95.lst', status="old", &
  & action="read" )

open( newunit = out, file = './EASIFEM_BLAS.f90', status="replace", &
  & action="write" )

DO
read( in, *, IOSTAT=iostat) temp
len = LEN_TRIM(temp)
if(temp(1:1) .eq. '#') cycle
write( out, "(A)" ) '#include "./' // temp(1:len) // '"'
if( iostat .LT. 0 ) exit
END DO

end program main