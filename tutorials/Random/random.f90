!> authors: Dr. Vikas Sharma
!
! This program test random_method to generate random number

program main
use easifem
implicit none

type( random_ ) :: obj

call initiate( obj )

CALL Display( &
  & RandomValue( obj, From=1_I4B, To=10_I4B ), &
  & "RandomValue( obj, From=1, To=10 ) :: " )

CALL Display( &
  & RandomValue( obj, From=1.0_DFP, To=10.0_DFP ), &
  & "RandomValue( obj, From=1.0_DFP, To=10.0_DFP ) :: " )


end program main