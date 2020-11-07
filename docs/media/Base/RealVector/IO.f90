!> authors: Dr. Vikas Sharma
!
! This program showns IO operations for realvector data type
program main
  use easifem
  implicit none

  REAL( DFP ) :: a, b

  CALL READLINE( a, b, "1,2,3")
  CALL Display( [a,b], "[a,b] :: ")

end program main