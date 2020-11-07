!> authors: Dr. Vikas Sharma
!
! This program shows how to construct `realmatrix_`

program main
  use easifem
  implicit none

  type( realmatrix_ ) :: obj1, obj2

  call RANDOM_NUMBER( obj1, 12, 12)
  call display( obj1, "obj1= ")
  call convert( from=obj1, to=obj2, conversion=nodesToDOF, nns=3, tdof=4)
  call display( obj2, "obj2")



end program main