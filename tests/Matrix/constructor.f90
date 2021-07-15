program main
use basetype
use basemethod
implicit none

! block
!   type(realmatrix_) :: obj
!   call allocateData(obj, [2,3])
!   call display( obj, "obj")
! end block

! block
!   type(realmatrix_) :: obj
!   call initiate(obj, [2,3])
!   call display( obj, "obj")
! end block

! block
!   type(realmatrix_) :: obj( 3 )
!   call initiate(obj, [2,3])
!   call display( obj, "obj")
! end block

real(dfp) :: randMatt( 4, 5 )
call RANDOM_NUMBER( randMatt )
call display( randMatt, "mat" )
call display( randMatt( [1,2,3], [2, 4] ), "section" )
call display( randMatt( 1:4:1,  1:5:2), "section" )


end program main