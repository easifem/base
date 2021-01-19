program main
  use basetype
  use basemethod
  implicit none

  ! block
  !   type( referenceline_ ) :: obj
  !   obj = ReferenceLine( )
  !   call display( obj, "Reference Line" )
  ! end block

  ! block
  !   type( referencetriangle_) :: obj
  !   obj = referencetriangle( )
  !   call display( obj, "Reference triangle" )
  ! end block

  block
    class( referenceelement_ ), pointer :: obj
    real( dfp ), allocatable :: xij( :, : )

    obj => referencetriangle_pointer(2)
    !call display( obj, "Reference triangle")
    xij = obj % lagrangepoints( 1 )
  end block
end program main