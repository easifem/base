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

  ! block
  !   type( referenceline_) :: refline
  !   real( dfp ), allocatable :: nodecoord( :, : )
  !   integer :: i, order

  !   order = 5
  !   refline = referenceline( )
  !   nodecoord = lagrangepoints( refline, order )

  !   write( *, * ) "#node coord file"
  !   do i = 1, SIZE( nodecoord, 2 )
  !     write( *, * ) i, nodecoord( 1, i), nodecoord( 2, i )
  !   end do
  ! end block

  ! block
  !   type( referencequadrangle_) :: refquad
  !   real( dfp ), allocatable :: nodecoord( :, : )
  !   integer :: i, order

  !   order = 7
  !   refquad = referencequadrangle( )
  !   nodecoord = lagrangepoints( refquad, order )

  !   write( *, * ) "#node coord file"
  !   do i = 1, SIZE( nodecoord, 2 )
  !     write( *, * ) i, nodecoord( 1, i), nodecoord( 2, i )
  !   end do
  ! end block

  block
    type( referenceline_) :: obj, obj2
    integer :: i, order

    order = 2
    obj = referenceline( 1 )
    obj2 = lagrangeelement( obj, 2 )
    call display( obj2, "lagrange line order 2")
  end block

end program main

! plot "log.txt" u 2:3 pt 6 notitle, "" u 2:3:1 w labels offset 1