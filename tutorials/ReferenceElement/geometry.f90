program main
  use basetype
  use basemethod
  implicit none

  ! line 
  block
    type( referenceline_ ) :: obj
    integer( i4b ), allocatable :: FM( :, : )
    obj = ReferenceLine( )
    call display( obj, "Reference Line" )
    FM = FacetMatrix( obj )
    call display( FM, "FM" )
  end block

  ! ! triangle 
  ! block
  !   type( referencetriangle_ ) :: obj
  !   integer( i4b ), allocatable :: FM( :, : )
  !   obj = ReferenceTriangle( )
  !   call display( obj, "Reference Triangle" )
  !   FM = FacetMatrix( obj )
  !   call display( FM, "FM" )
  ! end block

  ! ! Quadrangle 
  ! block
  !   type( referenceQuadrangle_ ) :: obj
  !   integer( i4b ), allocatable :: FM( :, : )
  !   obj = ReferenceQuadrangle( )
  !   call display( obj, "Reference Quadrangle" )
  !   FM = FacetMatrix( obj )
  !   call display( FM, "FM" )
  ! end block
end program main