program main
  use basemethod
  implicit none

  block
    real( dfp ), allocatable :: a( : )
    INTEGER( I4B ), allocatable :: b( : )
  
    allocate( a( 12 ) )
    call RANDOM_NUMBER( a )
    call display( a, "a( 1:12 ) = " )
  
    b = int( a * 10 )
    call display( b, "b( 1:12 ) = " )
  end block

  block
    real( dfp ), allocatable :: a( :, : )
    allocate( a( 12, 12 ) )
    call RANDOM_NUMBER( a )
    call display( a, "a( 1:12, 1:12 ) = " )
  end block

end program main