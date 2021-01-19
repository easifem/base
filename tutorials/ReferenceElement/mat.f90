program main

  INTEGER :: Order, n, I
  REAL, ALLOCATABLE :: XiJ( :, : ), R( :, : )

  WRITE( *, * ) "Enter Order"
  READ( *, * ) Order

  n = ( Order + 1 ) ** 2
  ALLOCATE( XiJ( 3, n ), R( n, n ) )

  R = 0.0
  DO I = 1, n
    R( I, I ) = 1.0
  END DO

  SELECT CASE( Order )
  CASE( 1 )
    
  CASE( 2 )
  END SELECT
  


end program main