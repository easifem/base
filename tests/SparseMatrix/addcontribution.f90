program main
  use easifem
  implicit none
  
  type( sparsematrix_ ) :: obj
  real( dfp ), allocatable :: val( :, : )

  call initiate( obj = obj, tdof = 2, tnodes = [8] )
  call setsparsity( obj = obj, row = 1, col = [1,2,7] )
  call setsparsity( obj = obj, row = 2, col = [2,1,3,6,7,8] )
  call setsparsity( obj = obj, row = 3, col = [3, 2, 4, 8] )
  call setsparsity( obj = obj, row = 4, col = [4,3,5,8] )
  call setsparsity( obj = obj, row = 5, col = [5,4,6,8] )
  call setsparsity( obj = obj, row = 6, col = [6,2,5,7,8] )
  call setsparsity( obj = obj, row = 7, col = [7,1,2,6] )
  call setsparsity( obj = obj, row = 8, col = [8,2,3,4,5,6] )
  allocate( val( 6, 6 ) ); call RANDOM_NUMBER( val ); call display( val, "val")
  call addcontribution( obj, [1,2,7], val, 1.0_dfp, DOfToNodes )
  call display( obj, "obj" )
end program main