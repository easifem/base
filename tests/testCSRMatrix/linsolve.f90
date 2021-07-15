program main
use easifem
implicit none

type( sparsematrix_ ) :: obj
real( dfp ), allocatable :: mat( :, : )

call initiate( obj = obj, tdof = 1, tnodes = [8] )
call setsparsity( obj = obj, row = 1, col = [1,2,7] )
call setsparsity( obj = obj, row = 2, col = [2,1,3,6,7,8] )
call setsparsity( obj = obj, row = 3, col = [3, 2, 4, 8] )
call setsparsity( obj = obj, row = 4, col = [4,3,5,8] )
call setsparsity( obj = obj, row = 5, col = [5,4,6,8] )
call setsparsity( obj = obj, row = 6, col = [6,2,5,7,8] )
call setsparsity( obj = obj, row = 7, col = [7,1,2,6] )
call setsparsity( obj = obj, row = 8, col = [8,2,3,4,5,6] )

allocate( mat( 3, 3 ) )
call RANDOM_NUMBER( mat ); call addcontribution( obj, [1,2,7], mat, 1.0_dfp, na)
call RANDOM_NUMBER( mat ); call addcontribution( obj, [2,3,8], mat, 1.0_dfp, na)
call RANDOM_NUMBER( mat ); call addcontribution( obj, [3,4,8], mat, 1.0_dfp, na)
call RANDOM_NUMBER( mat ); call addcontribution( obj, [4,5,8], mat, 1.0_dfp, na)
call RANDOM_NUMBER( mat ); call addcontribution( obj, [5,6,8], mat, 1.0_dfp, na)
call RANDOM_NUMBER( mat ); call addcontribution( obj, [2,6,7], mat, 1.0_dfp, na)
call RANDOM_NUMBER( mat ); call addcontribution( obj, [8,6,2], mat, 1.0_dfp, na)

block
  type( crs_ ) :: crsobj
  real( dfp ), allocatable :: x( : ), y( : )
  integer( i4b ) :: nrow, ncol

  call convert( from = obj, to = crsobj )
  nrow = SIZE( obj, 1 )
  ncol = SIZE( obj, 2 )


end block

end program main