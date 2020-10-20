! incomplete lu factorization
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
  type( csr_ ) :: csrobj, csr_l, csr_u
  INTEGER( I4B ) :: ierr
  real( dfp ), allocatable :: L( :, : ), U( :, : )

  call convert( from = obj, to = csrobj )
  call convert( from = csrobj, to = mat )
  call display( mat, "mat" )

  call ilu( csrobj, csr_l, csr_u, ierr, 1.0d-3, 1 )
  call display( ierr, "ierr=" )
  call convert( from = csr_l, to = L )
  call display( L, "L=" )
  call convert( from = csr_u, to = U )
  call display( U, "U=" )
  call display( MATMUL( L, U ), "LU" )

end block

end program main