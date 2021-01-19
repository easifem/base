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

! block
!   type( csr_ ) :: csrobj
!   INTEGER( I4B ) :: ierr
!   INTEGER( I4B ), ALLOCATABLE :: iperm( : ), ju( : ), jlu( : )
!   real( dfp ), allocatable :: alu( : )

!   call convert( from = obj, to = csrobj )
!   call convert( from = csrobj, to = mat )
!   call display( mat, "mat" )

!   call sparsekit_ilutp( csrobj, alu, jlu, ju, iperm, ierr )
!   call display( ierr, "ierr=" )

!   call display( alu, "alu", stdout, .true. )
!   call display( jlu, "jlu", stdout, .true. )
!   call display( ju, "ju", stdout, .true. )
!   call display( iperm, "iperm", stdout, .true. )
! end block

block
  type( csr_ ) :: csrobj, csr_l, csr_u
  INTEGER( I4B ) :: ierr, lfil = 5
  INTEGER( I4B ), ALLOCATABLE :: iperm( : )
  real( dfp ), allocatable :: L( :, : ), U( :, : )
  real( dfp ) :: droptol = 1.0e-4, permtol = 0.5


  call convert( from = obj, to = csrobj )
  call convert( from = csrobj, to = mat )
  call display( mat, "mat" )

  call sparsekit_ilutp( csrobj, csr_l, csr_u, iperm, ierr, droptol, permtol, &
    & lfil, csrobj % nrow )
  call display( ierr, "ierr=" )

  call convert( from = csr_l, to = L )
  call convert( from = csr_u, to = U )
  call display( L, "L" )
  call display( U, "U" )
  call display( MATMUL( L, U ), "LU" )
  call display( iperm, "iperm", stdout, .true. )


end block

end program main