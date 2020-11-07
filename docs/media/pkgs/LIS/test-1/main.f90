! learn how to set matrix in lis

! notes
!<--- method-1
!<--- call lis_matrix_create( int: comm, lis_matrix: A, int: ierr )

!<--- call lis_matrix_set_size( lis_matrix: A, lis_int: local_n, &
!       & lis_int: global_n, lis_int: ierr )

!<--- call lis_matrix_set_value( lis_int: flag, lis_int: i, lis_int:j, &
!       & lis_scalar: value, lis_matrix: A, lis_int: ierr )

!<--- call lis_matrix_set_type( lis_matix: A, lis_int: matrix_type, lis_int: ierr)
! call lis_matrix_set_type( A, LIS_MATRIX_CSR, ierr )

!<--- call lis_matrix_assemble( lis_matrix: A, lis_int: ierr )

!<--- call lis_matrix_destroy( lis_matrix: A, lis_int: ierr )


!<--- method-2
!<--- call lis_matrix_create( int: comm, lis_matrix: A, int: ierr )

!<--- call lis_matrix_set_size( lis_matrix: A, lis_int: local_n, &
!       & lis_int: global_n, lis_int: ierr )

!<--- call lis_matrix_set_csr( lis_int: nnz, lis_int: IA( : ), &
!       & lis_int: JA(), lis_scalar: A( : ), lis_matrix: A, lis_int: ierr )
!<--- call lis_matrix_assemble( lis_matrix: A, lis_int: ierr )
!<--- call lis_matrix_destroy( lis_matrix: A, lis_int: ierr )

program main
  use easifem
  implicit none

#include "lisf.h"

  type( sparsematrix_ ) :: tanmat
  real( dfp ), allocatable :: mat( :, : )
  type( IterationData_ ) :: Iterdata
  real( dfp ) :: sol0( 10 ), sol( 10 ), rhs( 10 )

  ! make csr matrix
  call display( 'flag1' )
  call initiate( obj = tanmat, tdof = 1, tnodes = [10] )
  call setsparsity( obj = tanmat, row = 1, col = [1,2] )
  call setsparsity( obj = tanmat, row = 2, col = [1,2,3] )
  call setsparsity( obj = tanmat, row = 3, col = [2,3,4] )
  call setsparsity( obj = tanmat, row = 4, col = [3,4,5] )
  call setsparsity( obj = tanmat, row = 5, col = [4,5,6] )
  call setsparsity( obj = tanmat, row = 6, col = [5,6,7] )
  call setsparsity( obj = tanmat, row = 7, col = [6,7,8] )
  call setsparsity( obj = tanmat, row = 8, col = [7,8,9] )
  call setsparsity( obj = tanmat, row = 9, col = [8,9,10] )
  call setsparsity( obj = tanmat, row = 10, col = [9,10] )
  call display( 'flag2' )
  call setsparsity( tanmat )
  call display( 'flag3' )
  allocate( mat( 2, 2 ) )
  mat = reshape( [1,-1,-1,1], [2,2])
  call addcontribution( tanmat, [1,2], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [2,3], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [3,4], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [4,5], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [5,6], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [6,7], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [7,8], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [8,9], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [9,10], mat, 1.0_dfp, NONE)
  call display( 'flag4' )
  call convert( from = tanmat, to = mat )
  call display( 'flag5' )
  call display( mat, 'mat' )
  call display( 'flag6' )

  block
    INTEGER( I4B ) :: mpi_comm, ierr, n, nnz
    TYPE( LIS_MATRIX )  :: A
    INTEGER( I4B ), ALLOCATABLE :: IA( : ), JA( : )
    REAL( DFP ), ALLOCATABLE :: Amat( : )

    n = size( tanmat, 1 )
    nnz = getNNZ( tanmat )
    mpi_comm = 0

    ALLOCATE( IA( 0 : n ), JA( 0 : nnz-1 ), Amat( 0:nnz-1) )
    IA( 0 : ) = tanmat % IA - 1
    JA( 0 : ) = tanmat % JA - 1
    Amat( 0 : ) = tanmat % A

    call lis_matrix_create( mpi_comm, A, ierr )
    call chkerr( ierr )
    call lis_matrix_set_size( A, 0, n, ierr )
    call chkerr( ierr )
    call lis_matrix_set_csr( nnz, IA,  JA, Amat, A, ierr )
    call chkerr( ierr )
    call lis_matrix_assemble( A, ierr )
    call chkerr( ierr )
    Amat( 0 : ) = 2.0*Amat ( 0 : )
    call lis_output_matrix( A, LIS_FMT_MM, './tanmat.mtx', ierr )
    call chkerr( ierr )
    ! call lis_matrix_destroy( A, ierr )
  end block
end program main

