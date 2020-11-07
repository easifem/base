! learn how to set solver

! notes
!<--- lis_solver_create( lis_solver: solver, lis_int: ierr )
!<-- lis_precon_create( lis_solver: solver, lis_precon: precon, lis_int: ierr  )
!<--- lis_solver_set_option( char: text, lis_solver:solver, lis_int: ierr )

!<-- lis_precon_destroy( lis_solver: solver, lis_precon: precon, lis_int: ierr  )
!<--- lis_solver_destroy( lis_solver: solver, lis_int: ierr)

program main
  use easifem
  implicit none

#include "lisf.h"

  type( sparsematrix_ ) :: tanmat
  real( dfp ), allocatable :: mat( :, : )
  type( IterationData_ ) :: Iterdata
  real( dfp ) :: sol( 10 ), rhs( 10 )

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
  mat = reshape( [1,0,0,1], [2,2])
  call addcontribution( tanmat, [1,2], mat, 1.0_dfp, NONE)
  mat = reshape( [1,-1,-1,1], [2,2])
  call addcontribution( tanmat, [2,3], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [3,4], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [4,5], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [5,6], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [6,7], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [7,8], mat, 1.0_dfp, NONE)
  call addcontribution( tanmat, [8,9], mat, 1.0_dfp, NONE)
  mat = reshape( [1,0,0,1], [2,2])
  call addcontribution( tanmat, [9,10], mat, 1.0_dfp, NONE)
  call display( 'flag4' )
  call convert( from = tanmat, to = mat )
  call display( 'flag5' )
  call display( mat, 'mat' )
  call display( 'flag6' )
  rhs = 0.0; rhs( 9:10 ) = 0.5; sol = 0.0; sol( 10 ) = 0.5
  call display( rhs, 'rhs')
  call display( sol, 'true-sol')
  call display( 'flag7' )

  block
    INTEGER( I4B ) :: mpi_comm, ierr, n, nnz
    TYPE( LIS_MATRIX )  :: lis_mat
    TYPE( LIS_VECTOR )  :: lis_rhs, lis_sol
    TYPE( LIS_SOLVER ) :: solver
    TYPE( LIS_PRECON ) :: precon
    INTEGER( I4B ), ALLOCATABLE :: IA( : ), JA( : )
    REAL( DFP ), ALLOCATABLE :: AA( : )

    n = size( tanmat, 1 )
    nnz = getNNZ( tanmat )
    mpi_comm = 0

    ALLOCATE( IA( 0 : n ), JA( 0 : nnz-1 ), AA( 0:nnz-1) )
    IA( 0 : ) = tanmat % IA - 1
    JA( 0 : ) = tanmat % JA - 1
    AA( 0 : ) = tanmat % A

    call display( 'flag7' )
    call lis_matrix_create( mpi_comm, lis_mat, ierr )
    call chkerr( ierr )
    call lis_matrix_set_size( lis_mat, 0, n, ierr )
    call chkerr( ierr )
    call lis_matrix_set_csr( nnz, IA,  JA, AA, lis_mat, ierr )
    call chkerr( ierr )
    call lis_matrix_assemble( lis_mat, ierr )
    call chkerr( ierr )
    call lis_output_matrix( lis_mat, LIS_FMT_MM, './tanmat.mtx', ierr )
    call chkerr( ierr )
    call display( 'flag8' )

    call lis_solver_create( solver, ierr )
    call chkerr( ierr )
    call lis_precon_create( solver, precon, ierr )
    call chkerr( ierr )
    call lis_solver_set_option( '-i cg', solver, ierr )
    call lis_solver_set_option( '-p ilu 1', solver, ierr )
    call lis_solver_set_option( '-print 3', solver, ierr )
    call lis_solver_set_option( '-tol 0.999999974737875E-5', solver, ierr )
    call lis_solver_set_option( '-initx_zeros 0', solver, ierr )
    call chkerr( ierr )

    call lis_vector_create( mpi_comm, lis_rhs, ierr )
    call chkerr( ierr )

    call lis_vector_create( mpi_comm, lis_sol, ierr )
    call chkerr( ierr )

    call lis_vector_set_size( lis_rhs, 0, n, ierr )
    call chkerr( ierr )

    call lis_vector_set_all( 0.0_dfp, lis_rhs, ierr )
    call chkerr( ierr )

    call lis_vector_duplicate( lis_rhs, lis_sol, ierr )
    call chkerr( ierr )

    call lis_vector_copy( lis_rhs, lis_sol, ierr )
    call chkerr( ierr )

    call lis_vector_set_values( LIS_INS_VALUE, 2, &
      & [9,10], [0.5_dfp, 0.5_dfp], lis_rhs, ierr )
    call chkerr( ierr )

    call lis_output_vector( lis_rhs, LIS_FMT_PLAIN, './rhs.mtx', ierr )
    call chkerr( ierr )

    call lis_vector_set_value( LIS_INS_VALUE, 10, &
      & 0.5_dfp, lis_sol, ierr )
    call chkerr( ierr )

    call lis_output_vector( lis_sol, LIS_FMT_PLAIN, './sol0.mtx', ierr )
    call chkerr( ierr )

    call lis_solve( lis_mat, lis_rhs, lis_sol, solver, ierr )
    call chkerr( ierr )

    sol = 0
    call lis_vector_get_values( lis_sol, 1, 10, sol, ierr )
    call chkerr( ierr )
    call display( sol, 'sol' )

    call lis_output_vector( lis_sol, LIS_FMT_PLAIN, './sol.mtx', ierr )
    call chkerr( ierr )

    call display( 'flag9' )
    call lis_solver_destroy( solver, ierr )
    call chkerr( ierr )
    call lis_precon_destroy( precon, ierr )
    call chkerr( ierr )
    call lis_matrix_unset( lis_mat, ierr )
    call chkerr( ierr )
    call lis_matrix_destroy( lis_mat, ierr )
    call chkerr( ierr )
    call display( 'flag10' )

  end block
end program main

