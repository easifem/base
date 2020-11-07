!----------------------------------------------
! Author    :: Vikas Sharma
! Institute :: Kyoto University, Japan
!----------------------------------------------
! Purpose   ::
! ============
! This program demonstrate how to set up CSR
! matrix in using LIS
!-----------------------------------------------

PROGRAM MAIN

  USE OMP_LIB
  IMPLICIT NONE

#include "lisf.h"

  LIS_MATRIX :: A
  LIS_VECTOR :: Lis_x, Lis_y
  INTEGER :: nnz, n, ierr, i
  INTEGER, ALLOCATABLE :: Index( : ), Ptr( : )
  DOUBLE PRECISION :: val, t1, t2
  DOUBLE PRECISION, ALLOCATABLE, TARGET :: Value( : )
  DOUBLE PRECISION, POINTER :: ValPtr( : )

  nnz = 9
  n = 4
  ALLOCATE( Index( 1:nnz ), Value( 1:nnz ), Ptr( 1:n+1 ) )
  Index = [0, 1, 0, 1, 1, 2, 0, 2, 3]
  Value = [11., 12.,  21., 22., 32., 33., 41., 43., 44.]
  Ptr = [0, 2, 4, 6, 9]

  CALL LIS_MATRIX_CREATE( 0, A, ierr )
  CALL LIS_MATRIX_SET_SIZE( A, 0, n, ierr )
  CALL LIS_MATRIX_SET_CSR( nnz, Ptr, Index, Value, A, ierr )
  CALL LIS_MATRIX_ASSEMBLE( A, ierr )
  !CALL LIS_OUTPUT_MATRIX( A, LIS_FMT_MM, "LIS_mat.txt", ierr )
  CALL LIS_VECTOR_CREATE( 0, Lis_x, ierr )
  CALL LIS_VECTOR_CREATE( 0, Lis_y, ierr )
  CALL LIS_VECTOR_SET_SIZE( Lis_x, 0, n, ierr )
  CALL LIS_VECTOR_SET_SIZE( Lis_y, 0, n, ierr )
  Value = [1.,1.,1.,1.]
  CALL LIS_VECTOR_SCATTER( Value, Lis_x, ierr )
  CALL LIS_MATVEC( A, Lis_x, Lis_y, ierr )
  CALL LIS_OUTPUT_VECTOR( Lis_x, LIS_FMT_MM, "LIS_x.txt", ierr )
  CALL LIS_OUTPUT_MATRIX( A, LIS_FMT_MM, "LIS_mat.txt", ierr )
  CALL LIS_OUTPUT_VECTOR( Lis_y, LIS_FMT_MM, "LIS_y.txt", ierr )
  
  CALL LIS_MATRIX_DESTROY( A, ierr )
  CALL LIS_VECTOR_DESTROY( Lis_x, ierr )
  CALL LIS_VECTOR_DESTROY( Lis_y, ierr )
  
END PROGRAM main
