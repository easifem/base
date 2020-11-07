PROGRAM main
  use easifem
  IMPLICIT NONE
#include "lisf.h"

  TYPE( LIS_MATRIX ) :: A
  INTEGER( I4B ) :: n, i, j, ierr
  REAL( DFP ) :: val
  INTEGER( I4B ) :: MPI_COMM

  MPI_COMM = 0
  n = 4

  CALL LIS_MATRIX_CREATE( 0, A, ierr )
  CALL LIS_MATRIX_SET_SIZE( A, 0, n, ierr )

  val = 2.0; i = 1; j = 1
  CALL LIS_MATRIX_SET_VALUE( LIS_ADD_VALUE, i, j, val, A, ierr )

  val = 1.0; i = 1; j = 2
  CALL LIS_MATRIX_SET_VALUE( LIS_ADD_VALUE, i, j, val, A, ierr )

  val = 1.0; i = 2; j = 1
  CALL LIS_MATRIX_SET_VALUE( LIS_ADD_VALUE, i, j, val, A, ierr )

  val = 1.0; i = 2; j = 2
  CALL LIS_MATRIX_SET_VALUE( LIS_ADD_VALUE, i, j, val, A, ierr )

  val = 1.0; i = 2; j = 2
  CALL LIS_MATRIX_SET_VALUE( LIS_ADD_VALUE, i, j, val, A, ierr )

  val = 1.0; i = 2; j = 3
  CALL LIS_MATRIX_SET_VALUE( LIS_ADD_VALUE, i, j, val, A, ierr )

  val = 1.0; i = 3; j = 2
  CALL LIS_MATRIX_SET_VALUE( LIS_ADD_VALUE, i, j, val, A, ierr )

  val = 2.0; i = 3; j = 3
  CALL LIS_MATRIX_SET_VALUE( LIS_ADD_VALUE, i, j, val, A, ierr )

  val = 1.0; i = 3; j = 4
  CALL LIS_MATRIX_SET_VALUE( LIS_ADD_VALUE, i, j, val, A, ierr )

  val = 1.0; i = 4; j = 3
  CALL LIS_MATRIX_SET_VALUE( LIS_ADD_VALUE, i, j, val, A, ierr )

  val = 2.0; i = 4; j = 4
  CALL LIS_MATRIX_SET_VALUE( LIS_ADD_VALUE, i, j, val, A, ierr )

  CALL LIS_MATRIX_SET_TYPE( A, LIS_MATRIX_CSR, ierr )
  CALL LIS_MATRIX_ASSEMBLE( A, ierr )

  CALL LIS_OUTPUT_MATRIX( A, LIS_FMT_MM, "ex1.txt", ierr )

END PROGRAM main
