PROGRAM MAIN
  USE OMP_LIB
  IMPLICIT NONE

#include "lisf.h"

  LIS_MATRIX :: A
  INTEGER :: n, i, j, ierr, tThreads, iThread
  DOUBLE PRECISION :: val, t1, t2

  n = 50000000

  CALL LIS_MATRIX_CREATE( 0, A, ierr )
  CALL LIS_MATRIX_SET_SIZE( A, 0, n, ierr )

  CALL CPU_TIME( t1 )
  DO i = 1, n
    
    val = REAL( i )
    j = i
    CALL LIS_MATRIX_SET_VALUE( LIS_INS_VALUE, i, j, val, A, ierr )

    !val = REAL( i-1 )
    !i = i; j = i+1
    !CALL LIS_MATRIX_SET_VALUE( LIS_INS_VALUE, i, j, val, A, ierr )

  END DO
  
  CALL CPU_TIME( t2 )

  WRITE( *, * ) "N :: ", n
  WRITE( *, * ) "Time for setting values :: ", t2-t1

  CALL CPU_TIME( t1 )
  CALL LIS_MATRIX_SET_TYPE( A, LIS_MATRIX_CSR, ierr )
  CALL CPU_TIME( t2 )
  WRITE( *, * ) "Time for setting type :: ", t2-t1

  CALL CPU_TIME( t1 )
  CALL LIS_MATRIX_ASSEMBLE( A, ierr )
  CALL CPU_TIME( t2 )
  WRITE( *, * ) "Time for assembly :: ", t2-t1

  CALL CPU_TIME( t1 )
  DO i = 1, n

    val = REAL( i )
    j = i
    CALL LIS_MATRIX_SET_VALUE( LIS_INS_VALUE, i, j, val, A, ierr )

  END DO

  CALL CPU_TIME( t2 )

  WRITE( *, * ) "N :: ", n
  WRITE( *, * ) "Time for setting values 2nd time :: ", t2-t1

  !CALL LIS_OUTPUT_MATRIX( A, LIS_FMT_MM, "ex2.txt", ierr )
  CALL CPU_TIME( t1 )
  CALL LIS_MATRIX_DESTROY( A, ierr )
  CALL CPU_TIME( t2 )
  WRITE( *, * ) "Time for destroy :: ", t2-t1

END PROGRAM main
