MODULE KeyValue_Method
!! This module contains interfaces of methods related to [[keyvalue_]]

!> authors: Dr. Vikas Sharma
!
!
!### Usage
!
! ```fortran
! program main
! use easifem
! implicit none
! type( keyvalue_ ) :: obj
! real( dfp ) :: vec( 3 ), mat( 3, 3 )
! call random_number( vec )
! call random_number( mat )
! obj = keyvalue( 'real-rank-0', 1.0_dfp )
! obj = 1.0_dfp
! call display( obj, 'obj' )
! obj = keyvalue( 'real-rank-1', vec )
! obj = [1.0_dfp, 1.0_dfp, 1.0_dfp]
! call display( obj, 'obj' )
! obj = keyvalue( 'real-rank-2', mat )
! call display( obj, 'obj' )
! end program main
! ```

USE BaseType
USE GlobalData
USE StringiFor

IMPLICIT NONE

PRIVATE

INTEGER, PARAMETER :: REAL_RANK_0 = 0
INTEGER, PARAMETER :: REAL_RANK_1 = 1
INTEGER, PARAMETER :: REAL_RANK_2 = 2
INTEGER, PARAMETER :: INT_RANK_0 = 3
INTEGER, PARAMETER :: INT_RANK_1 = 4
INTEGER, PARAMETER :: INT_RANK_2 = 5

#include "./constructor.inc"
#include "./setmethod.inc"
#include "./getmethod.inc"
#include "./contains.part"

END MODULE KeyValue_Method