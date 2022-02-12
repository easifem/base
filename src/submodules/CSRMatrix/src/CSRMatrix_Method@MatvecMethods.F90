! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This submodule contains the methods for sparse matrix

SUBMODULE(CSRMatrix_Method) MatVecMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      AMatvec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_AMatvec1
  CALL AMUX( SIZE( y ), x, y, obj%A, obj%csr%JA, obj%csr%IA )
END PROCEDURE csrMat_AMatvec1

!----------------------------------------------------------------------------
!                                                                     AMatvec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_AMatvec2
  CALL AMUXMS( SIZE( y ), x, y, A, JA )
END PROCEDURE csrMat_AMatvec2

!----------------------------------------------------------------------------
!                                                                   AtMatvec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_AtMatvec
  IF( isSquare( obj ) ) THEN
    CALL ATMUX( SIZE( y ), x, y, obj%A, obj%csr%JA, obj%csr%IA )
  ELSE
    CALL ATMUXR( SIZE( x ), SIZE( y ), x, y, obj%A, obj%csr%JA, obj%csr%IA )
  END IF
END PROCEDURE csrMat_AtMatvec

!----------------------------------------------------------------------------
!                                                                 MatVec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_MatVec1
  LOGICAL( LGT ) :: trans
  trans = INPUT( option=transp, default=.FALSE. )
  IF( trans ) THEN
    CALL AtMatvec( obj=obj, x=x, y=y )
  ELSE
    CALL AMatvec( obj=obj, x=x, y=y )
  END IF
END PROCEDURE csrMat_MatVec1

!----------------------------------------------------------------------------
!                                                                 MatVec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_MatVec2
  CALL AMatvec( A=A, JA=JA, x=x, y=y )
END PROCEDURE csrMat_MatVec2
END SUBMODULE MatVecMethods