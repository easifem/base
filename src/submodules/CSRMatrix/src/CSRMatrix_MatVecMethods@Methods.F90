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

!> author: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This submodule contains the methods for sparse matrix

SUBMODULE(CSRMatrix_MatVecMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      AMatvec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_AMatvec1
REAL(dfp), ALLOCATABLE :: y0(:)
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
!
add0 = input(default=.FALSE., option=addContribution)
scale0 = input(default=1.0_DFP, option=scale)
!
IF (add0) THEN
  ALLOCATE (y0(SIZE(y)))
  CALL AMUX(SIZE(y0), x, y0, obj%A, obj%csr%JA, obj%csr%IA)
  CALL AXPY(X=y0, Y=y, A=scale0)
  DEALLOCATE (y0)
ELSE
  CALL AMUX(SIZE(y), x, y, obj%A, obj%csr%JA, obj%csr%IA)
END IF
END PROCEDURE csrMat_AMatvec1

!----------------------------------------------------------------------------
!                                                                     AMatvec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_AMatvec2
REAL(dfp), ALLOCATABLE :: y0(:)
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
!
add0 = input(default=.FALSE., option=addContribution)
scale0 = input(default=1.0_DFP, option=scale)
!
IF (add0) THEN
  ALLOCATE (y0(SIZE(y)))
  CALL AMUXMS(SIZE(y0), x, y0, A, JA)
  CALL AXPY(X=y0, Y=y, A=scale0)
  DEALLOCATE (y0)
ELSE
  CALL AMUXMS(SIZE(y), x, y, A, JA)
END IF
END PROCEDURE csrMat_AMatvec2

!----------------------------------------------------------------------------
!                                                                   AtMatvec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_AtMatvec
REAL(DFP), ALLOCATABLE :: y0(:)
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: m, n
!
add0 = INPUT(default=.FALSE., option=addContribution)
scale0 = input(default=1.0_DFP, option=scale)
!
IF (add0) THEN
  ALLOCATE (y0(SIZE(y)))
  IF (isSquare(obj)) THEN
    CALL ATMUX(SIZE(y0), x, y0, obj%A, obj%csr%JA, obj%csr%IA)
  ELSE
    m = SIZE(obj, 2)
    n = SIZE(obj, 1)
    IF (SIZE(x) .NE. n .OR. SIZE(y) .NE. m) THEN
      CALL Errormsg( &
        & msg="Mismatch in shapes... nrow = "//tostring(n)// &
        & " ncol = "//tostring(m)//" size(x) = "//tostring(SIZE(x))// &
        & " size(y) = "//tostring(SIZE(y)), &
        & file=__FILE__, &
        & routine="csrMat_AtMatvec()", &
        & line=__LINE__, &
        & unitno=stderr)
      STOP
    END IF
    CALL ATMUXR(m, n, x, y0, obj%A, obj%csr%JA, obj%csr%IA)
  END IF
  CALL AXPY(X=y0, Y=y, A=scale0)
  DEALLOCATE (y0)
ELSE
  IF (isSquare(obj)) THEN
    CALL ATMUX(SIZE(y), x, y, obj%A, obj%csr%JA, obj%csr%IA)
  ELSE
    CALL ATMUXR(SIZE(x), SIZE(y), x, y, obj%A, obj%csr%JA, obj%csr%IA)
  END IF
END IF
END PROCEDURE csrMat_AtMatvec

!----------------------------------------------------------------------------
!                                                                    MatVec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_MatVec1
LOGICAL(LGT) :: trans
trans = INPUT(option=isTranspose, default=.FALSE.)
IF (trans) THEN
  CALL AtMatvec(obj=obj, x=x, y=y, addContribution=addContribution, &
  & scale=scale)
ELSE
  CALL AMatvec(obj=obj, x=x, y=y, addContribution=addContribution, &
  & scale=scale)
END IF
END PROCEDURE csrMat_MatVec1

!----------------------------------------------------------------------------
!                                                                    MatVec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_MatVec2
CALL AMatvec(A=A, JA=JA, x=x, y=y, addContribution=addContribution, &
  & scale=scale)
END PROCEDURE csrMat_MatVec2

END SUBMODULE Methods
