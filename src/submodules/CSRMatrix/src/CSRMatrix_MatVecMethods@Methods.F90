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
!                                                                   AMatvec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_AMatvec1
REAL(DFP) :: y0(SIZE(y))
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: tsize

add0 = input(default=.FALSE., option=addContribution)
scale0 = input(default=1.0_DFP, option=scale)
tsize = SIZE(y)

IF (add0) THEN
  CALL AMUX(tsize, x, y0, obj%A, obj%csr%JA, obj%csr%IA)
  CALL AXPY(X=y0, Y=y, A=scale0)
  RETURN
END IF

CALL AMUX(tsize, x, y, obj%A, obj%csr%JA, obj%csr%IA)
CALL SCAL(X=y, A=scale0)
END PROCEDURE csrMat_AMatvec1

!----------------------------------------------------------------------------
!                                                                     AMatvec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_AMatvec2
REAL(DFP) :: y0(SIZE(y))
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: tsize

add0 = input(default=.FALSE., option=addContribution)
scale0 = input(default=1.0_DFP, option=scale)
tsize = SIZE(y)

IF (add0) THEN
  CALL AMUXMS(tsize, x, y0, A, JA)
  CALL AXPY(X=y0, Y=y, A=scale0)
  RETURN
END IF

CALL AMUXMS(tsize, x, y, A, JA)
CALL SCAL(X=y, A=scale0)
END PROCEDURE csrMat_AMatvec2

!----------------------------------------------------------------------------
!                                                                   AtMatvec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_AtMatvec
REAL(DFP) :: y0(SIZE(y))
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: ty, tx, nrow, ncol
LOGICAL(LGT) :: squareCase, problem, rectCase

add0 = INPUT(default=.FALSE., option=addContribution)
scale0 = input(default=1.0_DFP, option=scale)
ty = SIZE(y)
tx = SIZE(x)
squareCase = isSquare(obj)
rectCase = isRectangle(obj)

ncol = SIZE(obj, 2) !ncol
nrow = SIZE(obj, 1) !nrow

problem = tx .NE. nrow .OR. ty .NE. ncol

IF (add0 .AND. squareCase) THEN
  CALL ATMUX(nrow, x, y0, obj%A, obj%csr%JA, obj%csr%IA)
  CALL AXPY(X=y0, Y=y, A=scale0)
  RETURN
END IF

IF (add0 .AND. rectCase .AND. problem) THEN
  CALL Errormsg( &
    & msg="Mismatch in shapes... nrow = "//tostring(nrow)// &
    & " ncol = "//tostring(ncol)//" size(x) = "//tostring(tx)// &
    & " size(y) = "//tostring(ty), &
    & file=__FILE__, &
    & routine="csrMat_AtMatvec()", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END IF

IF (add0 .AND. rectCase) THEN
  CALL ATMUXR(ncol, nrow, x, y0, obj%A, obj%csr%JA, obj%csr%IA)
  CALL AXPY(X=y0, Y=y, A=scale0)
  RETURN
END IF

IF (squareCase) THEN
  CALL ATMUX(nrow, x, y, obj%A, obj%csr%JA, obj%csr%IA)
  CALL SCAL(X=y, A=scale0)
  RETURN
END IF

CALL ATMUXR(ncol, nrow, x, y, obj%A, obj%csr%JA, obj%csr%IA)
CALL SCAL(X=y, A=scale0)
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
  RETURN
END IF

CALL AMatvec(obj=obj, x=x, y=y, addContribution=addContribution, &
  & scale=scale)
END PROCEDURE csrMat_MatVec1

!----------------------------------------------------------------------------
!                                                                    MatVec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_MatVec2
CALL AMatvec(A=A, JA=JA, x=x, y=y, addContribution=addContribution, &
  & scale=scale)
END PROCEDURE csrMat_MatVec2

END SUBMODULE Methods
