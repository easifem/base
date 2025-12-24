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
USE RealVector_Method, ONLY: RealVector_Size => Size
USE InputUtility, ONLY: Input
USE F95_BLAS, ONLY: AXPY, SCAL
USE Display_Method, ONLY: ToString
USE GlobalData, ONLY: stderr
USE ErrorHandling, ONLY: Errormsg
USE CSRMatrix_Method, ONLY: IsSquare, IsRectangle, &
                            CSRMatrix_Size => Size

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE CSRMatrixATMUX1
INTEGER(I4B) :: i, k

y = 0.0_DFP

DO i = 1, n
  DO k = ia(i), ia(i + 1) - 1
    y(ja(k)) = y(ja(k)) + x(i) * a(k)
  END DO
END DO

END PROCEDURE CSRMatrixATMUX1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE CSRMatrixATMUX2
INTEGER(I4B) :: i, k

y = 0.0_DFP

DO i = 1, n
  DO k = ia(i), ia(i + 1) - 1
    y(ja(k)) = y(ja(k)) + x(i) * a(k) * s
  END DO
END DO

END PROCEDURE CSRMatrixATMUX2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE CSRMatrixATMUX_Add_1
INTEGER(I4B) :: i, k

DO i = 1, n
  DO k = ia(i), ia(i + 1) - 1
    y(ja(k)) = y(ja(k)) + x(i) * a(k) * s
  END DO
END DO

END PROCEDURE CSRMatrixATMUX_Add_1

!----------------------------------------------------------------------------
!                                                            CSRMatrixAMUX
!----------------------------------------------------------------------------

MODULE PROCEDURE CSRMatrixAMUX1
REAL(DFP) :: t
INTEGER(I4B) :: i, k

DO i = 1, n
  ! compute the inner product of row i with vector x
  t = 0.0
  DO k = ia(i), ia(i + 1) - 1
    t = t + a(k) * x(ja(k))
  END DO
  ! store result in y(i)
  y(i) = t
END DO
END PROCEDURE CSRMatrixAMUX1

!----------------------------------------------------------------------------
!                                                            CSRMatrixAMUX
!----------------------------------------------------------------------------

MODULE PROCEDURE CSRMatrixAMUX2
REAL(DFP) :: t
INTEGER(I4B) :: i, k

DO i = 1, n
  ! compute the inner product of row i with vector x
  t = 0.0
  DO k = ia(i), ia(i + 1) - 1
    t = t + a(k) * x(ja(k))
  END DO
  ! store result in y(i)
  y(i) = s * t
END DO
END PROCEDURE CSRMatrixAMUX2

!----------------------------------------------------------------------------
!                                                            CSRMatrixAMUX
!----------------------------------------------------------------------------

MODULE PROCEDURE CSRMatrixAMUX_Add_1
REAL(DFP) :: t
INTEGER(I4B) :: i, k

DO i = 1, n
  ! compute the inner product of row i with vector x
  t = 0.0
  DO k = ia(i), ia(i + 1) - 1
    t = t + a(k) * x(ja(k))
  END DO
  ! store result in y(i)
  y(i) = y(i) + s * t
END DO
END PROCEDURE CSRMatrixAMUX_Add_1

!----------------------------------------------------------------------------
!                                                                   AMatvec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_AMatvec1
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: tsize

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)
tsize = SIZE(y)

IF (add0) THEN
  CALL CSRMatrixAMUX_Add(n=tsize, x=x, y=y, a=obj%A,  &
    & ja=obj%csr%JA, ia=obj%csr%IA, s=scale0)
  RETURN
END IF

CALL CSRMatrixAMUX(n=tsize, x=x, y=y, a=obj%A, &
                   ja=obj%csr%JA, ia=obj%csr%IA, s=scale0)

END PROCEDURE csrMat_AMatvec1

!----------------------------------------------------------------------------
!                                                                     AMatvec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_AMatvec2
REAL(DFP) :: y0(SIZE(y))
LOGICAL(LGT) :: add0
REAL(DFP) :: scale0
INTEGER(I4B) :: tsize

add0 = Input(default=.FALSE., option=addContribution)
scale0 = Input(default=1.0_DFP, option=scale)
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
scale0 = Input(default=1.0_DFP, option=scale)
ty = SIZE(y)
tx = SIZE(x)
squareCase = IsSquare(obj)
rectCase = IsRectangle(obj)

ncol = CSRMatrix_Size(obj, 2) !ncol
nrow = CSRMatrix_Size(obj, 1) !nrow

problem = tx .NE. nrow .OR. ty .NE. ncol

IF (add0 .AND. squareCase) THEN
  CALL ATMUX(nrow, x, y0, obj%A, obj%csr%JA, obj%csr%IA)
  CALL AXPY(X=y0, Y=y, A=scale0)
  RETURN
END IF

IF (add0 .AND. rectCase .AND. problem) THEN
  CALL Errormsg(msg="Mismatch in shapes... nrow = "//ToString(nrow)// &
                " ncol = "//ToString(ncol)//" size(x) = "//ToString(tx)// &
                " size(y) = "//ToString(ty), &
                file=__FILE__, &
                routine="csrMat_AtMatvec()", &
                line=__LINE__, &
                unitno=stderr)
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
trans = Input(option=isTranspose, default=.FALSE.)

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
             scale=scale)
END PROCEDURE csrMat_MatVec2

!----------------------------------------------------------------------------
!                                                                    MatVec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_MatVec3
INTEGER(I4B) :: n
n = RealVector_Size(x)
CALL csrMat_MatVec1(obj=obj, x=x%val(1:n), y=y%val(1:n), &
        isTranspose=isTranspose, addContribution=addContribution, scale=scale)
END PROCEDURE csrMat_MatVec3

END SUBMODULE Methods
