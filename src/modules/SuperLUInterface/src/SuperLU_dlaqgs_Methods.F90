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

MODULE SuperLU_dlaqgs_Methods
USE SuperLU_Types
USE ISO_C_BINDING, ONLY: C_CHAR, C_INT, C_FLOAT, C_DOUBLE, C_PTR
IMPLICIT NONE
PRIVATE

! *   Purpose
! *   =======
! *
! *   DLAQGS equilibrates a general sparse M by N matrix A using the row and
! *   scaling factors in the vectors R and C.
! *
! *   See supermatrix.h for the definition of 'SuperMatrix' structure.
! *
! *   Arguments
! *   =========
! *
! *   A       (input/output) SuperMatrix*
! *           On exit, the equilibrated matrix.  See EQUED for the form of
! *           the equilibrated matrix. The type of A can be:
! *            Stype = NC; Dtype = SLU_D; Mtype = GE.
! *
! *   R       (input) double*, dimension (A->nrow)
! *           The row scale factors for A.
! *
! *   C       (input) double*, dimension (A->ncol)
! *           The column scale factors for A.
! *
! *   ROWCND  (input) double
! *           Ratio of the smallest R(i) to the largest R(i).
! *
! *   COLCND  (input) double
! *           Ratio of the smallest C(i) to the largest C(i).
! *
! *   AMAX    (input) double
! *           Absolute value of largest matrix entry.
! *
! *   EQUED   (output) char*
! *           Specifies the form of equilibration that was done.
! *           = 'N':  No equilibration
! *           = 'R':  Row equilibration, i.e., A has been premultiplied by
! *                   diag(R).
! *           = 'C':  Column equilibration, i.e., A has been postmultiplied
! *                   by diag(C).
! *           = 'B':  Both row and column equilibration, i.e., A has been
! *                   replaced by diag(R) * A * diag(C).
! *
! *   Internal Parameters
! *   ===================
! *
! *   THRESH is a threshold value used to decide if row or column scaling
! *   should be done based on the ratio of the row or column scaling
! *   factors.  If ROWCND < THRESH, row scaling is done, and if
! *   COLCND < THRESH, column scaling is done.
! *
! *   LARGE and SMALL are threshold values used to decide if row scaling
! *   should be done based on the absolute size of the largest matrix
! *   element.  If AMAX > LARGE or AMAX < SMALL, row scaling is done.
! *
! *   =====================================================================
!
! void
! dlaqgs(SuperMatrix *A, double *r, double *c,
!         double rowcnd, double colcnd, double amax, char *equed)
!

INTERFACE
  SUBROUTINE dlaqgs(A, r, c, rowcnd, colcnd, amax, equed)&
    & BIND(C, name="dlaqgs")
    IMPORT :: SuperMatrix, C_DOUBLE, C_CHAR

    TYPE(SuperMatrix), INTENT(INOUT) :: A
    REAL(C_DOUBLE), INTENT(INOUT) :: r(*)
    REAL(C_DOUBLE), INTENT(INOUT) :: c(*)
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: rowcnd
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: colcnd
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: amax
    CHARACTER(1, kind=C_CHAR), INTENT(INOUT) :: equed
  END SUBROUTINE dlaqgs
END INTERFACE

PUBLIC :: dlaqgs

END MODULE SuperLU_dlaqgs_Methods
