! This program is mat part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR mat PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received mat copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE(EigenUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SymEigenValues2by2
REAL(DFP) :: i1, i2, a, b
i1 = mat(1, 1) + mat(2, 2)
i2 = (mat(1, 1) - mat(2, 2))**2 + 4.0_DFP * (mat(1, 2)**2)
a = 0.5_DFP * i1
! b = SQRT(a**2 - i2)
b = 0.5_DFP * SQRT(i2)
ans(1) = a - b
ans(2) = a + b
END PROCEDURE SymEigenValues2by2

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE SymEigenValues3by3
REAL(DFP) :: q, p, r
REAL(DFP), PARAMETER :: twothirdpi = 2.0_DFP * pi / 3.0_DFP
!!
r = mat(1, 2) * mat(1, 2) + mat(1, 3) * mat(1, 3) + mat(2, 3) * mat(2, 3)
q = (mat(1, 1) + mat(2, 2) + mat(3, 3)) / 3.0_DFP
!!
ans(1) = mat(1, 1) - q
ans(2) = mat(2, 2) - q
ans(3) = mat(3, 3) - q
!!
p = SQRT((ans(1) * ans(1) + ans(2) * ans(2) + ans(3) * ans(3) + 2 * r) &
  & / 6.0_DFP)
!!
r = (ans(1) * (ans(2) * ans(3) - mat(2, 3) * mat(2, 3)) &
  & - mat(1, 2) * (mat(1, 2) * ans(3) - mat(2, 3) * mat(1, 3)) &
  & + mat(1, 3) * (mat(1, 2) * mat(2, 3) - ans(2) * mat(1, 3))) &
  & / (p * p * p) * 0.5_DFP
!!
IF (r <= -1.0_DFP) THEN
  r = 0.5_DFP * twothirdpi
ELSE IF (r >= 1.0_DFP) THEN
  r = 0.0_DFP
ELSE
  r = acos(r) / 3.0_DFP
END IF
!!
ans(3) = q + 2 * p * cos(r)
ans(1) = q + 2 * p * cos(r + twothirdpi)
ans(2) = 3 * q - ans(1) - ans(3)
!!
END PROCEDURE SymEigenValues3by3

!----------------------------------------------------------------------------
!                                                            SymEigenValues
!----------------------------------------------------------------------------

MODULE PROCEDURE SymEigenValuesUpto3
!!
SELECT CASE (SIZE(mat, 1))
CASE (1)
  ans(1) = mat(1, 1)
CASE (2)
  ans = SymEigenValues2by2(mat=mat)
CASE (3)
  ans = SymEigenValues3by3(mat=mat)
END SELECT
END PROCEDURE SymEigenValuesUpto3

!----------------------------------------------------------------------------
!                                                            SymEigenValues
!----------------------------------------------------------------------------

MODULE PROCEDURE SymEigenValues
INTEGER(I4B) :: n
REAL(DFP), DIMENSION(SIZE(mat, 1), SIZE(mat, 2)) :: temp
!!
n = SIZE(mat, 1)
!!
SELECT CASE (n)
CASE (1)
  ans(1) = mat(1, 1)
CASE (2)
  ans = SymEigenValues2by2(mat=mat)
CASE (3)
  ans = SymEigenValues3by3(mat=mat)
CASE DEFAULT
#ifdef USE_LAPACK95
  temp = mat
  CALL SYEV(A=temp, W=ans, JOBZ="N")
#else
  CALL ErrorMsg( &
    & msg="This routine requires Lapack95 interface", &
    & file=__FILE__, &
    & line=__LINE__, &
    & routine="SymEigenValues()", &
    & unitno=stderr)
#endif
    !!
END SELECT
END PROCEDURE SymEigenValues

!----------------------------------------------------------------------------
!                                                            SymEigenValues
!----------------------------------------------------------------------------

MODULE PROCEDURE GetSymEigenValues_
INTEGER(I4B) :: n
!!
n = SIZE(mat, 1)
!!
SELECT CASE (n)
CASE (1)
  eigenValues(1) = mat(1, 1)
CASE (2)
  eigenValues = SymEigenValues2by2(mat=mat)
CASE (3)
  eigenValues = SymEigenValues3by3(mat=mat)
CASE DEFAULT
#ifdef USE_LAPACK95
  CALL SYEV(A=mat, W=eigenValues, JOBZ="N")
#else
  CALL ErrorMsg( &
    & msg="This routine requires Lapack95 interface", &
    & file=__FILE__, &
    & line=__LINE__, &
    & routine="SymEigenValues_()", &
    & unitno=stderr)
#endif
  !!
END SELECT
END PROCEDURE GetSymEigenValues_

!----------------------------------------------------------------------------
!                                                             SymEigenValues
!----------------------------------------------------------------------------

MODULE PROCEDURE GetSymEigenValues
REAL(DFP), DIMENSION(SIZE(mat, 1), SIZE(mat, 2)) :: temp
temp = mat
CALL GetSymEigenValues_(mat=temp, eigenValues=eigenValues)
END PROCEDURE GetSymEigenValues

!----------------------------------------------------------------------------
!                                                         GetSymEigenJacobi
!----------------------------------------------------------------------------

MODULE PROCEDURE GetSymEigenJacobi
INTEGER(I4B) :: i, ip, iq, n, ii, jj, tRot
REAL(DFP) :: c, g, h, s, sm, t, tau, theta, tresh
REAL(DFP), DIMENSION(SIZE(EigenValues)) :: b, z
REAL(DFP), DIMENSION(SIZE(mat, 1), SIZE(mat, 2)) :: tempMat
REAL(DFP), PARAMETER :: tol = 1.0E-10

tempMat = mat

n = SIZE(mat, 1)

!---- Initialize v to the identity matrix.
CALL unit_matrix(EigenVectors(:, :))

!---- Initialize b and d to the diagonal of A
!---- This vector will accumulate terms of the
!---- form tapq as in eq. (11.1.14).
b(:) = get_diag(tempMat(:, :))
EigenValues(:) = b(:)
z(:) = 0.0_DFP
tRot = 0

DO i = 1, MaxIter

  !---- SUM off-diagonal elements.
  sm = SUM(ABS(tempMat), mask=upper_triangle(n, n))
  IF (SOFTEQ(sm, 0.0_DFP, tol)) THEN

    !---- Restore the upper diagonal  part
    DO jj = 1, n
      DO ii = 1, jj - 1
        tempMat(ii, jj) = tempMat(jj, ii)
      END DO
    END DO

    !---- Sort the Eigen vector and Eigen values in decreasing order
    CALL SortEigenValues(EigenValues=EigenValues, &
      & EigenVectors=EigenVectors)
    RETURN

  END IF

  !---- The normal return, which relies on quadratic convergence
  !---- to machine underflow.
  tresh = MERGE(0.2_DFP * sm / n**2, 0.0_DFP, i < 4)

  !---- On the first three sweeps, we will rotate only IF tresh exceeded.
  DO ip = 1, n - 1
    DO iq = ip + 1, n
      g = 100.0_DFP * ABS(tempMat(ip, iq))
      !---- After four sweeps, skip the rotation IF the off-diagonal
      !---- element is small.
      IF ((i > 4) .AND. (ABS(EigenValues(ip)) + g .EQ. &
        & ABS(EigenValues(ip))) .AND. (ABS(EigenValues(iq)) &
        & + g .EQ. ABS(EigenValues(iq)))) THEN

        tempMat(ip, iq) = 0.0_DFP

      ELSE IF (ABS(tempMat(ip, iq)) .GT. tresh) THEN

        h = EigenValues(iq) - EigenValues(ip)

        IF (ABS(h) + g .EQ. ABS(h)) THEN
          t = tempMat(ip, iq) / h
        ELSE
          theta = 0.5_DFP * h / tempMat(ip, iq)
          t = 1.0_DFP / (ABS(theta) + SQRT(1.0_DFP + theta**2))
          IF (theta .LT. 0.0_DFP) t = -t
        END IF

        c = 1.0_DFP / SQRT(1 + t**2)
        s = t * c
        tau = s / (1.0_DFP + c)
        h = t * tempMat(ip, iq)
        z(ip) = z(ip) - h
        z(iq) = z(iq) + h
        EigenValues(ip) = EigenValues(ip) - h
        EigenValues(iq) = EigenValues(iq) + h
        tempMat(ip, iq) = 0.0_DFP

        CALL jrotate(tempMat(1:ip - 1, ip), tempMat(1:ip - 1, iq))
        ! Case of rotations 1 ≤ j < p.
        CALL jrotate(tempMat(ip, ip + 1:iq - 1), tempMat(ip + 1:iq - 1, iq))
        ! Case of rotations p < j < q.
        CALL jrotate(tempMat(ip, iq + 1:n), tempMat(iq, iq + 1:n))
        ! Case of rotations q < j ≤ n.
        CALL jrotate(EigenVectors(:, ip), EigenVectors(:, iq))
        tRot = tRot + 1
      END IF
    END DO
  END DO

  b(:) = b(:) + z(:)
  ! Update d with the SUM of tapq, and reinitialize z.
  EigenValues(:) = b(:)
  z(:) = 0.0_DFP
END DO

CONTAINS

PURE SUBROUTINE jrotate(a1, a2)
  REAL(DFP), DIMENSION(:), INTENT(INOUT) :: a1, a2
  REAL(DFP), DIMENSION(SIZE(a1)) :: wk1
  wk1(:) = a1(:)
  a1(:) = a1(:) - s * (a2(:) + a1(:) * tau)
  a2(:) = a2(:) + s * (wk1(:) - a2(:) * tau)
END SUBROUTINE jrotate

PURE FUNCTION get_diag(mat)
  REAL(DFP), DIMENSION(:, :), INTENT(IN) :: mat
  REAL(DFP), DIMENSION(SIZE(mat, 1)) :: get_diag
  INTEGER(I4B) :: j
  j = size(mat, 1)
  DO j = 1, size(mat, 1)
    get_diag(j) = mat(j, j)
  END DO
END FUNCTION get_diag

PURE SUBROUTINE unit_matrix(mat)
  REAL(DFP), DIMENSION(:, :), INTENT(OUT) :: mat
  INTEGER(I4B) :: i, n
  n = min(size(mat, 1), size(mat, 2))
  mat(:, :) = 0.0_sp
  DO i = 1, n
    mat(i, i) = 1.0_sp
  END DO
END SUBROUTINE unit_matrix

PURE SUBROUTINE SortEigenValues(EigenValues, EigenVectors)
  REAL(DFP), DIMENSION(:), INTENT(INOUT) :: EigenValues
  REAL(DFP), DIMENSION(:, :), INTENT(INOUT) :: EigenVectors
  INTEGER(I4B) :: i, j, n

  n = SIZE(EigenValues)

  DO i = 1, n - 1
    j = ImaxLoc(EigenValues(i:n))
    j = j + i - 1
    IF (j .NE. i) THEN
      CALL SWAP(EigenValues(i), EigenValues(j))
      CALL SWAP(EigenVectors(:, i), EigenVectors(:, j))
    END IF
  END DO
END SUBROUTINE SortEigenValues

PURE FUNCTION upper_triangle(j, k, extra)
  INTEGER(I4B), INTENT(IN) :: j, k
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: extra
  LOGICAL(LGT), DIMENSION(j, k) :: upper_triangle
  INTEGER(I4B) :: n
  n = 0
  IF (PRESENT(extra)) n = extra
  upper_triangle = (outerDiff(arth(1, 1, j), arth(1, 1, k)) < n)
END FUNCTION upper_triangle

! PURE FUNCTION lower_triangle(j,k,extra)
!   INTEGER(I4B), INTENT(IN) :: j,k
!   INTEGER(I4B), OPTIONAL, INTENT(IN) :: extra
!   LOGICAL(LGT), DIMENSION(j,k) :: lower_triangle
!   INTEGER(I4B) :: n
!   n=0
!   IF (PRESENT(extra)) n=extra
!   lower_triangle=(outerdIFf(arth_i(1,1,j),arth_i(1,1,k)) > -n)
! END FUNCTION lower_triangle
END PROCEDURE GetSymEigenJacobi

END SUBMODULE Methods
