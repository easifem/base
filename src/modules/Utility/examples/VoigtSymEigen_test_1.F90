!> author: Shion Shimizu
! date: 2026-11-17
! summary: VoigtSymEigen of a symmetric tensor in Voigt form

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE VoigtUtility, ONLY: VoigtSymEigen
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: diagonal tensor, eigenvalues in descending order
BLOCK
  REAL(DFP) :: v(6), vals(3), vecs(3, 3)

  v = [1.0_DFP, 3.0_DFP, 2.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
  CALL VoigtSymEigen(v=v, vals=vals, vecs=vecs)
  CALL OK(MAXVAL(ABS(vals - [3.0_DFP, 2.0_DFP, 1.0_DFP])) .LT. tol, &
          "VoigtSymEigen: diagonal, values")
  CALL OK(ABS(ABS(vecs(2, 1)) - 1.0_DFP) .LT. tol, &
          "VoigtSymEigen: diagonal, first vector is e2")
END BLOCK

! Test 2: full tensor, A q = lambda q and orthonormal vectors
BLOCK
  REAL(DFP) :: v(6), vals(3), vecs(3, 3)

  v = [4.0_DFP, 1.0_DFP, 2.0_DFP, 0.5_DFP, 0.3_DFP, 0.2_DFP]
  CALL VoigtSymEigen(v=v, vals=vals, vecs=vecs)
  CALL OK(EigenError(v, vals, vecs) .LT. tol, &
          "VoigtSymEigen: full, A q = lambda q")
  CALL OK(OrthoError(vecs) .LT. tol, "VoigtSymEigen: full, Q^T Q = I")
  CALL OK(vals(1) .GE. vals(2) .AND. vals(2) .GE. vals(3), &
          "VoigtSymEigen: full, descending")
END BLOCK

! Test 3: repeated eigenvalues
BLOCK
  REAL(DFP) :: v(6), vals(3), vecs(3, 3)

  v = [2.0_DFP, 2.0_DFP, 5.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
  CALL VoigtSymEigen(v=v, vals=vals, vecs=vecs)
  CALL OK(MAXVAL(ABS(vals - [5.0_DFP, 2.0_DFP, 2.0_DFP])) .LT. tol, &
          "VoigtSymEigen: repeated, values")
  CALL OK(OrthoError(vecs) .LT. tol, "VoigtSymEigen: repeated, Q^T Q = I")
END BLOCK

! Test 4: the tolerance is relative (large and small stresses)
BLOCK
  REAL(DFP) :: v(6), vals(3), vecs(3, 3), scales(2)
  INTEGER(I4B) :: ii

  scales = [1.0E+9_DFP, 1.0E-12_DFP]
  DO ii = 1, 2
    v = scales(ii) * [4.0_DFP, 1.0_DFP, 2.0_DFP, 0.5_DFP, 0.3_DFP, &
                      0.2_DFP]
    CALL VoigtSymEigen(v=v, vals=vals, vecs=vecs)
    CALL OK(EigenError(v, vals, vecs) .LT. tol, &
            "VoigtSymEigen: scaled, A q = lambda q")
    CALL OK(OrthoError(vecs) .LT. tol, "VoigtSymEigen: scaled, Q^T Q = I")
  END DO
END BLOCK

CONTAINS

!> max |A q_i - lambda_i q_i| / max |lambda|

FUNCTION EigenError(v, vals, vecs) RESULT(ans)
  REAL(DFP), INTENT(IN) :: v(6), vals(3), vecs(3, 3)
  REAL(DFP) :: ans
  REAL(DFP) :: A(3, 3)
  INTEGER(I4B) :: ii

  A = RESHAPE([v(1), v(4), v(6), v(4), v(2), v(5), v(6), v(5), v(3)], &
              [3, 3])
  ans = 0.0_DFP
  DO ii = 1, 3
    ans = MAX(ans, MAXVAL(ABS(MATMUL(A, vecs(:, ii)) - &
                              vals(ii) * vecs(:, ii))))
  END DO
  ans = ans / MAXVAL(ABS(vals))
END FUNCTION EigenError

!> max |Q^T Q - I|

FUNCTION OrthoError(vecs) RESULT(ans)
  REAL(DFP), INTENT(IN) :: vecs(3, 3)
  REAL(DFP) :: ans
  REAL(DFP) :: QtQ(3, 3)
  INTEGER(I4B) :: ii

  QtQ = MATMUL(TRANSPOSE(vecs), vecs)
  DO ii = 1, 3
    QtQ(ii, ii) = QtQ(ii, ii) - 1.0_DFP
  END DO
  ans = MAXVAL(ABS(QtQ))
END FUNCTION OrthoError

END PROGRAM main
