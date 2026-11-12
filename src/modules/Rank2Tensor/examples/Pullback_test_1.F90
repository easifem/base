!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pull back and push forward of tensors and vectors

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Rank2Tensor_Method, ONLY: Pullback
USE Rank2Tensor_Method, ONLY: PushForward
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
                                            2.0_DFP, 1.0_DFP, 0.0_DFP, &
                                            0.0_DFP, 0.0_DFP, 2.0_DFP], &
                                           [3, 3])
REAL(DFP), PARAMETER :: invF0(3, 3) = RESHAPE( &
                        [1.0_DFP, 0.0_DFP, 0.0_DFP, &
                         -2.0_DFP, 1.0_DFP, 0.0_DFP, &
                         0.0_DFP, 0.0_DFP, 0.5_DFP], [3, 3])
REAL(DFP), PARAMETER :: T0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, &
                                            4.0_DFP, 5.0_DFP, 6.0_DFP, &
                                            7.0_DFP, 8.0_DFP, 10.0_DFP], &
                                           [3, 3])

! Test 1: tensor pull back with known values
BLOCK
  TYPE(Rank2Tensor_) :: F, T, P

  F = Rank2Tensor(F0)
  T = Rank2Tensor(T0)
  P = Pullback(T, F, "CONTRAVAR", "CONTRAVAR")
  CALL OK(MAXVAL(ABS(P%T - MATMUL(invF0, MATMUL(T0, TRANSPOSE(invF0))))) &
          .LT. tol, "CONTRAVAR, CONTRAVAR: inv(F) T inv(F)^T")
  P = Pullback(T, F, "COVAR", "COVAR")
  CALL OK(MAXVAL(ABS(P%T - MATMUL(TRANSPOSE(F0), MATMUL(T0, F0)))) &
          .LT. tol, "COVAR, COVAR: F^T T F")
  P = Pullback(T, F, "NA", "NA")
  CALL OK(MAXVAL(ABS(P%T - T0)) .LT. tol, "NA, NA: T")
END BLOCK

! Test 2: tensor push forward with known values
BLOCK
  TYPE(Rank2Tensor_) :: F, T, P

  F = Rank2Tensor(F0)
  T = Rank2Tensor(T0)
  P = PushForward(T, F, "CONTRAVAR", "CONTRAVAR")
  CALL OK(MAXVAL(ABS(P%T - MATMUL(F0, MATMUL(T0, TRANSPOSE(F0))))) &
          .LT. tol, "push CONTRAVAR, CONTRAVAR: F T F^T")
  P = PushForward(T, F, "COVAR", "COVAR")
  CALL OK(MAXVAL(ABS(P%T - MATMUL(TRANSPOSE(invF0), MATMUL(T0, invF0)))) &
          .LT. tol, "push COVAR, COVAR: inv(F)^T T inv(F)")
END BLOCK

! Test 3: PushForward(Pullback(T)) = T for all index variances
BLOCK
  CHARACTER(9), PARAMETER :: names(3) = ["CONTRAVAR", "COVAR    ", &
                                         "NA       "]
  TYPE(Rank2Tensor_) :: F, T, P, Q
  INTEGER(I4B) :: ii, jj

  F = Rank2Tensor(F0)
  T = Rank2Tensor(T0)
  DO ii = 1, 3
    DO jj = 1, 3
      P = Pullback(T, F, TRIM(names(ii)), TRIM(names(jj)))
      Q = PushForward(P, F, TRIM(names(ii)), TRIM(names(jj)))
      CALL OK(MAXVAL(ABS(Q%T - T0)) .LT. tol, &
              "push(pull(T)) "//TRIM(names(ii))//" "//TRIM(names(jj)))
    END DO
  END DO
END BLOCK

! Test 4: vector pull back and push forward
BLOCK
  TYPE(Rank2Tensor_) :: F
  REAL(DFP) :: v(3), w(3)

  F = Rank2Tensor(F0)
  v = [1.0_DFP, -2.0_DFP, 0.5_DFP]
  w = Pullback(v, F, "CONTRAVAR")
  CALL OK(MAXVAL(ABS(w - [5.0_DFP, -2.0_DFP, 0.25_DFP])) .LT. tol, &
          "vector CONTRAVAR: inv(F) v")
  w = Pullback(v, F, "COVAR")
  CALL OK(MAXVAL(ABS(w - [1.0_DFP, 0.0_DFP, 1.0_DFP])) .LT. tol, &
          "vector COVAR: F^T v")
  w = PushForward(Pullback(v, F, "CONTRAVAR"), F, "CONTRAVAR")
  CALL OK(MAXVAL(ABS(w - v)) .LT. tol, "vector push(pull) CONTRAVAR")
  w = PushForward(Pullback(v, F, "COVAR"), F, "COVAR")
  CALL OK(MAXVAL(ABS(w - v)) .LT. tol, "vector push(pull) COVAR")
  w = PushForward(v, F, "NA")
  CALL OK(MAXVAL(ABS(w - v)) .LT. tol, "vector NA: v")
END BLOCK

END PROGRAM main
