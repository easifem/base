!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Derivatives of the mean stress, J2, J3 and the equivalent
!   stress, checked in closed form and with central finite differences
!
! The finite difference is taken along a symmetric direction E, so
! that D : E is compared with (f(sigma + hE) - f(sigma - hE)) / 2h.

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: EquivalentStress
USE Rank2Tensor_Method, ONLY: EquivalentStressDerivative
USE Rank2Tensor_Method, ONLY: J2
USE Rank2Tensor_Method, ONLY: J2Derivative
USE Rank2Tensor_Method, ONLY: J3
USE Rank2Tensor_Method, ONLY: J3Derivative
USE Rank2Tensor_Method, ONLY: MeanStress
USE Rank2Tensor_Method, ONLY: MeanStressDerivative
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
REAL(DFP), PARAMETER :: fdTol = 1.0E-6_DFP
REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
                        [10.0_DFP, 2.0_DFP, 3.0_DFP, &
                         2.0_DFP, -20.0_DFP, 4.0_DFP, &
                         3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
REAL(DFP), PARAMETER :: E(3, 3) = RESHAPE( &
                        [0.3_DFP, -0.2_DFP, 0.5_DFP, &
                         -0.2_DFP, 0.7_DFP, 0.1_DFP, &
                         0.5_DFP, 0.1_DFP, -0.4_DFP], [3, 3])

! Test 1: MeanStressDerivative is I/3 and matches finite differences
BLOCK
  TYPE(Rank2Tensor_) :: D
  REAL(DFP) :: fd

  D = MeanStressDerivative(Rank2Tensor(s0, .TRUE.))
  CALL OK(MAXVAL(ABS(D%T - Eye3 / 3.0_DFP)) .LT. tol, "mean: D = I/3")
  CALL OK(D%isSym, "mean: isSym is true")
  fd = (MeanStress(Rank2Tensor(s0 + h * E, .TRUE.)) &
        - MeanStress(Rank2Tensor(s0 - h * E, .TRUE.))) / (2.0_DFP * h)
  CALL OK(ABS(SUM(D%T * E) - fd) .LT. fdTol, "mean: D:E = finite diff")
END BLOCK

! Test 2: J2Derivative is dev(sigma) and matches finite differences
BLOCK
  TYPE(Rank2Tensor_) :: D
  REAL(DFP) :: fd, dev(3, 3)

  dev = s0 - (s0(1, 1) + s0(2, 2) + s0(3, 3)) / 3.0_DFP * Eye3
  D = J2Derivative(Rank2Tensor(s0, .TRUE.))
  CALL OK(MAXVAL(ABS(D%T - dev)) .LT. tol, "J2: D = dev(sigma)")
  CALL OK(D%isSym, "J2: isSym is true")
  fd = (J2(Rank2Tensor(s0 + h * E, .TRUE.)) &
        - J2(Rank2Tensor(s0 - h * E, .TRUE.))) / (2.0_DFP * h)
  CALL OK(ABS(SUM(D%T * E) - fd) .LT. fdTol * ABS(fd), &
          "J2: D:E = finite diff")
END BLOCK

! Test 3: J3Derivative matches finite differences and is deviatoric
BLOCK
  TYPE(Rank2Tensor_) :: D
  REAL(DFP) :: fd

  D = J3Derivative(Rank2Tensor(s0, .TRUE.))
  CALL OK(ABS(D%T(1, 1) + D%T(2, 2) + D%T(3, 3)) .LT. 1.0E-9_DFP, &
          "J3: trace of D = 0")
  CALL OK(MAXVAL(ABS(D%T - TRANSPOSE(D%T))) .LT. tol, "J3: D symmetric")
  fd = (J3(Rank2Tensor(s0 + h * E, .TRUE.)) &
        - J3(Rank2Tensor(s0 - h * E, .TRUE.))) / (2.0_DFP * h)
  CALL OK(ABS(SUM(D%T * E) - fd) .LT. fdTol * ABS(fd), &
          "J3: D:E = finite diff")
END BLOCK

! Test 4: EquivalentStressDerivative matches finite differences
BLOCK
  TYPE(Rank2Tensor_) :: D
  REAL(DFP) :: fd

  D = EquivalentStressDerivative(Rank2Tensor(s0, .TRUE.))
  CALL OK(MAXVAL(ABS(D%T - TRANSPOSE(D%T))) .LT. tol, &
          "equivalent: D symmetric")
  fd = (EquivalentStress(Rank2Tensor(s0 + h * E, .TRUE.)) &
        - EquivalentStress(Rank2Tensor(s0 - h * E, .TRUE.))) &
       / (2.0_DFP * h)
  CALL OK(ABS(SUM(D%T * E) - fd) .LT. fdTol * ABS(fd), &
          "equivalent: D:E = finite diff")
END BLOCK

! Test 5: uniaxial stress diag(6, 0, 0) gives D = diag(1, -1/2, -1/2)
BLOCK
  TYPE(Rank2Tensor_) :: D
  REAL(DFP) :: a0(3, 3)

  a0 = 0.0_DFP
  a0(1, 1) = 6.0_DFP
  D = EquivalentStressDerivative(Rank2Tensor(a0, .TRUE.))
  a0 = 0.0_DFP
  a0(1, 1) = 1.0_DFP
  a0(2, 2) = -0.5_DFP
  a0(3, 3) = -0.5_DFP
  CALL OK(MAXVAL(ABS(D%T - a0)) .LT. tol, "equivalent: uniaxial D")
END BLOCK

! Test 6: zero equivalent stress gives a zero derivative, not NaN
BLOCK
  TYPE(Rank2Tensor_) :: D

  D = EquivalentStressDerivative(Rank2Tensor(5.0_DFP * Eye3, .TRUE.))
  CALL OK(ALL(D%T .EQ. 0.0_DFP), "equivalent: hydrostatic D = 0")
  CALL OK(D%isSym, "equivalent: hydrostatic isSym is true")
END BLOCK

END PROGRAM main
