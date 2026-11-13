!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Haigh-Westergaard coordinates [xi, rho, theta] with the
!   cosine (default) and the sine Lode angle

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: CosineLode
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: SineLode
USE Rank2Tensor_Method, ONLY: HaighWestergaard
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: atol = 1.0E-6_DFP
REAL(DFP), PARAMETER :: pi = 4.0_DFP * ATAN(1.0_DFP)

! Test 1: uniaxial tension diag(6, 0, 0), default (cosine) Lode angle
BLOCK
  TYPE(Rank2Tensor_) :: sig
  REAL(DFP) :: s0(3, 3), hw(3)

  s0 = 0.0_DFP
  s0(1, 1) = 6.0_DFP
  sig = Rank2Tensor(s0, .TRUE.)
  hw = HaighWestergaard(sig)
  CALL OK(ABS(hw(1) - 6.0_DFP / SQRT(3.0_DFP)) .LT. tol, &
          "tension: xi = I1/sqrt(3)")
  CALL OK(ABS(hw(2) - SQRT(24.0_DFP)) .LT. tol, &
          "tension: rho = sqrt(2 J2)")
  CALL OK(ABS(hw(3)) .LT. atol, "tension: cosine theta = 0")
END BLOCK

! Test 2: uniaxial tension with explicit lodeType
BLOCK
  TYPE(Rank2Tensor_) :: sig
  REAL(DFP) :: s0(3, 3), hw(3)

  s0 = 0.0_DFP
  s0(1, 1) = 6.0_DFP
  sig = Rank2Tensor(s0, .TRUE.)
  hw = HaighWestergaard(sig, CosineLode)
  CALL OK(ABS(hw(3)) .LT. atol, "tension: CosineLode theta = 0")
  hw = HaighWestergaard(sig, SineLode)
  CALL OK(ABS(hw(3) + pi / 6.0_DFP) .LT. atol, &
          "tension: SineLode theta = -pi/6")
END BLOCK

! Test 3: uniaxial compression diag(-6, 0, 0)
BLOCK
  TYPE(Rank2Tensor_) :: sig
  REAL(DFP) :: s0(3, 3), hw(3)

  s0 = 0.0_DFP
  s0(1, 1) = -6.0_DFP
  sig = Rank2Tensor(s0, .TRUE.)
  hw = HaighWestergaard(sig)
  CALL OK(ABS(hw(1) + 6.0_DFP / SQRT(3.0_DFP)) .LT. tol, &
          "compression: xi < 0")
  CALL OK(ABS(hw(3) - pi / 3.0_DFP) .LT. atol, &
          "compression: cosine theta = pi/3")
  hw = HaighWestergaard(sig, SineLode)
  CALL OK(ABS(hw(3) - pi / 6.0_DFP) .LT. atol, &
          "compression: SineLode theta = pi/6")
END BLOCK

! Test 4: pure shear with sigma_12 = 4
BLOCK
  TYPE(Rank2Tensor_) :: sig
  REAL(DFP) :: s0(3, 3), hw(3)

  s0 = 0.0_DFP
  s0(1, 2) = 4.0_DFP
  s0(2, 1) = 4.0_DFP
  sig = Rank2Tensor(s0, .TRUE.)
  hw = HaighWestergaard(sig)
  CALL OK(ABS(hw(1)) .LT. tol, "shear: xi = 0")
  CALL OK(ABS(hw(2) - SQRT(32.0_DFP)) .LT. tol, "shear: rho = sqrt(32)")
  CALL OK(ABS(hw(3) - pi / 6.0_DFP) .LT. tol, &
          "shear: cosine theta = pi/6")
  hw = HaighWestergaard(sig, SineLode)
  CALL OK(ABS(hw(3)) .LT. tol, "shear: SineLode theta = 0")
END BLOCK

END PROGRAM main
