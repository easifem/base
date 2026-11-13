!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Mean stress and von Mises equivalent stress of uniaxial,
!   hydrostatic and pure shear stress states

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: EquivalentStress
USE Rank2Tensor_Method, ONLY: MeanStress
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: uniaxial stress diag(6, 0, 0)
BLOCK
  TYPE(Rank2Tensor_) :: sig
  REAL(DFP) :: s0(3, 3)

  s0 = 0.0_DFP
  s0(1, 1) = 6.0_DFP
  sig = Rank2Tensor(s0, .TRUE.)
  CALL OK(ABS(MeanStress(sig) - 2.0_DFP) .LT. tol, "uniaxial: mean = 2")
  CALL OK(ABS(EquivalentStress(sig) - 6.0_DFP) .LT. tol, &
          "uniaxial: equivalent = 6")
END BLOCK

! Test 2: hydrostatic stress 3 I
BLOCK
  TYPE(Rank2Tensor_) :: sig

  sig = Rank2Tensor(3.0_DFP * Eye3, .TRUE.)
  CALL OK(ABS(MeanStress(sig) - 3.0_DFP) .LT. tol, &
          "hydrostatic: mean = 3")
  CALL OK(ABS(EquivalentStress(sig)) .LT. tol, &
          "hydrostatic: equivalent = 0")
END BLOCK

! Test 3: pure shear with sigma_12 = 4
BLOCK
  TYPE(Rank2Tensor_) :: sig
  REAL(DFP) :: s0(3, 3)

  s0 = 0.0_DFP
  s0(1, 2) = 4.0_DFP
  s0(2, 1) = 4.0_DFP
  sig = Rank2Tensor(s0, .TRUE.)
  CALL OK(ABS(MeanStress(sig)) .LT. tol, "pure shear: mean = 0")
  CALL OK(ABS(EquivalentStress(sig) - 4.0_DFP * SQRT(3.0_DFP)) .LT. tol, &
          "pure shear: equivalent = sqrt(3) tau")
END BLOCK

! Test 4: general stress, mean = tr/3 and equivalent = sqrt(3 J2)
BLOCK
  TYPE(Rank2Tensor_) :: sig
  REAL(DFP) :: s0(3, 3)

  s0 = RESHAPE([10.0_DFP, 2.0_DFP, 3.0_DFP, &
                2.0_DFP, 20.0_DFP, 4.0_DFP, &
                3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
  sig = Rank2Tensor(s0, .TRUE.)
  ! J2 = ((s1-s2)^2+(s2-s3)^2+(s3-s1)^2)/6 + s12^2+s23^2+s13^2 = 129
  CALL OK(ABS(MeanStress(sig) - 20.0_DFP) .LT. tol, "general: mean = 20")
  CALL OK(ABS(EquivalentStress(sig) - SQRT(387.0_DFP)) .LT. tol, &
          "general: equivalent = sqrt(387)")
END BLOCK

END PROGRAM main
