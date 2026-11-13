!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Lode angle from J2 and J3, and from a tensor

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: CosineLode
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE GlobalData, ONLY: SineLode
USE Rank2Tensor_Method, ONLY: Dev
USE Rank2Tensor_Method, ONLY: LodeAngle
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: pi = 4.0_DFP * ATAN(1.0_DFP)

! Test 1: from J2 and J3, uniaxial tension (J2 = 3, J3 = 2)
BLOCK
  CALL OK(ABS(LodeAngle(CosineLode, 3.0_DFP, 2.0_DFP)) .LT. tol, &
          "J2, J3 uniaxial: cos form = 0")
  CALL OK(ABS(LodeAngle(SineLode, 3.0_DFP, 2.0_DFP) + pi / 6.0_DFP) &
          .LT. tol, "J2, J3 uniaxial: sin form = -pi/6")
END BLOCK

! Test 2: from J2 and J3, pure shear (J3 = 0) and zero J2
BLOCK
  CALL OK(ABS(LodeAngle(CosineLode, 9.0_DFP, 0.0_DFP) - pi / 6.0_DFP) &
          .LT. tol, "J2, J3 shear: cos form = pi/6")
  CALL OK(ABS(LodeAngle(SineLode, 9.0_DFP, 0.0_DFP)) .LT. tol, &
          "J2, J3 shear: sin form = 0")
  CALL OK(ABS(LodeAngle(CosineLode, 0.0_DFP, 0.0_DFP)) .LT. tol, &
          "J2 = 0 gives 0")
  CALL OK(ABS(LodeAngle(99, 9.0_DFP, 0.0_DFP) - pi / 6.0_DFP) .LT. tol, &
          "unknown lodeType is cos form")
END BLOCK

! Test 3: from a tensor, uniaxial tension with a pressure
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: mat(3, 3)

  mat = 5.0_DFP * Eye3
  mat(1, 1) = mat(1, 1) + 3.0_DFP
  obj = Rank2Tensor(mat, .TRUE.)
  CALL OK(ABS(LodeAngle(obj, CosineLode)) .LT. 1.0E-6_DFP, &
          "tensor uniaxial: cos form = 0")
  CALL OK(ABS(LodeAngle(obj, SineLode) + pi / 6.0_DFP) .LT. 1.0E-6_DFP, &
          "tensor uniaxial: sin form = -pi/6")
  obj = Dev(obj)
  CALL OK(ABS(LodeAngle(obj, SineLode, isDeviatoric=.TRUE.) &
              + pi / 6.0_DFP) .LT. 1.0E-6_DFP, &
          "tensor deviatoric: sin form = -pi/6")
END BLOCK

! Test 4: from a tensor, pure shear and an isotropic tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: mat(3, 3)

  mat = 0.0_DFP
  mat(1, 2) = 3.0_DFP
  mat(2, 1) = 3.0_DFP
  obj = Rank2Tensor(mat, .TRUE.)
  CALL OK(ABS(LodeAngle(obj, CosineLode) - pi / 6.0_DFP) .LT. tol, &
          "tensor shear: cos form = pi/6")
  obj = Rank2Tensor(2.0_DFP * Eye3, .TRUE.)
  CALL OK(ABS(LodeAngle(obj, CosineLode)) .LT. tol, &
          "tensor isotropic: 0")
END BLOCK

END PROGRAM main
