!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Isotropic and deviatoric parts of a tensor

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: Dev
USE Rank2Tensor_Method, ONLY: Deviatoric
USE Rank2Tensor_Method, ONLY: Iso
USE Rank2Tensor_Method, ONLY: Isotropic
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: Trace
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.0_DFP, &
                                            -1.0_DFP, 3.0_DFP, 1.0_DFP, &
                                            0.0_DFP, 1.0_DFP, 4.0_DFP], &
                                           [3, 3])

! Test 1: Isotropic part is tr(A)/3 I and is symmetric
BLOCK
  TYPE(Rank2Tensor_) :: obj, isoObj

  obj = Rank2Tensor(A0)
  isoObj = Isotropic(obj)
  CALL OK(MAXVAL(ABS(isoObj%T - 3.0_DFP * Eye3)) .LT. tol, &
          "Isotropic = 3 I")
  CALL OK(isoObj%isSym, "Isotropic: symmetric")
END BLOCK

! Test 2: Deviatoric part is A - tr(A)/3 I and is traceless
BLOCK
  TYPE(Rank2Tensor_) :: obj, devObj

  obj = Rank2Tensor(A0)
  devObj = Deviatoric(obj)
  CALL OK(MAXVAL(ABS(devObj%T - (A0 - 3.0_DFP * Eye3))) .LT. tol, &
          "Deviatoric = A - 3 I")
  CALL OK(ABS(Trace(devObj)) .LT. tol, "Deviatoric: trace is 0")
  CALL OK(.NOT. devObj%isSym, "Deviatoric: keeps isSym")
END BLOCK

! Test 3: A = Iso(A) + Dev(A)
BLOCK
  TYPE(Rank2Tensor_) :: obj, isoObj, devObj

  obj = Rank2Tensor(A0)
  isoObj = Iso(obj)
  devObj = Dev(obj)
  CALL OK(MAXVAL(ABS(isoObj%T + devObj%T - A0)) .LT. tol, &
          "A = Iso(A) + Dev(A)")
END BLOCK

! Test 4: Iso is the same as Isotropic, Dev is the same as Deviatoric
BLOCK
  TYPE(Rank2Tensor_) :: obj, t1, t2

  obj = Rank2Tensor(A0)
  t1 = Iso(obj)
  t2 = Isotropic(obj)
  CALL OK(MAXVAL(ABS(t1%T - t2%T)) .LT. tol, "Iso == Isotropic")
  t1 = Dev(obj)
  t2 = Deviatoric(obj)
  CALL OK(MAXVAL(ABS(t1%T - t2%T)) .LT. tol, "Dev == Deviatoric")
END BLOCK

END PROGRAM main
