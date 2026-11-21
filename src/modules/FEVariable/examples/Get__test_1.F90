!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Get the values of a FEVariable without allocation

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: Get_
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ii
REAL(DFP) :: v(24)

v = [(REAL(ii, DFP), ii=1, 24)]

! Test 1: scalar variables, output arrays larger than the data
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: r1(10), r2(5, 5)
  INTEGER(I4B) :: tsize, nrow, ncol

  obj = QuadratureVariable(v(1:4), Scalar, Space)
  CALL Get_(obj, Scalar, Space, r1, tsize)
  CALL OK(tsize .EQ. 4, "scalar space: tsize")
  CALL OK(ALL(r1(1:tsize) .EQ. v(1:4)), "scalar space")

  obj = QuadratureVariable(v(1:2), Scalar, Time)
  CALL Get_(obj, Scalar, Time, r1, tsize)
  CALL OK(ALL(r1(1:tsize) .EQ. v(1:2)), "scalar time")

  obj = QuadratureVariable(RESHAPE(v(1:6), [3, 2]), Scalar, SpaceTime)
  CALL Get_(obj, Scalar, SpaceTime, r2, nrow, ncol)
  CALL OK(nrow .EQ. 3 .AND. ncol .EQ. 2, "scalar spaceTime: nrow, ncol")
  CALL OK(ALL(r2(1:nrow, 1:ncol) .EQ. RESHAPE(v(1:6), [3, 2])), &
          "scalar spaceTime")
END BLOCK

! Test 2: vector variables
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: r1(10), r2(5, 5), r3(4, 4, 4)
  INTEGER(I4B) :: tsize, nrow, ncol, d1, d2, d3

  obj = QuadratureVariable(v(1:3), Vector, Constant)
  CALL Get_(obj, Vector, Constant, r1, tsize)
  CALL OK(tsize .EQ. 3, "vector constant: tsize")
  CALL OK(ALL(r1(1:tsize) .EQ. v(1:3)), "vector constant")

  obj = QuadratureVariable(RESHAPE(v(1:6), [2, 3]), Vector, Space)
  CALL Get_(obj, Vector, Space, r2, nrow, ncol)
  CALL OK(ALL(r2(1:nrow, 1:ncol) .EQ. RESHAPE(v(1:6), [2, 3])), &
          "vector space")

  obj = QuadratureVariable(RESHAPE(v(1:6), [3, 2]), Vector, Time)
  CALL Get_(obj, Vector, Time, r2, nrow, ncol)
  CALL OK(ALL(r2(1:nrow, 1:ncol) .EQ. RESHAPE(v(1:6), [3, 2])), &
          "vector time")

  obj = QuadratureVariable(RESHAPE(v(1:12), [3, 2, 2]), Vector, SpaceTime)
  CALL Get_(obj, Vector, SpaceTime, r3, d1, d2, d3)
  CALL OK(ALL([d1, d2, d3] .EQ. [3, 2, 2]), "vector spaceTime: dims")
  CALL OK(ALL(r3(1:d1, 1:d2, 1:d3) .EQ. RESHAPE(v(1:12), [3, 2, 2])), &
          "vector spaceTime")
END BLOCK

! Test 3: matrix variables
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: r2(5, 5), r3(4, 4, 4), r4(3, 3, 3, 3)
  INTEGER(I4B) :: nrow, ncol, d1, d2, d3, d4

  obj = QuadratureVariable(RESHAPE(v(1:6), [2, 3]), Matrix, Constant)
  CALL Get_(obj, Matrix, Constant, r2, nrow, ncol)
  CALL OK(ALL(r2(1:nrow, 1:ncol) .EQ. RESHAPE(v(1:6), [2, 3])), &
          "matrix constant")

  obj = QuadratureVariable(RESHAPE(v(1:12), [2, 2, 3]), Matrix, Space)
  CALL Get_(obj, Matrix, Space, r3, d1, d2, d3)
  CALL OK(ALL(r3(1:d1, 1:d2, 1:d3) .EQ. RESHAPE(v(1:12), [2, 2, 3])), &
          "matrix space")

  obj = QuadratureVariable(RESHAPE(v(1:8), [2, 2, 2]), Matrix, Time)
  CALL Get_(obj, Matrix, Time, r3, d1, d2, d3)
  CALL OK(ALL(r3(1:d1, 1:d2, 1:d3) .EQ. RESHAPE(v(1:8), [2, 2, 2])), &
          "matrix time")

  obj = QuadratureVariable(RESHAPE(v, [2, 2, 3, 2]), Matrix, SpaceTime)
  CALL Get_(obj, Matrix, SpaceTime, r4, d1, d2, d3, d4)
  CALL OK(ALL([d1, d2, d3, d4] .EQ. [2, 2, 3, 2]), &
          "matrix spaceTime: dims")
  CALL OK(ALL(r4(1:d1, 1:d2, 1:d3, 1:d4) .EQ. RESHAPE(v, [2, 2, 3, 2])), &
          "matrix spaceTime")
END BLOCK

END PROGRAM main
