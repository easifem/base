!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Values of quadrature variables from flat data and from sizes
!
! Test 1 checks the header of every overload. This test checks the
! values: flat data with a shape `s` is copied in column major order,
! extra data beyond `PRODUCT(s)` is ignored, and the size-only
! constructors set every value to zero.

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ii
REAL(DFP) :: v(24)

v = [(REAL(ii, DFP), ii=1, 24)]

! Test 1: vector, flat values with s
BLOCK
  TYPE(FEVariable_) :: obj

  obj = QuadratureVariable(v(1:6), Vector, Space, [3, 2])
  CALL OK(ALL(obj%val(1:6) .EQ. v(1:6)), "vector space s: val")

  obj = QuadratureVariable(v(1:6), Vector, Time, [2, 3])
  CALL OK(ALL(obj%val(1:6) .EQ. v(1:6)), "vector time s: val")

  obj = QuadratureVariable(v(1:12), Vector, SpaceTime, [2, 3, 2])
  CALL OK(ALL(obj%val(1:12) .EQ. v(1:12)), "vector spaceTime s: val")
END BLOCK

! Test 2: matrix, flat values with s
BLOCK
  TYPE(FEVariable_) :: obj

  obj = QuadratureVariable(v(1:6), Matrix, Constant, [2, 3])
  CALL OK(ALL(obj%val(1:6) .EQ. v(1:6)), "matrix constant s: val")

  obj = QuadratureVariable(v(1:12), Matrix, Space, [3, 2, 2])
  CALL OK(ALL(obj%val(1:12) .EQ. v(1:12)), "matrix space s: val")

  obj = QuadratureVariable(v(1:8), Matrix, Time, [2, 2, 2])
  CALL OK(ALL(obj%val(1:8) .EQ. v(1:8)), "matrix time s: val")
END BLOCK

! Test 3: flat data longer than PRODUCT(s) uses only the first values
BLOCK
  TYPE(FEVariable_) :: obj

  obj = QuadratureVariable(v(1:10), Scalar, SpaceTime, [2, 3])
  CALL OK(obj%len .EQ. 6, "scalar spaceTime long data: len")
  CALL OK(ALL(obj%val(1:6) .EQ. v(1:6)), &
          "scalar spaceTime long data: val")

  obj = QuadratureVariable(v(1:24), Matrix, SpaceTime, [2, 2, 2, 2])
  CALL OK(obj%len .EQ. 16, "matrix spaceTime long data: len")
  CALL OK(ALL(obj%val(1:16) .EQ. v(1:16)), &
          "matrix spaceTime long data: val")
END BLOCK

! Test 4: size-only constructors set all values to zero
BLOCK
  TYPE(FEVariable_) :: obj

  obj = QuadratureVariable(nrow=2, ncol=3, rank=Scalar, vartype=SpaceTime)
  CALL OK(IsZero(obj, 6), "scalar spaceTime sizes: zero")

  obj = QuadratureVariable(tsize=3, rank=Vector, vartype=Constant)
  CALL OK(IsZero(obj, 3), "vector constant tsize: zero")

  obj = QuadratureVariable(nrow=3, ncol=4, rank=Vector, vartype=Space)
  CALL OK(IsZero(obj, 12), "vector space sizes: zero")

  obj = QuadratureVariable(nrow=3, ncol=2, rank=Vector, vartype=Time)
  CALL OK(IsZero(obj, 6), "vector time sizes: zero")

  obj = QuadratureVariable(dim1=2, dim2=2, dim3=3, rank=Vector, &
                           vartype=SpaceTime)
  CALL OK(IsZero(obj, 12), "vector spaceTime sizes: zero")

  obj = QuadratureVariable(nrow=3, ncol=3, rank=Matrix, vartype=Constant)
  CALL OK(IsZero(obj, 9), "matrix constant sizes: zero")

  obj = QuadratureVariable(dim1=2, dim2=3, dim3=4, rank=Matrix, &
                           vartype=Space)
  CALL OK(IsZero(obj, 24), "matrix space sizes: zero")

  obj = QuadratureVariable(dim1=3, dim2=3, dim3=2, rank=Matrix, &
                           vartype=Time)
  CALL OK(IsZero(obj, 18), "matrix time sizes: zero")
END BLOCK

! Test 5: a new value replaces the old one in the same variable
BLOCK
  TYPE(FEVariable_) :: obj

  obj = QuadratureVariable(v(1:12), Vector, SpaceTime, [2, 3, 2])
  obj = QuadratureVariable(tsize=2, rank=Scalar, vartype=Space)
  CALL OK(obj%rank .EQ. fevaropt%scalar, "replace: rank")
  CALL OK(obj%varType .EQ. fevaropt%space, "replace: varType")
  CALL OK(obj%tshape .EQ. 1, "replace: tshape")
  CALL OK(IsZero(obj, 2), "replace: zero")
END BLOCK

CONTAINS

!> True if obj has len = n, capacity >= n and val(1:n) = 0
FUNCTION IsZero(obj, n) RESULT(ans)
  TYPE(FEVariable_), INTENT(IN) :: obj
  !! finite element variable
  INTEGER(I4B), INTENT(IN) :: n
  !! expected length
  LOGICAL(LGT) :: ans
  !! true if len, capacity and values match

  ans = (obj%len .EQ. n) .AND. (obj%capacity .GE. n) .AND. &
        ALL(obj%val(1:n) .EQ. 0.0_DFP)
END FUNCTION IsZero

END PROGRAM main
