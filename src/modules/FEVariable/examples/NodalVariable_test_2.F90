!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Values of nodal variables built from flat data or from sizes
!
! Checks the values stored by the overloads that take flat data with a
! shape `s`, and the zero values of the overloads that take only sizes.
! Test 1 of NodalVariable checks only the header for most of them.

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ii
REAL(DFP) :: v(24)

v = [(REAL(ii, DFP), ii=1, 24)]

! Test 1: vector, flat values with s are kept in column major order
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(v(1:6), Vector, Space, [3, 2])
  CALL OK(ALL(obj%val(1:6) .EQ. v(1:6)), "vector space s: val")

  obj = NodalVariable(v(1:6), Vector, Time, [2, 3])
  CALL OK(ALL(obj%val(1:6) .EQ. v(1:6)), "vector time s: val")

  obj = NodalVariable(v(1:12), Vector, SpaceTime, [2, 3, 2])
  CALL OK(ALL(obj%val(1:12) .EQ. v(1:12)), "vector spaceTime s: val")
END BLOCK

! Test 2: matrix, flat values with s are kept in column major order
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(v(1:6), Matrix, Constant, [2, 3])
  CALL OK(ALL(obj%val(1:6) .EQ. v(1:6)), "matrix constant s: val")

  obj = NodalVariable(v(1:12), Matrix, Space, [3, 2, 2])
  CALL OK(ALL(obj%val(1:12) .EQ. v(1:12)), "matrix space s: val")

  obj = NodalVariable(v(1:8), Matrix, Time, [2, 2, 2])
  CALL OK(ALL(obj%val(1:8) .EQ. v(1:8)), "matrix time s: val")
END BLOCK

! Test 3: flat values longer than PRODUCT(s), only the first ones are used
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(v(1:10), Scalar, SpaceTime, [2, 2])
  CALL OK(obj%len .EQ. 4, "scalar spaceTime long val: len")
  CALL OK(ALL(obj%val(1:4) .EQ. v(1:4)), "scalar spaceTime long val: val")
END BLOCK

! Test 4: element order of 2D values, val(ii, jj) goes to ii + (jj-1)*s(1)
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: a(2, 3)

  a = RESHAPE(v(1:6), [2, 3])
  obj = NodalVariable(a, Vector, Space)
  CALL OK(obj%val(5) .EQ. a(1, 3), "vector space: val(5) = a(1, 3)")
  CALL OK(obj%val(4) .EQ. a(2, 2), "vector space: val(4) = a(2, 2)")
END BLOCK

! Test 5: overloads with sizes only give zero values (scalar, vector)
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(nrow=2, ncol=3, rank=Scalar, vartype=SpaceTime)
  CALL OK(ALL(obj%val(1:6) .EQ. 0.0_DFP), "scalar spaceTime sizes: zero")

  obj = NodalVariable(tsize=3, rank=Vector, vartype=Constant)
  CALL OK(ALL(obj%val(1:3) .EQ. 0.0_DFP), "vector constant tsize: zero")

  obj = NodalVariable(nrow=3, ncol=4, rank=Vector, vartype=Space)
  CALL OK(ALL(obj%val(1:12) .EQ. 0.0_DFP), "vector space sizes: zero")

  obj = NodalVariable(nrow=3, ncol=2, rank=Vector, vartype=Time)
  CALL OK(ALL(obj%val(1:6) .EQ. 0.0_DFP), "vector time sizes: zero")

  obj = NodalVariable(dim1=2, dim2=2, dim3=3, rank=Vector, &
                      vartype=SpaceTime)
  CALL OK(ALL(obj%val(1:12) .EQ. 0.0_DFP), &
          "vector spaceTime sizes: zero")
END BLOCK

! Test 6: overloads with sizes only give zero values (matrix)
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(nrow=3, ncol=3, rank=Matrix, vartype=Constant)
  CALL OK(ALL(obj%val(1:9) .EQ. 0.0_DFP), "matrix constant sizes: zero")

  obj = NodalVariable(dim1=2, dim2=3, dim3=4, rank=Matrix, vartype=Space)
  CALL OK(ALL(obj%val(1:24) .EQ. 0.0_DFP), "matrix space sizes: zero")

  obj = NodalVariable(dim1=3, dim2=3, dim3=2, rank=Matrix, vartype=Time)
  CALL OK(ALL(obj%val(1:18) .EQ. 0.0_DFP), "matrix time sizes: zero")
END BLOCK

! Test 7: assigning a zero nodal variable to an object that holds data
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(v(1:8), Scalar, Space)
  obj = NodalVariable(tsize=4, rank=Scalar, vartype=Time)
  CALL OK(obj%len .EQ. 4, "reassign scalar time tsize: len")
  CALL OK(ALL(obj%val(1:4) .EQ. 0.0_DFP), &
          "reassign scalar time tsize: zero")
END BLOCK

! Test 8: empty data, zero nodes in space
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(tsize=0, rank=Scalar, vartype=Space)
  CALL OK(obj%isInit .AND. (obj%len .EQ. 0) .AND. (obj%s(1) .EQ. 0), &
          "scalar space tsize 0: header")
END BLOCK

END PROGRAM main
