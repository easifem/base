!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Create nodal variables of every rank and variable type

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
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ii
REAL(DFP) :: v(24)

v = [(REAL(ii, DFP), ii=1, 24)]

! Test 1: scalar, constant
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(5.0_DFP, Scalar, Constant)
  CALL OK(IsHeader(obj, fevaropt%scalar, fevaropt%constant, [1]), &
          "scalar constant: header")
  CALL OK(obj%val(1) .EQ. 5.0_DFP, "scalar constant: val")
END BLOCK

! Test 2: scalar, space (values and size)
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(v(1:4), Scalar, Space)
  CALL OK(IsHeader(obj, fevaropt%scalar, fevaropt%space, [4]), &
          "scalar space: header")
  CALL OK(ALL(obj%val(1:4) .EQ. v(1:4)), "scalar space: val")

  obj = NodalVariable(tsize=3, rank=Scalar, vartype=Space)
  CALL OK(IsHeader(obj, fevaropt%scalar, fevaropt%space, [3]), &
          "scalar space tsize: header")
  CALL OK(ALL(obj%val(1:3) .EQ. 0.0_DFP), "scalar space tsize: zero")
END BLOCK

! Test 3: scalar, time (values and size)
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(v(1:2), Scalar, Time)
  CALL OK(IsHeader(obj, fevaropt%scalar, fevaropt%time, [2]), &
          "scalar time: header")
  CALL OK(ALL(obj%val(1:2) .EQ. v(1:2)), "scalar time: val")

  obj = NodalVariable(tsize=2, rank=Scalar, vartype=Time)
  CALL OK(IsHeader(obj, fevaropt%scalar, fevaropt%time, [2]), &
          "scalar time tsize: header")
  CALL OK(ALL(obj%val(1:2) .EQ. 0.0_DFP), "scalar time tsize: zero")
END BLOCK

! Test 4: scalar, spaceTime (2D values, flat values with s, sizes)
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(RESHAPE(v(1:6), [3, 2]), Scalar, SpaceTime)
  CALL OK(IsHeader(obj, fevaropt%scalar, fevaropt%spacetime, [3, 2]), &
          "scalar spaceTime: header")
  CALL OK(ALL(obj%val(1:6) .EQ. v(1:6)), "scalar spaceTime: val")

  obj = NodalVariable(v(1:6), Scalar, SpaceTime, [2, 3])
  CALL OK(IsHeader(obj, fevaropt%scalar, fevaropt%spacetime, [2, 3]), &
          "scalar spaceTime s: header")
  CALL OK(ALL(obj%val(1:6) .EQ. v(1:6)), "scalar spaceTime s: val")

  obj = NodalVariable(nrow=2, ncol=2, rank=Scalar, vartype=SpaceTime)
  CALL OK(IsHeader(obj, fevaropt%scalar, fevaropt%spacetime, [2, 2]), &
          "scalar spaceTime sizes: header")
END BLOCK

! Test 5: vector, constant (values and size)
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(v(1:3), Vector, Constant)
  CALL OK(IsHeader(obj, fevaropt%vector, fevaropt%constant, [3]), &
          "vector constant: header")
  CALL OK(ALL(obj%val(1:3) .EQ. v(1:3)), "vector constant: val")

  obj = NodalVariable(tsize=2, rank=Vector, vartype=Constant)
  CALL OK(IsHeader(obj, fevaropt%vector, fevaropt%constant, [2]), &
          "vector constant tsize: header")
END BLOCK

! Test 6: vector, space (2D values, flat values with s, sizes)
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(RESHAPE(v(1:6), [2, 3]), Vector, Space)
  CALL OK(IsHeader(obj, fevaropt%vector, fevaropt%space, [2, 3]), &
          "vector space: header")
  CALL OK(ALL(obj%val(1:6) .EQ. v(1:6)), "vector space: val")

  obj = NodalVariable(v(1:6), Vector, Space, [3, 2])
  CALL OK(IsHeader(obj, fevaropt%vector, fevaropt%space, [3, 2]), &
          "vector space s: header")

  obj = NodalVariable(nrow=3, ncol=4, rank=Vector, vartype=Space)
  CALL OK(IsHeader(obj, fevaropt%vector, fevaropt%space, [3, 4]), &
          "vector space sizes: header")
END BLOCK

! Test 7: vector, time (2D values, flat values with s, sizes)
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(RESHAPE(v(1:6), [3, 2]), Vector, Time)
  CALL OK(IsHeader(obj, fevaropt%vector, fevaropt%time, [3, 2]), &
          "vector time: header")
  CALL OK(ALL(obj%val(1:6) .EQ. v(1:6)), "vector time: val")

  obj = NodalVariable(v(1:6), Vector, Time, [2, 3])
  CALL OK(IsHeader(obj, fevaropt%vector, fevaropt%time, [2, 3]), &
          "vector time s: header")

  obj = NodalVariable(nrow=3, ncol=2, rank=Vector, vartype=Time)
  CALL OK(IsHeader(obj, fevaropt%vector, fevaropt%time, [3, 2]), &
          "vector time sizes: header")
END BLOCK

! Test 8: vector, spaceTime (3D values, flat values with s, sizes)
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(RESHAPE(v(1:12), [3, 2, 2]), Vector, SpaceTime)
  CALL OK(IsHeader(obj, fevaropt%vector, fevaropt%spacetime, &
                   [3, 2, 2]), "vector spaceTime: header")
  CALL OK(ALL(obj%val(1:12) .EQ. v(1:12)), "vector spaceTime: val")

  obj = NodalVariable(v(1:12), Vector, SpaceTime, [2, 3, 2])
  CALL OK(IsHeader(obj, fevaropt%vector, fevaropt%spacetime, &
                   [2, 3, 2]), "vector spaceTime s: header")

  obj = NodalVariable(dim1=2, dim2=2, dim3=3, rank=Vector, vartype=SpaceTime)
  CALL OK(IsHeader(obj, fevaropt%vector, fevaropt%spacetime, &
                   [2, 2, 3]), "vector spaceTime sizes: header")
END BLOCK

! Test 9: matrix, constant (2D values, flat values with s, sizes)
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(RESHAPE(v(1:4), [2, 2]), Matrix, Constant)
  CALL OK(IsHeader(obj, fevaropt%matrix, fevaropt%constant, [2, 2]), &
          "matrix constant: header")
  CALL OK(ALL(obj%val(1:4) .EQ. v(1:4)), "matrix constant: val")

  obj = NodalVariable(v(1:6), Matrix, Constant, [2, 3])
  CALL OK(IsHeader(obj, fevaropt%matrix, fevaropt%constant, [2, 3]), &
          "matrix constant s: header")

  obj = NodalVariable(nrow=3, ncol=3, rank=Matrix, vartype=Constant)
  CALL OK(IsHeader(obj, fevaropt%matrix, fevaropt%constant, [3, 3]), &
          "matrix constant sizes: header")
END BLOCK

! Test 10: matrix, space (3D values, flat values with s, sizes)
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(RESHAPE(v(1:12), [2, 2, 3]), Matrix, Space)
  CALL OK(IsHeader(obj, fevaropt%matrix, fevaropt%space, [2, 2, 3]), &
          "matrix space: header")
  CALL OK(ALL(obj%val(1:12) .EQ. v(1:12)), "matrix space: val")

  obj = NodalVariable(v(1:12), Matrix, Space, [3, 2, 2])
  CALL OK(IsHeader(obj, fevaropt%matrix, fevaropt%space, [3, 2, 2]), &
          "matrix space s: header")

  obj = NodalVariable(dim1=2, dim2=3, dim3=4, rank=Matrix, vartype=Space)
  CALL OK(IsHeader(obj, fevaropt%matrix, fevaropt%space, [2, 3, 4]), &
          "matrix space sizes: header")
END BLOCK

! Test 11: matrix, time (3D values, flat values with s, sizes)
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(RESHAPE(v(1:8), [2, 2, 2]), Matrix, Time)
  CALL OK(IsHeader(obj, fevaropt%matrix, fevaropt%time, [2, 2, 2]), &
          "matrix time: header")
  CALL OK(ALL(obj%val(1:8) .EQ. v(1:8)), "matrix time: val")

  obj = NodalVariable(v(1:8), Matrix, Time, [2, 2, 2])
  CALL OK(IsHeader(obj, fevaropt%matrix, fevaropt%time, [2, 2, 2]), &
          "matrix time s: header")

  obj = NodalVariable(dim1=3, dim2=3, dim3=2, rank=Matrix, vartype=Time)
  CALL OK(IsHeader(obj, fevaropt%matrix, fevaropt%time, [3, 3, 2]), &
          "matrix time sizes: header")
END BLOCK

! Test 12: matrix, spaceTime (4D values, flat values with s, sizes)
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(RESHAPE(v(1:24), [2, 2, 3, 2]), Matrix, SpaceTime)
  CALL OK(IsHeader(obj, fevaropt%matrix, fevaropt%spacetime, &
                   [2, 2, 3, 2]), "matrix spaceTime: header")
  CALL OK(ALL(obj%val(1:24) .EQ. v(1:24)), "matrix spaceTime: val")

  obj = NodalVariable(v(1:24), Matrix, SpaceTime, [3, 2, 2, 2])
  CALL OK(IsHeader(obj, fevaropt%matrix, fevaropt%spacetime, &
                   [3, 2, 2, 2]), "matrix spaceTime s: header")
  CALL OK(ALL(obj%val(1:24) .EQ. v(1:24)), "matrix spaceTime s: val")

  obj = NodalVariable(dim1=2, dim2=2, dim3=2, dim4=2, rank=Matrix, &
                      vartype=SpaceTime)
  CALL OK(IsHeader(obj, fevaropt%matrix, fevaropt%spacetime, &
                   [2, 2, 2, 2]), "matrix spaceTime sizes: header")
  CALL OK(ALL(obj%val(1:16) .EQ. 0.0_DFP), &
          "matrix spaceTime sizes: zero")
END BLOCK

CONTAINS

!> True if obj is initiated as a nodal variable with the given
! rank, variable type and shape s, and len = PRODUCT(s)
FUNCTION IsHeader(obj, rank, vartype, s) RESULT(ans)
  TYPE(FEVariable_), INTENT(IN) :: obj
  !! finite element variable
  INTEGER(I4B), INTENT(IN) :: rank
  !! expected rank
  INTEGER(I4B), INTENT(IN) :: vartype
  !! expected variable type
  INTEGER(I4B), INTENT(IN) :: s(:)
  !! expected shape
  LOGICAL(LGT) :: ans
  !! true if all components match

  ans = obj%isInit .AND. (obj%defineOn .EQ. fevaropt%nodal) .AND. &
        (obj%rank .EQ. rank) .AND. (obj%varType .EQ. vartype) .AND. &
        (obj%tshape .EQ. SIZE(s)) .AND. &
        ALL(obj%s(1:SIZE(s)) .EQ. s) .AND. (obj%len .EQ. PRODUCT(s))
END FUNCTION IsHeader

END PROGRAM main
