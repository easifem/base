!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set the values of a FEVariable with scale and addContribution

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
USE FEVariable_Method, ONLY: Set
USE FEVariable_Method, ONLY: SHAPE
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ii
REAL(DFP) :: v(24)

! Each test sets val with scale = 2 (obj = 2 val), then adds val with
! scale = 1 and addContribution = .TRUE. (obj = 3 val).

v = [(REAL(ii, DFP), ii=1, 24)]

! Test 1: scalar constant
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val

  val = 5.0_DFP
  obj = NodalVariable(0.0_DFP, Scalar, Constant)
  CALL Set(obj=obj, val=val, rank=Scalar, vartype=Constant, scale=2.0_DFP, &
           addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%constant, [1], 2.0_DFP * [val]), &
          "scalar constant: set with scale")
  CALL Set(obj=obj, val=val, rank=Scalar, vartype=Constant, scale=1.0_DFP, &
           addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%constant, [1], 3.0_DFP * [val]), &
          "scalar constant: add contribution")
END BLOCK

! Test 2: scalar space
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val(4)

  val = v(1:4)
  obj = NodalVariable(tsize=4, rank=Scalar, vartype=Space)
  CALL Set(obj=obj, val=val, rank=Scalar, vartype=Space, scale=2.0_DFP, &
           addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%space, [4], 2.0_DFP * val), &
          "scalar space: set with scale")
  CALL Set(obj=obj, val=val, rank=Scalar, vartype=Space, scale=1.0_DFP, &
           addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%space, [4], 3.0_DFP * val), &
          "scalar space: add contribution")
END BLOCK

! Test 3: scalar time
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val(2)

  val = v(1:2)
  obj = NodalVariable(tsize=2, rank=Scalar, vartype=Time)
  CALL Set(obj=obj, val=val, rank=Scalar, vartype=Time, scale=2.0_DFP, &
           addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%time, [2], 2.0_DFP * val), &
          "scalar time: set with scale")
  CALL Set(obj=obj, val=val, rank=Scalar, vartype=Time, scale=1.0_DFP, &
           addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%time, [2], 3.0_DFP * val), &
          "scalar time: add contribution")
END BLOCK

! Test 4: scalar spaceTime
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val(3, 2)

  val = RESHAPE(v(1:6), [3, 2])
  obj = NodalVariable(nrow=3, ncol=2, rank=Scalar, &
                      vartype=SpaceTime)
  CALL Set(obj=obj, val=val, rank=Scalar, vartype=SpaceTime, scale=2.0_DFP, &
           addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%spacetime, [3, 2], 2.0_DFP * v(1:6)), &
          "scalar spaceTime: set with scale")
  CALL Set(obj=obj, val=val, rank=Scalar, vartype=SpaceTime, scale=1.0_DFP, &
           addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%spacetime, [3, 2], 3.0_DFP * v(1:6)), &
          "scalar spaceTime: add contribution")
END BLOCK

! Test 5: vector constant
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val(3)

  val = v(1:3)
  obj = NodalVariable(tsize=3, rank=Vector, vartype=Constant)
  CALL Set(obj=obj, val=val, rank=Vector, vartype=Constant, scale=2.0_DFP, &
           addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%constant, [3], 2.0_DFP * val), &
          "vector constant: set with scale")
  CALL Set(obj=obj, val=val, rank=Vector, vartype=Constant, scale=1.0_DFP, &
           addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%constant, [3], 3.0_DFP * val), &
          "vector constant: add contribution")
END BLOCK

! Test 6: vector space
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val(2, 3)

  val = RESHAPE(v(1:6), [2, 3])
  obj = NodalVariable(nrow=2, ncol=3, rank=Vector, vartype=Space)
  CALL Set(obj=obj, val=val, rank=Vector, vartype=Space, scale=2.0_DFP, &
           addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%space, [2, 3], 2.0_DFP * v(1:6)), &
          "vector space: set with scale")
  CALL Set(obj=obj, val=val, rank=Vector, vartype=Space, scale=1.0_DFP, &
           addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%space, [2, 3], 3.0_DFP * v(1:6)), &
          "vector space: add contribution")
END BLOCK

! Test 7: vector time
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val(3, 2)

  val = RESHAPE(v(1:6), [3, 2])
  obj = NodalVariable(nrow=3, ncol=2, rank=Vector, vartype=Time)
  CALL Set(obj=obj, val=val, rank=Vector, vartype=Time, scale=2.0_DFP, &
           addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%time, [3, 2], 2.0_DFP * v(1:6)), &
          "vector time: set with scale")
  CALL Set(obj=obj, val=val, rank=Vector, vartype=Time, scale=1.0_DFP, &
           addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%time, [3, 2], 3.0_DFP * v(1:6)), &
          "vector time: add contribution")
END BLOCK

! Test 8: vector spaceTime
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val(3, 2, 2)

  val = RESHAPE(v(1:12), [3, 2, 2])
  obj = NodalVariable(dim1=3, dim2=2, dim3=2, rank=Vector, &
                      vartype=SpaceTime)
  CALL Set(obj=obj, val=val, rank=Vector, vartype=SpaceTime, scale=2.0_DFP, &
           addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%spacetime, [3, 2, 2], 2.0_DFP * v(1:12)), &
          "vector spaceTime: set with scale")
  CALL Set(obj=obj, val=val, rank=Vector, vartype=SpaceTime, scale=1.0_DFP, &
           addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%spacetime, [3, 2, 2], 3.0_DFP * v(1:12)), &
          "vector spaceTime: add contribution")
END BLOCK

! Test 9: matrix constant
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val(2, 3)

  val = RESHAPE(v(1:6), [2, 3])
  obj = NodalVariable(nrow=2, ncol=3, rank=Matrix, &
                      vartype=Constant)
  CALL Set(obj=obj, val=val, rank=Matrix, vartype=Constant, scale=2.0_DFP, &
           addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%constant, [2, 3], 2.0_DFP * v(1:6)), &
          "matrix constant: set with scale")
  CALL Set(obj=obj, val=val, rank=Matrix, vartype=Constant, scale=1.0_DFP, &
           addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%constant, [2, 3], 3.0_DFP * v(1:6)), &
          "matrix constant: add contribution")
END BLOCK

! Test 10: matrix space
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val(2, 2, 3)

  val = RESHAPE(v(1:12), [2, 2, 3])
  obj = NodalVariable(dim1=2, dim2=2, dim3=3, rank=Matrix, &
                      vartype=Space)
  CALL Set(obj=obj, val=val, rank=Matrix, vartype=Space, scale=2.0_DFP, &
           addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%space, [2, 2, 3], 2.0_DFP * v(1:12)), &
          "matrix space: set with scale")
  CALL Set(obj=obj, val=val, rank=Matrix, vartype=Space, scale=1.0_DFP, &
           addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%space, [2, 2, 3], 3.0_DFP * v(1:12)), &
          "matrix space: add contribution")
END BLOCK

! Test 11: matrix time
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val(2, 2, 2)

  val = RESHAPE(v(1:8), [2, 2, 2])
  obj = NodalVariable(dim1=2, dim2=2, dim3=2, rank=Matrix, &
                      vartype=Time)
  CALL Set(obj=obj, val=val, rank=Matrix, vartype=Time, scale=2.0_DFP, &
           addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%time, [2, 2, 2], 2.0_DFP * v(1:8)), &
          "matrix time: set with scale")
  CALL Set(obj=obj, val=val, rank=Matrix, vartype=Time, scale=1.0_DFP, &
           addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%time, [2, 2, 2], 3.0_DFP * v(1:8)), &
          "matrix time: add contribution")
END BLOCK

! Test 12: matrix spaceTime
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val(2, 2, 3, 2)

  val = RESHAPE(v(1:24), [2, 2, 3, 2])
  obj = NodalVariable(dim1=2, dim2=2, dim3=3, dim4=2, &
                      rank=Matrix, vartype=SpaceTime)
  CALL Set(obj=obj, val=val, rank=Matrix, vartype=SpaceTime, scale=2.0_DFP, &
           addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%spacetime, [2, 2, 3, 2], 2.0_DFP * v(1:24)), &
          "matrix spaceTime: set with scale")
  CALL Set(obj=obj, val=val, rank=Matrix, vartype=SpaceTime, scale=1.0_DFP, &
           addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%spacetime, [2, 2, 3, 2], 3.0_DFP * v(1:24)), &
          "matrix spaceTime: add contribution")
END BLOCK

! Test 13: set fewer values than the capacity of obj
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(tsize=6, rank=Scalar, vartype=Space)
  CALL Set(obj=obj, val=v(1:4), rank=Scalar, vartype=Space, &
           scale=1.0_DFP, addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%space, [4], v(1:4)), &
          "smaller val: len = 4, s = [4]")
  CALL OK(obj%capacity .EQ. 6, "smaller val: capacity = 6 is kept")
END BLOCK

! Test 14: Set changes the variable type and the total shape
BLOCK
  TYPE(FEVariable_) :: obj
  INTEGER(I4B), ALLOCATABLE :: s(:)

  obj = NodalVariable(tsize=6, rank=Scalar, vartype=Space)
  CALL Set(obj=obj, val=RESHAPE(v(1:6), [2, 3]), rank=Scalar, &
           vartype=SpaceTime, scale=1.0_DFP, addContribution=.FALSE.)
  s = SHAPE(obj)
  CALL OK(obj%tshape .EQ. 2, "space to spaceTime: tshape = 2")
  CALL OK(SIZE(s) .EQ. 2, "space to spaceTime: SIZE(SHAPE) = 2")
  CALL OK(ALL(s .EQ. [2, 3]), "space to spaceTime: SHAPE = [2, 3]")

  CALL Set(obj=obj, val=1.0_DFP, rank=Scalar, vartype=Constant, &
           scale=1.0_DFP, addContribution=.FALSE.)
  CALL OK(obj%tshape .EQ. 1, "spaceTime to constant: tshape = 1")
  CALL OK(obj%varType .EQ. fevaropt%constant, &
          "spaceTime to constant: varType")
END BLOCK

CONTAINS

!> True if obj has variable type vartype, shape s, len = PRODUCT(s)
! and values ans
FUNCTION IsSet(obj, vartype, s, ans) RESULT(isok)
  TYPE(FEVariable_), INTENT(IN) :: obj
  !! finite element variable
  INTEGER(I4B), INTENT(IN) :: vartype
  !! expected variable type
  INTEGER(I4B), INTENT(IN) :: s(:)
  !! expected shape
  REAL(DFP), INTENT(IN) :: ans(:)
  !! expected values, SIZE(ans) = PRODUCT(s)
  LOGICAL(LGT) :: isok
  !! true if all match

  isok = (obj%varType .EQ. vartype) .AND. (obj%len .EQ. PRODUCT(s)) &
         .AND. ALL(obj%s(1:SIZE(s)) .EQ. s) &
         .AND. ALL(ABS(obj%val(1:obj%len) - ans) .LT. 1.0E-12_DFP)
END FUNCTION IsSet

END PROGRAM main
