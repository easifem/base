!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Set on quadrature variables, change of variable type, scale

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
USE FEVariable_Method, ONLY: Set
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ii
REAL(DFP) :: v(12)

v = [(REAL(ii, DFP), ii=1, 12)]

! Test 1: vector quadrature variable, Set keeps rank, defineOn, capacity
BLOCK
  TYPE(FEVariable_) :: obj
  INTEGER(I4B) :: cap0

  obj = QuadratureVariable(nrow=3, ncol=4, rank=Vector, vartype=Space)
  cap0 = obj%capacity

  CALL Set(obj=obj, val=RESHAPE(v(1:6), [3, 2]), rank=Vector, &
           vartype=Space, scale=1.0_DFP, addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%space, 2, [3, 2], v(1:6)), &
          "vector space: values, len, s, tshape")
  CALL OK(obj%defineOn .EQ. fevaropt%quadrature, &
          "vector space: defineOn is quadrature")
  CALL OK(obj%rank .EQ. fevaropt%vector, "vector space: rank is vector")
  CALL OK(obj%capacity .EQ. cap0, "vector space: capacity is kept")

  ! Test 2: change the variable type to time
  CALL Set(obj=obj, val=RESHAPE(v(1:4), [2, 2]), rank=Vector, &
           vartype=Time, scale=1.0_DFP, addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%time, 2, [2, 2], v(1:4)), &
          "vector space to time")
  CALL OK(obj%defineOn .EQ. fevaropt%quadrature, &
          "vector time: defineOn is quadrature")

  ! Test 3: change the variable type to spaceTime
  CALL Set(obj=obj, val=RESHAPE(v(1:12), [2, 2, 3]), rank=Vector, &
           vartype=SpaceTime, scale=1.0_DFP, addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%spacetime, 3, [2, 2, 3], v(1:12)), &
          "vector time to spaceTime")

  ! Test 4: change the variable type to constant
  CALL Set(obj=obj, val=v(1:3), rank=Vector, vartype=Constant, &
           scale=1.0_DFP, addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%constant, 1, [3], v(1:3)), &
          "vector spaceTime to constant")
  CALL OK(obj%capacity .EQ. cap0, "vector constant: capacity is kept")
END BLOCK

! Test 5: matrix quadrature variable, change of variable type
BLOCK
  TYPE(FEVariable_) :: obj
  INTEGER(I4B) :: cap0

  obj = QuadratureVariable(nrow=2, ncol=2, rank=Matrix, vartype=Constant)
  cap0 = obj%capacity

  CALL Set(obj=obj, val=RESHAPE(v(1:4), [2, 1, 2]), rank=Matrix, &
           vartype=Time, scale=1.0_DFP, addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%time, 3, [2, 1, 2], v(1:4)), &
          "matrix constant to time")

  CALL Set(obj=obj, val=RESHAPE(v(1:4), [1, 2, 1, 2]), rank=Matrix, &
           vartype=SpaceTime, scale=1.0_DFP, addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%spacetime, 4, [1, 2, 1, 2], v(1:4)), &
          "matrix time to spaceTime")

  CALL Set(obj=obj, val=RESHAPE(v(1:4), [2, 1, 2]), rank=Matrix, &
           vartype=Space, scale=1.0_DFP, addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%space, 3, [2, 1, 2], v(1:4)), &
          "matrix spaceTime to space")

  CALL Set(obj=obj, val=RESHAPE(v(1:4), [2, 2]), rank=Matrix, &
           vartype=Constant, scale=1.0_DFP, addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%constant, 2, [2, 2], v(1:4)), &
          "matrix space to constant")
  CALL OK(obj%defineOn .EQ. fevaropt%quadrature, &
          "matrix: defineOn is quadrature")
  CALL OK(obj%rank .EQ. fevaropt%matrix, "matrix: rank is matrix")
  CALL OK(obj%capacity .EQ. cap0, "matrix: capacity is kept")
END BLOCK

! Test 6: zero and negative scale
BLOCK
  TYPE(FEVariable_) :: obj

  obj = QuadratureVariable(tsize=3, rank=Scalar, vartype=Time)
  CALL Set(obj=obj, val=v(1:3), rank=Scalar, vartype=Time, &
           scale=1.0_DFP, addContribution=.FALSE.)

  ! scale = 0 with addContribution keeps the values
  CALL Set(obj=obj, val=v(4:6), rank=Scalar, vartype=Time, &
           scale=0.0_DFP, addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%time, 1, [3], v(1:3)), &
          "scale = 0, addContribution: values kept")

  ! adding -1 * val to val gives zero
  CALL Set(obj=obj, val=v(1:3), rank=Scalar, vartype=Time, &
           scale=-1.0_DFP, addContribution=.TRUE.)
  CALL OK(IsSet(obj, fevaropt%time, 1, [3], [0.0_DFP, 0.0_DFP, 0.0_DFP]), &
          "scale = -1, addContribution: values are zero")

  ! scale = 0 without addContribution sets zero
  CALL Set(obj=obj, val=1.0_DFP, rank=Scalar, vartype=Constant, &
           scale=0.0_DFP, addContribution=.FALSE.)
  CALL OK(IsSet(obj, fevaropt%constant, 1, [1], [0.0_DFP]), &
          "scale = 0: value is zero")
END BLOCK

CONTAINS

!> True if obj has variable type vartype, total shape tshape, shape s,
! len = PRODUCT(s) and values ans
FUNCTION IsSet(obj, vartype, tshape, s, ans) RESULT(isok)
  TYPE(FEVariable_), INTENT(IN) :: obj
  !! finite element variable
  INTEGER(I4B), INTENT(IN) :: vartype
  !! expected variable type
  INTEGER(I4B), INTENT(IN) :: tshape
  !! expected total shape, SIZE(s)
  INTEGER(I4B), INTENT(IN) :: s(:)
  !! expected shape
  REAL(DFP), INTENT(IN) :: ans(:)
  !! expected values, SIZE(ans) = PRODUCT(s)
  LOGICAL(LGT) :: isok
  !! true if all match

  isok = (obj%varType .EQ. vartype) .AND. (obj%tshape .EQ. tshape) &
         .AND. (obj%len .EQ. PRODUCT(s)) &
         .AND. ALL(obj%s(1:SIZE(s)) .EQ. s) &
         .AND. ALL(ABS(obj%val(1:obj%len) - ans) .LT. 1.0E-12_DFP)
END FUNCTION IsSet

END PROGRAM main
