!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Division of FEVariables without allocation by Division_

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: Division_
USE FEVariable_Method, ONLY: GetRankCase
USE FEVariable_Method, ONLY: GetVarCase
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a(3), c(3), vs(2, 3)
INTEGER(I4B) :: ii
TYPE(FEVariable_) :: obj1, obj2, ans

a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
c = [100.0_DFP, 200.0_DFP, 300.0_DFP]
vs = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])

! ans is initiated with the rank and shape of the result
ans = QuadratureVariable(nrow=2, ncol=3, rank=Vector, vartype=Space)

! Test 1: Division_(obj1, obj2, ans)
BLOCK
  obj1 = QuadratureVariable(vs, Vector, Space)
  obj2 = QuadratureVariable(vs + 1.0_DFP, Vector, Space)
  CALL Division_(obj1=obj1, obj2=obj2, ans=ans)
  CALL OK(ans%len .EQ. 6, "obj1 / obj2: len")
  CALL OK(ALL(ABS(ans%val(1:6) - RESHAPE(vs / (vs + 1.0_DFP), [6])) &
              .LT. tol), "obj1 / obj2: values")
END BLOCK

! Test 2: Division_(obj, val, ans)
BLOCK
  obj1 = QuadratureVariable(vs, Vector, Space)
  CALL Division_(obj=obj1, val=0.5_DFP, ans=ans)
  CALL OK(ALL(ABS(ans%val(1:6) - (RESHAPE(vs, [6]) / 0.5_DFP)) &
              .LT. tol), "obj / val: values")

  CALL Division_(obj=obj1, val=0.5_DFP, ans=ans, objOnRHS=.TRUE.)
  CALL OK(ALL(ABS(ans%val(1:6) - (0.5_DFP / RESHAPE(vs, [6]))) &
              .LT. tol), "val / obj with objOnRHS: values")
END BLOCK

! Test 3: Division_ with precomputed rankCase and varCase
BLOCK
  INTEGER(I4B) :: rankCase, varCase

  obj1 = QuadratureVariable(a, Scalar, Space)
  obj2 = QuadratureVariable(c, Scalar, Space)
  rankCase = GetRankCase(obj1%rank, obj2%rank)
  varCase = GetVarCase(obj1%varType, obj2%varType)
  CALL Division_(obj1=obj1, obj2=obj2, ans=ans, varCase=varCase, &
                 rankCase=rankCase)
  CALL OK(ans%len .EQ. 3, "with cases: len = 3")
  CALL OK(ans%varType .EQ. fevaropt%space, "with cases: space")
  CALL OK(ALL(ABS(ans%val(1:3) - (a / c)) .LT. tol), &
          "with cases: values")
END BLOCK

! Test 4: scalar constant / vector constant into ans
BLOCK
  obj1 = QuadratureVariable(2.0_DFP, Scalar, Constant)
  obj2 = QuadratureVariable(a, Vector, Constant)
  CALL Division_(obj1=obj1, obj2=obj2, ans=ans)
  CALL OK(ans%len .EQ. 3, "scalar / vector constant: len")
  CALL OK(ALL(ABS(ans%val(1:3) - (2.0_DFP / a)) .LT. tol), &
          "scalar / vector constant: values")
END BLOCK

END PROGRAM main
