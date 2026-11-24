!> author: OpenAI
! date: 2026-11-21
! summary: SUPG overloads agree for absent and nonpositive time steps

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetSUPGParam
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: coords(1, 2) = &
                        RESHAPE([-1.0_DFP, 1.0_DFP], [1, 2])
REAL(DFP), PARAMETER :: steps(3) = [-2.0_DFP, 0.0_DFP, 2.0_DFP]
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
TYPE(FEVariable_) :: c, val, nu
INTEGER(I4B) :: opt

CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=coords, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=coords, N=cell%N, dNdXi=cell%dNdXi)
c = NodalVariable([1.0_DFP], Vector, Constant)
val = NodalVariable([-1.0_DFP, 1.0_DFP], Scalar, Space)
nu = NodalVariable(0.25_DFP, Scalar, Constant)

! Test 1: t1 = 1 and t3 = 1/4, with both combination options
DO opt = 1, 2
  BLOCK
    TYPE(FEVariable_) :: tauReal, tauField
    REAL(DFP) :: expected, t2
    INTEGER(I4B) :: caseIndex
    CHARACTER(24) :: label

    IF (opt .EQ. 1) THEN
      expected = 1.0_DFP / SQRT(1.0_DFP + 0.25_DFP**2)
    ELSE
      expected = 1.0_DFP / 1.25_DFP
    END IF
    CALL GetSUPGParam(obj=cell, tau=tauReal, c=c, val=val, &
                      nu=0.25_DFP, opt=opt)
    CALL OK(ALL(ABS(Get(tauReal, Scalar, Space) - expected) .LT. tol), &
            "absent dt: steady known result")
    DO caseIndex = 1, SIZE(steps)
      SELECT CASE (caseIndex)
      CASE (1)
        label = "negative dt"
      CASE (2)
        label = "zero dt"
      CASE DEFAULT
        label = "positive dt"
      END SELECT
      t2 = 0.0_DFP
      IF (steps(caseIndex) .GT. 0.0_DFP) t2 = 2.0_DFP / steps(caseIndex)
      IF (opt .EQ. 1) THEN
        expected = 1.0_DFP / SQRT(1.0_DFP + 0.25_DFP**2 + t2**2)
      ELSE
        expected = 1.0_DFP / (1.25_DFP + t2)
      END IF
      CALL GetSUPGParam(obj=cell, tau=tauReal, c=c, val=val, &
                        nu=0.25_DFP, dt=steps(caseIndex), opt=opt)
      CALL GetSUPGParam(obj=cell, tau=tauField, c=c, val=val, &
                        nu=nu, dt=steps(caseIndex), opt=opt)
      CALL OK(ALL(ABS(Get(tauReal, Scalar, Space) - expected) .LT. tol), &
              TRIM(label)//": real nu known result")
      CALL OK(ALL(ABS(Get(tauField, Scalar, Space) - expected) .LT. tol), &
              TRIM(label)//": FEVariable nu known result")
      CALL OK(ALL(ABS(Get(tauReal, Scalar, Space) - &
                      Get(tauField, Scalar, Space)) .LT. tol), &
              TRIM(label)//": overload equivalence")
    END DO
  END BLOCK
END DO
END PROGRAM main
