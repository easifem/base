!> author: Vikas Sharma, Ph. D.
! date: 2026-11-19
! summary: Projection of dNT/dx on a vector FEVariable, no allocation

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: GetProjectionOfdNTdXt_
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: SetdNTdXt
USE ElemShapeData_Method, ONLY: SetJacobian
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
! space element [0, 2]: x = 1 + xi
REAL(DFP), PARAMETER :: xst(1, 2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, 0.0_DFP, &
                                                2.0_DFP], [1, 2, 2])
! u = c(a) x at time node a, c = [1, 3]; projection on the vector [2]
REAL(DFP), PARAMETER :: ust(2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, 0.0_DFP, &
                                             6.0_DFP], [2, 2])
REAL(DFP), PARAMETER :: c(1) = [2.0_DFP]
TYPE(ElemShapeData_) :: line
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
TYPE(QuadraturePoint_) :: quad
INTEGER(I4B) :: ipt

CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=line, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)
CALL Initiate(stelemsd, line)
DO ipt = 1, 2
  CALL ALLOCATE (stelemsd(ipt), nsd=1, xidim=1, nns=2, nips=2, nnt=2)
  stelemsd(ipt)%T = line%N(:, ipt)
  stelemsd(ipt)%N = line%N
  stelemsd(ipt)%dNdXi = line%dNdXi
  CALL SetJacobian(obj=stelemsd(ipt), val=xst, dNdXi=line%dNdXi, &
                   T=stelemsd(ipt)%T)
  CALL SetdNTdXt(obj=stelemsd(ipt))
END DO


! Test 1: one time point, constant c = 2 and nodal space c = 2
BLOCK
  REAL(DFP) :: ans(3, 3, 3), ref
  INTEGER(I4B) :: dim1, dim2, dim3, ips

  ref = 2.0_DFP * (stelemsd(1)%T(1) * 1.0_DFP + stelemsd(1)%T(2) * 3.0_DFP)
  CALL GetProjectionOfdNTdXt_(obj=stelemsd(1), &
                              c=NodalVariable(c, Vector, Constant), &
                              crank=Vector, ans=ans, dim1=dim1, dim2=dim2, &
                              dim3=dim3)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 2, 2]), &
          "dim1 = nns, dim2 = nnt, dim3 = nips")
  DO ips = 1, 2
    CALL OK(ABS(SUM(ust * ans(1:2, 1:2, ips)) - ref) .LT. tol, &
            "constant c: c . du/dx at each point")
  END DO

  CALL GetProjectionOfdNTdXt_(obj=stelemsd(1), &
                              c=NodalVariable(RESHAPE([2.0_DFP, 2.0_DFP], &
                                                      [1, 2]), Vector, &
                                              Space), &
                              crank=Vector, ans=ans, dim1=dim1, dim2=dim2, &
                              dim3=dim3)
  CALL OK(ABS(SUM(ust * ans(1:2, 1:2, 1)) - ref) .LT. tol, &
          "nodal space c: c . du/dx")
END BLOCK

! Test 2: all time points, constant c and nodal space-time c
BLOCK
  REAL(DFP) :: ans(3, 3, 3, 3), ref
  INTEGER(I4B) :: dim1, dim2, dim3, dim4, ips, jpt

  CALL GetProjectionOfdNTdXt_(obj=stelemsd, &
                              c=NodalVariable(c, Vector, Constant), &
                              crank=Vector, ans=ans, dim1=dim1, dim2=dim2, &
                              dim3=dim3, dim4=dim4)
  CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. [2, 2, 2, 2]), &
          "dims = nns, nnt, nips, nipt")
  DO jpt = 1, 2
    ref = 2.0_DFP * (stelemsd(jpt)%T(1) * 1.0_DFP + &
                     stelemsd(jpt)%T(2) * 3.0_DFP)
    DO ips = 1, 2
      CALL OK(ABS(SUM(ust * ans(1:2, 1:2, ips, jpt)) - ref) .LT. tol, &
              "constant c: c . du/dx at each space-time point")
    END DO
  END DO

  CALL GetProjectionOfdNTdXt_(obj=stelemsd, &
                              c=NodalVariable(RESHAPE([2.0_DFP, 2.0_DFP, &
                                                       2.0_DFP, 2.0_DFP], &
                                                      [1, 2, 2]), Vector, &
                                              SpaceTime), &
                              crank=Vector, ans=ans, dim1=dim1, dim2=dim2, &
                              dim3=dim3, dim4=dim4)
  ref = 2.0_DFP * (stelemsd(2)%T(1) * 1.0_DFP + stelemsd(2)%T(2) * 3.0_DFP)
  CALL OK(ABS(SUM(ust * ans(1:2, 1:2, 1, 2)) - ref) .LT. tol, &
          "nodal space-time c: c . du/dx")
END BLOCK

END PROGRAM main
