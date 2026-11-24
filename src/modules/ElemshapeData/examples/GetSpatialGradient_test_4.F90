!> author: Vikas Sharma, Ph. D.
! date: 2026-11-24
! summary: Matrix spatial gradient when no gradient can be computed

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE ElemShapeData_Method, ONLY: GetSpatialGradient
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! Test 1: space-time matrix val with a space-only cell. No gradient can
! be computed, so ans is allocated with shape (nrow, ncol, nsd, nips)
! and set to zero.
BLOCK
  REAL(DFP), ALLOCATABLE :: g4(:, :, :, :)
  REAL(DFP) :: mst(3, 2, 4, 2)

  mst = 1.0_DFP
  CALL GetSpatialGradient(obj=cell, ans=g4, &
                          val=NodalVariable(mst, Matrix, SpaceTime))
  CALL OK(ALLOCATED(g4), "space-time val, space obj: ans is allocated")
  IF (ALLOCATED(g4)) THEN
    CALL OK(ALL(SHAPE(g4) .EQ. [3, 2, 2, cell%nips]), &
            "space-time val, space obj: shape (nrow, ncol, nsd, nips)")
    CALL OK(ALL(g4 .EQ. 0.0_DFP), "space-time val, space obj: ans = 0")
  END IF
END BLOCK

END PROGRAM main
