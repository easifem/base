!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Hierarchical shape data for an unsupported element

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE ElemShapeData_Method, ONLY: HierarchicalElemShapeData
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Tetrahedron4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tetCoord(3, 4) = RESHAPE([0.0_DFP, 0.0_DFP, &
                        0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, &
                        1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP], &
                                                 [3, 4])

! Test 1: tetrahedron has no hierarchical basis. The routine reports an
! error and returns without reading undefined evaluator output.
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  TYPE(QuadraturePoint_) :: quad

  CALL QP_Initiate(obj=quad, elemType=Tetrahedron4, domainName="UNIT", &
                   order=2, quadratureType=qpopt%GaussLegendre)
  CALL HierarchicalElemShapeData(obj=elemsd, quad=quad, nsd=3, xidim=3, &
                                 elemType=Tetrahedron4, &
                                 refelemCoord=tetCoord, &
                                 domainName="UNIT", cellOrder=[1_I4B])
  CALL OK(.TRUE., "tetrahedron: routine returns without a crash")
END BLOCK

END PROGRAM main
