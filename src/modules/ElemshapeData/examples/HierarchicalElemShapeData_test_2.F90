!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Hierarchical shape data with default orientations

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE ElemShapeData_Method, ONLY: HierarchicalElemShapeData
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])

! Test 1: line, order 3, no cellOrient gives the same data as +1
BLOCK
  TYPE(ElemShapeData_) :: a, b
  TYPE(QuadraturePoint_) :: quad

  CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                   nips=[4], quadratureType=qpopt%GaussLegendre)
  CALL HierarchicalElemShapeData(obj=a, quad=quad, nsd=1, xidim=1, &
                                 elemType=Line2, refelemCoord=lineCoord, &
                                 domainName="BIUNIT", cellOrder=[3_I4B])
  CALL HierarchicalElemShapeData(obj=b, quad=quad, nsd=1, xidim=1, &
                                 elemType=Line2, refelemCoord=lineCoord, &
                                 domainName="BIUNIT", cellOrder=[3_I4B], &
                                 cellOrient=[1_I4B])
  CALL OK(a%nns .EQ. b%nns, "line p3: same nns")
  CALL OK(ALL(ABS(a%N - b%N) .LT. tol), "line p3: default orient, same N")
  CALL OK(ALL(ABS(a%dNdXi - b%dNdXi) .LT. tol), &
          "line p3: default orient, same dNdXi")
END BLOCK

! Test 2: quadrangle, order 3, no orientations give the same data as +1
BLOCK
  TYPE(ElemShapeData_) :: a, b
  TYPE(QuadraturePoint_) :: quad
  INTEGER(I4B) :: faceOrder(3, 4), faceOrient(3, 4)

  faceOrder = 3
  faceOrient = 1
  CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                   nips=[4], quadratureType=qpopt%GaussLegendre)
  CALL HierarchicalElemShapeData(obj=a, quad=quad, nsd=2, xidim=2, &
                                 elemType=Quadrangle4, &
                                 refelemCoord=quadCoord, &
                                 domainName="BIUNIT", &
                                 cellOrder=[3_I4B, 3_I4B], &
                                 faceOrder=faceOrder)
  CALL HierarchicalElemShapeData(obj=b, quad=quad, nsd=2, xidim=2, &
                                 elemType=Quadrangle4, &
                                 refelemCoord=quadCoord, &
                                 domainName="BIUNIT", &
                                 cellOrder=[3_I4B, 3_I4B], &
                                 faceOrder=faceOrder, &
                                 cellOrient=[1_I4B, 1_I4B, 1_I4B], &
                                 faceOrient=faceOrient)
  CALL OK(a%nns .EQ. b%nns, "quadrangle p3: same nns")
  CALL OK(ALL(ABS(a%N - b%N) .LT. tol), &
          "quadrangle p3: default orient, same N")
  CALL OK(ALL(ABS(a%dNdXi - b%dNdXi) .LT. tol), &
          "quadrangle p3: default orient, same dNdXi")
END BLOCK

END PROGRAM main
