!> author: Vikas Sharma, Ph. D.
! date: 2026-11-23
! summary: MdEncode writes ws, coord and NLaplacian tables

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: MdEncode
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])

! Test 1: a quadrangle has ws, coord and NLaplacian tables
BLOCK
  TYPE(ElemShapeData_) :: cell
  TYPE(QuadraturePoint_) :: quad
  TYPE(String) :: ans

  CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                   nips=[2], quadratureType=qpopt%GaussLegendre)
  CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                             elemType=Quadrangle4, &
                             refelemCoord=quadCoord, &
                             domainName="BIUNIT", order=1)
  ans = MdEncode(cell)
  CALL OK(INDEX(ans%chars(), "**ws**") .GT. 0, "ws table")
  CALL OK(INDEX(ans%chars(), "**coord**") .GT. 0, "coord table")
  CALL OK(INDEX(ans%chars(), "**NLaplacian**") .GT. 0, &
          "NLaplacian table")
END BLOCK

! Test 2: missing arrays are reported as Not ALLOCATED
BLOCK
  TYPE(ElemShapeData_) :: empty
  TYPE(String) :: ans

  ans = MdEncode(empty)
  CALL OK(INDEX(ans%chars(), "**ws Not ALLOCATED**") .GT. 0, &
          "empty: ws Not ALLOCATED")
  CALL OK(INDEX(ans%chars(), "**coord Not ALLOCATED**") .GT. 0, &
          "empty: coord Not ALLOCATED")
  CALL OK(INDEX(ans%chars(), "**NLaplacian Not ALLOCATED**") .GT. 0, &
          "empty: NLaplacian Not ALLOCATED")
END BLOCK

END PROGRAM main
