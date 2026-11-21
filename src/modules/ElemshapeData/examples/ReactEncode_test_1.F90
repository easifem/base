!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: React encoding of ElemShapeData_ returns an empty string

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE ElemShapeData_Method, ONLY: ElemshapeData_ReactEncode
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
TYPE(ElemShapeData_) :: elemsd
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)

! Test 1: it is not implemented, so the text is empty
BLOCK
  TYPE(String) :: ans

  ans = ElemshapeData_ReactEncode(elemsd)
  CALL OK(LEN(ans%chars()) .EQ. 0, "filled shape data: empty text")
END BLOCK

! Test 2: an empty shape data also gives an empty text
BLOCK
  TYPE(String) :: ans
  TYPE(ElemShapeData_) :: empty

  ans = ElemshapeData_ReactEncode(empty)
  CALL OK(LEN(ans%chars()) .EQ. 0, "empty shape data: empty text")
END BLOCK

END PROGRAM main
