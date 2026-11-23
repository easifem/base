!> author: Vikas Sharma, Ph. D.
! date: 2026-11-23
! summary: Display writes NLaplacian and, for space-time data, nnt

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: Display
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])

! Test 1: NLaplacian is written, or NOT ALLOCATED
BLOCK
  TYPE(ElemShapeData_) :: cell, empty
  TYPE(QuadraturePoint_) :: quad

  CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                   nips=[2], quadratureType=qpopt%GaussLegendre)
  CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                             elemType=Quadrangle4, &
                             refelemCoord=quadCoord, &
                             domainName="BIUNIT", order=1)

  CALL OK(HasText(cell, "NLaplacian:"), "NLaplacian is written")
  CALL OK(HasText(empty, "NLaplacian: NOT ALLOCATED"), &
          "empty: NLaplacian NOT ALLOCATED")
END BLOCK

! Test 2: nnt is written for space-time shape data
BLOCK
  TYPE(STElemShapeData_) :: st

  st%nnt = 2
  CALL OK(HasText(st, "nnt:"), "space-time: nnt is written")
END BLOCK

CONTAINS

!> True if Display(obj) writes a line that contains text
FUNCTION HasText(obj, text) RESULT(ans)
  CLASS(ElemShapeData_), INTENT(IN) :: obj
  !! element shape data to display
  CHARACTER(*), INTENT(IN) :: text
  !! text to look for
  LOGICAL(LGT) :: ans
  !! true if some line contains text
  INTEGER(I4B) :: unitNo, ios
  CHARACTER(256) :: line, msg

  ans = .FALSE.
  OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
  CALL Display(obj, "shape data", unitNo=unitNo)
  REWIND (unitNo)
  readLoop: DO
    READ (unitNo, '(A)', iostat=ios, iomsg=msg) line
    IF (ios .NE. 0) EXIT readLoop
    ans = ans .OR. INDEX(line, text) .GT. 0
  END DO readLoop
  CLOSE (unitNo)
END FUNCTION HasText

END PROGRAM main
