!> author: Vikas Sharma, Ph. D.
! date: 2026-11-19
! summary: Display space-time shape data at every time point

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: Display
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
TYPE(ElemShapeData_) :: timeElemsd
TYPE(QuadraturePoint_) :: quad
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)

! a linear time element with 2 Gauss points gives 2 STElemShapeData_
CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=timeElemsd, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)
CALL Initiate(stelemsd, timeElemsd)

! Test 1: each time point is written with msg and its number
BLOCK
  INTEGER(I4B) :: unitNo, ios, nTime
  CHARACTER(256) :: line, msg
  LOGICAL(LGT) :: has1, has2

  OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
  CALL Display(stelemsd, "st", unitNo=unitNo)
  REWIND (unitNo)

  has1 = .FALSE.
  has2 = .FALSE.
  nTime = 0
  readLoop: DO
    READ (unitNo, '(A)', iostat=ios, iomsg=msg) line
    IF (ios .NE. 0) EXIT readLoop
    has1 = has1 .OR. INDEX(line, "st(1)") .GT. 0
    has2 = has2 .OR. INDEX(line, "st(2)") .GT. 0
    IF (INDEX(line, "SHAPE FUNCTION IN TIME") .GT. 0) nTime = nTime + 1
  END DO readLoop
  CLOSE (unitNo)

  CALL OK(has1, "time point 1 is written as st(1)")
  CALL OK(has2, "time point 2 is written as st(2)")
  CALL OK(nTime .EQ. 2, "time data is written for each time point")
END BLOCK

END PROGRAM main
