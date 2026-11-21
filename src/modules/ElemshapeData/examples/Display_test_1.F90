!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Display the content of ElemShapeData_

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE ElemShapeData_Method, ONLY: Display
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
! physical cell: rectangle [0, 2] x [0, 1], so x = 1 + xi, y = (1 + eta) / 2
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
! physical facet: bottom edge from (0, 0) to (2, 0)
REAL(DFP), PARAMETER :: xfacet(2, 2) = RESHAPE([0.0_DFP, 0.0_DFP, &
                                                2.0_DFP, 0.0_DFP], [2, 2])
TYPE(ElemShapeData_) :: cell, facet
TYPE(QuadraturePoint_) :: quad, facetQuad

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL QP_Initiate(obj=facetQuad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL LagrangeElemShapeData(obj=facet, quad=facetQuad, nsd=2, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)

! Test 1: Display writes the message and the sizes to a unit
BLOCK
  INTEGER(I4B) :: unitNo, ios
  CHARACTER(256) :: line, msg
  LOGICAL(LGT) :: hasMsg, hasNsd, hasNns, hasNips

  OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
  CALL Display(cell, "my shape data", unitNo=unitNo)
  REWIND (unitNo)

  hasMsg = .FALSE.
  hasNsd = .FALSE.
  hasNns = .FALSE.
  hasNips = .FALSE.
  readLoop: DO
    READ (unitNo, '(A)', iostat=ios, iomsg=msg) line
    IF (ios .NE. 0) EXIT readLoop
    hasMsg = hasMsg .OR. INDEX(line, "my shape data") .GT. 0
    hasNsd = hasNsd .OR. INDEX(line, "nsd:") .GT. 0
    hasNns = hasNns .OR. INDEX(line, "nns:") .GT. 0
    hasNips = hasNips .OR. INDEX(line, "nips:") .GT. 0
  END DO readLoop
  CLOSE (unitNo)

  CALL OK(hasMsg, "message is written")
  CALL OK(hasNsd, "nsd is written")
  CALL OK(hasNns, "nns is written")
  CALL OK(hasNips, "nips is written")
END BLOCK

END PROGRAM main
