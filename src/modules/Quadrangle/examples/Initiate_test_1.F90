!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing Initiate of ReferenceQuadrangle_ with vertex
! coordinates, a domain name, and no optional argument

PROGRAM main
USE BaseType, ONLY: ReferenceQuadrangle_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReferenceQuadrangle_Method, ONLY: Initiate
USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(ReferenceQuadrangle_) :: obj

! Test 1: no optional argument gives the biunit quadrangle
BLOCK
  CALL Initiate(obj=obj, nsd=2_I4B)
  CALL OK(TRIM(obj%domainName) .EQ. "BIUNIT", "default: domain BIUNIT")
  CALL OK(MAXVAL(ABS(obj%xij - RefCoord_Quadrangle("BIUNIT"))) .LT. tol, &
          "default: biunit vertices")
  CALL OK(ALL(obj%entityCounts .EQ. [4, 4, 1, 0]), &
          "default: 4 nodes, 4 edges, 1 face")
  CALL OK(SIZE(obj%topology) .EQ. 9, "default: 9 topology entries")
  CALL OK(ALL(obj%topology(8)%nptrs .EQ. [4, 1]), &
          "default: edge 4 joins nodes 4 and 1")
END BLOCK

! Test 2: the domain name is converted to upper case
BLOCK
  CALL Initiate(obj=obj, nsd=2_I4B, domainName="unit")
  CALL OK(TRIM(obj%domainName) .EQ. "UNIT", "domainName: domain UNIT")
  CALL OK(MAXVAL(ABS(obj%xij - RefCoord_Quadrangle("UNIT"))) .LT. tol, &
          "domainName: unit vertices")
END BLOCK

! Test 3: vertices of the unit square give the domain UNIT
BLOCK
  CALL Initiate(obj=obj, nsd=2_I4B, xij=RefCoord_Quadrangle("UNIT"))
  CALL OK(TRIM(obj%domainName) .EQ. "UNIT", "xij: unit square is UNIT")
END BLOCK

! Test 4: other vertices give the domain GENERAL, and xij wins over
! domainName
BLOCK
  REAL(DFP) :: xij(2, 4)

  xij(:, 1) = [0.0_DFP, 0.0_DFP]
  xij(:, 2) = [2.0_DFP, 0.0_DFP]
  xij(:, 3) = [1.0_DFP, 1.0_DFP]
  xij(:, 4) = [0.0_DFP, 1.0_DFP]

  CALL Initiate(obj=obj, nsd=3_I4B, xij=xij, domainName="BIUNIT")
  CALL OK(TRIM(obj%domainName) .EQ. "GENERAL", "xij: domain GENERAL")
  CALL OK(MAXVAL(ABS(obj%xij - xij)) .LT. tol, "xij: vertices copied")
  CALL OK(obj%nsd .EQ. 3, "xij: nsd is stored")
END BLOCK

END PROGRAM main
