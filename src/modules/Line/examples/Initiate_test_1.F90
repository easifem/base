!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Initiate a reference line from coordinates or from a domain name.

PROGRAM main
USE BaseType, ONLY: ReferenceLine_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE ReferenceLine_Method, ONLY: Initiate
USE ReferenceLine_Method, ONLY: ReferenceLine
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: unit coordinates set the domain name to UNIT
BLOCK
  TYPE(ReferenceLine_) :: obj
  REAL(DFP) :: xij(1, 2)
  xij(1, :) = [0.0_DFP, 1.0_DFP]
  CALL Initiate(obj=obj, nsd=1_I4B, xij=xij)
  CALL OK(TRIM(obj%domainName) == "UNIT", "xij on [0, 1] gives UNIT")
  CALL OK(MAXVAL(ABS(obj%xij - xij)) < tol, "xij is stored")
END BLOCK

! Test 2: other coordinates set the domain name to GENERAL
BLOCK
  TYPE(ReferenceLine_) :: obj
  REAL(DFP) :: xij(1, 2)
  xij(1, :) = [2.0_DFP, 5.0_DFP]
  CALL Initiate(obj=obj, nsd=1_I4B, xij=xij)
  CALL OK(TRIM(obj%domainName) == "GENERAL", "xij on [2, 5] gives GENERAL")
  CALL OK(MAXVAL(ABS(obj%xij - xij)) < tol, "General xij is stored")
END BLOCK

! Test 3: the domain name is not case sensitive
BLOCK
  TYPE(ReferenceLine_) :: obj
  obj = ReferenceLine(nsd=2_I4B, domainName="biunit")
  CALL OK(TRIM(obj%domainName) == "BIUNIT", "Lower case biunit accepted")
  CALL OK(MAXVAL(ABS(obj%xij(1, :) - [-1.0_DFP, 1.0_DFP])) < tol, &
          "BIUNIT coordinates")
END BLOCK

! Test 4: topology and entity counts of a linear line
BLOCK
  TYPE(ReferenceLine_) :: obj
  obj = ReferenceLine(nsd=3_I4B, domainName="UNIT")
  CALL OK(obj%nsd == 3, "nsd is stored")
  CALL OK(obj%name == Line2 .AND. obj%order == 1, "Linear Line2 element")
  CALL OK(ALL(obj%entityCounts == [2, 1, 0, 0]), "Two points, one line")
  CALL OK(SIZE(obj%topology) == 3, "Two point and one line topology")
  CALL OK(ALL(obj%topology(3)%nptrs == [1, 2]), "Line topology nodes")
END BLOCK

END PROGRAM main
