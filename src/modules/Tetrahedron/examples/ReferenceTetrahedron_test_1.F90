!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing ReferenceTetrahedron with BIUNIT and with given xij

PROGRAM main
USE BaseType, ONLY: ReferenceTetrahedron_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Tetrahedron4
USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: BIUNIT domain name sets the BIUNIT vertices
BLOCK
  TYPE(ReferenceTetrahedron_) :: obj
  REAL(DFP) :: x(3, 4)
  obj = ReferenceTetrahedron(nsd=3_I4B, domainName="biunit")
  x = RefCoord_Tetrahedron("BIUNIT")
  CALL OK(TRIM(obj%domainName) .EQ. "BIUNIT", "BIUNIT domain name")
  CALL OK(MAXVAL(ABS(obj%xij - x)) .LT. tol, "BIUNIT vertices")
END BLOCK

! Test 2: xij equal to the UNIT vertices is detected as UNIT
BLOCK
  TYPE(ReferenceTetrahedron_) :: obj
  obj = ReferenceTetrahedron(nsd=3_I4B, xij=RefCoord_Tetrahedron("UNIT"))
  CALL OK(TRIM(obj%domainName) .EQ. "UNIT", "xij detected as UNIT")
END BLOCK

! Test 3: other xij gives a GENERAL element with the same topology
BLOCK
  TYPE(ReferenceTetrahedron_) :: obj
  REAL(DFP) :: x(3, 4)
  x = 2.0_DFP * RefCoord_Tetrahedron("UNIT")
  obj = ReferenceTetrahedron(nsd=3_I4B, xij=x)
  CALL OK(TRIM(obj%domainName) .EQ. "GENERAL", "xij detected as GENERAL")
  CALL OK(MAXVAL(ABS(obj%xij - x)) .LT. tol, "xij is copied")
  CALL OK(obj%name .EQ. Tetrahedron4 .AND. obj%order .EQ. 1, &
          "name and order")
  CALL OK(ALL(obj%entityCounts .EQ. [4, 6, 4, 1]), "entity counts")
  CALL OK(SIZE(obj%topology) .EQ. 15, "topology size")
  CALL OK(ALL(obj%topology(15)%nptrs .EQ. [1, 2, 3, 4]), "cell nodes")
END BLOCK

END PROGRAM main
