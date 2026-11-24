!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Construct a linear reference line with the ReferenceLine
! function.

PROGRAM main
USE BaseType, ONLY: ReferenceLine_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE ReferenceLine_Method, ONLY: ReferenceLine
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: default domain is BIUNIT
BLOCK
  TYPE(ReferenceLine_) :: obj
  obj = ReferenceLine(nsd=1_I4B)
  CALL OK(TRIM(obj%domainName) == "BIUNIT", "Default domain is BIUNIT")
  CALL OK(MAXVAL(ABS(obj%xij(1, :) - [-1.0_DFP, 1.0_DFP])) < tol, &
          "Default coordinates are [-1, 1]")
  CALL OK(obj%name == Line2 .AND. obj%order == 1, "Linear Line2")
END BLOCK

! Test 2: UNIT domain
BLOCK
  TYPE(ReferenceLine_) :: obj
  obj = ReferenceLine(nsd=2_I4B, domainName="UNIT")
  CALL OK(TRIM(obj%domainName) == "UNIT", "UNIT domain")
  CALL OK(MAXVAL(ABS(obj%xij(1, :) - [0.0_DFP, 1.0_DFP])) < tol, &
          "UNIT coordinates are [0, 1]")
  CALL OK(obj%nsd == 2, "nsd is stored")
END BLOCK

! Test 3: xij overrides domainName
BLOCK
  TYPE(ReferenceLine_) :: obj
  REAL(DFP) :: xij(1, 2)
  xij(1, :) = [-1.0_DFP, 1.0_DFP]
  obj = ReferenceLine(nsd=1_I4B, xij=xij, domainName="UNIT")
  CALL OK(TRIM(obj%domainName) == "BIUNIT", &
          "xij on [-1, 1] gives BIUNIT, domainName ignored")
END BLOCK

! Test 4: the high order element procedure is attached
BLOCK
  TYPE(ReferenceLine_) :: obj
  obj = ReferenceLine(nsd=1_I4B)
  CALL OK(ASSOCIATED(obj%highOrderElement), &
          "highOrderElement is associated")
END BLOCK

END PROGRAM main
