!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: SearchNearestCoord is another name for Loc_NearestPoint

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: SearchNearestCoord
USE MiscUtility, ONLY: Loc_NearestPoint
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: xij(2, 20), x(2)
INTEGER(I4B) :: ii, id

! 20 points close to the origin
DO ii = 1, 20
  xij(1, ii) = 0.05_DFP * REAL(ii, DFP)
  xij(2, ii) = 0.03_DFP * REAL(ii, DFP)
END DO

! put the point x in column 15
x = [11.0_DFP, 100.0_DFP]
xij(:, 15) = x

id = SearchNearestCoord(Array=xij, x=x)
CALL OK(id .EQ. 15_I4B, "x is found in column 15")
CALL OK(id .EQ. Loc_NearestPoint(xij, x), &
        "same result as Loc_NearestPoint")

END PROGRAM main
