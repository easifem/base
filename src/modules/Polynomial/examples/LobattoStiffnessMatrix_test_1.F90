!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return the stiffness matrix on the biunit interval.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LobattoPolynomialUtility, ONLY: LobattoStiffnessMatrix
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: a(3, 3)

a = LobattoStiffnessMatrix(n=2_I4B)
CALL OK(MAXVAL(ABS(a-TRANSPOSE(a))) < 1.0E-11_DFP, &
  "Matrix is symmetric")
CALL OK(ABS(a(1, 1)-(0.5_DFP)) < 1.0E-11_DFP .AND. &
  ABS(a(1, 2)-(-0.5_DFP)) < 1.0E-11_DFP, &
  "Endpoint block agrees with exact integrals")
END PROGRAM main
