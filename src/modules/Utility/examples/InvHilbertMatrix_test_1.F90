!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: InvHilbertMatrix against the exact integer inverse

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LinearAlgebraUtility, ONLY: InvHilbertMatrix
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a1(1, 1), a2(2, 2), a3(3, 3), a5(5, 5)
REAL(DFP) :: e2(2, 2), e3(3, 3)

! n = 1
a1 = InvHilbertMatrix(1_I4B)
CALL OK(ABS(a1(1, 1) - 1.0_DFP) .LE. tol, "n = 1")

! n = 2
a2 = InvHilbertMatrix(2_I4B)
e2 = RESHAPE([4.0_DFP, -6.0_DFP, -6.0_DFP, 12.0_DFP], [2, 2])
CALL OK(ALL(ABS(a2 - e2) .LE. tol), "n = 2")

! n = 3
a3 = InvHilbertMatrix(3_I4B)
e3 = RESHAPE([9.0_DFP, -36.0_DFP, 30.0_DFP, &
              -36.0_DFP, 192.0_DFP, -180.0_DFP, &
              30.0_DFP, -180.0_DFP, 180.0_DFP], [3, 3])
CALL OK(ALL(ABS(a3 - e3) .LE. tol), "n = 3")

! n = 5, a few entries
a5 = InvHilbertMatrix(5_I4B)
CALL OK(ABS(a5(1, 1) - 25.0_DFP) .LE. tol, "n = 5, entry (1,1)")
CALL OK(ABS(a5(2, 4) - 26880.0_DFP) .LE. tol, "n = 5, entry (2,4)")
CALL OK(ABS(a5(5, 5) - 44100.0_DFP) .LE. tol, "n = 5, entry (5,5)")

! the inverse is symmetric
CALL OK(ALL(ABS(a5 - TRANSPOSE(a5)) .LE. tol), "symmetric")

END PROGRAM main
