!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: NormalRandomValue for n = 1 and for seed = 0
!
! Description:
!   Regression test for bug M-05 in bug-reports/Utility_bug_report.md.
!
!   - For n = 1 both uniform values came from the same seed, so they
!     were equal. Now the pair is UniformRandomValue(2, seed).
!   - seed = 0 gave uniform values of zero and LOG(0). Now seed = 0 is
!     the same as seed = 1.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE RandomUtility, ONLY: NormalRandomValue
USE RandomUtility, ONLY: UniformRandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: pi = 3.141592653589793_DFP
REAL(DFP) :: x1(1), u(2), x5(5), y5(5)

! n = 1: the first value of a Box-Muller pair
x1 = NormalRandomValue(1_I4B, seed=12345_I4B)
u = UniformRandomValue(2_I4B, seed=12345_I4B)
CALL OK(ABS(x1(1) - SQRT(-2.0_DFP * LOG(u(1))) &
            * COS(2.0_DFP * pi * u(2))) .LT. tol, "n = 1")

! n = 1 is the first value of n = 2 with the same seed
u = NormalRandomValue(2_I4B, seed=12345_I4B)
CALL OK(ABS(x1(1) - u(1)) .LT. tol, "n = 1 matches n = 2")

! seed = 0 gives finite values, the same as seed = 1
x5 = NormalRandomValue(5_I4B, seed=0_I4B)
y5 = NormalRandomValue(5_I4B, seed=1_I4B)
CALL OK(ALL(ABS(x5) .LT. HUGE(1.0_DFP)), "seed 0 gives finite values")
CALL OK(ALL(x5 .EQ. y5), "seed 0 is the same as seed 1")

END PROGRAM main
