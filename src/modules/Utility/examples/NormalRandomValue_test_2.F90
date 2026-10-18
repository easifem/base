!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: NormalRandomValue for an even number of values
!
! Description:
!   Regression test for bug H-03 in bug-reports/Utility_bug_report.md.
!   For even n the routine assigned UniformRandomValue(n, seed), a
!   vector of size n, to its work array r(n + 1). With -fbounds-check
!   this stops the program with an array bound mismatch.
!
!   For even n the values come in Box-Muller pairs from
!   u = UniformRandomValue(n, seed):
!     x(2k-1) = SQRT(-2 LOG(u(2k-1))) COS(2 pi u(2k))
!     x(2k)   = SQRT(-2 LOG(u(2k-1))) SIN(2 pi u(2k))

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE RandomUtility, ONLY: NormalRandomValue
USE RandomUtility, ONLY: UniformRandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: pi = 3.141592653589793_DFP
INTEGER(I4B), PARAMETER :: n = 10000
REAL(DFP) :: x2(2), u2(2), x6(6), u6(6), e6(6)
REAL(DFP) :: x(n), y(n), mu, sd

! n = 2: one Box-Muller pair
x2 = NormalRandomValue(2_I4B, seed=12345_I4B)
u2 = UniformRandomValue(2_I4B, seed=12345_I4B)
CALL OK(ABS(x2(1) - SQRT(-2.0_DFP * LOG(u2(1))) &
            * COS(2.0_DFP * pi * u2(2))) .LT. tol, "n = 2: x(1)")
CALL OK(ABS(x2(2) - SQRT(-2.0_DFP * LOG(u2(1))) &
            * SIN(2.0_DFP * pi * u2(2))) .LT. tol, "n = 2: x(2)")

! n = 6: three pairs
x6 = NormalRandomValue(6_I4B, seed=7_I4B)
u6 = UniformRandomValue(6_I4B, seed=7_I4B)
e6(1:5:2) = SQRT(-2.0_DFP * LOG(u6(1:5:2))) &
            * COS(2.0_DFP * pi * u6(2:6:2))
e6(2:6:2) = SQRT(-2.0_DFP * LOG(u6(1:5:2))) &
            * SIN(2.0_DFP * pi * u6(2:6:2))
CALL OK(ALL(ABS(x6 - e6) .LT. tol), "n = 6: Box-Muller pairs")

! large even n: sample mean and standard deviation
x = NormalRandomValue(n, seed=12345_I4B)
mu = SUM(x) / n
sd = SQRT(SUM((x - mu)**2) / (n - 1))
CALL OK(ABS(mu) .LT. 0.05_DFP, "even n: mean is close to 0")
CALL OK(ABS(sd - 1.0_DFP) .LT. 0.05_DFP, "even n: std is close to 1")

! the same seed gives the same values
y = NormalRandomValue(n, seed=12345_I4B)
CALL OK(ALL(x .EQ. y), "even n: same seed gives the same values")

END PROGRAM main
