!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: Matmul with contracted sizes that differ, and edge cases
!
! Description:
!   Regression test for bug M-04 in bug-reports/Utility_bug_report.md.
!
!   - Every combination now sums over the first MIN(n, m) indices,
!     where n = SIZE(a1, RANK(a1)) and m = SIZE(a2, 1). Before, rank
!     4 and rank 1 read a2 out of bounds for n > m, and rank 2 and
!     rank 1 x rank 1 had a shape mismatch.
!   - The rank-1 x rank-4 function gave an out-of-bounds read for an
!     empty a1.
!   - The nested Matmul_ routines did not set dim1, dim2, ... when the
!     last extent of a2 was zero.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MatmulUtility, ONLY: Matmul
USE MatmulUtility, ONLY: Matmul_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a4(2, 2, 2, 3), a3(2, 2, 3), a2m(2, 3), v1(3)
REAL(DFP) :: s2(2), s4(4), b3(2, 2, 2), c3(4, 2, 2), e4(0, 2, 2, 2)
REAL(DFP) :: r3(2, 2, 2), r2(2, 2), want3(2, 2, 2), want2(2, 2)
REAL(DFP) :: ans4(2, 2, 2, 1), z0(0), b2(2, 0)
INTEGER(I4B) :: i, j, k, l, d1, d2, d3, d4

a4 = RESHAPE([(REAL(i, DFP), i=1, 24)], SHAPE(a4))
a3 = RESHAPE([(REAL(i, DFP), i=1, 12)], SHAPE(a3))
a2m = RESHAPE([(REAL(i, DFP), i=1, 6)], SHAPE(a2m))
v1 = [1.0_DFP, 2.0_DFP, 3.0_DFP]
s2 = [10.0_DFP, 20.0_DFP]
s4 = [10.0_DFP, 20.0_DFP, 30.0_DFP, 40.0_DFP]
b3 = RESHAPE([(REAL(i, DFP), i=1, 8)], SHAPE(b3))
c3 = RESHAPE([(REAL(i, DFP), i=1, 16)], SHAPE(c3))

! rank 4 x rank 1, n = 3 > m = 2: sum over l = 1, 2
want3 = 0.0_DFP
DO l = 1, 2
  want3 = want3 + s2(l) * a4(:, :, :, l)
END DO
r3 = Matmul(a4, s2)
CALL OK(ALL(ABS(r3 - want3) .LT. tol), "r4 x r1, n > m")

! rank 4 x rank 1, n = 3 < m = 4: sum over l = 1, 2, 3
want3 = 0.0_DFP
DO l = 1, 3
  want3 = want3 + s4(l) * a4(:, :, :, l)
END DO
r3 = Matmul(a4, s4)
CALL OK(ALL(ABS(r3 - want3) .LT. tol), "r4 x r1, n < m")

! rank 3 x rank 1, n = 3 > m = 2
want2 = 10.0_DFP * a3(:, :, 1) + 20.0_DFP * a3(:, :, 2)
r2 = Matmul(a3, s2)
CALL OK(ALL(ABS(r2 - want2) .LT. tol), "r3 x r1, n > m")

! rank 2 x rank 3, n = 3 > m = 2
r3 = Matmul(a2m, b3)
DO k = 1, 2
  want3(:, :, k) = MATMUL(a2m(:, 1:2), b3(:, :, k))
END DO
CALL OK(ALL(ABS(r3 - want3) .LT. tol), "r2 x r3, n > m")

! rank 2 x rank 3, n = 3 < m = 4
r3 = Matmul(a2m, c3)
DO k = 1, 2
  want3(:, :, k) = MATMUL(a2m, c3(1:3, :, k))
END DO
CALL OK(ALL(ABS(r3 - want3) .LT. tol), "r2 x r3, n < m")

! rank 1 x rank 1: 1*10 + 2*20 in both orders
CALL OK(ABS(Matmul(v1, s2) - 50.0_DFP) .LT. tol, "r1 x r1, n > m")
CALL OK(ABS(Matmul(s2, v1) - 50.0_DFP) .LT. tol, "r1 x r1, n < m")

! rank 1 x rank 3, n = 3 > m = 2
r2 = Matmul(v1, b3)
DO k = 1, 2
  DO j = 1, 2
    want2(j, k) = v1(1) * b3(1, j, k) + v1(2) * b3(2, j, k)
  END DO
END DO
CALL OK(ALL(ABS(r2 - want2) .LT. tol), "r1 x r3, n > m")

! rank 1 x rank 4 with an empty a1 gives zero
r3 = Matmul(z0, e4)
CALL OK(ALL(r3 .EQ. 0.0_DFP), "r1 x r4, empty a1 gives zero")

! the sizes are set even when the last extent of a2 is zero
d1 = -1
d2 = -1
d3 = -1
d4 = -1
CALL Matmul_(a1=a4(:, :, :, 1:2), a2=b2, ans=ans4, dim1=d1, dim2=d2, &
             dim3=d3, dim4=d4)
CALL OK(d1 .EQ. 2_I4B .AND. d2 .EQ. 2_I4B .AND. d3 .EQ. 2_I4B, &
        "r4 x r2, SIZE(a2, 2) = 0: dim1 to dim3 are set")
CALL OK(d4 .EQ. 0_I4B, "r4 x r2, SIZE(a2, 2) = 0: dim4 is 0")

END PROGRAM main
