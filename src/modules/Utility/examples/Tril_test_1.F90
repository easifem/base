!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Tril returns the lower triangular part as a full matrix

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE TriagUtility, ONLY: Tril
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(3, 3), u(3, 3), i, j
REAL(DFP) :: b(3, 5), v(3, 5), c(5, 3), w(5, 3)

DO j = 1, 3
  DO i = 1, 3
    a(i, j) = 10 * i + j
  END DO
END DO

! integer (3,3), default diagNo = 0
u = Tril(a)
CALL OK(ALL(u .EQ. RESHAPE([11, 21, 31, 0, 22, 32, 0, 0, 33], &
                           [3, 3])), "integer, diagNo=0")

! diagNo = 1 and diagNo = -1
u = Tril(a, diagNo=1_I4B)
CALL OK(ALL(u .EQ. RESHAPE([11, 21, 31, 12, 22, 32, 0, 23, 33], &
                           [3, 3])), "integer, diagNo=1")
u = Tril(a, diagNo=-1_I4B)
CALL OK(ALL(u .EQ. RESHAPE([0, 21, 31, 0, 0, 32, 0, 0, 0], &
                           [3, 3])), "integer, diagNo=-1")

! real rectangular matrices
DO j = 1, 5
  DO i = 1, 3
    b(i, j) = REAL(10 * i + j, DFP)
  END DO
END DO
c = TRANSPOSE(b)

v = Tril(b)
CALL OK(IsPart(b, v, 0_I4B), "real (3,5), diagNo=0")
v = Tril(b, diagNo=2_I4B)
CALL OK(IsPart(b, v, 2_I4B), "real (3,5), diagNo=2")
w = Tril(c, diagNo=-1_I4B)
CALL OK(IsPart(c, w, -1_I4B), "real (5,3), diagNo=-1")

CONTAINS

! true if u keeps a where j - i .LE. k and is zero elsewhere
PURE FUNCTION IsPart(a, u, k) RESULT(ans)
  REAL(DFP), INTENT(IN) :: a(:, :)
  REAL(DFP), INTENT(IN) :: u(:, :)
  INTEGER(I4B), INTENT(IN) :: k
  LOGICAL(LGT) :: ans
  INTEGER(I4B) :: i, j
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

  ans = .TRUE.
  DO j = 1, SIZE(a, 2)
    DO i = 1, SIZE(a, 1)
      IF (j - i .LE. k) THEN
        ans = ans .AND. ABS(u(i, j) - a(i, j)) .LT. tol
      ELSE
        ans = ans .AND. ABS(u(i, j)) .LT. tol
      END IF
    END DO
  END DO
END FUNCTION IsPart

END PROGRAM main
