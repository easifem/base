!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Intersection of explicit 2D lines when one line is a point.
!
! The line through (0,1) and (1,1) is y = 1. The point (5,1) lies on it.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Line_Method, ONLY: lines_exp_int_2d
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: q1(2), q2(2), pt(2)

q1 = [0.0_DFP, 1.0_DFP]
q2 = [1.0_DFP, 1.0_DFP]
pt = [5.0_DFP, 1.0_DFP]

! Test 1: first line is a point on the second line
BLOCK
  REAL(DFP) :: p(2)
  INTEGER(I4B) :: ival
  CALL lines_exp_int_2d(pt, pt, q1, q2, ival, p)
  CALL OK(ival == 1, "Point on the line: one intersection")
  CALL OK(MAXVAL(ABS(p - pt)) < tol, "Intersection is the point")
END BLOCK

! Test 2: second line is a point on the first line
BLOCK
  REAL(DFP) :: p(2)
  INTEGER(I4B) :: ival
  CALL lines_exp_int_2d(q1, q2, pt, pt, ival, p)
  CALL OK(ival == 1, "Line and point on it: one intersection")
  CALL OK(MAXVAL(ABS(p - pt)) < tol, "Intersection is the point")
END BLOCK

! Test 3: first line is a point off the second line
BLOCK
  REAL(DFP) :: p(2)
  INTEGER(I4B) :: ival
  CALL lines_exp_int_2d([5.0_DFP, 2.0_DFP], [5.0_DFP, 2.0_DFP], q1, q2, &
                        ival, p)
  CALL OK(ival == 0, "Point off the line: no intersection")
END BLOCK

! Test 4: both lines are the same point
BLOCK
  REAL(DFP) :: p(2)
  INTEGER(I4B) :: ival
  CALL lines_exp_int_2d(pt, pt, pt, pt, ival, p)
  CALL OK(ival == 1 .AND. MAXVAL(ABS(p - pt)) < tol, &
          "Two equal points: one intersection")
END BLOCK

! Test 5: parallel lines do not intersect
BLOCK
  REAL(DFP) :: p(2)
  INTEGER(I4B) :: ival
  CALL lines_exp_int_2d(q1, q2, [0.0_DFP, 2.0_DFP], [1.0_DFP, 2.0_DFP], &
                        ival, p)
  CALL OK(ival == 0, "Parallel lines: no intersection")
END BLOCK

END PROGRAM main
