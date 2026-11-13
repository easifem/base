!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: SWAP of vectors and matrices of different sizes
!
! Description:
!   Regression test for bug M-40 in bug-reports/Utility_bug_report.md.
!   The sizes were not checked: the integer versions looped over
!   SIZE(a), and the real vector version used whole-array assignment.
!   Now only the common part is exchanged.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SwapUtility, ONLY: SWAP
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ia(3), ib(2), im(2, 2), jm(3, 1)
REAL(DFP) :: ra(2), rb(3)

! integer vectors, a is longer
ia = [1_I4B, 2_I4B, 3_I4B]
ib = [7_I4B, 8_I4B]
CALL SWAP(ia, ib)
CALL OK(ALL(ia .EQ. [7_I4B, 8_I4B, 3_I4B]), "integer: a(1:2) swapped")
CALL OK(ALL(ib .EQ. [1_I4B, 2_I4B]), "integer: b swapped")

! real vectors, b is longer
ra = [1.0_DFP, 2.0_DFP]
rb = [7.0_DFP, 8.0_DFP, 9.0_DFP]
CALL SWAP(ra, rb)
CALL OK(ALL(ra .EQ. [7.0_DFP, 8.0_DFP]), "real: a swapped")
CALL OK(ALL(rb .EQ. [1.0_DFP, 2.0_DFP, 9.0_DFP]), &
        "real: b(3) is not changed")

! integer matrices, only the common block (2, 1)
im = RESHAPE([1_I4B, 2_I4B, 3_I4B, 4_I4B], [2, 2])
jm = RESHAPE([7_I4B, 8_I4B, 9_I4B], [3, 1])
CALL SWAP(im, jm)
CALL OK(ALL(im(:, 1) .EQ. [7_I4B, 8_I4B]) .AND. &
        ALL(im(:, 2) .EQ. [3_I4B, 4_I4B]), "matrix: a swapped in block")
CALL OK(ALL(jm(:, 1) .EQ. [1_I4B, 2_I4B, 9_I4B]), &
        "matrix: b swapped in block")

END PROGRAM main
