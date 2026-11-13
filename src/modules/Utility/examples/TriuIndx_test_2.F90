!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: TriuIndx from the shape of a matrix

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriagUtility, ONLY: TriuIndx
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(3, 5)
INTEGER(I4B) :: b(5, 3)

a = 0.0_DFP
b = 0_I4B

! only the shape of A is used
CALL OK(ALL(TriuIndx(a) .EQ. TriuIndx(3_I4B, 5_I4B)), &
        "real (3,5), default diagNo")
CALL OK(ALL(TriuIndx(a, diagNo=1_I4B) .EQ. &
            TriuIndx(3_I4B, 5_I4B, 1_I4B)), "real (3,5), diagNo=1")
CALL OK(ALL(TriuIndx(b, diagNo=-1_I4B) .EQ. &
            TriuIndx(5_I4B, 3_I4B, -1_I4B)), "integer (5,3), diagNo=-1")

! the values of A do not matter
a = 7.0_DFP
CALL OK(ALL(TriuIndx(a) .EQ. TriuIndx(3_I4B, 5_I4B)), "values ignored")
CALL OK(SIZE(TriuIndx(a), 1) .EQ. 12_I4B, "real (3,5): size")

END PROGRAM main
