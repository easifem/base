!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: TrilIndx from the shape of a matrix

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriagUtility, ONLY: TrilIndx
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(3, 5)
INTEGER(I4B) :: b(5, 3)

a = 0.0_DFP
b = 0_I4B

! only the shape of A is used
CALL OK(ALL(TrilIndx(a) .EQ. TrilIndx(3_I4B, 5_I4B)), &
        "real (3,5), default diagNo")
CALL OK(ALL(TrilIndx(a, diagNo=1_I4B) .EQ. &
            TrilIndx(3_I4B, 5_I4B, 1_I4B)), "real (3,5), diagNo=1")
CALL OK(ALL(TrilIndx(b, diagNo=-1_I4B) .EQ. &
            TrilIndx(5_I4B, 3_I4B, -1_I4B)), "integer (5,3), diagNo=-1")

! the values of A do not matter
b = 7_I4B
CALL OK(ALL(TrilIndx(b) .EQ. TrilIndx(5_I4B, 3_I4B)), "values ignored")
CALL OK(SIZE(TrilIndx(b), 1) .EQ. 12_I4B, "integer (5,3): size")

END PROGRAM main
