PROGRAM main
USE GlobalData, ONLY: I4B, LGT, DFP
USE AppendUtility, ONLY: Append
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B), ALLOCATABLE :: C(:), B(:), A(:)
! Append A and B vectors to C vector"
! Note that C is not allocated here, but this is not a problem.
A = [1]
B = [2]
CALL Append(C, A, B)
CALL OK(ALL(C .EQ. [1, 2]), "tests(1):")
! A is vector B is scalar"
! Append A (Vector) and B (Scalar) to C vector"
A = [1]
CALL Append(C, A, 2)
CALL OK(ALL(C .EQ. [1, 2]), "tests(2):")
END PROGRAM main
