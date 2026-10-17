PROGRAM main
USE GlobalData, ONLY: I4B, LGT, DFP
USE AppendUtility, ONLY: Append
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B), ALLOCATABLE :: C(:), B(:), A(:)
! A and B are vectors"
A = [1, 2]
B = [3, 4]
! Append B(1) and B(2) to A and assign to C.
CALL Append(C, A, B, mask=[.TRUE., .TRUE.])
CALL OK(ALL(C .EQ. [1, 2, 3, 4]), "Append:")
! Append B(1) to A and assign to C.
CALL Append(C, A, B, mask=[.TRUE., .FALSE.])
CALL OK(ALL(C .EQ. [1, 2, 3]), "Append:")
! Do not append B to A, just assign A to C.
CALL Append(C, A, B, mask=[.FALSE., .FALSE.])
CALL OK(ALL(C .EQ. [1, 2]), "Append:")
! A is vector B is scalar
! Append 3 to A and then assign result to C
CALL Append(C, A, 3, mask=.TRUE.)
CALL OK(ALL(C .EQ. [1, 2, 3]), "Append:")
! Do not append 3 to A, assign result to C, that is C =A
CALL Append(C, A, 3, mask=.FALSE.)
CALL OK(ALL(C .EQ. [1, 2]), "Append:")
END PROGRAM main
