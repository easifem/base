!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetSym for an integer matrix

PROGRAM main
USE GlobalData, ONLY: I4B
USE SymUtility, ONLY: GetSym
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: mat(3, 3), mat0(3, 3), expected(3, 3)

! mat0 = | 1 4 7 |
!        | 2 5 8 |
!        | 3 6 9 |
mat0 = RESHAPE([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])

! use the upper triangle, mat is changed in place
mat = mat0
CALL GetSym(mat, "U")
expected = RESHAPE([1, 4, 7, 4, 5, 8, 7, 8, 9], [3, 3])
CALL OK(ALL(mat .EQ. expected), "from upper triangle")
CALL OK(ALL(mat .EQ. TRANSPOSE(mat)), "result is symmetric")

! use the lower triangle
mat = mat0
CALL GetSym(mat, "L")
expected = RESHAPE([1, 2, 3, 2, 5, 6, 3, 6, 9], [3, 3])
CALL OK(ALL(mat .EQ. expected), "from lower triangle")

! lower case is also accepted
mat = mat0
CALL GetSym(mat, "l")
CALL OK(ALL(mat .EQ. expected), "from = l")

! any other value leaves mat unchanged
mat = mat0
CALL GetSym(mat, "X")
CALL OK(ALL(mat .EQ. mat0), "unknown from does nothing")

END PROGRAM main
