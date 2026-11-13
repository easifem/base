!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Sym for an integer matrix

PROGRAM main
USE GlobalData, ONLY: I4B
USE SymUtility, ONLY: Sym
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: mat(3, 3), ans(3, 3), expected(3, 3), one(1, 1)

! mat = | 1 4 7 |
!       | 2 5 8 |
!       | 3 6 9 |
mat = RESHAPE([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])

! use the upper triangle
ans = Sym(mat, "U")
expected = RESHAPE([1, 4, 7, 4, 5, 8, 7, 8, 9], [3, 3])
CALL OK(ALL(ans .EQ. expected), "from upper triangle")
CALL OK(ALL(ans .EQ. TRANSPOSE(ans)), "result is symmetric")

! use the lower triangle
ans = Sym(mat, "L")
expected = RESHAPE([1, 2, 3, 2, 5, 6, 3, 6, 9], [3, 3])
CALL OK(ALL(ans .EQ. expected), "from lower triangle")
CALL OK(ALL(ans .EQ. TRANSPOSE(ans)), "result is symmetric")

! lower case is also accepted
CALL OK(ALL(Sym(mat, "u") .EQ. Sym(mat, "U")), "from = u")
CALL OK(ALL(Sym(mat, "l") .EQ. Sym(mat, "L")), "from = l")

! the input is not changed
CALL OK(mat(2, 1) .EQ. 2_I4B .AND. mat(1, 2) .EQ. 4_I4B, &
        "mat is unchanged")

! 1x1 matrix
one = 5_I4B
CALL OK(ALL(Sym(one, "U") .EQ. 5_I4B), "1x1 matrix")

END PROGRAM main
