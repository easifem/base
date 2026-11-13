!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reverse a part of an integer vector in place

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReverseUtility, ONLY: Reverse
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(6)

! reverse the whole vector (even length)
a = [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B]
CALL Reverse(a, 1_I4B, 6_I4B)
CALL OK(ALL(a .EQ. [6_I4B, 5_I4B, 4_I4B, 3_I4B, 2_I4B, 1_I4B]), &
        "reverse a(1:6)")

! reverse a part (odd length), other entries are untouched
a = [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B]
CALL Reverse(a, 2_I4B, 4_I4B)
CALL OK(ALL(a .EQ. [1_I4B, 4_I4B, 3_I4B, 2_I4B, 5_I4B, 6_I4B]), &
        "reverse a(2:4)")

! reversing twice gives back the input
CALL Reverse(a, 2_I4B, 4_I4B)
CALL OK(ALL(a .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B]), &
        "reverse twice")

! n1 = n2 and n1 > n2 do nothing
CALL Reverse(a, 3_I4B, 3_I4B)
CALL OK(ALL(a .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B]), &
        "n1 = n2")
CALL Reverse(a, 5_I4B, 2_I4B)
CALL OK(ALL(a .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B]), &
        "n1 > n2")

END PROGRAM main
