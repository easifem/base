!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Total shape of the result of two variables, all rank cases
!
! Calls GetTotalShape(rankCase, varCase, tshape1, tshape2) for each rank
! case and each group of variable cases. The helper Check takes
! `[rankCase, varCase]`, `[tshape1, tshape2]` and the expected result.
! The total shape of one variable is:
!
! - scalar: constant 1, space 1, time 1, spaceTime 2
! - vector: constant 1, space 2, time 2, spaceTime 3
! - matrix: constant 2, space 3, time 3, spaceTime 4

PROGRAM main
USE FEVariable_Method, ONLY: GetTotalShape
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: scalar and scalar (rankCase 00)
BLOCK
  CALL Check([00, 00], [1, 1], 1, "00, const const")
  CALL Check([00, 13], [1, 2], 2, "00, space spaceTime")
  CALL Check([00, 31], [2, 1], 2, "00, spaceTime space")
  CALL Check([00, 21], [1, 1], 2, "00, time space")
END BLOCK

! Test 2: vector and vector (rankCase 11)
BLOCK
  CALL Check([11, 00], [1, 1], 1, "11, const const")
  CALL Check([11, 01], [1, 2], 2, "11, const space")
  CALL Check([11, 30], [3, 1], 3, "11, spaceTime const")
  CALL Check([11, 21], [2, 2], 3, "11, time space")
END BLOCK

! Test 3: matrix and matrix (rankCase 22)
BLOCK
  CALL Check([22, 22], [3, 3], 3, "22, time time")
  CALL Check([22, 03], [2, 4], 4, "22, const spaceTime")
  CALL Check([22, 20], [3, 2], 3, "22, time const")
  CALL Check([22, 12], [3, 3], 4, "22, space time")
END BLOCK

! Test 4: scalar and vector (rankCase 01)
BLOCK
  CALL Check([01, 13], [1, 3], 3, "01, space spaceTime")
  CALL Check([01, 31], [2, 2], 3, "01, spaceTime space")
  CALL Check([01, 12], [1, 2], 3, "01, space time")
END BLOCK

! Test 5: scalar and matrix (rankCase 02)
BLOCK
  CALL Check([02, 01], [1, 3], 3, "02, const space")
  CALL Check([02, 20], [1, 2], 3, "02, time const")
  CALL Check([02, 31], [2, 3], 4, "02, spaceTime space")
  CALL Check([02, 21], [1, 3], 4, "02, time space")
END BLOCK

! Test 6: vector and scalar (rankCase 10)
BLOCK
  CALL Check([10, 32], [3, 1], 3, "10, spaceTime time")
  CALL Check([10, 13], [2, 2], 3, "10, space spaceTime")
  CALL Check([10, 12], [2, 1], 3, "10, space time")
END BLOCK

! Test 7: vector and matrix (rankCase 12)
BLOCK
  CALL Check([12, 11], [2, 3], 3, "12, space space")
  CALL Check([12, 31], [3, 3], 4, "12, spaceTime space")
  CALL Check([12, 10], [2, 2], 3, "12, space const")
  CALL Check([12, 12], [2, 3], 4, "12, space time")
END BLOCK

! Test 8: matrix and scalar (rankCase 20)
BLOCK
  CALL Check([20, 10], [3, 1], 3, "20, space const")
  CALL Check([20, 03], [2, 2], 4, "20, const spaceTime")
  CALL Check([20, 21], [3, 1], 4, "20, time space")
END BLOCK

! Test 9: matrix and vector (rankCase 21)
BLOCK
  CALL Check([21, 30], [4, 1], 4, "21, spaceTime const")
  CALL Check([21, 01], [2, 2], 3, "21, const space")
  CALL Check([21, 12], [3, 2], 4, "21, space time")
END BLOCK

CONTAINS

!> Checks GetTotalShape(rv(1), rv(2), ts(1), ts(2)) = expected.
SUBROUTINE Check(rv, ts, expected, label)
  INTEGER(I4B), INTENT(IN) :: rv(2)
  INTEGER(I4B), INTENT(IN) :: ts(2)
  INTEGER(I4B), INTENT(IN) :: expected
  CHARACTER(*), INTENT(IN) :: label

  CALL OK(GetTotalShape(rankCase=rv(1), varCase=rv(2), tshape1=ts(1), &
                        tshape2=ts(2)) .EQ. expected, label)
END SUBROUTINE Check

END PROGRAM main
