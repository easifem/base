!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Shape of the result of two variables, all rank cases
!
! Calls GetShape(rankCase, varCase, s1, s2, tshape1, tshape2, ans, tsize)
! for each rank case and each group of variable cases. The helper Check
! takes `[rankCase, varCase]`, `s1`, `s2` and the expected shape. The
! data used:
!
! - scalar: constant [1], space [nns], time [nnt], spaceTime [nns, nnt]
! - vector: constant [nc], space [nc, nns], time [nc, nnt],
!   spaceTime [nc, nns, nnt]
! - matrix: constant [nr, ncl], space [nr, ncl, nns], ...
!
! with nc = 3, nr = 2, ncl = 4, nns = 5, nnt = 2.

PROGRAM main
USE FEVariable_Method, ONLY: GetShape
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: scalar and scalar (rankCase 00)
BLOCK
  CALL Check([00, 00], [1], [1], [1], "00, const const")
  CALL Check([00, 13], [5], [5, 2], [5, 2], "00, space spaceTime")
  CALL Check([00, 31], [5, 2], [5], [5, 2], "00, spaceTime space")
END BLOCK

! Test 2: vector and vector (rankCase 11)
BLOCK
  CALL Check([11, 11], [3, 5], [3, 5], [3, 5], "11, space space")
  CALL Check([11, 01], [3], [3, 5], [3, 5], "11, const space")
  CALL Check([11, 30], [3, 5, 2], [3], [3, 5, 2], "11, spaceTime const")
  CALL Check([11, 21], [3, 2], [3, 5], [3, 5, 2], "11, time space")
END BLOCK

! Test 3: matrix and matrix (rankCase 22)
BLOCK
  CALL Check([22, 22], [2, 4, 2], [2, 4, 2], [2, 4, 2], "22, time time")
  CALL Check([22, 03], [2, 4], [2, 4, 5, 2], [2, 4, 5, 2], &
             "22, const spaceTime")
  CALL Check([22, 20], [2, 4, 2], [2, 4], [2, 4, 2], "22, time const")
  CALL Check([22, 12], [2, 4, 5], [2, 4, 2], [2, 4, 5, 2], &
             "22, space time")
  CALL Check([22, 21], [2, 4, 2], [2, 4, 5], [2, 4, 5, 2], &
             "22, time space")
END BLOCK

! Test 4: scalar and vector (rankCase 01)
BLOCK
  CALL Check([01, 13], [5], [3, 5, 2], [3, 5, 2], "01, space spaceTime")
  CALL Check([01, 31], [5, 2], [3, 5], [3, 5, 2], "01, spaceTime space")
  CALL Check([01, 12], [5], [3, 2], [3, 5, 2], "01, space time")
  CALL Check([01, 21], [2], [3, 5], [3, 5, 2], "01, time space")
END BLOCK

! Test 5: scalar and matrix (rankCase 02)
BLOCK
  CALL Check([02, 01], [1], [2, 4, 5], [2, 4, 5], "02, const space")
  CALL Check([02, 20], [2], [2, 4], [2, 4, 2], "02, time const")
  CALL Check([02, 12], [5], [2, 4, 2], [2, 4, 5, 2], "02, space time")
  CALL Check([02, 21], [2], [2, 4, 5], [2, 4, 5, 2], "02, time space")
END BLOCK

! Test 6: vector and scalar (rankCase 10)
BLOCK
  CALL Check([10, 32], [3, 5, 2], [2], [3, 5, 2], "10, spaceTime time")
  CALL Check([10, 13], [3, 5], [5, 2], [3, 5, 2], "10, space spaceTime")
  CALL Check([10, 12], [3, 5], [2], [3, 5, 2], "10, space time")
  CALL Check([10, 21], [3, 2], [5], [3, 5, 2], "10, time space")
END BLOCK

! Test 7: vector and matrix (rankCase 12)
BLOCK
  CALL Check([12, 11], [3, 5], [2, 4, 5], [2, 4, 5], "12, space space")
  CALL Check([12, 31], [3, 5, 2], [2, 4, 5], [2, 4, 5, 2], &
             "12, spaceTime space")
  ! vector space [3, 5] and matrix time [2, 4, 2]: [2, 4, 5, 2]
  CALL Check([12, 12], [3, 5], [2, 4, 2], [2, 4, 5, 2], &
             "12, space time")
  ! vector time [3, 2] and matrix space [2, 4, 5]: [2, 4, 5, 2]
  CALL Check([12, 21], [3, 2], [2, 4, 5], [2, 4, 5, 2], &
             "12, time space")
END BLOCK

! Test 8: matrix and scalar (rankCase 20)
BLOCK
  CALL Check([20, 10], [2, 4, 5], [1], [2, 4, 5], "20, space const")
  CALL Check([20, 03], [2, 4], [5, 2], [2, 4, 5, 2], &
             "20, const spaceTime")
  CALL Check([20, 12], [2, 4, 5], [2], [2, 4, 5, 2], "20, space time")
  CALL Check([20, 21], [2, 4, 2], [5], [2, 4, 5, 2], "20, time space")
END BLOCK

! Test 9: matrix and vector (rankCase 21)
BLOCK
  CALL Check([21, 30], [2, 4, 5, 2], [3], [2, 4, 5, 2], &
             "21, spaceTime const")
  CALL Check([21, 01], [2, 4], [3, 5], [2, 4, 5], "21, const space")
  CALL Check([21, 12], [2, 4, 5], [3, 2], [2, 4, 5, 2], &
             "21, space time")
END BLOCK

CONTAINS

!> Calls GetShape with rankCase = rv(1), varCase = rv(2),
! tshape1 = SIZE(s1), tshape2 = SIZE(s2), and checks tsize and
! ans(1:tsize) against expected.
SUBROUTINE Check(rv, s1, s2, expected, label)
  INTEGER(I4B), INTENT(IN) :: rv(2)
  INTEGER(I4B), INTENT(IN) :: s1(:)
  INTEGER(I4B), INTENT(IN) :: s2(:)
  INTEGER(I4B), INTENT(IN) :: expected(:)
  CHARACTER(*), INTENT(IN) :: label

  INTEGER(I4B) :: ans(4), tsize
  LOGICAL :: isok

  ans = 0
  CALL GetShape(rankCase=rv(1), varCase=rv(2), s1=s1, s2=s2, &
                tshape1=SIZE(s1), tshape2=SIZE(s2), ans=ans, tsize=tsize)
  isok = tsize .EQ. SIZE(expected)
  IF (isok) isok = ALL(ans(1:tsize) .EQ. expected)
  CALL OK(isok, label)
END SUBROUTINE Check

END PROGRAM main
