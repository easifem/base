!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Partition for integer vectors

PROGRAM main
USE GlobalData, ONLY: I4B
USE PartitionUtility, ONLY: Partition
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(7), b(6), c(5), d(2), e(4), ip

! pivot is a(1) = 5
a = [5_I4B, 2_I4B, 8_I4B, 1_I4B, 9_I4B, 3_I4B, 7_I4B]
ip = 0_I4B
CALL Partition(a, 1_I4B, 7_I4B, ip)
CALL OK(ip .EQ. 4_I4B, "pivot index")
CALL OK(a(ip) .EQ. 5_I4B, "pivot value")
CALL OK(ALL(a(1:ip - 1) .LE. a(ip)), "left part <= pivot")
CALL OK(ALL(a(ip + 1:7) .GT. a(ip)), "right part > pivot")
CALL OK(ALL(a .EQ. [1_I4B, 2_I4B, 3_I4B, 5_I4B, 9_I4B, 8_I4B, 7_I4B]), &
        "partitioned vector")

! only b(2:5) is partitioned, the pivot is b(2) = 4
b = [10_I4B, 4_I4B, 6_I4B, 1_I4B, 3_I4B, 20_I4B]
CALL Partition(b, 2_I4B, 5_I4B, ip)
CALL OK(ip .EQ. 4_I4B, "pivot index of b(2:5)")
CALL OK(ALL(b .EQ. [10_I4B, 1_I4B, 3_I4B, 4_I4B, 6_I4B, 20_I4B]), &
        "partitioned b(2:5)")
CALL OK(b(1) .EQ. 10_I4B .AND. b(6) .EQ. 20_I4B, "outside is untouched")

! equal values, the pivot ends at the right end
c = [3_I4B, 3_I4B, 3_I4B, 3_I4B, 99_I4B]
CALL Partition(c, 1_I4B, 4_I4B, ip)
CALL OK(ip .EQ. 4_I4B, "equal values")
CALL OK(ALL(c(1:4) .EQ. 3_I4B) .AND. c(5) .EQ. 99_I4B, "values kept")

! a single entry
d = [7_I4B, 1_I4B]
CALL Partition(d, 1_I4B, 1_I4B, ip)
CALL OK(ip .EQ. 1_I4B .AND. ALL(d .EQ. [7_I4B, 1_I4B]), "single entry")

! the pivot is the smallest value
e = [1_I4B, 2_I4B, 3_I4B, 4_I4B]
CALL Partition(e, 1_I4B, 4_I4B, ip)
CALL OK(ip .EQ. 1_I4B, "smallest pivot")
CALL OK(ALL(e .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B]), "sorted input kept")

END PROGRAM main
