!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Partition for real vectors

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE PartitionUtility, ONLY: Partition
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(4), b(5), c(5)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B) :: ip

! pivot is a(1) = 0.5
a = [0.5_DFP, 0.25_DFP, 0.75_DFP, 0.125_DFP]
ip = 0_I4B
CALL Partition(a, 1_I4B, 4_I4B, ip)
CALL OK(ip .EQ. 3_I4B, "pivot index")
CALL OK(ABS(a(ip) - 0.5_DFP) .LT. tol, "pivot value")
CALL OK(ALL(a(1:ip - 1) .LE. a(ip)), "left part <= pivot")
CALL OK(ALL(a(ip + 1:4) .GT. a(ip)), "right part > pivot")
CALL OK(ALL(ABS(a - [0.125_DFP, 0.25_DFP, 0.5_DFP, 0.75_DFP]) .LT. tol), &
        "partitioned vector")

! the pivot is the largest value of b(1:4)
b = [4.0_DFP, 3.0_DFP, 2.0_DFP, 1.0_DFP, 100.0_DFP]
CALL Partition(b, 1_I4B, 4_I4B, ip)
CALL OK(ip .EQ. 4_I4B, "largest pivot")
CALL OK(ALL(ABS(b - [1.0_DFP, 3.0_DFP, 2.0_DFP, 4.0_DFP, 100.0_DFP]) &
            .LT. tol), "partitioned b(1:4), b(5) untouched")

! post-condition on a longer vector
c = [0.5_DFP, 0.25_DFP, 0.75_DFP, 0.125_DFP, 1.0_DFP]
CALL Partition(c, 1_I4B, 5_I4B, ip)
CALL OK(ip .EQ. 3_I4B, "pivot index of c")
CALL OK(ALL(c(1:ip - 1) .LE. c(ip)) .AND. ALL(c(ip + 1:5) .GT. c(ip)), &
        "post-condition of c")

END PROGRAM main
