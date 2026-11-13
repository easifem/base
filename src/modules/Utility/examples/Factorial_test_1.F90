!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Factorial of an integer

PROGRAM main
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: Factorial
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: n

CALL OK(Factorial(0_I4B) .EQ. 1_I4B, "0! = 1")
CALL OK(Factorial(1_I4B) .EQ. 1_I4B, "1! = 1")
CALL OK(Factorial(5_I4B) .EQ. 120_I4B, "5! = 120")
CALL OK(Factorial(10_I4B) .EQ. 3628800_I4B, "10! = 3628800")

! 12! is the largest factorial that fits in INTEGER(I4B)
CALL OK(Factorial(12_I4B) .EQ. 479001600_I4B, "12! = 479001600")

! n! = n (n-1)!
n = 7_I4B
CALL OK(Factorial(n) .EQ. n * Factorial(n - 1_I4B), "n! = n (n-1)!")

END PROGRAM main
