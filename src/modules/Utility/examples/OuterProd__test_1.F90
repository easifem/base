!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: OuterProd_ of rank-1 and rank-1 arrays

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE ProductUtility, ONLY: OuterProd
USE ProductUtility, ONLY: OuterProd_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B), PARAMETER :: expected(2) = [2, 3]
REAL(DFP) :: a(2), b(3)
REAL(DFP) :: ans(3, 4)
REAL(DFP) :: want
INTEGER(I4B) :: nrow, ncol
INTEGER(I4B) :: ii, i1, i2
LOGICAL(LGT) :: isok

a = [(REAL(ii, DFP), ii=1, 2)]
b = [(REAL(ii, DFP), ii=1, 3)]

! ans is one larger than needed in every dimension
ans = 1.0_DFP

! ans = 2 * ans + 3 * (outer product)
CALL OuterProd_(a=a, b=b, anscoeff=2.0_DFP, scale=3.0_DFP, &
                ans=ans, nrow=nrow, ncol=ncol)

CALL OK(ALL([nrow, ncol] .EQ. expected), "sizes written in ans")

isok = .TRUE.
DO i2 = 1, 3
  DO i1 = 1, 2
    want = 2.0_DFP + 3.0_DFP * a(i1) * b(i2)
    isok = isok .AND. (ABS(ans(i1, i2) - want) .LT. tol)
  END DO
END DO
CALL OK(isok, "leading block of ans")
CALL OK(ABS(ans(2, 3) - 20.0_DFP) .LT. tol, "last entry")

! entries outside the leading block are not changed
isok = .TRUE.
isok = isok .AND. ALL(ABS(ans(3, :) - 1.0_DFP) .LT. tol)
isok = isok .AND. ALL(ABS(ans(:, 4) - 1.0_DFP) .LT. tol)
CALL OK(isok, "rest of ans is unchanged")

! with anscoeff = 0 and scale = 1 we get OuterProd
CALL OuterProd_(a=a, b=b, anscoeff=0.0_DFP, scale=1.0_DFP, &
                ans=ans, nrow=nrow, ncol=ncol)
CALL OK(ALL(ABS(ans(1:2, 1:3) - OuterProd(a, b)) &
        .LT. tol), "same as OuterProd")

END PROGRAM main
