!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: OuterProd_ of rank-1, rank-1 and rank-1 arrays

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE ProductUtility, ONLY: OuterProd
USE ProductUtility, ONLY: OuterProd_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B), PARAMETER :: expected(3) = [2, 3, 2]
REAL(DFP) :: a(2), b(3), c(2)
REAL(DFP) :: ans(3, 4, 3)
REAL(DFP) :: want
INTEGER(I4B) :: dim1, dim2, dim3
INTEGER(I4B) :: ii, i1, i2, i3
LOGICAL(LGT) :: isok

a = [(REAL(ii, DFP), ii=1, 2)]
b = [(REAL(ii, DFP), ii=1, 3)]
c = [(REAL(ii, DFP), ii=1, 2)]

! ans is one larger than needed in every dimension
ans = 1.0_DFP

! ans = 2 * ans + 3 * (outer product)
CALL OuterProd_(a=a, b=b, c=c, anscoeff=2.0_DFP, scale=3.0_DFP, &
                ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

CALL OK(ALL([dim1, dim2, dim3] .EQ. expected), "sizes written in ans")

isok = .TRUE.
DO i3 = 1, 2
  DO i2 = 1, 3
    DO i1 = 1, 2
      want = 2.0_DFP + 3.0_DFP * a(i1) * b(i2) * c(i3)
      isok = isok .AND. (ABS(ans(i1, i2, i3) - want) .LT. tol)
    END DO
  END DO
END DO
CALL OK(isok, "leading block of ans")
CALL OK(ABS(ans(2, 3, 2) - 38.0_DFP) .LT. tol, "last entry")

! entries outside the leading block are not changed
isok = .TRUE.
isok = isok .AND. ALL(ABS(ans(3, :, :) - 1.0_DFP) .LT. tol)
isok = isok .AND. ALL(ABS(ans(:, 4, :) - 1.0_DFP) .LT. tol)
isok = isok .AND. ALL(ABS(ans(:, :, 3) - 1.0_DFP) .LT. tol)
CALL OK(isok, "rest of ans is unchanged")

! with anscoeff = 0 and scale = 1 we get OuterProd
CALL OuterProd_(a=a, b=b, c=c, anscoeff=0.0_DFP, scale=1.0_DFP, &
                ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
CALL OK(ALL(ABS(ans(1:2, 1:3, 1:2) - OuterProd(a, b, c)) &
        .LT. tol), "same as OuterProd")

END PROGRAM main
