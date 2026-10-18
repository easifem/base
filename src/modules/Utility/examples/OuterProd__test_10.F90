!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: OuterProd_ of rank-2, rank-1 and rank-1 arrays

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE ProductUtility, ONLY: OuterProd
USE ProductUtility, ONLY: OuterProd_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B), PARAMETER :: expected(4) = [2, 3, 2, 3]
REAL(DFP) :: a(2, 3), b(2), c(3)
REAL(DFP) :: ans(3, 4, 3, 4)
REAL(DFP) :: want
INTEGER(I4B) :: dim1, dim2, dim3, dim4
INTEGER(I4B) :: ii, i1, i2, i3, i4
LOGICAL(LGT) :: isok

a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
b = [(REAL(ii, DFP), ii=1, 2)]
c = [(REAL(ii, DFP), ii=1, 3)]

! ans is one larger than needed in every dimension
ans = 1.0_DFP

! ans = 2 * ans + 3 * (outer product)
CALL OuterProd_(a=a, b=b, c=c, anscoeff=2.0_DFP, scale=3.0_DFP, &
                ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)

CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. expected), "sizes written in ans")

isok = .TRUE.
DO i4 = 1, 3
  DO i3 = 1, 2
    DO i2 = 1, 3
      DO i1 = 1, 2
        want = 2.0_DFP + 3.0_DFP * a(i1, i2) * b(i3) * c(i4)
        isok = isok .AND. (ABS(ans(i1, i2, i3, i4) - want) .LT. tol)
      END DO
    END DO
  END DO
END DO
CALL OK(isok, "leading block of ans")
CALL OK(ABS(ans(2, 3, 2, 3) - 110.0_DFP) .LT. tol, "last entry")

! entries outside the leading block are not changed
isok = .TRUE.
isok = isok .AND. ALL(ABS(ans(3, :, :, :) - 1.0_DFP) .LT. tol)
isok = isok .AND. ALL(ABS(ans(:, 4, :, :) - 1.0_DFP) .LT. tol)
isok = isok .AND. ALL(ABS(ans(:, :, 3, :) - 1.0_DFP) .LT. tol)
isok = isok .AND. ALL(ABS(ans(:, :, :, 4) - 1.0_DFP) .LT. tol)
CALL OK(isok, "rest of ans is unchanged")

! with anscoeff = 0 and scale = 1 we get OuterProd
CALL OuterProd_(a=a, b=b, c=c, anscoeff=0.0_DFP, scale=1.0_DFP, &
                ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
CALL OK(ALL(ABS(ans(1:2, 1:3, 1:2, 1:3) - OuterProd(a, b, c)) &
        .LT. tol), "same as OuterProd")

END PROGRAM main
