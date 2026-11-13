!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: OTimesTilda of two matrices

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE ProductUtility, ONLY: OTimesTilda
USE ProductUtility, ONLY: OTimesTilda_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a(2, 2), b(2, 3), ans(5, 7), want
INTEGER(I4B) :: nrow, ncol, ii, jj, pp, qq
LOGICAL(LGT) :: isok

! a = [1 2; 3 4], b = [1 2 3; 4 5 6]
a = RESHAPE([1.0_DFP, 3.0_DFP, 2.0_DFP, 4.0_DFP], [2, 2])
b = RESHAPE([1.0_DFP, 4.0_DFP, 2.0_DFP, 5.0_DFP, 3.0_DFP, 6.0_DFP], &
            [2, 3])

! ans is larger than needed
ans = 1.0_DFP
CALL OTimesTilda(a=a, b=b, ans=ans, nrow=nrow, ncol=ncol, &
                 anscoeff=0.0_DFP, scale=1.0_DFP)

CALL OK(nrow .EQ. 4_I4B, "nrow = SIZE(a, 1) * SIZE(b, 1)")
CALL OK(ncol .EQ. 6_I4B, "ncol = SIZE(a, 2) * SIZE(b, 2)")

! ans((ii-1)*2+pp, (jj-1)*3+qq) = a(ii,jj) * b(pp,qq)
isok = .TRUE.
DO jj = 1, 2
  DO ii = 1, 2
    DO qq = 1, 3
      DO pp = 1, 2
        want = a(ii, jj) * b(pp, qq)
        isok = isok .AND. &
               (ABS(ans((ii - 1) * 2 + pp, (jj - 1) * 3 + qq) - want) &
                .LT. tol)
      END DO
    END DO
  END DO
END DO
CALL OK(isok, "block structure of the Kronecker product")

! entries computed by hand
CALL OK(ABS(ans(1, 4) - 2.0_DFP) .LT. tol, "ans(1,4) = a(1,2) * b(1,1)")
CALL OK(ABS(ans(4, 6) - 24.0_DFP) .LT. tol, "ans(4,6) = a(2,2) * b(2,3)")

! entries outside ans(1:4, 1:6) are not changed
CALL OK(ALL(ABS(ans(5, :) - 1.0_DFP) .LT. tol) .AND. &
        ALL(ABS(ans(:, 7) - 1.0_DFP) .LT. tol), "rest of ans unchanged")

! ans = 1 * ans + 2 * (a kron b), called through the alias OTimesTilda_
CALL OTimesTilda_(a=a, b=b, ans=ans, nrow=nrow, ncol=ncol, &
                  anscoeff=1.0_DFP, scale=2.0_DFP)
CALL OK(ABS(ans(4, 6) - 72.0_DFP) .LT. tol, "anscoeff and scale")

END PROGRAM main
