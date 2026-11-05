!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Timing of SetSparsity(obj, row, col) on a Hex8 mesh
!
! Description:
!   Mimics the FEDOF element loop: a structured ne**3 Hex8 mesh with a
!   3-component DOF in FMT_NODES, and one SetSparsity(row, col=conn)
!   call per element node. Prints the time spent in the row loop and in
!   the final SetSparsity(obj) call, and checks nnz against the exact
!   value (3*ne + 1)**3 * tdof**2, IA(nrow+1) - 1 = nnz, and that JA is
!   strictly increasing inside every row.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: DOF_
USE CSRSparsity_Method, ONLY: Initiate
USE CSRSparsity_Method, ONLY: SetSparsity
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE DOF_Method, ONLY: Initiate
USE DOF_Method, ONLY: DEALLOCATE
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: FMT_NODES
USE ISO_FORTRAN_ENV, ONLY: INT64
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: ne = 20
!! number of elements in each direction
INTEGER(I4B), PARAMETER :: tdof = 3
!! number of components per node
INTEGER(I4B), PARAMETER :: tnodes = (ne + 1)**3
INTEGER(I4B), PARAMETER :: nrow = tnodes * tdof

TYPE(CSRSparsity_) :: csr
TYPE(DOF_) :: dof
INTEGER(I4B) :: conn(8), i, j, k, ii, expected
INTEGER(INT64) :: c0, c1, c2, rate
LOGICAL :: isSorted

CALL Initiate(obj=dof, tNodes=[tnodes], names=["U"], &
              spacecompo=[tdof], timecompo=[1_I4B], &
              storagefmt=FMT_NODES)
CALL Initiate(obj=csr, ncol=nrow, nrow=nrow, idof=dof, jdof=dof)

CALL SYSTEM_CLOCK(count=c0, count_rate=rate)

DO k = 1, ne
  DO j = 1, ne
    DO i = 1, ne
      CALL HexConn(i, j, k, conn)
      DO ii = 1, 8
        CALL SetSparsity(obj=csr, row=conn(ii), col=conn)
      END DO
    END DO
  END DO
END DO

CALL SYSTEM_CLOCK(count=c1)
CALL SetSparsity(csr)
CALL SYSTEM_CLOCK(count=c2)

WRITE (*, "(A, F10.4, A)") "# row loop   : ", &
  REAL(c1 - c0, DFP) / REAL(rate, DFP), " s"
WRITE (*, "(A, F10.4, A)") "# final call : ", &
  REAL(c2 - c1, DFP) / REAL(rate, DFP), " s"

expected = (3 * ne + 1)**3 * tdof**2
CALL OK(csr%nnz .EQ. expected, "nnz = (3*ne+1)**3 * tdof**2")
CALL OK(csr%IA(nrow + 1) - 1 .EQ. csr%nnz, "IA(nrow+1) - 1 = nnz")

isSorted = .TRUE.
DO i = 1, nrow
  DO ii = csr%IA(i) + 1, csr%IA(i + 1) - 1
    IF (csr%JA(ii) .LE. csr%JA(ii - 1)) isSorted = .FALSE.
  END DO
END DO
CALL OK(isSorted, "JA strictly increasing in each row")

CALL DEALLOCATE (csr)
CALL DEALLOCATE (dof)

CONTAINS

!> Node numbers of Hex8 element (i, j, k) of a structured mesh
SUBROUTINE HexConn(i, j, k, conn)
  INTEGER(I4B), INTENT(IN) :: i, j, k
  INTEGER(I4B), INTENT(OUT) :: conn(8)
  INTEGER(I4B) :: n1, n2

  n1 = ne + 1
  n2 = n1 * n1
  conn(1) = i + (j - 1) * n1 + (k - 1) * n2
  conn(2) = conn(1) + 1
  conn(3) = conn(2) + n1
  conn(4) = conn(1) + n1
  conn(5:8) = conn(1:4) + n2
END SUBROUTINE HexConn

END PROGRAM main
! # row loop   : (machine dependent)
! # final call : (machine dependent)
! ok 1 - nnz = (3*ne+1)**3 * tdof**2
! ok 2 - IA(nrow+1) - 1 = nnz
! ok 3 - JA strictly increasing in each row
