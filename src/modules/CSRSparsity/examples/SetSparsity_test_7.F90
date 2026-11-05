!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Timing of SetSparsity(obj, row(:), col(:)) on a Hex8 mesh
!
! Description:
!   Mimics the node-to-node path of AbstractMesh: a structured ne**3
!   Hex8 mesh, a 3-component DOF in FMT_NODES, and one
!   SetSparsity(obj, row(:), col(:)) call where col(i) holds the nodes
!   connected to node i (itself included, no repeats). Prints the time
!   spent in that call and in the final SetSparsity(obj) call, and checks
!   nnz against the exact value (3*ne + 1)**3 * tdof**2,
!   IA(nrow+1) - 1 = nnz, and that JA is strictly increasing inside
!   every row.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: DOF_
USE BaseType, ONLY: IntVector_
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

INTEGER(I4B), PARAMETER :: ne = 40
!! number of elements in each direction
!! ne = 40: 68921 nodes, 206763 rows, nnz = 15,912,121
INTEGER(I4B), PARAMETER :: tdof = 3
!! number of components per node
INTEGER(I4B), PARAMETER :: tnodes = (ne + 1)**3
INTEGER(I4B), PARAMETER :: nrow = tnodes * tdof

TYPE(CSRSparsity_) :: csr
TYPE(DOF_) :: dof
TYPE(IntVector_), ALLOCATABLE :: n2n(:)
INTEGER(I4B), ALLOCATABLE :: row(:)
INTEGER(I4B) :: i, ii, expected
INTEGER(INT64) :: c0, c1, c2, rate
LOGICAL :: isSorted

CALL NodeToNodes(n2n)
row = [(i, i=1, tnodes)]

CALL Initiate(obj=dof, tNodes=[tnodes], names=["U"], &
              spacecompo=[tdof], timecompo=[1_I4B], &
              storagefmt=FMT_NODES)
CALL Initiate(obj=csr, ncol=nrow, nrow=nrow, idof=dof, jdof=dof)

CALL SYSTEM_CLOCK(count=c0, count_rate=rate)
CALL SetSparsity(obj=csr, row=row, col=n2n)
CALL SYSTEM_CLOCK(count=c1)
CALL SetSparsity(csr)
CALL SYSTEM_CLOCK(count=c2)

WRITE (*, "(A, F10.4, A)") "# rows call  : ", &
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
DEALLOCATE (n2n, row)

CONTAINS

!> Nodes connected to each node of a structured Hex8 mesh: all nodes
!> within one step in each direction, the node itself included
SUBROUTINE NodeToNodes(n2n)
  TYPE(IntVector_), ALLOCATABLE, INTENT(OUT) :: n2n(:)
  INTEGER(I4B) :: n1, i, j, k, a, b, c, node, cnt
  INTEGER(I4B) :: buf(27)

  n1 = ne + 1
  ALLOCATE (n2n(tnodes))
  DO k = 1, n1
    DO j = 1, n1
      DO i = 1, n1
        node = i + (j - 1) * n1 + (k - 1) * n1 * n1
        cnt = 0
        DO c = MAX(1, k - 1), MIN(n1, k + 1)
          DO b = MAX(1, j - 1), MIN(n1, j + 1)
            DO a = MAX(1, i - 1), MIN(n1, i + 1)
              cnt = cnt + 1
              buf(cnt) = a + (b - 1) * n1 + (c - 1) * n1 * n1
            END DO
          END DO
        END DO
        n2n(node)%val = buf(1:cnt)
      END DO
    END DO
  END DO
END SUBROUTINE NodeToNodes

END PROGRAM main
! # rows call  : (machine dependent)
! # final call : (machine dependent)
! ok 1 - nnz = (3*ne+1)**3 * tdof**2
! ok 2 - IA(nrow+1) - 1 = nnz
! ok 3 - JA strictly increasing in each row

! ne = 20:
! # rows call  :     0.0057 s
! # final call :     0.0108 s
!
! ne = 40 (3 runs):
! # rows call  :     0.036 - 0.040 s
! # final call :     0.092 - 0.096 s
