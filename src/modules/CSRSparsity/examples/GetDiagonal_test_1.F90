!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: GetDiagonal with and without idiag
!
! Description:
!   Uses a 3 by 3 matrix with no entry at (2, 2)
!
!     10  .  20
!     30  .  40
!      . 50  60
!
!   and checks the main diagonal and offset diagonals, the stored
!   positions, and the second call of GetDiagonal(obj, A, diag, offSet),
!   which reuses them.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE CSRSparsity_Method, ONLY: GetDiagonal
USE CSRSparsity_Method, ONLY: Initiate
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: A(6)
INTEGER(I4B), PARAMETER :: IA(4) = [1, 3, 5, 7]
INTEGER(I4B), PARAMETER :: JA(6) = [1, 3, 1, 3, 2, 3]

A = [10.0_DFP, 20.0_DFP, 30.0_DFP, 40.0_DFP, 50.0_DFP, 60.0_DFP]

! Test 1: diag and idiag
BLOCK
  TYPE(CSRSparsity_) :: csr
  REAL(DFP), ALLOCATABLE :: diag(:)
  INTEGER(I4B), ALLOCATABLE :: idiag(:)
  CALL Initiate(obj=csr, IA=IA, JA=JA)

  CALL GetDiagonal(obj=csr, A=A, diag=diag, idiag=idiag)
  CALL OK(ALL(ABS(diag - [10.0_DFP, 0.0_DFP, 60.0_DFP]) .LT. tol), &
          "main: diag")
  CALL OK(ALL(idiag .EQ. [1, 0, 6]), "main: idiag")

  CALL GetDiagonal(obj=csr, A=A, diag=diag, idiag=idiag, offSet=1_I4B)
  CALL OK(ALL(ABS(diag - [0.0_DFP, 40.0_DFP, 0.0_DFP]) .LT. tol), &
          "offSet 1: diag")
  CALL OK(ALL(idiag .EQ. [0, 4, 0]), "offSet 1: idiag")

  CALL GetDiagonal(obj=csr, A=A, diag=diag, idiag=idiag, offSet=-1_I4B)
  CALL OK(ALL(ABS(diag - [0.0_DFP, 30.0_DFP, 50.0_DFP]) .LT. tol), &
          "offSet -1: diag")
  CALL OK(ALL(idiag .EQ. [0, 3, 5]), "offSet -1: idiag")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 2: the first call stores the positions in the object
BLOCK
  TYPE(CSRSparsity_) :: csr
  REAL(DFP), ALLOCATABLE :: diag(:)
  INTEGER(I4B), PARAMETER :: IA1(4) = [1, 3, 4, 6]
  INTEGER(I4B), PARAMETER :: JA1(5) = [1, 3, 2, 1, 3]
  REAL(DFP) :: A1(5)
  A1 = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP]
  CALL Initiate(obj=csr, IA=IA1, JA=JA1)
  CALL OK(.NOT. csr%isDiagStored, "stored: not before first call")

  CALL GetDiagonal(obj=csr, A=A1, diag=diag)
  CALL OK(csr%isDiagStored, "stored: after first call")
  CALL OK(ALL(csr%idiag .EQ. [1, 3, 5]), "stored: idiag")
  CALL OK(ALL(ABS(diag - [1.0_DFP, 3.0_DFP, 5.0_DFP]) .LT. tol), &
          "stored: diag, first call")

  A1 = 2.0_DFP * A1
  CALL GetDiagonal(obj=csr, A=A1, diag=diag)
  CALL OK(ALL(ABS(diag - [2.0_DFP, 6.0_DFP, 10.0_DFP]) .LT. tol), &
          "stored: diag, second call")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 3: another diagonal, and a row with no entry
BLOCK
  TYPE(CSRSparsity_) :: csr
  REAL(DFP), ALLOCATABLE :: diag(:)
  CALL Initiate(obj=csr, IA=IA, JA=JA)
  CALL GetDiagonal(obj=csr, A=A, diag=diag, offSet=-1_I4B)
  CALL GetDiagonal(obj=csr, A=A, diag=diag, offSet=1_I4B)
  CALL OK(ALL(ABS(diag - [0.0_DFP, 40.0_DFP, 0.0_DFP]) .LT. tol), &
          "second offSet: diag")
  CALL OK(.NOT. csr%isDiagStored, "second offSet: nothing is stored")
  CALL GetDiagonal(obj=csr, A=A, diag=diag)
  CALL GetDiagonal(obj=csr, A=A, diag=diag)
  CALL OK(ALL(ABS(diag - [10.0_DFP, 0.0_DFP, 60.0_DFP]) .LT. tol), &
          "stored: row with no entry")
  CALL DEALLOCATE (csr)
END BLOCK

END PROGRAM main
