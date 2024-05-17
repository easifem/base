! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>

! the implementation of deig, zeig, deigvals, and zeigvals are copied from
! linalg.f90 available in https://github.com/certik/fortran-utils
! and are modified to suit the needs of EASIFEM library

SUBMODULE(GE_EigenValueMethods) Methods
USE BaseMethod, ONLY: ErrorMsg, LA_GEEV, stderr, tostring, &
                      display
USE AssertUtility
IMPLICIT NONE
COMPLEX(DFP), PARAMETER :: i_ = (0, 1)
CONTAINS

!----------------------------------------------------------------------------
!                                                                 getEig
!----------------------------------------------------------------------------

MODULE PROCEDURE deig
! LAPACK variables for DGEEV:
REAL(DFP), ALLOCATABLE :: At(:, :), vl(:, :), vr(:, :), wi(:), &
                          work(:), wr(:)
INTEGER(I4B) :: info, lda, ldvl, ldvr, lwork, n, i

CHARACTER(*), PARAMETER :: myName = "deig"

lda = SIZE(A(:, 1))
n = SIZE(A(1, :))
! CALL Assert(Mat=A, s=[n, n], msg="[ARG ERROR] :: A should be square", &
!             file=__FILE__, line=__LINE__, routine=myName)
! CALL Assert(n1=SIZE(lam), n2=n, msg="[ARG ERROR] :: size of lam should be "// &
!             "equal to "//tostring(n), file=__FILE__, line=__LINE__, &
!             routine=myName)
! CALL Assert(mat=c, s=[n, n], msg="[ARG ERROR] :: shape of c should be"// &
!             "the same as one of A", file=__FILE__, line=__LINE__, &
!             routine=myName)
ldvl = n
ldvr = n
lwork = 8 * n ! TODO: can this size be optimized? query first?
ALLOCATE (At(lda, n), wr(n), wi(n), vl(ldvl, n), vr(ldvr, n), &
          work(lwork))
At = A

CALL LA_GEEV('N', 'V', n, At, lda, wr, wi, vl, ldvl, vr, ldvr, &
             work, lwork, info)
IF (info .NE. 0) CALL geevErrorMsg(info, n)

lam = wr + i_ * wi
! as DGEEV has a rather complicated way of returning the eigenvectors,
! it is necessary to build the complex array of eigenvectors from
! two real arrays:
DO i = 1, n
  IF (wi(i) > 0.0) THEN ! first of two conjugate eigenvalues
    c(:, i) = vr(:, i) + i_ * vr(:, i + 1)
  ELSEIF (wi(i) < 0.0_DFP) THEN ! second of two conjugate eigenvalues
    c(:, i) = vr(:, i - 1) - i_ * vr(:, i)
  ELSE
    c(:, i) = vr(:, i)
  END IF
END DO

END PROCEDURE deig

!----------------------------------------------------------------------------
!                                                                 getEig
!----------------------------------------------------------------------------

MODULE PROCEDURE zeig
! LAPACK variables:
INTEGER(I4B) :: info, lda, ldvl, ldvr, lwork, n, lrwork
REAL(DFP), ALLOCATABLE :: rwork(:)
COMPLEX(DFP), ALLOCATABLE :: vl(:, :), vr(:, :), work(:)

CHARACTER(*), PARAMETER :: myName = "zeig"

lda = SIZE(A(:, 1))
n = SIZE(A(1, :))
! CALL Assert(Mat=A, s=[n, n], msg="[ARG ERROR] :: A should be square", &
!             file=__FILE__, line=__LINE__, routine=myName)
! CALL Assert(n1=SIZE(lam), n2=n, msg="[ARG ERROR] :: size of lam should be "// &
!             "equal to "//tostring(n), file=__FILE__, line=__LINE__, &
!             routine=myName)
! CALL Assert(mat=c, s=[n, n], msg="[ARG ERROR] :: shape of c should be"// &
!             "the same as one of A", file=__FILE__, line=__LINE__, &
!             routine=myName)
ldvl = n
ldvr = n
lwork = 8 * n ! TODO: can this size be optimized? query first?
lrwork = 2 * n
ALLOCATE (vl(ldvl, n), vr(ldvr, n), work(lwork), rwork(lrwork))
c = A
CALL LA_GEEV('N', 'V', n, c, lda, lam, vl, ldvl, vr, ldvr, work, &
             lwork, rwork, info)
IF (info .NE. 0) CALL geevErrorMsg(info, n)
c = vr

END PROCEDURE zeig

!----------------------------------------------------------------------------
!                                                                 getEigVals
!----------------------------------------------------------------------------

MODULE PROCEDURE deigvals
! LAPACK variables for DGEEV:
REAL(DFP), ALLOCATABLE :: At(:, :), vl(:, :), vr(:, :), wi(:), work(:), wr(:)
INTEGER :: info, lda, ldvl, ldvr, lwork, n

CHARACTER(*), PARAMETER :: myName = "deigvals"

lda = SIZE(A(:, 1))
n = SIZE(A(1, :))
! CALL assert_shape(A, [n, n], "solve", "A")
ldvl = n
ldvr = n
lwork = 8 * n ! TODO: can this size be optimized? query first?
ALLOCATE (At(lda, n), wr(n), wi(n), vl(ldvl, n), &
          vr(ldvr, n), work(lwork))
At = A

CALL LA_GEEV('N', 'N', n, At, lda, wr, wi, vl, ldvl, vr, ldvr, &
             work, lwork, info)
IF (info .NE. 0) CALL geevErrorMsg(info, n)

lam = wr + i_ * wi
END PROCEDURE deigvals

!----------------------------------------------------------------------------
!                                                                getEigVals_2
!----------------------------------------------------------------------------

MODULE PROCEDURE zeigvals
! LAPACK variables:
INTEGER :: info, lda, ldvl, ldvr, lwork, n, lrwork
REAL(DFP), ALLOCATABLE :: rwork(:)
COMPLEX(DFP), ALLOCATABLE :: At(:, :), vl(:, :), vr(:, :), work(:)

lda = SIZE(A(:, 1))
n = SIZE(A(1, :))
! CALL assert_shape(A, [n, n], "solve", "A")
ldvl = n
ldvr = n
lwork = 8 * n ! TODO: can this size be optimized? query first?
lrwork = 2 * n
ALLOCATE (At(lda, n), vl(ldvl, n), vr(ldvr, n), &
          work(lwork), rwork(lrwork))
At = A
CALL LA_GEEV('N', 'N', n, At, lda, lam, vl, ldvl, vr, ldvr, work, &
             lwork, rwork, info)
IF (info .NE. 0) CALL geevErrorMsg(info, n)

END PROCEDURE zeigvals

!----------------------------------------------------------------------------
!                                                                geevErrorMsg
!----------------------------------------------------------------------------

SUBROUTINE geevErrorMsg(info, n)
  INTEGER(I4B), INTENT(IN) :: info, n

  CALL Display(info, "LA_GEEV returned info = ")
  IF (info .LT. 0) THEN
    CALL display("The "//tostring(-info)//"-th argument "// &
                 "had an illegal value.")
  ELSE
    CALL display("The QR algorithm failed to compute all the")
    CALL display("eigenvalues, and no eigenvectors have been computed;")
    CALL display("elements "//tostring(info + 1)//":"//tostring(n)// &
                 " of WR and WI contain eigenvalues which converged.")
  END IF
  CALL ErrorMsg( &
    & msg="ERROR IN LA_GEEV", &
    & file=__FILE__, &
    & line=__LINE__, &
    & routine="zeigvals", &
    & unitno=stderr)
  STOP
END SUBROUTINE geevErrorMsg

END SUBMODULE Methods
