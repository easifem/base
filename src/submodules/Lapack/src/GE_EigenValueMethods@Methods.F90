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
USE BaseMethod, ONLY: ErrorMsg, GEEV, stderr, stdout, tostring, &
                      Display
USE AssertUtility
IMPLICIT NONE
COMPLEX(DFPC), PARAMETER :: i_ = (0.0_DFP, 1.0_DFP)
CONTAINS

!----------------------------------------------------------------------------
!                                                                 getEig
!----------------------------------------------------------------------------

MODULE PROCEDURE deig
! LAPACK variables for DGEEV:
REAL(DFP), ALLOCATABLE :: At(:, :), vr(:, :), wi(:), wr(:)
INTEGER(I4B) :: info, lda, ldvr, n, i

CHARACTER(*), PARAMETER :: myName = "deig"

lda = SIZE(A, 1)
n = SIZE(A, 2)
! CALL Assert(Mat=A, s=[n, n], msg="[ARG ERROR] :: A should be square", &
!             file=__FILE__, line=__LINE__, routine=myName)
! CALL Assert(n1=SIZE(lam), n2=n, msg="[ARG ERROR] :: size of lam should be "// &
!             "equal to "//tostring(n), file=__FILE__, line=__LINE__, &
!             routine=myName)
! CALL Assert(mat=c, s=[n, n], msg="[ARG ERROR] :: shape of c should be"// &
!             "the same as one of A", file=__FILE__, line=__LINE__, &
!             routine=myName)
ldvr = n
ALLOCATE (At(lda, n), wr(n), wi(n), vr(ldvr, n))
At = A

CALL GEEV(A=At, WR=wr, WI=wi, VR=vr, INFO=info)
IF (info .NE. 0) CALL GeevErrorMsg(info, n)

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
INTEGER(I4B) :: info, ldvr, n
REAL(DFP), ALLOCATABLE :: rwork(:)
COMPLEX(DFPC), ALLOCATABLE :: vr(:, :)

CHARACTER(*), PARAMETER :: myName = "zeig"

n = SIZE(A, 2)
! CALL Assert(Mat=A, s=[n, n], msg="[ARG ERROR] :: A should be square", &
!             file=__FILE__, line=__LINE__, routine=myName)
! CALL Assert(n1=SIZE(lam), n2=n, msg="[ARG ERROR] :: size of lam should be "// &
!             "equal to "//tostring(n), file=__FILE__, line=__LINE__, &
!             routine=myName)
! CALL Assert(mat=c, s=[n, n], msg="[ARG ERROR] :: shape of c should be"// &
!             "the same as one of A", file=__FILE__, line=__LINE__, &
!             routine=myName)
ldvr = n
ALLOCATE (vr(ldvr, n))
c = A
CALL GEEV(A=c, W=lam, VR=vr, INFO=info)
IF (info .NE. 0) CALL GeevErrorMsg(info, n)
c = vr

END PROCEDURE zeig

!----------------------------------------------------------------------------
!                                                                 getEigVals
!----------------------------------------------------------------------------

MODULE PROCEDURE deigvals
! LAPACK variables for DGEEV:
REAL(DFP), ALLOCATABLE :: At(:, :), wi(:), wr(:)
INTEGER(I4B) :: info, lda, ldvr, n, i

CHARACTER(*), PARAMETER :: myName = "deigvals"

lda = SIZE(A, 1)
n = SIZE(A, 2)
ldvr = n
ALLOCATE (At(lda, n), wr(n), wi(n))
At = A

CALL GEEV(A=At, WR=wr, WI=wi, INFO=info)
IF (info .NE. 0) CALL GeevErrorMsg(info, n)

lam = wr + i_ * wi
END PROCEDURE deigvals

!----------------------------------------------------------------------------
!                                                                getEigVals_2
!----------------------------------------------------------------------------

MODULE PROCEDURE zeigvals
! LAPACK variables:
INTEGER(I4B) :: info, lda, n
COMPLEX(DFPC), ALLOCATABLE :: At(:, :)

CHARACTER(*), PARAMETER :: myName = "zeigvals"

lda = SIZE(A, 1)
n = SIZE(A, 2)
ALLOCATE (At(lda, n))
At = A
CALL GEEV(A=At, W=lam, INFO=info)
IF (info .NE. 0) CALL GeevErrorMsg(info, n)

END PROCEDURE zeigvals

!----------------------------------------------------------------------------
!                                                                geevErrorMsg
!----------------------------------------------------------------------------

SUBROUTINE GeevErrorMsg(info, n)
  INTEGER(I4B), INTENT(IN) :: info, n

  CALL Display(info, "LA_GEEV returned info = ", unitno=stdout)
  IF (info .LT. 0) THEN
    CALL Display("The "//tostring(-info)//"-th argument "// &
                 "had an illegal value.", unitno=stderr)
  ELSE
    CALL Display("The QR algorithm failed to compute all the", unitno=stderr)
    CALL Display("eigenvalues, and no eigenvectors have been computed;", &
                 unitno=stderr)
    CALL Display("elements "//tostring(info + 1)//":"//tostring(n)// &
                 " of WR and WI contain eigenvalues which converged.", &
                 unitno=stderr)
  END IF
  CALL ErrorMsg( &
    & msg="ERROR IN LA_GEEV", &
    & file=__FILE__, &
    & line=__LINE__, &
    & routine="zeigvals", &
    & unitno=stderr)
  STOP
END SUBROUTINE GeevErrorMsg

END SUBMODULE Methods
