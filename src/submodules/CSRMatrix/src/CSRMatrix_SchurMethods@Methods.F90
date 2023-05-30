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
!

!> author: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This submodule contains the methods for sparse matrix

SUBMODULE(CSRMatrix_SchurMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   AMatvec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_AMatvec
REAL(DFP), ALLOCATABLE :: dummyVec(:)
INTEGER(I4B) :: m, n

n = SIZE(B, 1)
m = SIZE(B, 2)

IF (n .NE. SIZE(A, 1) &
  & .OR. SIZE(A, 2) .NE. n &
  & .OR. SIZE(x) .NE. m &
  & .OR. SIZE(y) .NE. m) THEN
  CALL Errormsg( &
    & msg="Shape of A and B are not compatible", &
    & file=__FILE__, &
    & routine="csrMat_AMatvec()", &
    & line=__LINE__, &
    & unitno=stderr)
  STOP
END IF

ALLOCATE (dummyVec(n))

CALL MatVec(obj=B, x=x, y=dummyVec, isTranspose=.FALSE.)
CALL LinSolve(A=A, B=dummyVec, isTranspose=.FALSE., isFactored=.TRUE.)
CALL MatVec(obj=B, x=dummyVec, y=y, isTranspose=.TRUE.)

DEALLOCATE (dummyVec)

END PROCEDURE csrMat_AMatvec

!----------------------------------------------------------------------------
!                                                                   AtMatvec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_AtMatVec
REAL(DFP), ALLOCATABLE :: dummyVec(:)
INTEGER(I4B) :: m, n
LOGICAL(LGT) :: isASym0

n = SIZE(B, 1)
m = SIZE(B, 2)

IF (n .NE. SIZE(A, 1) &
  & .OR. SIZE(A, 2) .NE. n &
  & .OR. SIZE(x) .NE. m &
  & .OR. SIZE(y) .NE. m) THEN
  CALL Errormsg( &
    & msg="Shape of A and B are not compatible", &
    & file=__FILE__, &
    & routine="csrMat_AtMatvec()", &
    & line=__LINE__, &
    & unitno=stderr)
  STOP
END IF

isASym0 = Input(option=isASym, default=.FALSE.)

ALLOCATE (dummyVec(n))

CALL MatVec(obj=B, x=x, y=dummyVec, isTranspose=.FALSE.)

CALL LinSolve( &
  & A=A, &
  & B=dummyVec, &
  & isTranspose=(.NOT. isASym0), &
  & isFactored=.TRUE.)

CALL MatVec( &
  & obj=B, &
  & x=dummyVec, &
  & y=y, &
  & isTranspose=.TRUE.)

DEALLOCATE (dummyVec)

END PROCEDURE csrMat_AtMatVec

!----------------------------------------------------------------------------
!                                                                     MatVec
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_SchurMatVec
LOGICAL(LGT) :: trans
trans = INPUT(option=isTranspose, default=.FALSE.)
IF (trans) THEN
  CALL csrMat_AtMatvec(A=A, B=B, x=x, y=y, isASym=isASym)
ELSE
  CALL csrMat_AMatvec(A=A, B=B, x=x, y=y)
END IF
END PROCEDURE csrMat_SchurMatVec

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SymSchurLargestEigenVal1
CHARACTER(*), PARAMETER :: myName = "SymSchurLargestEigenVal1"
INTEGER(I4B), PARAMETER :: nev = 1
INTEGER(I4B) :: ido, lworkl, ldv, info, iparam(11), ipntr(11), maxIter0, n, &
  & ncv0
CHARACTER(1), PARAMETER :: bmat = "I"
CHARACTER(2) :: which0
REAL(DFP) :: tol0, d(nev), sigma
REAL(DFP), ALLOCATABLE :: resid(:), v(:, :), workd(:), workl(:)
TYPE(String) :: err_msg
LOGICAL(LGT), ALLOCATABLE :: SELECT(:)
!
! int scalar
!
which0 = INPUT(default="LA", option=which)
n = SIZE(B, 2)
ncv0 = input(default=MIN(20_I4B, n), option=ncv)
lworkl = ncv0 * (ncv0 + 8)
ALLOCATE (resid(n), v(n, ncv0), workd(3 * n), workl(lworkl), SELECT(ncv0))
ldv = SIZE(v, 1)
ido = 0
info = 0
maxIter0 = INPUT(option=maxIter, default=10 * n)
tol0 = INPUT(option=tol, default=zero)
!
! iparam
!
iparam(1) = 1 !! ishift
iparam(3) = maxIter0 !! maxiter
iparam(4) = 1 !! nb
iparam(7) = 1 !! mode
iparam(2) = 0 !! deprecated
iparam(5) = 0 !! out
iparam(6) = 0 !! iupd, deprecated
iparam(8) = 0 !! np, na
iparam(9:11) = 0 !! OUT
ipntr = 0
!
DO
  CALL F77_SAUPD( &
    & ido=ido, bmat=bmat, n=n, which=which0, nev=nev, &
    & tol=tol0, resid=resid, ncv=ncv0, v=v, ldv=ldv, &
    & iparam=iparam, ipntr=ipntr, workd=workd, workl=workl,  &
    & lworkl=lworkl, info=info)
  !
  IF (info .NE. 0) THEN
    err_msg = SAUPD_ErrorMsg(info)
    CALL Display(err_msg, msg="", unitno=stdout)
    ans = 0.0_DFP
    EXIT
  END IF
  !
  IF (ido .EQ. -1 .OR. ido .EQ. 1) THEN
    !
    ! Perform MatVec Mult
    ! y = MATMUL(mat, X)
    ! x => WORKD(ipntr(1):ipntr(1)+N-1)
    ! y => WORKD(ipntr(2):ipntr(2)+N-1)
    !
    CALL SchurMatVec( &
      & A=A, &
      & B=B, &
      & x=workd(ipntr(1):ipntr(1) + n - 1), &
      & y=workd(ipntr(2):ipntr(2) + n - 1), &
      & isTranspose=.FALSE., &
      & isASym=.TRUE.)
    !
  ELSE
    EXIT
  END IF
END DO
!
! we are not getting rvec, therefore ldz=1,
! othereise ldz = N
!
CALL F77_SEUPD( &
  & rvec=.FALSE., howmny='All', SELECT=SELECT, &
  & d=d, z=v, ldz=1, sigma=sigma, &
  & bmat=bmat, n=n, which=which0, nev=nev, tol=tol0, &
  & resid=resid, ncv=ncv0, v=v, ldv=ldv, &
  & iparam=iparam, ipntr=ipntr, workd=workd, &
  & workl=workl, lworkl=lworkl, info=info)
!
IF (info .NE. 0) THEN
  err_msg = SEUPD_ErrorMsg(INFO)
  CALL Display(err_msg, msg="", unitno=stdout)
  ans = 0.0_DFP
ELSE
  ans = d(1)
END IF
!
DEALLOCATE (resid, v, workd, workl, SELECT)
!
END PROCEDURE SymSchurLargestEigenVal1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SymSchurLargestEigenVal2
CHARACTER(*), PARAMETER :: myName = "SymSchurLargestEigenVal2"
INTEGER(I4B) :: ido, lworkl, ldv, info, iparam(11), ipntr(11), maxIter0, n, &
  & ncv0
CHARACTER(1), PARAMETER :: bmat = "I"
CHARACTER(2) :: which0
REAL(DFP) :: tol0, sigma
REAL(DFP), ALLOCATABLE :: resid(:), v(:, :), workd(:), workl(:)
TYPE(String) :: err_msg
LOGICAL(LGT), ALLOCATABLE :: SELECT(:)
!
! int scalar
!
which0 = INPUT(default="LA", option=which)
n = SIZE(B, 2)

ncv0 = input(default=MIN(MAX(20_I4B, 2 * nev + 1), n), option=ncv)
lworkl = ncv0 * (ncv0 + 8)
ALLOCATE (resid(n), v(n, ncv0), workd(3 * n), workl(lworkl), SELECT(ncv0))
ldv = SIZE(v, 1)
ido = 0
info = 0
maxIter0 = INPUT(option=maxIter, default=10 * n)
tol0 = INPUT(option=tol, default=zero)
!
! iparam
!
iparam(1) = 1 !! ishift
iparam(3) = maxIter0 !! maxiter
iparam(4) = 1 !! nb
iparam(7) = 1 !! mode
iparam(2) = 0 !! deprecated
iparam(5) = 0 !! out
iparam(6) = 0 !! iupd, deprecated
iparam(8) = 0 !! np, na
iparam(9:11) = 0 !! OUT
ipntr = 0
!
DO
  CALL F77_SAUPD( &
    & ido=ido, bmat=bmat, n=n, which=which0, nev=nev, &
    & tol=tol0, resid=resid, ncv=ncv0, v=v, ldv=ldv, &
    & iparam=iparam, ipntr=ipntr, workd=workd, workl=workl,  &
    & lworkl=lworkl, info=info)
  !
  IF (info .NE. 0) THEN
    err_msg = SAUPD_ErrorMsg(info)
    CALL Display(err_msg, msg="", unitno=stdout)
    ans = 0.0_DFP
    EXIT
  END IF
  !
  IF (ido .EQ. -1 .OR. ido .EQ. 1) THEN
    !
    ! Perform MatVec Mult
    ! y = MATMUL(mat, X)
    ! x => WORKD(ipntr(1):ipntr(1)+N-1)
    ! y => WORKD(ipntr(2):ipntr(2)+N-1)
    !
    CALL SchurMatVec( &
      & A=A, &
      & B=B, &
      & x=workd(ipntr(1):ipntr(1) + n - 1), &
      & y=workd(ipntr(2):ipntr(2) + n - 1), &
      & isTranspose=.FALSE., &
      & isASym=.TRUE.)
    !
  ELSE
    EXIT
  END IF
END DO
!
! we are not getting rvec, therefore ldz=1,
! othereise ldz = N
!
CALL F77_SEUPD( &
  & rvec=.FALSE., howmny='All', SELECT=SELECT, &
  & d=ans, z=v, ldz=1, sigma=sigma, &
  & bmat=bmat, n=n, which=which0, nev=nev, tol=tol0, &
  & resid=resid, ncv=ncv0, v=v, ldv=ldv, &
  & iparam=iparam, ipntr=ipntr, workd=workd, &
  & workl=workl, lworkl=lworkl, info=info)
!!
IF (info .NE. 0) THEN
  err_msg = SEUPD_ErrorMsg(INFO)
  CALL Display(err_msg, msg="", unitno=stdout)
  ans = 0.0_DFP
END IF
!
DEALLOCATE (resid, v, workd, workl, SELECT)
!
END PROCEDURE SymSchurLargestEigenVal2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
