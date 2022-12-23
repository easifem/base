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

SUBMODULE(ARPACK_SAUPD) Methods
USE BaseMethod, ONLY: ErrorMsg, Input, F77_SAUPD, F77_SEUPD, Display, &
  & GetLU, LUSolve
USE GlobalData, ONLY: stdout, zero

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

MODULE PROCEDURE SAUPD_ErrorMsg
SELECT CASE (INFO)
CASE (1)
  ans = "ERROR [from SAUPD]: : Maximum number of iterations reached"
CASE (2)
  ans = "ERROR [from SAUPD]: : No longer an informational error. &
    & Deprecated starting with release 2 of ARPACK."
CASE (3)
  ans = "ERROR [from SAUPD]: : No shifts could be applied during implicit, &
    &  Arnoldi update, try increasing NCV."
CASE (-1)
  ans = "ERROR [from SAUPD]: : N must be positive."
CASE (-2)
  ans = "ERROR [from SAUPD]: : NEV must be positive."
CASE (-3)
  ans = "ERROR [from SAUPD]: : NCV must be greater than NEV and less &
    & than or equal to N."
CASE (-4)
  ans = "ERROR [from SAUPD]: : The maximum number of Arnoldi update &
    & iterations allowed must be greater than zero."
CASE (-5)
  ans = "ERROR [from SAUPD]: : WHICH must be one of 'LM', 'SM', &
    & 'LA', 'SA', 'BE'"
CASE (-6)
  ans = "ERROR [from SAUPD]: : BMAT must be one of 'I' or 'G'."
CASE (-7)
  ans = "ERROR [from SAUPD]: : Length of private work array WORKL &
    & is not sufficient."
CASE (-8)
  ans = "ERROR [from SAUPD]: : Error return from trid. &
    & eigenvalue calculation. Informatinal &
    & error from LAPACK routine SSTEQR."
CASE (-9)
  ans = "ERROR [from SAUPD]: : Starting vector is zero."
CASE (-10)
  ans = "ERROR [from SAUPD]: : IPARAM(7) must be 1"
CASE (-11)
  ans = "ERROR [from SAUPD]: : IPARAM(7) = 1 and BMAT = 'G' &
    & are incompatible."
CASE (-12)
  ans = "ERROR [from SAUPD]: : IPARAM(1) must be equal to 0 or 1."
CASE (-13)
  ans = "ERROR [from SAUPD]: : NEV and WHICH = 'BE' are incompatible."
CASE (-9999)
  ans = "ERROR [from SAUPD]: : Could not build an Arnoldi factorization. &
    & IPARAM(5) returns the size of the current Arnoldi factorization. &
    & The user is advised to check that enough workspace and &
    & array storage has been allocated."
CASE DEFAULT
  ans = "ERROR [from SAUPD]: : Unknown error has occured!"
END SELECT
END PROCEDURE SAUPD_ErrorMsg

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SEUPD_ErrorMsg
SELECT CASE (INFO)
CASE (-1)
  ans = "ERROR [from SEUPD]: : N must be positive."
CASE (-2)
  ans = "ERROR [from SEUPD]: : NEV must be positive."
CASE (-3)
  ans = "ERROR [from SEUPD]: : NCV must be greater than NEV and less &
    & than or equal to N."
CASE (-5)
  ans = "ERROR [from SEUPD]: : WHICH must be one of 'LM', 'SM', &
    & 'LA', 'SA', 'BE'"
CASE (-6)
  ans = "ERROR [from SEUPD]: : BMAT must be one of 'I' or 'G'."
CASE (-7)
  ans = "ERROR [from SEUPD]: : Length of private work array WORKL &
    & is not sufficient."
CASE (-8)
  ans = "ERROR [from SEUPD]: : Error return from trid. &
    & eigenvalue calculation. Informatinal &
    & error from LAPACK routine SSTEQR."
CASE (-9)
  ans = "ERROR [from SEUPD]: : Starting vector is zero."
CASE (-10)
  ans = "ERROR [from SEUPD]: : IPARAM(7) must be 1"
CASE (-11)
  ans = "ERROR [from SEUPD]: : IPARAM(7) = 1 and BMAT = 'G' &
    & are incompatible."
CASE (-12)
  ans = "ERROR [from SEUPD]: : NEV and WHICH = 'BE' are incompatible."
CASE (-14)
  ans = "ERROR [from SEUPD]: : SSAUPD did not find any eigenvalues &
    & to sufficient accuracy."
CASE (-15)
  ans = "ERROR [from SEUPD]: : HOWMNY must be one of 'A' or 'S' &
    & if RVEC = .true."
CASE (-16)
  ans = "ERROR [from SEUPD]: : HOWMNY = 'S' not yet implemented"
CASE (-17)
  ans = "ERROR [from SEUPD]: : SSEUPD got a different count of the &
    & number of converged Ritz values than SSAUPD got. &
    & This indicates the user probably made an error in &
    & passing data from SSAUPD to SSEUPD or that the data was &
    & modified before entering SSEUPD."
CASE DEFAULT
  ans = "ERROR [from SEUPD]: : Unknown error has occured!"
END SELECT
END PROCEDURE SEUPD_ErrorMsg

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLargestEigenVal1
CHARACTER(LEN=*), PARAMETER :: myName = "SymLargestEigenVal1"
!!
!! Internal variables
!!
INTEGER(I4B), PARAMETER :: nev = 1
INTEGER(I4B) :: ido, lworkl, ldv, info, iparam(11), ipntr(11), maxIter0, n, &
  & ncv0
CHARACTER(LEN=1), PARAMETER :: bmat = "I"
CHARACTER(LEN=2) :: which0
REAL(DFP) :: tol0, d(nev), sigma
REAL(DFP), ALLOCATABLE :: resid(:), v(:, :), workd(:), workl(:)
TYPE(String) :: err_msg
LOGICAL(LGT), ALLOCATABLE :: SELECT(:)
!!
!! int scalar
!!
which0 = INPUT(default="LA", option=which)
n = SIZE(mat, 1)
ncv0 = input(default=MIN(20_I4B, n), option=ncv)
lworkl = ncv0 * (ncv0 + 8)
ALLOCATE (resid(n), v(n, ncv0), workd(3 * n), workl(lworkl), SELECT(ncv0))
ldv = SIZE(v, 1)
ido = 0
info = 0
maxIter0 = INPUT(option=maxIter, default=10 * n)
tol0 = INPUT(option=tol, default=zero)
!!
!! iparam
!!
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
!!
DO
  CALL F77_SAUPD( &
    & ido=ido, bmat=bmat, n=n, which=which0, nev=nev, &
    & tol=tol0, resid=resid, ncv=ncv0, v=v, ldv=ldv, &
    & iparam=iparam, ipntr=ipntr, workd=workd, workl=workl,  &
    & lworkl=lworkl, info=info)
  !!
  IF (info .NE. 0) THEN
    err_msg = SAUPD_ErrorMsg(info)
    CALL Display(err_msg, msg="", unitno=stdout)
    ans = 0.0_DFP
    EXIT
  END IF
  !!
  IF (ido .EQ. -1 .OR. ido .EQ. 1) THEN
    !!
    !! Perform MatVec Mult
    !! y = MATMUL(mat, X)
    !! x => WORKD(ipntr(1):ipntr(1)+N-1)
    !! y => WORKD(ipntr(2):ipntr(2)+N-1)
    !!
    workd(ipntr(2):ipntr(2)+n-1) = MATMUL(mat, workd(ipntr(1):ipntr(1)+n-1))
    !!
  ELSE
    EXIT
  END IF
END DO
!!
!! we are not getting rvec, therefore ldz=1,
!! othereise ldz = N
!!
CALL F77_SEUPD( &
  & rvec=.FALSE., howmny='All', SELECT=SELECT, &
  & d=d, z=v, ldz=1, sigma=sigma, &
  & bmat=bmat, n=n, which=which0, nev=nev, tol=tol0, &
  & resid=resid, ncv=ncv0, v=v, ldv=ldv, &
  & iparam=iparam, ipntr=ipntr, workd=workd, &
  & workl=workl, lworkl=lworkl, info=info)
!!
IF (info .NE. 0) THEN
  err_msg = SEUPD_ErrorMsg(INFO)
  CALL Display(err_msg, msg="", unitno=stdout)
  ans = 0.0_DFP
ELSE
  ans = d(1)
END IF
!!
END PROCEDURE SymLargestEigenVal1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLargestEigenVal2
CHARACTER(LEN=*), PARAMETER :: myName = "SymLargestEigenVal2"
!!
!! Internal variables
INTEGER(I4B) :: ido, lworkl, ldv, info, iparam(11), ipntr(11), maxIter0, n, &
  & ncv0
CHARACTER(LEN=1), PARAMETER :: bmat = "I"
CHARACTER(LEN=2) :: which0
REAL(DFP) :: tol0, sigma
REAL(DFP), ALLOCATABLE :: resid(:), v(:, :), workd(:), workl(:)
TYPE(String) :: err_msg
LOGICAL(LGT), ALLOCATABLE :: SELECT(:)
!!
!! int scalar
!!
which0 = INPUT(default="LA", option=which)
n = SIZE(mat, 1)
ncv0 = input(default=MIN(MAX(20_I4B, 2 * nev + 1), n), option=ncv)
lworkl = ncv0 * (ncv0 + 8)
ALLOCATE (resid(n), v(n, ncv0), workd(3 * n), workl(lworkl), SELECT(ncv0))
ldv = SIZE(v, 1)
ido = 0
info = 0
maxIter0 = INPUT(option=maxIter, default=10 * n)
tol0 = INPUT(option=tol, default=zero)
!!
!! iparam
!!
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
!!
DO
  CALL F77_SAUPD( &
    & ido=ido, bmat=bmat, n=n, which=which0, nev=nev, &
    & tol=tol0, resid=resid, ncv=ncv0, v=v, ldv=ldv, &
    & iparam=iparam, ipntr=ipntr, workd=workd, workl=workl,  &
    & lworkl=lworkl, info=info)
  !!
  IF (info .NE. 0) THEN
    err_msg = SAUPD_ErrorMsg(info)
    CALL Display(err_msg, msg="", unitno=stdout)
    ans = 0.0_DFP
    EXIT
  END IF
  !!
  IF (ido .EQ. -1 .OR. ido .EQ. 1) THEN
    !!
    !! Perform MatVec Mult
    !! y = MATMUL(mat, X)
    !! x => WORKD(ipntr(1):ipntr(1)+N-1)
    !! y => WORKD(ipntr(2):ipntr(2)+N-1)
    !!
    workd(ipntr(2):ipntr(2)+n-1) = MATMUL(mat, workd(ipntr(1):ipntr(1)+n-1))
    !!
  ELSE
    EXIT
  END IF
END DO
!!
!! we are not getting rvec, therefore ldz=1,
!! othereise ldz = N
!!
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
!!
END PROCEDURE SymLargestEigenVal2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SymSmallestEigenVal1
CHARACTER(LEN=*), PARAMETER :: myName = "SymSmallestEigenVal1"
!!
!! Internal variables
!!
INTEGER(I4B), PARAMETER :: nev = 1
INTEGER(I4B) :: ido, lworkl, ldv, info, iparam(11), ipntr(11), maxIter0, n, &
  & ncv0, ii
CHARACTER(LEN=1), PARAMETER :: bmat = "I"
CHARACTER(LEN=2) :: which0
REAL(DFP) :: tol0, d(nev), sigma0
REAL(DFP), ALLOCATABLE :: resid(:), v(:, :), workd(:), workl(:)
TYPE(String) :: err_msg
LOGICAL(LGT), ALLOCATABLE :: SELECT(:)
REAL(DFP) :: mat0(SIZE(mat, 1), SIZE(mat, 2))
INTEGER(I4B) :: ipiv(SIZE(mat, 1))
!!
!! int scalar
!!
sigma0 = INPUT(default=0.0_DFP, option=sigma)
!!
!! note to get smallest value, we transform the problem to
!! find largest value.
!!
IF (PRESENT(which)) THEN
  which0 = "L"//which(2:2)
ELSE
  which0 = "LA"
END IF
!!
n = SIZE(mat, 1)
ncv0 = input(default=MIN(20_I4B, n), option=ncv)
lworkl = ncv0 * (ncv0 + 8)
ALLOCATE (resid(n), v(n, ncv0), workd(3 * n), workl(lworkl), SELECT(ncv0))
ldv = SIZE(v, 1)
ido = 0
info = 0
maxIter0 = INPUT(option=maxIter, default=10 * n)
tol0 = INPUT(option=tol, default=zero)
!!
!! iparam
!!
iparam(1) = 1 !! ishift
iparam(3) = maxIter0 !! maxiter
iparam(4) = 1 !! nb
iparam(7) = 3 !! mode
iparam(2) = 0 !! deprecated
iparam(5) = 0 !! out
iparam(6) = 0 !! iupd, deprecated
iparam(8) = 0 !! np, na
iparam(9:11) = 0 !! OUT
ipntr = 0
!!
!! make a copy of mat  in mat0
!! we will then form mat - sigma*I
!! then we will compute LU decomposition
!!
mat0 = mat
DO CONCURRENT(ii=1:n)
  mat0(ii, ii) = mat0(ii, ii) - sigma0
END DO
!!
CALL GetLU(A=mat0, IPIV=ipiv, INFO=info)
!!
DO
  CALL F77_SAUPD( &
    & ido=ido, bmat=bmat, n=n, which=which0, nev=nev, &
    & tol=tol0, resid=resid, ncv=ncv0, v=v, ldv=ldv, &
    & iparam=iparam, ipntr=ipntr, workd=workd, workl=workl,  &
    & lworkl=lworkl, info=info)
  !!
  IF (info .NE. 0) THEN
    err_msg = SAUPD_ErrorMsg(info)
    CALL Display(err_msg, msg="", unitno=stdout)
    ans = 0.0_DFP
    EXIT
  END IF
  !!
  IF (ido .EQ. -1 .OR. ido .EQ. 1) THEN
    !!
    !! LU Solve
    !! mat0 * y = x
    !! x => WORKD(ipntr(1):ipntr(1)+N-1)
    !! y => WORKD(ipntr(2):ipntr(2)+N-1)
    !!
    WORKD(ipntr(2):ipntr(2) + N - 1) = &
      & WORKD(ipntr(1):ipntr(1) + N - 1)
    !!
    CALL LUSolve(A=mat0, B=WORKD(ipntr(2):ipntr(2) + N - 1), IPIV=ipiv)
    !!
    !! TODO check error
    !!
  ELSE
    EXIT
  END IF
END DO
!!
!! we are not getting rvec, therefore ldz=1,
!! othereise ldz = N
!!
CALL F77_SEUPD( &
  & rvec=.FALSE., howmny='All', SELECT=SELECT, &
  & d=d, z=v, ldz=1, sigma=sigma0, &
  & bmat=bmat, n=n, which=which0, nev=nev, tol=tol0, &
  & resid=resid, ncv=ncv0, v=v, ldv=ldv, &
  & iparam=iparam, ipntr=ipntr, workd=workd, &
  & workl=workl, lworkl=lworkl, info=info)
!!
IF (info .NE. 0) THEN
  err_msg = SEUPD_ErrorMsg(INFO)
  CALL Display(err_msg, msg="", unitno=stdout)
  ans = 0.0_DFP
ELSE
  ans = d(1)
END IF
!!
END PROCEDURE SymSmallestEigenVal1

END SUBMODULE Methods
