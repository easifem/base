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

SUBMODULE(CSRMatrix_SpectralMethods) Methods
USE String_Class, ONLY: String
USE CSRMatrix_Method, ONLY: MatVec, Size
USE GlobalData, ONLY: stdout, zero
USE BaseMethod, ONLY: Display, Input, SEUPD_ErrorMsg, SAUPD_ErrorMsg, &
  & F77_SEUPD, F77_SAUPD
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                      SymSmallestEigenVal
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLargestEigenVal1
CHARACTER(*), PARAMETER :: myName = "SymLargestEigenVal1"
!!
!! Internal variables
!!
INTEGER(I4B), PARAMETER :: nev = 1
INTEGER(I4B) :: ido, lworkl, ldv, info, iparam(11), ipntr(11), maxIter0, n, &
  & ncv0
CHARACTER(1), PARAMETER :: bmat = "I"
CHARACTER(2) :: which0
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
    CALL MatVec( &
     & obj=mat, &
     & x=workd(ipntr(1):ipntr(1) + n - 1), &
     & y=workd(ipntr(2):ipntr(2) + n - 1))
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
!                                                     SymLargestEigenVal
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLargestEigenVal2
CHARACTER(*), PARAMETER :: myName = "SymLargestEigenVal2"
!!
!! Internal variables
INTEGER(I4B) :: ido, lworkl, ldv, info, iparam(11), ipntr(11), maxIter0, n, &
  & ncv0
CHARACTER(1), PARAMETER :: bmat = "I"
CHARACTER(2) :: which0
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
    CALL MatVec( &
     & obj=mat, &
     & x=workd(ipntr(1):ipntr(1) + n - 1), &
     & y=workd(ipntr(2):ipntr(2) + n - 1))
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
!                                                      SymSmallestEigenVal
!----------------------------------------------------------------------------

MODULE PROCEDURE SymSmallestEigenVal1
CHARACTER(*), PARAMETER :: myName = "SymSmallestEigenVal1"
!!
!! Internal variables
!!
INTEGER(I4B), PARAMETER :: nev = 1
INTEGER(I4B) :: ido, lworkl, ldv, info, iparam(11), ipntr(11), maxIter0, n, &
  & ncv0, ii
CHARACTER(1), PARAMETER :: bmat = "I", uplo = "U"
CHARACTER(2) :: which0
REAL(DFP) :: tol0, d(nev), sigma0
REAL(DFP), ALLOCATABLE :: resid(:), v(:, :), workd(:), workl(:)
TYPE(String) :: err_msg
LOGICAL(LGT), ALLOCATABLE :: SELECT(:)
REAL(DFP) :: mat0(SIZE(mat, 1), SIZE(mat, 2))
INTEGER(I4B) :: ipiv(SIZE(mat, 1)), info1
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
CALL SymGetLU(A=mat0, IPIV=ipiv, UPLO=uplo, INFO=info1)
!!
IF (info1 .NE. 0) THEN
  CALL ErrorMsg( &
    & msg="Error occured in SymGetLU() errorCode="//tostring(info1), &
    & file=__FILE__, &
    & line=__LINE__, &
    & routine="SymSmallestEigenVal1()")
  STOP
END IF
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
    !!
    !!
    CALL SymLUSolve(A=mat0, B=WORKD(ipntr(2):ipntr(2) + N - 1),  &
      & IPIV=ipiv, UPLO=uplo, INFO=info1)
    !!
    IF (info1 .NE. 0) THEN
      CALL ErrorMsg( &
        & msg="Error occured in SymGetLU() errorCode="//tostring(info1), &
        & file=__FILE__, &
        & line=__LINE__, &
        & routine="SymSmallestEigenVal1()")
      STOP
    END IF
    !!
  ELSE
    EXIT
  END IF
END DO
!!
!! we are not getting rvec, therefore ldz=1,
!! othereise ldz = N
!!
IF (info .EQ. 0) THEN
  CALL F77_SEUPD( &
    & rvec=.FALSE., howmny='All', SELECT=SELECT, &
    & d=d, z=v, ldz=1, sigma=sigma0, &
    & bmat=bmat, n=n, which=which0, nev=nev, tol=tol0, &
    & resid=resid, ncv=ncv0, v=v, ldv=ldv, &
    & iparam=iparam, ipntr=ipntr, workd=workd, &
    & workl=workl, lworkl=lworkl, info=info)
    !!
  IF (info .NE. 0) THEN
    err_msg = SEUPD_ErrorMsg(info)
    CALL Display(err_msg, msg="", unitno=stdout)
    STOP
  ELSE
    ans = d(1)
  END IF
END IF
!!
END PROCEDURE SymSmallestEigenVal1

!----------------------------------------------------------------------------
!                                                      SymSmallestEigenVal
!----------------------------------------------------------------------------

MODULE PROCEDURE SymSmallestEigenVal2

END PROCEDURE SymSmallestEigenVal2

END SUBMODULE Methods
