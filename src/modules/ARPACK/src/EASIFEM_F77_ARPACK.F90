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

MODULE EASIFEM_F77_ARPACK
USE GlobalData, ONLY: I4B, Real32, Real64
IMPLICIT NONE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE F77_SAUPD

  SUBROUTINE SSAUPD(ido, bmat, n, which, nev, tol, resid, ncv,  &
    & v, ldv, iparam, ipntr, workd, workl, lworkl, info)
    !!
    IMPORT :: I4B, Real32
    INTEGER(I4B), PARAMETER :: DFP = Real32
    INTEGER(I4B), INTENT(INOUT) :: ido
    CHARACTER(LEN=1), INTENT(IN) :: bmat
    INTEGER(I4B), INTENT(IN) :: n
    CHARACTER(LEN=2), INTENT(IN) :: which
    INTEGER(I4B), INTENT(IN) :: nev
    REAL(DFP), INTENT(IN) :: tol
    REAL(DFP), INTENT(INOUT) :: resid(:)
    INTEGER(I4B), INTENT(IN) :: ncv
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    INTEGER(I4B), INTENT(IN) :: ldv
    INTEGER(I4B), INTENT(INOUT) :: iparam(11)
    INTEGER(I4B), INTENT(INOUT) :: ipntr(11)
    REAL(DFP), INTENT(INOUT) :: workd(:)
    INTEGER(I4B), INTENT(IN) :: lworkl
    REAL(DFP), INTENT(INOUT) :: workl(:)
    INTEGER(I4B), INTENT(INOUT) :: info
  END SUBROUTINE SSAUPD

  SUBROUTINE DSAUPD(ido, bmat, n, which, nev, tol, resid, ncv,  &
    & v, ldv, iparam, ipntr, workd, workl, lworkl, info)
    !!
    IMPORT :: I4B, Real64
    INTEGER(I4B), PARAMETER :: DFP = Real64
    INTEGER(I4B) :: ido
    CHARACTER(LEN=1) :: bmat
    INTEGER(I4B) :: n
    CHARACTER(LEN=2) :: which
    INTEGER(I4B) :: nev
    REAL(DFP) :: tol
    REAL(DFP) :: resid(n)
    INTEGER(I4B) :: ncv
    REAL(DFP) :: v(n, ncv)
    INTEGER(I4B) :: ldv
    INTEGER(I4B) :: iparam(11)
    INTEGER(I4B) :: ipntr(11)
    REAL(DFP) :: workd(3 * n)
    INTEGER(I4B) :: lworkl
    REAL(DFP) :: workl(lworkl)
    INTEGER(I4B) :: info
  END SUBROUTINE DSAUPD

END INTERFACE F77_SAUPD

PUBLIC :: F77_SAUPD

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE F77_SEUPD

  SUBROUTINE SSEUPD(rvec, howmny, select, d, &
    & z, ldz, sigma, bmat,&
    & n, which, nev, tol,&
    & resid, ncv, v, ldv,&
    & iparam, ipntr, workd, workl,&
    & lworkl, info)
    !!
    USE GlobalData, ONLY: I4B, DFP => Real32, LGT
    !!
    LOGICAL(LGT) :: rvec
    CHARACTER(LEN=*) :: howmny
    INTEGER(I4B) :: ncv
    LOGICAL(LGT) :: select(:)
    INTEGER(I4B) :: nev
    REAL(DFP) :: d(nev)
    INTEGER(I4B) :: n
    REAL(DFP) :: z(n, nev)
    INTEGER(I4B) :: ldz
    REAL(DFP) :: sigma
    CHARACTER(LEN=*) :: bmat
    CHARACTER(LEN=*) :: which
    REAL(DFP) :: tol
    REAL(DFP) :: resid(n)
    REAL(DFP) :: v(n, ncv)
    INTEGER(I4B) :: ldv
    INTEGER(I4B) :: iparam(11)
    INTEGER(I4B) :: ipntr(11)
    REAL(DFP) :: workd(3 * n)
    INTEGER(I4B) :: lworkl
    REAL(DFP) :: workl(lworkl)
    INTEGER(I4B) :: info
  END SUBROUTINE SSEUPD

  SUBROUTINE DSEUPD(rvec, howmny, select, d, &
    & z, ldz, sigma, bmat,&
    & n, which, nev, tol,&
    & resid, ncv, v, ldv,&
    & iparam, ipntr, workd, workl,&
    & lworkl, info)
    !!
    USE GlobalData, ONLY: I4B, DFP => Real64, LGT
    !!
    LOGICAL(LGT) :: rvec
    CHARACTER(LEN=*) :: howmny
    INTEGER(I4B) :: ncv
    LOGICAL(LGT) :: select(:)
    INTEGER(I4B) :: nev
    REAL(DFP) :: d(nev)
    INTEGER(I4B) :: n
    REAL(DFP) :: z(n, nev)
    INTEGER(I4B) :: ldz
    REAL(DFP) :: sigma
    CHARACTER(LEN=*) :: bmat
    CHARACTER(LEN=*) :: which
    REAL(DFP) :: tol
    REAL(DFP) :: resid(n)
    REAL(DFP) :: v(n, ncv)
    INTEGER(I4B) :: ldv
    INTEGER(I4B) :: iparam(11)
    INTEGER(I4B) :: ipntr(11)
    REAL(DFP) :: workd(3 * n)
    INTEGER(I4B) :: lworkl
    REAL(DFP) :: workl(lworkl)
    INTEGER(I4B) :: info
  END SUBROUTINE DSEUPD

END INTERFACE F77_SEUPD

PUBLIC :: F77_SEUPD

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE EASIFEM_F77_ARPACK
