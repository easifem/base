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

SUBMODULE(Sym_Lapack_Method) LUMethods
USE BaseMethod, ONLY: Display, Input
USE F95_LAPACK, ONLY: SYTRF, LACPY, LAPMR, POTRF, SYTRS, SYTRI
USE F77_LAPACK, ONLY: SYCONV => LA_SYCONV
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     getLU
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetLU_1
CALL LACPY(A=A, B=LU, UPLO=UPLO)
CALL SYTRF(A=LU, UPLO=UPLO, IPIV=IPIV, INFO=INFO)
END PROCEDURE SymGetLU_1

!----------------------------------------------------------------------------
!                                                                     getLU
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetLU_2
CALL SYTRF(A=A, UPLO=UPLO, IPIV=IPIV, INFO=INFO)
END PROCEDURE SymGetLU_2

!----------------------------------------------------------------------------
!                                                                     getLU
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetLDL_1
CHARACTER(LEN=1) :: luplo
INTEGER(I4B) :: linfo, n, ii
INTEGER(I4B), ALLOCATABLE :: ipiv0(:)

n = SIZE(A, 1)
ALLOCATE (ipiv0(n))

luplo = INPUT(option=UPLO, default="U")

LU = 0.0_DFP
CALL LACPY(A=A, B=LU, UPLO=luplo)
CALL SYTRF(A=LU, UPLO=luplo, IPIV=ipiv0, INFO=linfo)
CALL SYCONV(UPLO=luplo, WAY="C", N=n, A=LU, LDA=n, &
  & IPIV=ipiv0, E=E, INFO=linfo)

DO CONCURRENT(ii=1:n)
  D(ii) = LU(ii, ii)
  LU(ii, ii) = 1.0_DFP
END DO

DO ii = 1, n
  IF (ipiv0(ii) .GT. 0 .AND. ipiv0(ii) .NE. ii) THEN
    ipiv0(ipiv0(ii)) = ii
  END IF
END DO

CALL LAPMR(X=LU, K=ipiv0, FORWRD=.TRUE.)

IF (PRESENT(info)) info = linfo
IF (PRESENT(IPIV)) IPIV = ipiv0
DEALLOCATE (ipiv0)
END PROCEDURE SymGetLDL_1

!----------------------------------------------------------------------------
!                                                                     getLU
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetLDL_2
INTEGER(I4B) :: n, ii
INTEGER(I4B), ALLOCATABLE :: ipiv0(:)
CHARACTER(LEN=1) :: luplo

luplo = INPUT(default="U", option=UPLO)

n = SIZE(A, 1)
ALLOCATE (ipiv0(n))

! make some entries of A to zero

IF (luplo .EQ. "U") THEN
  DO CONCURRENT(ii=1:n - 1)
    A(ii + 1:, ii) = 0.0_DFP
  END DO
ELSE
  DO CONCURRENT(ii=2:n)
    A(:ii - 1, ii) = 0.0_DFP
  END DO
END IF

CALL SYTRF(A=A, UPLO=uplo, IPIV=ipiv0, INFO=info)
CALL SYCONV(UPLO=uplo, WAY="C", N=n, A=A, LDA=n, &
  & IPIV=ipiv0, E=E, INFO=info)

DO CONCURRENT(ii=1:n)
  D(ii) = A(ii, ii)
  A(ii, ii) = 1.0_DFP
END DO

DO ii = 1, n
  IF (ipiv0(ii) .GT. 0 .AND. ipiv0(ii) .NE. ii) THEN
    ipiv0(ipiv0(ii)) = ii
  END IF
END DO

CALL LAPMR(X=A, K=ipiv0, FORWRD=.TRUE.)

IF (PRESENT(IPIV)) IPIV = ipiv0
DEALLOCATE (ipiv0)
END PROCEDURE SymGetLDL_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetCholesky_1
!
! Make a copy of LU
!
CALL LACPY(A=A, B=LU, UPLO=uplo)
CALL POTRF(A=LU, uplo=uplo, info=info)
!
END PROCEDURE SymGetCholesky_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetCholesky_2
CALL POTRF(A=A, uplo=uplo, info=info)
END PROCEDURE SymGetCholesky_2

!----------------------------------------------------------------------------
!                                                                SymLUSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLUSolve_1
CALL SYTRS(A=A, B=B, IPIV=IPIV, UPLO=UPLO, INFO=INFO)
END PROCEDURE SymLUSolve_1

!----------------------------------------------------------------------------
!                                                                 SymLUSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLUSolve_2
CALL SYTRS(A=A, B=B, IPIV=IPIV, UPLO=UPLO, INFO=INFO)
END PROCEDURE SymLUSolve_2

!----------------------------------------------------------------------------
!                                                                 SymLUSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLUSolve_3
X = B
CALL SYTRS(A=A, B=X, IPIV=IPIV, UPLO=UPLO, INFO=INFO)
END PROCEDURE SymLUSolve_3

!----------------------------------------------------------------------------
!                                                                 SymLUSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLUSolve_4
X = B
CALL SYTRS(A=A, B=X, IPIV=IPIV, UPLO=UPLO, INFO=INFO)
END PROCEDURE SymLUSolve_4

!----------------------------------------------------------------------------
!                                                                     Inv
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetInv_1
CALL LACPY(A=A, B=invA, UPLO=UPLO)
CALL SYTRI(A=invA, IPIV=IPIV, UPLO=UPLO, info=INFO)
END PROCEDURE SymGetInv_1

!----------------------------------------------------------------------------
!                                                                     Inv
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetInv_2
CALL SYTRI(A=A, IPIV=IPIV, UPLO=UPLO, INFO=INFO)
END PROCEDURE SymGetInv_2

END SUBMODULE LUMethods
