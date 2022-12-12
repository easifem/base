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

MODULE SymUtility
USE GlobalData, ONLY: DFP, I4B, Real32, Real64, Int32, Int64, Int8, Int16
IMPLICIT NONE
PRIVATE
PUBLIC :: Sym
PUBLIC :: GetSym

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-12
! summary: Make symmetric matrix form lower or upper triangular matrix
!
!# Introduction
!
! This method returns a symmetric matrix from the lower or upper
! triangular part of a given square dense matrix.
!
! If `from = U`, then upper triangle part of mat is used
! If `from = L`, then lower triangle part of mat is used

INTERFACE
  !!
  MODULE PURE FUNCTION Sym_Int8(mat, from) RESULT(ans)
    INTEGER(Int8), INTENT(IN) :: mat(:, :)
    CHARACTER(LEN=1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
    INTEGER(Int8) :: ans(SIZE(mat, 1), SIZE(mat, 2))
  END FUNCTION Sym_Int8
  !!
  MODULE PURE FUNCTION Sym_Int16(mat, from) RESULT(ans)
    INTEGER(Int16), INTENT(IN) :: mat(:, :)
    CHARACTER(LEN=1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
    INTEGER(Int16) :: ans(SIZE(mat, 1), SIZE(mat, 2))
  END FUNCTION Sym_Int16
  !!
  MODULE PURE FUNCTION Sym_Int32(mat, from) RESULT(ans)
    INTEGER(Int32), INTENT(IN) :: mat(:, :)
    CHARACTER(LEN=1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
    INTEGER(Int32) :: ans(SIZE(mat, 1), SIZE(mat, 2))
  END FUNCTION Sym_Int32
  !!
  MODULE PURE FUNCTION Sym_Int64(mat, from) RESULT(ans)
    INTEGER(Int64), INTENT(IN) :: mat(:, :)
    CHARACTER(LEN=1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
    INTEGER(Int64) :: ans(SIZE(mat, 1), SIZE(mat, 2))
  END FUNCTION Sym_Int64
  !!
  MODULE PURE FUNCTION Sym_Real32(mat, from) RESULT(ans)
    REAL(Real32), INTENT(IN) :: mat(:, :)
    CHARACTER(LEN=1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
    REAL(Real32) :: ans(SIZE(mat, 1), SIZE(mat, 2))
  END FUNCTION Sym_Real32
  !!
  MODULE PURE FUNCTION Sym_Real64(mat, from) RESULT(ans)
    REAL(Real64), INTENT(IN) :: mat(:, :)
    CHARACTER(LEN=1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
    REAL(Real64) :: ans(SIZE(mat, 1), SIZE(mat, 2))
  END FUNCTION Sym_Real64
  !!
END INTERFACE

INTERFACE Sym
  MODULE PROCEDURE Sym_Int8, Sym_Int16, Sym_Int32, &
    & Sym_Int64, Sym_Real32, Sym_Real64
END INTERFACE Sym

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-12
! summary: Make symmetric matrix form lower or upper triangular matrix
!
!# Introduction
!
! This method returns a symmetric matrix from the lower or upper
! triangular part of a given square dense matrix.
!
! If `from = U`, then upper triangle part of mat is used
! If `from = L`, then lower triangle part of mat is used

INTERFACE
  MODULE PURE SUBROUTINE GetSym_Int8(mat, from)
    INTEGER(Int8), INTENT(INOUT) :: mat(:, :)
    CHARACTER(LEN=1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
  END SUBROUTINE GetSym_Int8
  !!
  MODULE PURE SUBROUTINE GetSym_Int16(mat, from)
    INTEGER(Int16), INTENT(INOUT) :: mat(:, :)
    CHARACTER(LEN=1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
  END SUBROUTINE GetSym_Int16
  !!
  MODULE PURE SUBROUTINE GetSym_Int32(mat, from)
    INTEGER(Int32), INTENT(INOUT) :: mat(:, :)
    CHARACTER(LEN=1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
  END SUBROUTINE GetSym_Int32
  !!
  MODULE PURE SUBROUTINE GetSym_Int64(mat, from)
    INTEGER(Int64), INTENT(INOUT) :: mat(:, :)
    CHARACTER(LEN=1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
  END SUBROUTINE GetSym_Int64
  !!
  MODULE PURE SUBROUTINE GetSym_Real32(mat, from)
    REAL(Real32), INTENT(INOUT) :: mat(:, :)
    CHARACTER(LEN=1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
  END SUBROUTINE GetSym_Real32
  !!
  MODULE PURE SUBROUTINE GetSym_Real64(mat, from)
    REAL(Real64), INTENT(INOUT) :: mat(:, :)
    CHARACTER(LEN=1), INTENT(IN) :: from
    !! from = "U", then upper triangular part must be provided
    !! from = "L", then lower triangular part must be provided
  END SUBROUTINE GetSym_Real64
END INTERFACE

INTERFACE GetSym
  MODULE PROCEDURE GetSym_Int8, GetSym_Int16, GetSym_Int32, &
    & GetSym_Int64, GetSym_Real32, GetSym_Real64
END INTERFACE GetSym

END MODULE SymUtility
