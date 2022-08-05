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

PUBLIC :: LUSOLVE
PUBLIC :: LUTSOLVE

!----------------------------------------------------------------------------
!                                                    LUSOLVE@LUsolveMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jul 2021
! summary: This routine solves the LU x = y
!
! This routine solves the system `LU x = y`, given an LU decomposition of a
! matrix stored in (`ALU, JLU, JU`) modified sparse row format (MSR).
! This ALU, JLU, JU are created by calling ILUT methods described above

INTERFACE
MODULE SUBROUTINE csrMat_LUSOLVE( sol, rhs, alu, jlu, ju )
  REAL( DFP ), INTENT( INOUT ) :: sol( : )
  REAL( DFP ), INTENT( IN ) :: rhs( : )
  REAL( DFP ), INTENT( IN ) :: alu( : )
  INTEGER( I4B ), INTENT( IN ) :: jlu( : )
  INTEGER( I4B ), INTENT( IN ) :: ju( : )
END SUBROUTINE csrMat_LUSOLVE
END INTERFACE

INTERFACE LUSOLVE
MODULE PROCEDURE csrMat_LUSOLVE
END INTERFACE LUSOLVE

!----------------------------------------------------------------------------
!                                                             LUTSOLVE@ILUT
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jul 2021
! summary: This routine solves the (LU)^T x = y
!
! This routine solves the system `(LU)^T x = y`, given an LU decomposition of
! a matrix stored in (`ALU, JLU, JU`) modified sparse row format (MSR).
! This ALU, JLU, JU are created by calling ILUT methods described above

INTERFACE
MODULE SUBROUTINE csrMat_LUTSOLVE( sol, rhs, alu, jlu, ju )
  REAL( DFP ), INTENT( INOUT ) :: sol( : )
  REAL( DFP ), INTENT( IN ) :: rhs( : )
  REAL( DFP ), INTENT( IN ) :: alu( : )
  INTEGER( I4B ), INTENT( IN ) :: jlu( : )
  INTEGER( I4B ), INTENT( IN ) :: ju( : )
END SUBROUTINE csrMat_LUTSOLVE
END INTERFACE

INTERFACE LUTSOLVE
  MODULE PROCEDURE csrMat_LUTSOLVE
END INTERFACE LUTSOLVE


! !----------------------------------------------------------------------------
! !                                                          LSolve@LinAlg
! !----------------------------------------------------------------------------

! !> author: Vikas Sharma, Ph. D.
! ! date: 14 July 2021
! ! summary: Solve Lx = y by forward elimination technique will be used
! !
! !# Introduction
! ! This subroutine Solve Lx = y by forward elimination technique will be used
! ! Here L is lower triangular matrix with unit diag in CSR format
!
! INTERFACE
! MODULE SUBROUTINE csrMat_LSolve( obj, x, y )
!   TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
!     !! Sparse matrix
!   REAL( DFP ), INTENT( IN ) :: y( : )
!     !! This contains RHS
!   REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: x( : )
!     !! This contains solution
! END SUBROUTINE csrMat_LSolve
! END INTERFACE
!
! INTERFACE LSolve
!   MODULE PROCEDURE csrMat_LSolve
! END INTERFACE LSolve
!
! PUBLIC :: LSolve

!----------------------------------------------------------------------------
! !                                                           USolve@LinAlg
! !----------------------------------------------------------------------------

! !> author: Vikas Sharma, Ph. D.
! ! date: 14 July 2021
! ! summary: Solve Ux = y by backward elimination technique will be used
! !
! !# Introduction
! !- This subroutine solve Ux = y by backward elimination technique will be
! ! used
! ! - Here U is upper triangular matrix with unit diag in CSR format

! INTERFACE
! MODULE SUBROUTINE csrMat_USolve( obj, x, y )
!   TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
!     !! Sparse matrix in upper triangle form
!   REAL( DFP ), INTENT( IN ) :: y( : )
!     !! RHS
!   REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: x( : )
!     !! Solution
! END SUBROUTINE csrMat_USolve
! END INTERFACE

! INTERFACE USolve
!   MODULE PROCEDURE csrMat_USolve
! END INTERFACE USolve

! PUBLIC :: USolve