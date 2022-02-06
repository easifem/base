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

PUBLIC :: MatVec
PUBLIC :: AMatVec
PUBLIC :: AtMatvec

!----------------------------------------------------------------------------
!                                                    AMatVec1@MatvecMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 july 2021
! summary: This routine computes y = A*x

INTERFACE
MODULE SUBROUTINE csrMat_AMatVec1( obj, x, y )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), INTENT( INOUT ) :: y( : )
END SUBROUTINE csrMat_AMatVec1
END INTERFACE

INTERFACE AMatVec
  MODULE PROCEDURE csrMat_AMatVec1
END INTERFACE AMatVec

!----------------------------------------------------------------------------
!                                                    AMatVec2@MatvecMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 july 2021
! summary: This routine computes y = A*x, A is in MSR format

INTERFACE
MODULE SUBROUTINE csrMat_AMatVec2( A, JA, x, y )
  REAL( DFP ) , INTENT( IN ) :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: JA( : )
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), INTENT( INOUT ) :: y( : )
END SUBROUTINE csrMat_AMatVec2
END INTERFACE

INTERFACE AMatVec
  MODULE PROCEDURE csrMat_AMatVec2
END INTERFACE AMatVec

!----------------------------------------------------------------------------
!                                                     AtMatvec@MatvecMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 july 2021
! summary: This routine computes y = A*x

INTERFACE
MODULE SUBROUTINE csrMat_AtMatvec( obj, x, y )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), INTENT( INOUT ) :: y( : )
END SUBROUTINE csrMat_AtMatvec
END INTERFACE

INTERFACE AtMatvec
  MODULE PROCEDURE csrMat_AtMatvec
END INTERFACE AtMatvec

!----------------------------------------------------------------------------
!                                                             Matvec@MatVec
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This routine performs matrix-vector multiplication
!
!# Introduction
! y = A*x

INTERFACE
MODULE SUBROUTINE csrMat_MatVec1( obj, x, y, transp )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), INTENT( INOUT ) :: y( : )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: transp
END SUBROUTINE csrMat_MatVec1
END INTERFACE

INTERFACE MatVec
  MODULE PROCEDURE csrMat_MatVec1
END INTERFACE MatVec

!----------------------------------------------------------------------------
!                                                             Matvec@MatVec
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This routine performs matrix-vector multiplication
!
!# Introduction
! y = A*x

INTERFACE
MODULE SUBROUTINE csrMat_MatVec2( A, JA, x, y )
  REAL( DFP ) , INTENT( IN ) :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: JA( : )
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), INTENT( INOUT ) :: y( : )
END SUBROUTINE csrMat_MatVec2
END INTERFACE

INTERFACE MatVec
  MODULE PROCEDURE csrMat_MatVec2
END INTERFACE MatVec
