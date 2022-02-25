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

!> authors: Vikas Sharma, Ph. D.
! date: 13 Jul 2021
! summary: Input output related methods

SUBMODULE(CSRSparsity_Method) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_shape
  Ans = [ obj%nrow,  obj%ncol ]
END PROCEDURE csr_shape

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_size
  IF( PRESENT( Dims ) ) THEN
    IF( Dims .EQ. 1 ) THEN
      Ans = obj%nrow
    ELSE
      Ans = obj%ncol
    END IF
  ELSE
    Ans = obj%nrow * obj%ncol
  END IF
END PROCEDURE csr_size

!----------------------------------------------------------------------------
!                                                                      getNNZ
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_getNNZ
  INTEGER( I4B ) :: irow
  Ans = obj%nnz
END PROCEDURE csr_getNNZ


!----------------------------------------------------------------------------
!                                                                 getDiagonal
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_getDiagonal1
  INTEGER( I4B ) :: len0
  CALL Reallocate( diag, obj%nrow, idiag, obj%nrow )
  CALL GETDIA( &
    & obj%nrow,&
    & obj%ncol,&
    & 0,&
    & A,&
    & obj%JA,&
    & obj%IA,&
    & len0,&
    & diag,&
    & idiag,&
    & INPUT( option=offset, default=0 ))
END PROCEDURE csr_getDiagonal1

!----------------------------------------------------------------------------
!                                                               getDiagonal
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_getDiagonal2
  INTEGER( I4B ) :: ii
  !!
  IF( obj%isDiagStored ) THEN
    !!
    CALL Reallocate( diag, obj%nrow )
    DO ii = 1, SIZE( diag )
      diag( ii ) = A( obj%idiag( ii ) )
    END DO
    !!
  ELSE
    CALL Reallocate( diag, obj%nrow )
    CALL GETDIA( &
      & obj%nrow,&
      & obj%ncol,&
      & 0,&
      & A,&
      & obj%JA,&
      & obj%IA,&
      & ii,&
      & diag,&
      & obj%idiag,&
      & INPUT( option=offset, default=0 ))
    obj%isDiagStored = .TRUE.
  END IF
  !!
END PROCEDURE csr_getDiagonal2

END SUBMODULE GetMethods