!-----------------------------------------------------------------
! FPL (Fortran Parameter List)
! Copyright (c) 2015 Santiago Badia, Alberto F. Martín,
! Javier Principe and Víctor Sande.
! All rights reserved.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3.0 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library.
!-----------------------------------------------------------------

MODULE DimensionsWrapper7D

USE DimensionsWrapper

IMPLICIT NONE
PRIVATE

TYPE, EXTENDS(DimensionsWrapper_t), ABSTRACT :: DimensionsWrapper7D_t
  PRIVATE
CONTAINS
  PROCEDURE(DimensionsWrapper7D_Set), DEFERRED :: Set
  PROCEDURE(DimensionsWrapper7D_Get), DEFERRED :: Get
  PROCEDURE(DimensionsWrapper7D_GetPointer), DEFERRED :: GetPointer
END TYPE

ABSTRACT INTERFACE
  SUBROUTINE DimensionsWrapper7D_Set(this, VALUE)
    IMPORT DimensionsWrapper7D_t
    CLASS(DimensionsWrapper7D_t), INTENT(INOUT) :: this
    CLASS(*), INTENT(IN) :: VALUE(:, :, :, :, :, :, :)
  END SUBROUTINE

  SUBROUTINE DimensionsWrapper7D_Get(this, VALUE)
    IMPORT DimensionsWrapper7D_t
    CLASS(DimensionsWrapper7D_t), INTENT(IN) :: this
    CLASS(*), INTENT(OUT) :: VALUE(:, :, :, :, :, :, :)
  END SUBROUTINE

  FUNCTION DimensionsWrapper7D_GetPointer(this) RESULT(VALUE)
    IMPORT DimensionsWrapper7D_t
    CLASS(DimensionsWrapper7D_t), TARGET, INTENT(IN) :: this
    CLASS(*), POINTER :: VALUE(:, :, :, :, :, :, :)
  END FUNCTION

  SUBROUTINE DimensionsWrapper7D_GetPolymorphic(this, VALUE)
    IMPORT DimensionsWrapper7D_t
    CLASS(DimensionsWrapper7D_t), INTENT(IN) :: this
    CLASS(*), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :, :, :, :, :, :)
  END SUBROUTINE
END INTERFACE

PUBLIC :: DimensionsWrapper7D_t

END MODULE DimensionsWrapper7D
