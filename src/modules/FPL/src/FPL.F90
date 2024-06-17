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

#define ParameterList_t ParameterList_
#define ParameterListIterator_t ParameterListIterator_

MODULE FPL
USE ParameterList
USE WrapperFactoryListSingleton
PRIVATE 
PUBLIC :: ParameterList_t, ParameterListIterator_t
PUBLIC :: FPL_Init
PUBLIC :: FPL_Finalize

CONTAINS

!----------------------------------------------------------------------------
!                                                                 
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-02
! summary:         Initialize FPL

SUBROUTINE FPL_Init()
  CALL TheWrapperFactoryList_Init()
END SUBROUTINE FPL_Init

!----------------------------------------------------------------------------
!                                                                 
!----------------------------------------------------------------------------

SUBROUTINE FPL_Finalize()
  CALL TheWrapperFactoryList%Free()
END SUBROUTINE FPL_Finalize

END MODULE FPL
