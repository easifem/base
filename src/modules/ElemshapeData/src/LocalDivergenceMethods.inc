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

PUBLIC :: getLocalDivergence
PUBLIC :: LocalDivergence

!----------------------------------------------------------------------------
!                                       getLocalDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local Divergence of a vector

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_1(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:)
    !! local Divergence at integration points
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space nodal values of vector in `xiJ` format
    !! row index: space component
    !! col index: node number
  END SUBROUTINE elemsd_getLocalDivergence_1
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_1
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                          getLocalDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local Divergence of a vector
!
! $$
! \frac{\partial v_{i} }{\partial \xi_{j} } =v^{a}_{iI} T_{a}\frac
! {\partial N^{I}}{\partial \xi_{j} }
! $$
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_2(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:)
    !! local Divergence at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
    !! first index: space compo
    !! second index: space node
    !! third index: time node
  END SUBROUTINE elemsd_getLocalDivergence_2
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_2
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                      getLocalDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local Divergence of a vector
!
! $$
! \frac{\partial v_{i} }{\partial \xi_{j} } =v^{a}_{iI} T_{a}\frac
! {\partial N^{I}}{\partial \xi_{j} }
! $$
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_3(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:)
    !! local Divergence of vector at integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector finite-element variable
  END SUBROUTINE elemsd_getLocalDivergence_3
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_3
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                      getLocalDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local Divergence of a matrix

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_4(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! local Divergence at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space nodal values of matrix in (i,j,I) format
  END SUBROUTINE elemsd_getLocalDivergence_4
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_4
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                       getLocalDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local Divergence of a vector

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_5(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! local Divergence at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values of matrix in (i,j,I,a) format
  END SUBROUTINE elemsd_getLocalDivergence_5
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_5
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                       getLocalDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local Divergence of a vector

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_6(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! local Divergence at integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space/space-time nodal values of matrix in (i,j,I) format
  END SUBROUTINE elemsd_getLocalDivergence_6
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_6
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                      getLocalDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local Divergence
!
!# Introduction
!
! - This routine returns local Divergence in [[FEVariable_]]
! the input is also a [[FEVariable_]].
! - This routine can be considered as a master routine
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_7(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: lg
    !! local Divergence of scalar/vector/matrix at space integration points
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE elemsd_getLocalDivergence_7
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_7
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!                                      getLocalDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the Local Divergence

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalDivergence_8(obj, lg, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(INOUT) :: lg
    !! Local Divergence of scalar/vector/matrix at space-time
    !! integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space time nodal values of scalar/vector/matrix
  END SUBROUTINE elemsd_getLocalDivergence_8
END INTERFACE

INTERFACE getLocalDivergence
  MODULE PROCEDURE elemsd_getLocalDivergence_8
END INTERFACE getLocalDivergence

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION elemsd_LocalDivergence_1(obj, val) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION elemsd_LocalDivergence_1
END INTERFACE

INTERFACE localDivergence
  MODULE PROCEDURE elemsd_LocalDivergence_1
END INTERFACE localDivergence

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION elemsd_LocalDivergence_2(obj, val) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION elemsd_LocalDivergence_2
END INTERFACE

INTERFACE localDivergence
  MODULE PROCEDURE elemsd_LocalDivergence_2
END INTERFACE localDivergence
