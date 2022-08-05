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

PUBLIC :: getDivergence
PUBLIC :: Divergence

!----------------------------------------------------------------------------
!                                       getDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence of a vector

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getDivergence_1(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:)
    !!  Divergence at integration points
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space nodal values of vector in `xiJ` format
    !! row index: space component
    !! col index: node number
  END SUBROUTINE elemsd_getDivergence_1
END INTERFACE

INTERFACE getDivergence
  MODULE PROCEDURE elemsd_getDivergence_1
END INTERFACE getDivergence

!----------------------------------------------------------------------------
!                                          getDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence of a vector
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getDivergence_2(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:)
    !!  Divergence at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
  END SUBROUTINE elemsd_getDivergence_2
END INTERFACE

INTERFACE getDivergence
  MODULE PROCEDURE elemsd_getDivergence_2
END INTERFACE getDivergence

!----------------------------------------------------------------------------
!                                      getDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence of a vector
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getDivergence_3(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:)
    !!  Divergence of vector at integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector finite-element variable
  END SUBROUTINE elemsd_getDivergence_3
END INTERFACE

INTERFACE getDivergence
  MODULE PROCEDURE elemsd_getDivergence_3
END INTERFACE getDivergence

!----------------------------------------------------------------------------
!                                      getDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence of a matrix

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getDivergence_4(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !!  Divergence at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space nodal values of matrix in (i,j,I) format
  END SUBROUTINE elemsd_getDivergence_4
END INTERFACE

INTERFACE getDivergence
  MODULE PROCEDURE elemsd_getDivergence_4
END INTERFACE getDivergence

!----------------------------------------------------------------------------
!                                       getDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence of a vector

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getDivergence_5(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !!  Divergence at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values of matrix in (i,j,I,a) format
  END SUBROUTINE elemsd_getDivergence_5
END INTERFACE

INTERFACE getDivergence
  MODULE PROCEDURE elemsd_getDivergence_5
END INTERFACE getDivergence

!----------------------------------------------------------------------------
!                                       getDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence of a vector

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getDivergence_6(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !!  Divergence at integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space/space-time nodal values of matrix in (i,j,I) format
  END SUBROUTINE elemsd_getDivergence_6
END INTERFACE

INTERFACE getDivergence
  MODULE PROCEDURE elemsd_getDivergence_6
END INTERFACE getDivergence

!----------------------------------------------------------------------------
!                                      getDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getDivergence_7(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: lg
    !!  Divergence of scalar/vector/matrix at space integration points
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE elemsd_getDivergence_7
END INTERFACE

INTERFACE getDivergence
  MODULE PROCEDURE elemsd_getDivergence_7
END INTERFACE getDivergence

!----------------------------------------------------------------------------
!                                      getDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getDivergence_8(obj, lg, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(INOUT) :: lg
    !!  Divergence of scalar/vector/matrix at space-time
    !! integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space time nodal values of scalar/vector/matrix
  END SUBROUTINE elemsd_getDivergence_8
END INTERFACE

INTERFACE getDivergence
  MODULE PROCEDURE elemsd_getDivergence_8
END INTERFACE getDivergence

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION elemsd_Divergence_1(obj, val) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION elemsd_Divergence_1
END INTERFACE

INTERFACE Divergence
  MODULE PROCEDURE elemsd_Divergence_1
END INTERFACE Divergence

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION elemsd_Divergence_2(obj, val) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION elemsd_Divergence_2
END INTERFACE

INTERFACE Divergence
  MODULE PROCEDURE elemsd_Divergence_2
END INTERFACE Divergence
