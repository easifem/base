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

MODULE ElemshapeData_DivergenceMethods
USE BaseType, ONLY: ElemShapeData_, STElemshapeData_, FEVariable_
USE GlobalData, ONLY: DFP, I4B, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: GetDivergence
PUBLIC :: Divergence

!----------------------------------------------------------------------------
!                                       GetDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence of a vector

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_1(obj, val, ans, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space nodal values of vector in `xiJ` format
    !! row index: space component
    !! col index: node number
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !!  Divergence at integration points
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of ans
  END SUBROUTINE elemsd_GetDivergence_1
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                          GetDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence of a vector

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_2(obj, val, ans, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
    !! spaceComponent
    !! number of nodes in space
    !! number of nodes in time
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !!  Divergence at integration points
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE elemsd_GetDivergence_2
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                      GetDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence of a vector
!
INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_3(obj, val, ans, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector finite-element variable
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !!  Divergence of vector at integration points
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of ans
  END SUBROUTINE elemsd_GetDivergence_3
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                      GetDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence of a matrix

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_4(obj, val, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space nodal values of matrix in (i,j,I) format
    !! dim1 =  component
    !! dim2 = component
    !! dim3 = nns
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !!  Divergence at integration points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE elemsd_GetDivergence_4
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                       GetDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence of a vector

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_5(obj, val, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values of matrix in (i,j,I,a) format
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !!  Divergence at integration points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns of ans
  END SUBROUTINE elemsd_GetDivergence_5
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                       GetDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence of a vector

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_6(obj, val, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space/space-time nodal values of matrix in (i,j,I) format
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !!  Divergence at integration points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns of ans
  END SUBROUTINE elemsd_GetDivergence_6
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                      GetDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_7(obj, val, ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !!  Divergence of scalar/vector/matrix at space integration points
  END SUBROUTINE elemsd_GetDivergence_7
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                      GetDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the  Divergence

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_8(obj, val, ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space time nodal values of scalar/vector/matrix
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !!  Divergence of scalar/vector/matrix at space-time
    !! integration points
  END SUBROUTINE elemsd_GetDivergence_8
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Divergence
  MODULE PURE FUNCTION elemsd_Divergence_1(obj, val) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION elemsd_Divergence_1
END INTERFACE Divergence

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Divergence
  MODULE PURE FUNCTION elemsd_Divergence_2(obj, val) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION elemsd_Divergence_2
END INTERFACE Divergence

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ElemshapeData_DivergenceMethods
